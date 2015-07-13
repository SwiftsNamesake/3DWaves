-- |
-- Module      : Southpaw.WaveFront.Parsers
-- Description : descr
-- Copyright   : (c) Jonatan H Sundqvist, February 8 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
-- 
-- Created February 8 2015-- Wavefront - Parsers.hs
-- Migrated to separate project on February 21 2015

-- TODO | - Appropriate container types (eg. bytestring, vector)
--        - Grammar specification
--        - Incremental parsing (?)
--        - Improve naming scheme
--        - Separate MTL and OBJ parsers (?) (...)
--        - Separate parsing, IO and testing (...)
--        - Additional attributes (lighting, splines, etc.)
--        - FFI (...)
--        - Debugging information (line number, missing file, missing values, etc.) (...)
--        - Proper Haddock coverage, including headers (...)
--        - Model type (...)
--        - Caching (?)
--        - Performance, profiling, optimisations
--        - PrintfArg instances for the types defined in this module
--        - Reconciling Cabal and hierarchical modules
--        - Dealing with paths in lib statements (requires knowledge of working directories)
--        - Move comments and specification to separate files (eg. README)
--        - Inline comments (for internals, implementation)
--
--        - Parser bugs
--          -- Negative coordinates enclosed in parentheses
--
--        - Decide on a public interface (exports) (API)
--          -- Model will be the main API type
--          -- Export functions for working with the output data (eg. unzipIndices :: [(Int, Int, Int)] -> ([Int], [Int], [Int]))

-- SPEC | - 
--        - 



---------------------------------------------------------------------------------------------------
-- GHC Extensions
---------------------------------------------------------------------------------------------------
{-# LANGUAGE UnicodeSyntax #-}



---------------------------------------------------------------------------------------------------
-- Section
---------------------------------------------------------------------------------------------------
module Southpaw.WaveFront.Parsers (parseOBJ, parseMTL, loadOBJ, loadMTL, MTL(), OBJ()) where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Data.List (isPrefixOf)
import Data.Char (isSpace)

import Data.Either (rights, isLeft)

import Text.Printf   (printf)
import System.IO     (hFlush, stdout)
import Control.Monad (forM_)
-- import Control.Concurrent (threadDelay)

import Southpaw.Utilities.Utilities (split)

import qualified Data.Map as Map



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- OBJ parser types -------------------------------------------------------------------------------
-- | Represents a single (valid) OBJ token
--
-- TODO: Polymorphic numerical types (?)
-- TODO: Add context, metadata (eg. line numbers, filename) (?)
-- TODO: Naming scheme (added OBJ prefix to prevent name clashes; cf. Face type)
-- TODO: Comment token (preserve comments in parser output or remove them) (?)
data OBJToken = OBJVertex  Float Float Float |
                OBJNormal  Float Float Float |
                OBJTexture Float Float       |
                OBJFace [(Int, Int, Int)]    | -- TODO: Associate material with each face, handle absent indices

                UseMTL String | --  
                LibMTL String | -- TODO: Use actual MTL type

                Group  [String] | -- TODO: Do grouped faces have to be consecutive?
                Object [String]   -- TODO: What is the difference between group and object?
                deriving (Eq, Show) -- TODO: Derive Read (?)


-- |
-- TODO: Use error type instead of String, allowing us to distinguish invalid data
--       from eg. comments and blank lines (?)
type OBJRow = (Either String OBJToken, String)


-- | Output type of the OBJ parser. Currently a list of line number and token (or error string) pairs
--
-- TODO: Rename (?)
-- TODO: Use Integral for line number (?)
--
type OBJ = [(Int, OBJRow, String)]


-- MTL parser types -------------------------------------------------------------------------------
-- | Represents a single (valid) MTL token
--
-- TODO: Is the alpha channel optional, ignored, disallowed?
-- TODO: Include support for ('Ns', 'Ni', 'd', 'Tr', 'illum')
-- 
data MTLToken = Ambient  Float Float Float Float | -- Ka
                Diffuse  Float Float Float Float | -- Kd
                Specular Float Float Float Float | -- Ks

        MapDiffuse  String | -- map_Kd
        NewMaterial String   -- newmtl
        deriving (Eq, Show)


-- | Output type of the single-row MTL parser.
type MTLRow = (Either String MTLToken, String)


-- | Output type of the MTL parser. Currently a list of line number and token (or error string) pairs
--
-- TODO: Add type for processed MTL (eg. a map between names and materials)
--
type MTL = [(Int, MTLRow, String)] -- (line number, MTL token, comment)


-- |
type MTLTable = Map.Map String (Map.Map String Material)

-- |
type Colour = (Float, Float, Float, Float)


-- |
-- TODO: Do all materials have an ambient, a diffuse and a specular colour (?)
-- TODO: Support more attributes (entire spec) (?)
-- TODO: Lenses (?)
data Material = Material { ambient :: Colour, diffuse :: Colour, specular, texture :: Maybe String }



-- General types ----------------------------------------------------------------------------------
type Vector num = (num, num, num) -- Queen Vectoria
type Point  num = (num, num)      -- Haskell is no longer Point-free


-- API types --------------------------------------------------------------------------------------
-- |
-- TODO: Validation (eg. length ivertices == length == ivertices == length itextures if length isn't 0)
-- TOOD: Pack indices in a tuple (eg. indices :: [(Int, Int, Int)]) (?)
-- TOOD: Use (String, String) for the names of the mtl file and material instead of Material (?)
data Face = Face { indices :: ([Int], [Int], [Int]), material :: Material }


-- | Abstract representation of an OBJ model with associated MTL definitions.
-- 
--
-- TODO: Rename (?)
-- TODO: Include metadata, comments, rejected data (?)
-- TODO: Separate type for processed OBJTokens (ie. token + context)
-- TODO: Perform index lookups (?)
-- TODO: Reconsider the types (especially of the materials)
--
data Model = Model { vertices  :: [Vector Float],
                     normals   :: [Vector Float],
                     textures  :: [Point  Float],
                     faces     :: [Face],
                     materials :: Map.Map String (Map.Map String Material), -- TODO: Type synonym (?)
                     groups    :: Map.Map String (Int, Int),
                     objects   :: Map.Map String (Int, Int)
                   } deriving (Show)



---------------------------------------------------------------------------------------------------
-- Functions (pure)
---------------------------------------------------------------------------------------------------
-- Parsers ----------------------------------------------------------------------------------------
-- | This function creates an OBJToken or error for each line in the input data
-- 
-- TODO: Use appropriate container type (cf. TODO section)
-- TODO: Extract filter predicate (isComment, isEmpty)
-- TODO: Is it even necessary to strip whitespace?
-- TODO: Function for composing predicates (?)
-- TODO: Should this function separate the various fields (eg. [(Vertices, Faces, Materials, Groups)] instead of [Maybe OBJToken])
--
parseOBJ :: String -> OBJ
parseOBJ = enumerate . map parseOBJRow . lines -- . rows


-- | Generates a token given a single valid OBJ row, or an error value if the input is malformed. 
--
-- TODO: Correctness (complete function, no runtime exceptions)
-- TODO: Rename 'which' (?)
-- TODO: Handle invalid rows (how to deal with mangled definitions w.r.t indices?)
-- TODO: Extract value parsing logic (eg. pattern matching, converting, handle errors)
-- TODO: Named errors (typed?) rather than Nothing (cf. Either) (?)
-- TODO: Additional values, currently unsupported attributes (ignore?) (pattern match against the entire line, eg. ["vn", x, y, z])
-- TODO: Dealing with MTL definitions (pass in names, MTL value, return list of MTL dependencies)
-- TODO: Take 1-based indexing into account straight away (?)
-- TODO: Deal with absent texture and normal indices
-- TODO: Strip trailing comments (✓)
-- TODO: Don't ignore leftover values (errors?)
--
parseOBJRow :: String -> OBJRow -- Maybe OBJToken
parseOBJRow ln
  | isComment ln || null ln = Left ln
  | otherwise               = withoutComment ln $ \ tokens -> let (which:values) = words tokens in case which of
    "v"  -> vector OBJVertex values -- Vertex
    "vn" -> vector OBJNormal values -- Normal
    -- TODO: Clean this up
    -- TODO: More generic way of unpacking the right number of values and applying read (?)
    "vt" -> let (x:y:[]) = values in Right $ OBJTexture (read x) (read y) -- Texture
    -- TODO: Clean this up
    -- TODO: Handle invalid data (✓)
    -- TODO: Capture invalid vertex definitions (cf. sequence) (✓)
    -- TODO: Deal with missing indices some other way (reflect it in the output somehow, using the Maybe type?)
    -- TODO: More generic way of unpacking the right number of values and applying read (?)
    -- We append two additional indices to the index list, since `triplet` expects a three-tuple. Otherwise, omitting the optional
    -- texture and normal indices would lead to an error.

    "f"  -> either (Left . const ln) (Right . Face) . sequence . map (vector triplet . take 3 . (++ ["1", "1"]) . split '/') $ values -- Face
    "g"  -> Right . Group  $ values -- Group
    "o"  -> Right . Object $ values -- Object
    "s"  -> Left ln                 -- Smooth shading
    "mtllib" -> Right . LibMTL $ head values --
    "usemtl" -> Right . UseMTL $ head values --
    _        -> Left ln -- TODO More informative errors
    where triplet a b c = (a, b, c) -- TODO: Use tuple sections (?)


-- |
-- process the OBJ tokens
parseMTL :: String -> MTL
parseMTL = enumerate . map parseMTLRow . lines


-- | 
-- process the MTL tokens
-- TODO: cf. parseOBJRow
parseMTLRow :: String -> MTLRow
parseMTLRow ln
  | isComment ln || null ln = Left ln
  | otherwise               = withoutComment ln $ \ tokens -> let (which:values) = words tokens in case which of
    "Ka" -> withChannels Ambient  values -- Ka
    "Kd" -> withChannels Diffuse  values -- Kd
    "Ks" -> withChannels Specular values -- Ks
    "map_Kd" -> withName MapDiffuse values -- map_Kd
    "newmtl" -> withName Material   values -- newmtl
    _        -> Left ln
    where withChannels token [r,g,b,a] = Right $ token (read r) (read g) (read b) (read a) --
          withChannels token [r,g,b]   = Right $ token (read r) (read g) (read b) (1.0)    -- Alpha is 1.0 by default (use Maybe instead?)
          withChannels _      _        = Left  $ "Pattern match failed"

          withName token (name:[]) = Right $ token name
          withName _      _        = Left  $ "Pattern match failed"


-- | 
-- TODO: Use map for materials (?)
-- TODO: How to retrieve MTL data
-- TODO: How to deal with errors, including no-parse, index errors, etc.
-- TODO: Performance, how are 'copies' of coordinates handled (?)
-- TODO: Performance, one pass (with a fold perhaps)
-- TODO: Use a more efficient data structure (especially w.r.t indexing; cf. Vector)
-- TODO: Consider preserving the indices (rather than generating a list of duplicated vertices).
--       This would preserve space (in cases where vertices are often re-used), as well as being
--       very compatible with index buffers on graphics cards.
--
-- TODO: Keep map of materials and list of textures in final output (inside model or as items in a tuple) (?)
--
-- I never knew pattern matching in list comprehensions could be used to filter by constructor
-- let rows = parseOBJ data in ([ v | @v(Vertex {}) <- rows], [ v | @v(Vertex {}) <- rows])
createModel :: OBJ -> MTLTable -> Model
createModel modeldata materials = let tokens       = rights . map snd $ modeldata -- TODO: Vat do vee du viz ze dissidents, kommandant?
                                     theMaterials = retrieve [ name | UseMTL name   <- tokens ] -- Retrieve MTL data
                                     theFaces     = [ face    | face@(Face{})       <- tokens ]
                                     theGroups    = [ group   | group@(Face{})      <- tokens ]
                                     theObjects   = [ object  | object@(Face{})     <- tokens ]
                                     theSelects   = [ select  | select@(Face{})     <- tokens ]
                                     theObject    = [ object  | object@(Face{})     <- tokens ]
                                  in Model { vertices  = [ vertex | vertex@(Vertex{})    <- tokens ],
                                             normals   = [ normal | normal@(Normal{})    <- tokens ],
                                             textures  = [ texture | texture@(Texture{}) <- tokens ],
                                             faces     = theFaces,
                                             groups    = theGroups,
                                             objects   = theObjects,
                                             materials = theMaterials } 


-- Parsing utilities ------------------------------------------------------------------------------
-- | Predicate for determining if a String is a comment. Comments are preceded by a '#' and any
-- number of whitespace characters (not including linebreaks). Support for comments at the end
-- of a line has yet to be added.
--
-- TODO: Drop comments at the end of a line (?)
-- TODO: Add stripComment (or extractComment) which consumes a line up until the first '#'.
-- This would allow for tokens and comments to appear on the same line.
isComment :: String -> Bool
isComment = isPrefixOf "#" . dropWhile isSpace


-- | Strips a trailing comment from an MTL or OBJ line.
dropComment :: String -> String
dropComment = takeWhile (/= '#')


-- |
takeComment :: String -> String
takeComment = dropWhile (/= '#')


-- |
withoutComment :: String -> (String -> a) -> (a, String)
withoutComment row parse = let (tokens, comment) = span (/= '#') in (parse tokens, comment)


-- |
enumerate :: [(token, comment)] -> [(Int, token, comment)]
enumerate = zipWith [1..] prepend
  where prepend n (token, comment) = (n, token, comment)


-- | Splits a string into rows and filters out unimportant elements (empty lines and comments)
-- NOTE: This function is probably obsolete due to comments being included by the parsers
-- TODO: Higher order function for composing predicates
rows :: String -> [String]
rows = filter (not . satisfiesAny [null, isComment]) . lines
  where satisfiesAny predicates x = any ($ x) predicates

-- |
-- TODO: Use readMaybe (?)
-- TODO: Variadic 'unpacking' (or is that sinful?)
-- TODO: Rename (?)
vector :: Read r => (r -> r -> r -> b) -> [String] -> Either String b
vector token (x:y:z:[]) = Right $ token (read x) (read y) (read z) -- TODO: Add back the Maybe wrapper (?)
vector _      _         = Left  $ "Pattern match failed"



---------------------------------------------------------------------------------------------------
-- Functions (IO)
---------------------------------------------------------------------------------------------------
-- Loading data -----------------------------------------------------------------------------------
-- |
--
-- TODO: Use bytestrings (?)
--
loadOBJ :: String -> IO OBJ
loadOBJ fn = do
  rawOBJ <- readFile fn    --
  return $ parseOBJ rawOBJ --


-- |
--
-- TODO: Use bytestrings (?)
-- TODO: Merge OBJ and MTL parsers (and plug in format-specific code as needed) (?)
--
loadMTL :: String -> IO MTL
loadMTL fn = do
  rawMTL <- readFile fn    -- Unparsed MTL data (text)
  return $ parseMTL rawMTL --


-- |
-- Loads an OBJ model from file, including associated materials
loadModel :: String -> IO Model
loadModel fn = do
  obj <- loadOBJ fn
  return $ error "Not done yet"



-- General utilities ------------------------------------------------------------------------------
-- | Counts the number of elements that satisfy the predicate
count :: (a -> Bool) -> [a] -> Int
count p = length . filter p



-- IO utilities -----------------------------------------------------------------------------------
-- | 
promptContinue :: String -> IO ()
promptContinue prompt = do
  putStr prompt
  hFlush stdout
  getChar
  putChar '\n'



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "This is where the checks should be."

  let path = "C:/Users/Jonatan/Desktop/Python/experiments/WaveFront/"
  
  forM_ ["queen", "cube"] $ \ fn -> do
    printf "\nParsing OBJ file: %s.obj\n" fn
    model <- loadOBJ $ printf (path ++ "data/%s.obj") fn
    printf "Found %d invalid rows in OBJ file (m comments, n blanks, o errors).\n" (count isLeft $ map snd model)

    promptContinue "Press any key to continue..."

    mapM_ print ["[" ++ show n ++ "] " ++ show token | (n, Right token) <- model ]
    -- TODO: Print culprit lines (✓)

    promptContinue "Press any key to continue..."

    printf "\nParsing MTL file: %s.mtl\n" fn
    materials <- loadMTL $ printf (path ++ "data/%s.mtl") fn
    printf "Found %d invalid rows in MTL file (m comments, n blanks, o errors).\n" (count isLeft $ map snd materials)
    mapM_ print ["[" ++ show n ++ "] " ++ show token | (n, Right token) <- materials ]

    promptContinue "Press any key to continue..."