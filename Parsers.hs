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
--          -- Export certain utilities (eg. second, perhaps in another module) (?)

-- SPEC | - 
--        - 



---------------------------------------------------------------------------------------------------
-- GHC Extensions
---------------------------------------------------------------------------------------------------
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}



---------------------------------------------------------------------------------------------------
-- Section
---------------------------------------------------------------------------------------------------
module Southpaw.WaveFront.Parsers (parseOBJ, parseMTL,
                                   loadOBJ, loadMTL,
                                   loadModel,
                                   MTL(), OBJ(), Model(..), Face(..),
                                   createModel) where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Data.List   (groupBy)
import Data.Maybe  (listToMaybe)
import Data.Either (rights, isLeft)

import Text.Printf (printf)
import Text.Read   (readMaybe, readEither)

import System.IO     (hFlush, stdout)
import Control.Monad (forM_)
-- import Control.Concurrent (threadDelay)

import Southpaw.Utilities.Utilities (pairwise, cuts)
import Southpaw.WaveFront.Utilities

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
data OBJToken = OBJVertex  Float Float Float          |
                OBJNormal  Float Float Float          |
                OBJTexture Float Float                |
                OBJFace [(Int, Maybe Int, Maybe Int)] | -- TODO: Associate material with each face, handle absent indices

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
type OBJ = [(Int, Either String OBJToken, String)]


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
type MTL = [(Int, Either String MTLToken, String)] -- (line number, MTL token, comment)


-- |
type MTLTable = Map.Map String (Map.Map String Material)


-- General types ----------------------------------------------------------------------------------
type Vector num = (num, num, num) -- Queen Vectoria
type Point  num = (num, num)      -- Haskell is no longer Point-free


-- API types --------------------------------------------------------------------------------------
-- |
-- TODO: Validation (eg. length ivertices == length == ivertices == length itextures if length isn't 0)
-- TOOD: Pack indices in a tuple (eg. indices :: [(Int, Int, Int)]) (?)
-- TOOD: Use (String, String) for the names of the mtl file and material instead of Material (?)
data Face = Face { indices :: [(Int, Maybe Int, Maybe Int)], material :: Material } deriving (Show)


-- |
type Colour = (Float, Float, Float, Float)


-- |
-- TODO: Do all materials have an ambient, a diffuse and a specular colour (?)
-- TODO: Support more attributes (entire spec) (?)
-- TODO: Lenses (?)
data Material = Material { ambient :: Colour, diffuse :: Colour, specular :: Colour, texture :: Maybe String } deriving (Show)


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
                     groups    :: Map.Map [String] (Int, Int), -- TODO: Type synonym
                     objects   :: Map.Map [String] (Int, Int)  -- TODO: Type synonym
                   } deriving (Show)



---------------------------------------------------------------------------------------------------
-- Functions (pure)
---------------------------------------------------------------------------------------------------
-- OBJ parsing ------------------------------------------------------------------------------------
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
parseOBJRow ln = withoutComment ln $ \ tokens -> let (which:values) = words tokens in case which of
    "v"  -> vector OBJVertex values -- Vertex
    "vn" -> vector OBJNormal values -- Normal
    "vt" -> texture values -- Texture
    "f"  -> either (Left . const ln) (Right . OBJFace) . sequence . map (vertex . cuts '/') $ values -- Face
    "g"  -> Right . Group  $ values -- Group
    "o"  -> Right . Object $ values -- Object
    "s"  -> Left ln                 -- Smooth shading
    "mtllib" -> Right . LibMTL $ head values --
    "usemtl" -> Right . UseMTL $ head values --
    _        -> Left ln -- TODO More informative errors
    where vertex [svi, sti, sni] = readEither svi >>= \ vi -> Right $ (vi, readMaybe sti, readMaybe sni) --
          vertex indices         = Left  $ "Face vertex with too many indices: " ++ show indices         --
          texture [sx, sy] = sequence (map readEither [sx, sy]) >>= \ [x, y] -> Right $ OBJTexture x y   -- TOOD: Refactor
          texture  _       = Left $ "Texture token with the wrong number of coordinates: " ++ show values 


-- MTL parsing ------------------------------------------------------------------------------------
-- |
-- process the OBJ tokens
parseMTL :: String -> MTL
parseMTL = enumerate . map parseMTLRow . lines


-- | 
-- process the MTL tokens
-- TODO: cf. parseOBJRow
parseMTLRow :: String -> MTLRow
parseMTLRow ln = withoutComment ln $ \ tokens -> let (which:values) = words tokens in case which of
    "Ka" -> withChannels Ambient  values -- Ka
    "Kd" -> withChannels Diffuse  values -- Kd
    "Ks" -> withChannels Specular values -- Ks
    "map_Kd" -> withName MapDiffuse values  -- map_Kd
    "newmtl" -> withName NewMaterial values -- newmtl
    _        -> Left ln
    where withChannels token [r,g,b,a] = Right $ token (read r) (read g) (read b) (read a) --
          withChannels token [r,g,b]   = Right $ token (read r) (read g) (read b) (1.0)    -- Alpha is 1.0 by default (use Maybe instead?)
          withChannels _      _        = Left  $ "Pattern match failed"

          withName token (name:[]) = Right $ token name
          withName _      _        = Left  $ "Pattern match failed"


-- Parser output churners (OBJ) -------------------------------------------------------------------
-- | Creates a mapping between group names and the corresponding bounds ([lower, upper)). Invalid
--   tokens are simply discarded by this function.
--
-- TODO: Figure out how to deal with multiple group names (eg. "g mesh1 nose head")
groupsOf :: [OBJToken] -> Map.Map [String] (Int, Int)
groupsOf = buildIndexMapWith . filter notObject
  where notObject (Object _) = False
        notObject  _         = True


-- |
objectsOf :: [OBJToken] -> Map.Map [String] (Int, Int)
objectsOf = buildIndexMapWith . filter notGroup
  where notGroup (Group _) = False
        notGroup  _        = True


-- | Creates a mapping between names (of groups or objects) to face indices
buildIndexMapWith :: [OBJToken] -> Map.Map [String] (Int, Int)
buildIndexMapWith tokens = Map.fromList . pairwise zipIndices . reverse . addLastIndex $ foldl update (0, []) $ tokens
  where addLastIndex (nfaces, groups') = ([], nfaces):groups'
        zipIndices (names, low) (_, upp) = (names, (low, upp))
        update (nfaces, groups') token = case token of
          Group   names -> (nfaces,   (names, nfaces):groups')
          Object  names -> (nfaces,   (names, nfaces):groups')
          OBJFace _     -> (nfaces+1, groups')
          _             -> (nfaces, groups')


-- |
-- 
-- TODO: Don't use foldl (?)
-- TODO: Deal with errors (eg. missing materials)
-- TODO: Improve naming scheme (lots of primes)
facesOf :: [OBJToken] -> MTLTable -> [Either String Face]
facesOf tokens materials = reverse . third . foldl update ("", "", []) $ tokens
  where retrieve lib mat       = Map.lookup lib materials >>= Map.lookup mat
        createFace lib mat ind = case retrieve lib mat of
                                   Nothing -> Left  $ "No such material: " ++ lib ++ "." ++ mat
                                   Just m  -> Right $ Face { indices=ind, material=m }
        update (lib', material', faces') token = case token of
                                                   OBJFace ind -> (lib', material', createFace lib' material' ind : faces')
                                                   LibMTL  lib -> (lib,  material', faces')
                                                   UseMTL  mat -> (lib', mat,       faces')


-- Parser output churners (MTL) -------------------------------------------------------------------
-- | Constructs a map between names and materials. Partially or wholly undefined materials
--   are mapped to a string detailing the error (eg. Left "missing specular").
--
-- TODO: Debug information (eg. attributes without an associated material)
-- TODO: Pass in error function (would allow for more flexible error handling) (?)
-- TODO: Filter out parser failures (?)
-- TOOD: Deal with duplicated attributes (probably won't crop up in any real situations)
materialsOf :: [MTLToken] -> Map.Map String (Either String Material)
materialsOf tokens = Map.fromList . rights $ map createMaterial groups
 where groups = groupBy (const . isnew) tokens
       isnew (NewMaterial _) = True  -- TODO: Rename isnew
       isnew  _              = False
       createMaterial (NewMaterial name:attrs) = Right $ (name, fromAttributes attrs)
       createMaterial  attrs                   = Left  $ "Free-floating attributes: " ++ show attrs
       fromAttributes  attrs
         | any null colours = Left  $ "Missing colour(s)" -- TODO: More elaborate message (eg. which colour)
         | otherwise        = Right $ Material { ambient=head a, diffuse=head d, specular=head s, texture=listToMaybe [ name | MapDiffuse name <- attrs ] }
         where colours@[d, s, a] = [[ (r, g, b, a) | Diffuse  r g b a <- attrs ],
                                    [ (r, g, b, a) | Specular r g b a <- attrs ],
                                    [ (r, g, b, a) | Ambient  r g b a <- attrs ]]


-- |
-- TODO: Debug information (eg. how many invalid materials were filtered out)
-- TODO: Refactor, simplify
createMTLTable :: [(String, [MTLToken])] -> MTLTable
createMTLTable mtls = Map.fromList . map (\ (name, tokens) -> (name, Map.mapMaybe prune . materialsOf $ tokens)) $ mtls
  where prune (Right mat) = Just mat
        prune (Left  _)   = Nothing

-- API functions ----------------------------------------------------------------------------------
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
createModel tokens materials = let modeldata  = rights $ map second tokens -- TODO: Vat do vee du viz ze dissidents, kommandant?
                               in Model { vertices  = [ (x, y, z) | OBJVertex  x y z <- modeldata ],
                                          normals   = [ (x, y, z) | OBJNormal  x y z <- modeldata ],
                                          textures  = [ (x, y)    | OBJTexture x y   <- modeldata ],
                                          faces     = rights $ facesOf modeldata materials,
                                          groups    = groupsOf  modeldata,
                                          objects   = objectsOf modeldata,
                                          materials = materials } 



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
-- TODO: Better names (than 'mtls' and 'fns') (?)
-- TODO: Refactor, simplify
loadMaterials :: [String] -> IO MTLTable
loadMaterials fns = do
  mtls <- mapM loadMTL fns --
  return . createMTLTable . zip fns . map tokensOf $ mtls
  where tokensOf = rights . map second


-- |
-- Loads an OBJ model from file, including associated materials
loadModel :: String -> IO Model
loadModel fn = do
  obj       <- loadOBJ fn
  materials <- loadMaterials [ name | LibMTL name <- rights $ map second obj ]
  return $ createModel obj materials
  where loadWithName name = loadMTL name >>= return . (name,) 


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
    printf "Found %d invalid rows in OBJ file (m comments, n blanks, o errors).\n" (count isLeft $ map second model)

    promptContinue "Press any key to continue..."

    mapM_ print ["[" ++ show n ++ "] " ++ show token | (n, Right token, comment) <- model ]
    -- TODO: Print culprit lines (✓)

    promptContinue "Press any key to continue..."

    printf "\nParsing MTL file: %s.mtl\n" fn
    materials <- loadMTL $ printf (path ++ "data/%s.mtl") fn
    printf "Found %d invalid rows in MTL file (m comments, n blanks, o errors).\n" (count isLeft $ map second materials)
    mapM_ print ["[" ++ show n ++ "] " ++ show token | (n, Right token, comment) <- materials ]

    promptContinue "Press any key to continue..."