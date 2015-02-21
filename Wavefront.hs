--
-- Wavefront.hs
-- Parser and loader for WaveFront obj models
--
-- Jonatan H Sundqvist
-- February 8 2015
-- 
-- Migrated to separate project on February 21 2015

-- TODO | - Appropriate container types (eg. bytestring, vector)
--        - Grammar specification
--        - Incremental parsing (?)
--        - Improve naming scheme
--        - Separate MTL and OBJ parsers (?)
--        - Separate parsing, IO and testing
--        - Additional attributes (lighting, splines, etc.)
--        - FFI
--        - Debugging information (line number, missing file, missing values, etc.)
--        - Proper Haddock coverate, including headers
--        - Model type
--        - Caching (?)
--        - Performance, profiling, optimisations
--        - PrintfArg instances for the types defined in this module
--        - Decide on a public interface (exports)

-- SPEC | -
--        -


{-# LANGUAGE ForeignFunctionInterface #-}

module Wavefront (parseOBJ, parseMTL, main) where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Data.List (isPrefixOf, groupBy, unfoldr)
import Data.Char (isSpace)
import Data.Function (on)
import Data.Either (rights)

import Text.Printf (printf)
import Control.Concurrent (threadDelay)
import System.IO (hFlush, stdout)



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- |
-- TODO: Polymorphic numerical types (?)
-- TODO: Add context, metadata (eg. line numbers, filename) (?)
data OBJToken = Vertex  Float Float Float |
                Normal  Float Float Float |
                Texture Float Float       |
                Face [(Int, Int, Int)]    | -- TODO: Associate material with each face, handle absent indices

                UseMTL String | --  
                LibMTL String | -- TODO: Use actual MTL type

                Group  [String] | -- TODO: Do grouped faces have to be consecutive?
                Object [String]   -- TODO: What is the difference between group and object?
                deriving (Eq, Show) -- TODO: Derive Read (?)


-- |
-- TODO: Use error type instead of String (?)
-- This would allow us to distinguish invalid data from eg. comments and blank lines
type OBJRow = Either String OBJToken


-- |
-- TODO: Rename (?)
-- TODO: Use Integral for line number (?)
type OBJ = [(Int, OBJRow)]


-- | Abstract representation of an OBJ model with associated MTL definitions.
-- 
--
-- TODO: Rename (?)
-- TODO: Include metadata, comments, rejected data (?)
-- TODO: Separate type for processeed OBJTokens (ie. token + context)
-- TODO: Perform index lookups (?)
--
data Model = Model { vertices  :: [OBJToken],
                     normals   :: [OBJToken],
                     textures  :: [OBJToken],
                     faces     :: [OBJToken],
                     selects   :: [OBJToken], -- TODO: Rename (UseMTL) (?) 
                     materials :: [OBJToken],
                     groups    :: [OBJToken],
                     objects   :: [OBJToken] } deriving (Show)


-- | 
--
-- TODO: Is the alpha channel optional, ignored, disallowed?
-- 
data MTLToken = Ambient  Float Float Float | -- Ka
                Diffuse  Float Float Float | -- Kd
                Specular Float Float Float | -- Ks

        MapDiffuse String | -- map_Kd
        Material   String   -- newmtl
        deriving (Eq, Show)
        -- ('Ns', 'Ni', 'd', 'Tr', 'illum')


-- |
type MTLRow = Either String MTLToken


-- |
type MTL = [(Int, MTLRow)]



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
parseOBJ = zip [1..] . map parseOBJRow . lines -- . rows

-- I never knew pattern matching in list comprehensions could be used to filter by constructor
-- let rows = parseOBJ data in ([ v | @v(Vertex {}) <- rows], [ v | @v(Vertex {}) <- rows])


-- | Generates a token given a single
--
-- TODO: Correctness (complete function, no runtime exceptions)
-- TODO: Rename 'which' (?)
-- TODO: Handle invalid rows
-- TODO: Extract value parsing logic (eg. pattern matching, converting, handle errors)
-- TODO: Named errors (typed?) rather than Nothing (cf. Either) (?)
-- TODO: Additional values, currently unsupported attributes (ignore?) (pattern match against the entire line, eg. ["vn", x, y, z])
-- TODO: Dealing with MTL definitions (pass in names, MTL value, return list of MTL dependencies)
-- TODO: Take 1-based indexing into account straight away (?)
-- TODO: Deal with absent texture and normal indices
--
parseOBJRow :: String -> OBJRow -- Maybe OBJToken
parseOBJRow ln
  | isComment ln || null ln = Left ln
  | otherwise               = let (which:values) = words ln in case which of
    "v"  -> vector Vertex values -- Vertex
    "vn" -> vector Normal values -- Normal
    -- TODO: Clean this up
    -- TODO: More generic way of unpacking the right number of values and applying read (?)
    "vt" -> let (x:y:[]) = values in Right $ Texture (read x) (read y) -- Texture
    -- TODO: Clean this up
    -- TODO: Handle invalid data (✓)
    -- TODO: Capture invalid vertex definitions (cf. sequence) (✓)
    -- ("Invalid vertex: "++) .
    "f"  -> either (Left . const ln) (Right . Face) . sequence . map (vector triplet . splitOn '/') $ values -- Face
    "g"  -> Right . Group  $ values -- Group
    "o"  -> Right . Object $ values -- Object
    "s"  -> Left ln -- Smooth shading
    "mtllib" -> Right . LibMTL $ head values --
    "usemtl" -> Right . UseMTL $ head values --
    _        -> Left ln
    where triplet a b c = (a, b, c) -- TODO: Use tuple sections (?)


-- |
-- process the OBJ tokens
parseMTL :: String -> MTL
parseMTL = zip [1..] . map parseMTLRow . lines


-- | 
-- process the MTL tokens
-- TODO: cf. parseOBJRow
parseMTLRow :: String -> MTLRow
parseMTLRow ln
  | isComment ln || null ln = Left ln
  | otherwise               = let (which:values) = words ln in case which of
    "Ka" -> withChannels Ambient  values -- Ka
    "Kd" -> withChannels Diffuse  values -- Kd
    "Ks" -> withChannels Specular values -- Ks
    "map_Kd" -> withName MapDiffuse values -- map_Kd
    "newmtl" -> withName Material   values -- newmtl
    _        -> Left ln
    where withChannels token (r:g:b:[]) = Right $ token (read r) (read g) (read b) -- TODO: No alpha channel (optional?) (?) (read a) 
          withChannels _      _         = Left  $ "Pattern match failed"           -- TODO: No alpha channel (optional?) (?) (read a)

          withName token (name:[]) = Right $ token name
          withName _      _        = Left "Pattern match failed"


-- | 
-- TODO: Use map for materials (?)
-- TODO: How to retrieve MTL data
-- TODO: How to deal with errors, including no-parse, index errors, etc.
-- TODO: Performance, how are 'copies' of coordinates handled (?)
createModel :: OBJ -> ([String] -> [MTL]) -> Model
createModel modeldata retrieve = let tokens    = rights . map snd $ modeldata -- TODO: Vat do vee du viz ze dissidents, kommandant?
                                     materials = retrieve [ name | UseMTL name <- tokens] -- Retrieve MTL data
                                     vertices  = [ vertex  | vertex@(Vertex{})   <- tokens ]
                                     normals   = [ normal  | normal@(Normal{})   <- tokens ]
                                     textures  = [ texture | texture@(Texture{}) <- tokens ]
                                 in error "Still under construction. Step away or put on a hard hat."


{-let model = Model { vertices  = [ vertex   | @vertex(Vertex{})],
                      normals   = [ normal   | @normal(Normal{})],
                      textures  = [ texture  | @texture(Texture{})],
                      faces     = [ face     | @face(Face{})],
                      selects   = [ select   | @select(Vertex{})],
                      materials = [ material | @material(Vertex{})],
                      groups    = [ group    | @group(Vertex{})],
                      objects   = [ object   | @object(Vertex{})] }
-}

-- Parsing utilities ------------------------------------------------------------------------------
-- |
-- TODO: Clean up or use existing function
-- TODO: Rename (?)
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c s = unfoldr cut s
  where cut [] = Nothing
        cut xs = let (token, rest) = span (/=c) xs in Just (token, dropWhile (==c) rest)
-- splitOn c s = filter (/=[c]) . groupBy ((==) `on` (==c)) $ s


-- |
-- TODO: Drop comments at the end of a line (?)
isComment :: String -> Bool
isComment = isPrefixOf "#" . dropWhile isSpace


-- |
rows :: String -> [String]
rows = filter (\ ln -> not $ any ($ ln) [null, isComment]) . lines


-- |
-- TODO: Use readMaybe (?)
-- TODO: Variadic 'unpacking' (or is that sinful?)
vector :: Read r => (r -> r -> r -> b) -> [String] -> Either String b
vector token (x:y:z:[]) = Right $ token (read x) (read y) (read z) -- TODO: Add back the Maybe wrapper (?)
vector _      _         = Left  "Pattern match failed"



---------------------------------------------------------------------------------------------------
-- Functions (IO)
---------------------------------------------------------------------------------------------------
-- Loading data -----------------------------------------------------------------------------------
-- |
-- TODO: Use bytestrings (?)
loadOBJ :: String -> IO OBJ
loadOBJ fn = do
  rawOBJ <- readFile fn    --
  return $ parseOBJ rawOBJ --


-- |
-- TODO: Use bytestrings (?)
loadMTL :: String -> IO MTL
loadMTL fn = do
  rawMTL <- readFile fn    --
  return $ parseMTL rawMTL --


-- |
-- Loads an OBJ model from file, including associated materials
loadModel :: String -> IO Model
loadModel fn = do
  obj <- loadOBJ fn
  return $ error "Not done yet"


--  -----------------------------------------------------------------------------------
-- | 
promptContinue :: String -> IO ()
promptContinue prompt = do
  putStr prompt
  hFlush stdout
  getChar
  putChar '\n'



---------------------------------------------------------------------------------------------------
-- Pure foreign function interface
---------------------------------------------------------------------------------------------------
-- foreign export ccall parseOBJ :: String -> OBJ
-- foreign export ccall parseMTL :: String -> MTL



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "This is where the checks should be."

  let path = "C:/Users/Jonatan/Desktop/Python/experiments/WaveFront/"
  
  flip mapM_ ["queen", "cube"] $ \ fn -> do
    printf "\nParsing OBJ file: %s.obj\n" fn
    model <- loadOBJ $ printf (path ++ "data/%s.obj") fn
    printf "Found %d invalid rows in OBJ file (n comments, m blanks, o errors).\n" $ length [ 1 | Left _ <- map snd model ]

    promptContinue "Press any key to continue..."

    mapM_ print ["[" ++ show n ++ "] " ++ show token | (n, Right token) <- model ]
    -- TODO: Print culprit lines (✓)

    promptContinue "Press any key to continue..."

    printf "\nParsing MTL file: %s.mtl\n" fn
    materials <- loadMTL $ printf (path ++ "data/%s.mtl") fn
    printf "Found %d invalid rows in MTL file (n comments, m blanks, o errors).\n" $ length [ 1 | Left _ <- map snd materials ]
    mapM_ print ["[" ++ show n ++ "] " ++ show token | (n, Right token) <- materials ]

    promptContinue "Press any key to continue..."