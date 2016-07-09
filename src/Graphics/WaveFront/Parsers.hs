-- |
-- Module      : Graphics.WaveFront.Parsers
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, February 8 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--
-- Created February 8 2015
-- Wavefront - Parsers.hs
-- Migrated to separate project on February 21 2015

-- TODO | - Appropriate container types (eg. bytestring, vector)
--        - Grammar specification
--        - Incremental parsing (?)
--        - Improve naming scheme
--
--        - Separate MTL and OBJ parsers (?) (...)
--        - Separate parsing, processing, logging, IO and testing (...)
--          -- Proper path handling (eg. include root in MTLTable or not)
--
--        - Additional attributes (lighting, splines, etc.)
--        - FFI (...)
--        - Debugging information (line number, missing file, missing values, etc.) (...)
--        - Proper Haddock coverage, including headers (...)
--        - Model type (...)
--        - Caching (?)
--        - Performance, profiling, optimisations
--          -- Strict or lazy (eg. with Data.Map) (?)
--          -- Multi-threading
--          -- Appropriate container types
--
--        - PrintfArg instances for the types defined in this module
--        - Reconciling Cabal and hierarchical modules
--        - Dealing with paths in lib statements (requires knowledge of working directories)
--        - Move comments and specification to separate files (eg. README)
--        - Inline comments (for internals, implementation)
--
--        - Full OBJ spec compliance
--          -- Do the usemtl and libmtl statements affect vertices or faces (?)
--
--        - Parser bugs
--          -- Negative coordinates enclosed in parentheses
--
--        - Decide on a public interface (exports) (API)
--          -- Model will be the main API type
--          -- Processing utils (eg. iterating over model faces; withModelFaces :: ((Material, [(Vertex, Maybe Normalcoords, Maybe Texcoords)]) -> b) -> Model -> [b])
--          -- Export functions for working with the output data (eg. unzipIndices :: [(Int, Int, Int)] -> ([Int], [Int], [Int]))
--          -- Export certain utilities (eg. second, perhaps in another module) (?)

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Extensions
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- Section
--------------------------------------------------------------------------------------------------------------------------------------------
module Graphics.WaveFront.Parsers (parseOBJ, parseMTL,
                                   facesOf,  materialsOf,
                                   modelAttributes, tessellate, boundingbox,
                                   hasTextures, textures,
                                   module Graphics.WaveFront.Types,
                                   BoundingBox(..), Vector(..),
                                   createModel, createMTLTable) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import           Data.List   (groupBy, unzip4)
import           Data.Maybe  (listToMaybe, catMaybes)
import           Data.Either (rights)
import           Data.Char   (isSpace)
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Text.Parsec as Parsec
import           Text.Parsec                          ((<?>), (<|>), ParsecT, Stream, char)
import           Text.ParserCombinators.Parsec.Number (floating3)

import Text.Read     (readMaybe, readEither)
import Control.Monad (liftM)
import Control.Lens hiding (indices)

-- import Southpaw.Utilities.Utilities (pairwise, cuts)

import Cartesian.Space.Types (BoundingBox(..), Vector3D(..))

import Graphics.WaveFront.Types
import Graphics.WaveFront.Utilities



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions (pure)
--------------------------------------------------------------------------------------------------------------------------------------------

-- OBJ parsing -----------------------------------------------------------------------------------------------------------------------------

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
-- TODO: Correctness (total function, no runtime exceptions)
-- TODO: Rename 'which' (?)
-- TODO: Handle invalid rows (how to deal with mangled definitions w.r.t indices?)
-- TODO: Extract value parsing logic (eg. pattern matching, converting, handle errors)

-- TODO: Named errors (typed?) rather than Nothing (cf. Either) (?)
--       Type for unsupported but valid (according to spec) attributes (?)
--       Type for specific attribute that failed to parse (eg. "f 1/2 0/p 1.5/x")
--
-- TODO: Additional values, currently unsupported attributes (ignore?) (pattern match against the entire line, eg. ["vn", x, y, z])
-- TODO: Dealing with MTL definitions (pass in names, MTL value, return list of MTL dependencies)
-- TODO: Take 1-based indexing into account straight away (?)
-- TODO: Deal with absent texture and normal indices (✓)
-- TODO: Strip trailing comments (✓)
-- TODO: Don't ignore leftover values (errors?) (...)
--
parseOBJRow :: String -> (Int -> OBJRow) -- Maybe OBJToken
parseOBJRow ln = parseTokenWith ln $ \ line -> let (attr:values) = words line in case (attr:values) of
    ("f":_:_:_:_)      -> either (Left . noparse . const line) (Right . OBJFace) . sequence . map (ivertex . cuts '/') $ values -- Face
    ["v",  sx, sy, sz] -> vector (\ [x, y, z] -> OBJVertex  x y z) [sx, sy, sz] (noparse line)                                  -- Vertex
    ["vn", sx, sy, sz] -> vector (\ [x, y, z] -> OBJNormal  x y z) [sx, sy, sz] (noparse line)                                  -- Normal
    ["vt", sx, sy]     -> vector (\ [x, y]    -> OBJTexture x y)   [sx, sy]     (noparse line)                                  -- Texture
    ("g":_:_)          -> Right $ Group  values                                 -- Group
    ("o":_:_)          -> Right $ Object values                                 -- Object
    ("s":_:_)          -> Left  $ noparse line                                  -- Smooth shading
    ["mtllib", lib]    -> Right $ LibMTL lib                                    --
    ["usemtl", mtl]    -> Right $ UseMTL mtl                                    --
    _                  -> Left  $ noparse line                                  -- TODO: More informative errors
    where
      -- ivertex :: [String] -> Either OBJNoParse OBJToken
      ivertex [svi, sti, sni] = readEither svi >>= \ vi -> Right $ (vi, readMaybe sti, readMaybe sni) -- TODO: Refactor, simplify
      ivertex [svi, sti]      = readEither svi >>= \ vi -> Right $ (vi, readMaybe sti, Nothing)       -- TODO: Refactor, simplify
      ivertex [svi]           = readEither svi >>= \ vi -> Right $ (vi, Nothing,       Nothing)       -- TODO: Refactor, simplify
      ivertex _               = Left ln

      noparse line
        | null line || all isSpace line = OBJEmpty
        | isComment line                = OBJComment line
        | otherwise                     = OBJNoParse line


-- | Returns a list of annotated parsing errors or the original OBJ data structure if
-- validateOBJ :: OBJ -> Either [OBJNoParse] OBJ
-- validateOBJ _ = undefined

-- MTL parsing -----------------------------------------------------------------------------------------------------------------------------

-- | Produces a list of MTL tokens, with associated line numbers and comments
parseMTL :: String -> MTL
parseMTL = enumerate . map parseMTLRow . lines


-- | Parses a single MTL row.
--
-- TOOD: Simplify 'withChannels'
-- TOOD: Process the MTL tokens (✗)
-- TODO: cf. parseOBJRow
--
parseMTLRow :: String -> (Int -> MTLRow)
parseMTLRow ln = parseTokenWith ln $ \ line  -> let (which:values) = words line in case which:values of
    ("Ka":sr:sg:sb:rest) -> withChannels Ambient  sr sg sb rest line -- Ka
    ("Kd":sr:sg:sb:rest) -> withChannels Diffuse  sr sg sb rest line -- Kd
    ("Ks":sr:sg:sb:rest) -> withChannels Specular sr sg sb rest line -- Ks
    ["map_Kd", name]     -> Right $ MapDiffuse  name                 -- map_Kd
    ["newmtl", name]     -> Right $ NewMaterial name                 -- newmtl
    _                    -> Left  $ noparse line                     --
    where
      withChannels f sr sg sb []   line = vector (\[r, g, b] -> f r g b Nothing)        [sr, sg, sb] (noparse line) -- TODO: Refactor, simplify
      withChannels f sr sg sb [sa] line = vector (\[r, g, b] -> f r g b $ readMaybe sa) [sr, sg, sb] (noparse line) -- TODO: Refactor, simplify
      withChannels _ _  _  _   _   line = Left $ noparse line

      noparse line
        | null line || all isSpace line = MTLEmpty
        | isComment line                = MTLComment line
        | otherwise                     = MTLNoParse line

-- Parser output churners (OBJ) ------------------------------------------------------------------------------------------------------------

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
--
-- TODO: Refactor, simplify
--
buildIndexMapWith :: [OBJToken] -> Map.Map [String] (Int, Int)
buildIndexMapWith tokens = Map.fromList . pairwise zipIndices . reverse . addLastIndex $ foldl update (0, []) $ tokens
  where addLastIndex (nfaces, groups') = ([], nfaces):groups'
        zipIndices (names, low) (_, upp) = (names, (low, upp))
        update (nfaces, groups') token = case token of
          Group   names -> (nfaces,   (names, nfaces):groups')
          Object  names -> (nfaces,   (names, nfaces):groups')
          OBJFace _     -> (nfaces+1, groups')
          _             -> (nfaces,   groups')


-- | Filters out faces from a stream of OBJTokens and attaches the currently selected material,
--   as defined by the most recent LibMTL and UseMTL tokens.
--
-- TODO: Don't use foldl (?)
-- TODO: Deal with errors (eg. missing materials)
-- TODO: Improve naming scheme (lots of primes)
-- TODO: Default material, take 'error-handling' function (?)
--
facesOf :: [OBJToken] -> MTLTable -> [Either String Face]
facesOf tokens table = reverse . (^._3) . foldl update ("", "", []) $ tokens
  where retrieve lib mat       = Map.lookup lib table >>= Map.lookup mat
        createFace lib mat ind = case retrieve lib mat of
                                   Nothing -> Left  $ "No such material: " ++ lib ++ "." ++ mat
                                   Just m  -> Right $ Face { indices=ind, material=m }
        update (lib', material', faces') token = case token of
                                                   OBJFace ind -> (lib', material', createFace lib' material' ind : faces')
                                                   LibMTL  lib -> (lib,  material', faces')
                                                   UseMTL  mat -> (lib', mat,       faces')
                                                   _           -> (lib', material', faces')

-- Parser output churners (MTL) ------------------------------------------------------------------------------------------------------------

-- | Constructs a map between names and materials. Partially or wholly undefined materials
--   are mapped to a string detailing the error (eg. Left "missing specular").
--
-- TODO: Debug information (eg. attributes without an associated material)
-- TODO: Pass in error function (would allow for more flexible error handling) (?)
-- TODO: Filter out parser failures (?)
-- TOOD: Deal with duplicated attributes (probably won't crop up in any real situations)
materialsOf :: [MTLToken] -> Map.Map String (Either String Material)
materialsOf tokens = Map.fromList . rights $ map createMaterial thegroups
  where
    thegroups = groupBy (((not . isnew) .) . flip const) tokens -- TODO: Refactor this atrocity
    isnew (NewMaterial _) = True  -- TODO: Rename isnew
    isnew  _              = False
    createMaterial (NewMaterial name:attrs) = Right $ (name, fromAttributes attrs)
    createMaterial  attrs                   = Left  $ "Free-floating attributes: " ++ show attrs
    fromAttributes  attrs
      | any null colours = Left  $ "Missing colour(s)" -- TODO: More elaborate message (eg. which colour)
      | otherwise        = Right $ Material { ambient=head amb, diffuse=head diff, specular=head spec, texture=listToMaybe [ name | MapDiffuse name <- attrs ] }
      where colours@[diff, spec, amb] = [[ (r, g, b, maybe 1.0 id a) | Diffuse  r g b a <- attrs ],
                                         [ (r, g, b, maybe 1.0 id a) | Specular r g b a <- attrs ],
                                         [ (r, g, b, maybe 1.0 id a) | Ambient  r g b a <- attrs ]]


-- |
-- TODO: Debug information (eg. how many invalid materials were filtered out)
-- TODO: Refactor, simplify
createMTLTable :: [(String, [MTLToken])] -> MTLTable
createMTLTable mtls = Map.fromList . map (\ (name, tokens) -> (name, Map.mapMaybe prune . materialsOf $ tokens)) $ mtls
  where prune (Right mat) = Just mat
        prune (Left  _)   = Nothing

-- API functions ---------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Use map for materials (?)
-- TODO: How to retrieve MTL data
-- TODO: How to deal with errors, including no-parse, index errors, etc. (use applicative?)
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
createModel tokens thematerials = let modeldata  = rights $ map (^._2) tokens -- TODO: Vat do vee du viz ze dissidents, kommandant?
                                  in Model { vertices  = [ (x, y, z) | OBJVertex  x y z <- modeldata ],
                                             normals   = [ (x, y, z) | OBJNormal  x y z <- modeldata ],
                                             texcoords = [ (x, y)    | OBJTexture x y   <- modeldata ],
                                             faces     = map tessellate . rights $ facesOf modeldata thematerials,
                                             groups    = groupsOf  modeldata,
                                             objects   = objectsOf modeldata,
                                             materials = thematerials }


-- | Extracts vertex, normal, texture and material data from a model
-- TODO: Figure out how to deal with missing indices
modelAttributes :: Model -> (Vertices, TexCoords, Normals, Materials)
modelAttributes model = unzip4 $ concat [ map (attributesAt mat) theindices | Face { material=mat, indices=theindices } <- faces model]
  where
    vertexAt   = (vertices model !!)          . subtract 1 --
    normalAt   = liftM $ (normals   model !!) . subtract 1 --
    texcoordAt = liftM $ (texcoords model !!) . subtract 1 --
    attributesAt mat (vi, ti, ni) = (vertexAt vi, texcoordAt ti, normalAt ni, mat)
    -- collect (vs, ts, ns, mats)    = (Vertices vs, TexCoords ts, Normals ns, Materials mats)


-- |
-- TODO: Specialise to [[Face]] (?)
-- TODO: Check vertex count (has to be atleast three)
-- TODO: Better names (?)
tessellate :: Face -> Face
tessellate face@(Face { indices=ind }) = face { indices=triangles ind }
  where triangles (a:rest) = concat $ pairwise (\b c -> [a, b, c]) rest


-- |
-- unpackModelAttributes ::


-- |
-- TODO: Deal with empty vertex lists (?)
-- boundingbox :: (RealFloat n, Ord n) => Model -> BoundingBox (Vector3D n)
boundingbox :: Model -> BoundingBox (Vector3D Float)
boundingbox model = BoundingBox { centreOf=Vector3D (minx+maxx) (miny+maxy) (minz+maxz) * Vector3D 0.5 0 0, sizeOf=Vector3D (maxx-minx) (maxy-miny) (maxz-minz) }
  where
    minmax :: (Ord o) => [o] -> (o, o)
    minmax (v:alues) = foldr (\val acc -> (min val (fst acc), max val (snd acc))) (v, v) alues              -- TODO: Factor out

    [(minx, maxx), (miny, maxy), (minz, maxz)] = [ minmax . map (^.f) $ vertices model | f <- [_1, _2, _3]] -- TODO: Make sure the order is right

-- Model queries ---------------------------------------------------------------------------------------------------------------------------

-- |
hasTextures :: Model -> Bool
hasTextures =  not . Set.null . textures -- (/= Nothing)


-- | All texture names as a list
-- TODO: Wrap in ;aybe (instead of empty list) (?)
textures :: Model -> Set.Set String
textures = Set.fromList . catMaybes . map texture . concatMap Map.elems . Map.elems . materials
