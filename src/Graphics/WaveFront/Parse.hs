-- |
-- Module      : Graphics.WaveFront.Parse
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
--          -- Remove 'parse-' prefix, import qualified (eg. 'Parse.obj')
--
--        - Separate MTL and OBJ parsers (?) (...)
--        - Separate parsing, processing, logging, IO and testing (...)
--          -- Proper path handling (eg. include root in MTLTable or not)
--
--        - Additional attributes (lighting, splines, etc.)
--        - FFI (...)
--        - Debugging information (line number, missing file, missing values, etc.) (...)
--        - Proper Haddock coverage, including headers (...)
--        - Model type (✓)
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
--          -- Negative coordinates enclosed in parentheses (...)
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
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- Section
--------------------------------------------------------------------------------------------------------------------------------------------
-- TODO: Clean this up
module Graphics.WaveFront.Parse (
  parseOBJ, parseMTL,
  facesOf,  materialsOf,
  modelAttributes, tessellate, boundingbox,
  hasTextures, textures,
  module Graphics.WaveFront.Types, -- TODO: Don't export internal types (duh)
  BoundingBox(..),
  createModel, createMTLTable
) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import           Data.Int    (Int64)
import           Data.List   (groupBy, unzip4)
import           Data.Maybe  (listToMaybe, catMaybes)
import           Data.Either (rights)
import           Data.Char   (isSpace)
import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Data.Text as T

import qualified Data.Attoparsec.Text       as Atto
import qualified Data.Attoparsec.Combinator as Atto

import Control.Monad       (liftM)
import Control.Lens ((^.), _1, _2, _3)
import Control.Applicative (pure, liftA2, (<$>), (<*>), (<*), (*>), (<|>))

import Linear.V2 (V2(..))
import Linear.V3 (V3(..))

import Cartesian.Types  (BoundingBox(..))
import Cartesian.Lenses (x, y, z)

import Graphics.WaveFront.Types



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
-- format :: (Stream s' Identity Char) => ParsecT s' u Identity (FormatToken)
parseOBJ :: Atto.Parser (SimpleOBJ)
parseOBJ = Atto.sepBy parseOBJRow lineSeparator <* Atto.endOfInput


-- | Parses a token given a single valid OBJ row, or an error value if the input is malformed.
--
-- TODO: Correctness (total function, no runtime exceptions)
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
-- TODO: Use ListLike or Monoid (or maybe Indexable, since that's the real requirement) (?)
parseOBJRow :: Atto.Parser (SimpleOBJToken)
parseOBJRow = token <* ignore comment -- TODO: Let the separator handle comments (?)
  where
    -- Parses an OBJ token
    token :: Atto.Parser (SimpleOBJToken)
    token = (Atto.string "f"  *> face)    <|> --
            -- TODO: How to deal with common prefix (v, vn, vt) (backtrack?)
            (Atto.string "vn" *> normal)  <|>
            (Atto.string "vt" *> texture) <|>
            (Atto.string "v"  *> vertex)  <|>
            (Atto.string "o"  *> object)  <|>
            (Atto.string "g"  *> group)   <|>
            -- Atto.string "s" -- Smooth shading (TODO: Don't ignore)
            (Atto.string "mtllib" *> lib) <|>
            (Atto.string "usemtl" *> use)
    
    -- TODO: Expose these parsers for testing purposes (?)
    face    = OBJFace    <$> atleast 3 (space *> ivertex)
    normal  = OBJNormal  <$> point3D
    texture = OBJTexture <$> point2D
    vertex  = OBJVertex  <$> point3D
    
    object  = Object <$> atleast 1 (space *> word)
    group   = Group  <$> atleast 1 (space *> word)

    lib = LibMTL <$> (space *> word)
    use = UseMTL <$> (space *> word)

    -- A single vertex definition with indices for vertex position, normal, and texture coordinates
    -- TODO: Should the slashes be optional?
    ivertex = VertexIndices <$>
                (Atto.decimal          <* Atto.char '/') <*>
                (optional Atto.decimal <* Atto.char '/') <*>
                (optional Atto.decimal)
    
-- Jon's little helpers --------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Make sure this is right
-- OBJ rows may be separated by one or more lines of comments and whitespace, or empty lines.
lineSeparator :: Atto.Parser ()
lineSeparator = Atto.skipMany1 $ ignore space *> ignore comment *> Atto.endOfLine


-- |
comment :: Atto.Parser T.Text
comment = Atto.skipSpace *> Atto.char '#' *> Atto.takeTill (\c -> (c == '\r') || (c == '\n')) -- TODO: Is the newline consumed (?)


-- |
-- TODO: Use 'try' to enforce backtracking (?)
optional :: Atto.Parser a -> Atto.Parser (Maybe a)
optional p = Atto.option Nothing (Just <$> p)


-- | Like Atto.skipMany, except it skips one match at the most
ignore :: Atto.Parser a -> Atto.Parser ()
ignore p = optional p *> pure ()


-- | 
atleast :: Int -> Atto.Parser a -> Atto.Parser [a]
atleast n p = liftA2 (++) (Atto.count n p) (Atto.many' p)


-- | Skips atleast one white space character (not including newlines and carriage returns)
space :: Atto.Parser ()
space = Atto.skipMany1 (Atto.satisfy isLinearSpace)


-- |
-- TODO: Unicode awareness (cf. Data.Char.isSpace)
isLinearSpace :: Char -> Bool
isLinearSpace c = (c == ' ') || (c == '\t')


-- |
word :: Atto.Parser T.Text
word = T.pack <$> Atto.many1 Atto.letter


-- |
parenthesised :: Atto.Parser a -> Atto.Parser a
parenthesised p = Atto.char '(' *> p <* Atto.char ')'


-- |
-- TODO: Polymorphic
coord :: Fractional f => Atto.Parser f
coord = space *> (parenthesised Atto.rational <|> Atto.rational)


-- | Parser a single colour channel
-- TODO: Clamp to [0,1] (cf. partial from monadplus) (?)
channel :: Fractional f => Atto.Parser f
channel = space *> (parenthesised Atto.rational <|> Atto.rational)


-- |
colour :: Fractional f => Atto.Parser (Colour f)
colour = Colour <$> channel <*> channel <*> channel <*> Atto.option 1 channel


-- | 
point3D :: Atto.Parser (V3 Double)
point3D = V3 <$> coord <*> coord <*> coord


-- |
point2D :: Atto.Parser (V2 Double)
point2D = V2 <$> coord <*> coord

-- MTL parsing -----------------------------------------------------------------------------------------------------------------------------

-- | Produces a list of MTL tokens, with associated line numbers and comments
parseMTL :: Atto.Parser (SimpleMTL)
parseMTL = Atto.sepBy parseMTLRow lineSeparator <* Atto.endOfInput


-- | Parses a single MTL row.
--
-- TOOD: Simplify 'withChannels'
-- TOOD: Process the MTL tokens (✗)
-- TODO: cf. parseOBJRow
--
parseMTLRow :: Atto.Parser (SimpleMTLToken)
parseMTLRow = token <* ignore comment
    where
      -- TODO: How to deal with common prefix (Ka, Kd, Ks) (backtrack?)
      token = (Atto.string "Ka"     *> ambient)     <|>
              (Atto.string "Kd"     *> diffuse)     <|>
              (Atto.string "Ks"     *> specular)    <|>
              (Atto.string "map_Kd" *> mapDiffuse)  <|>
              (Atto.string "newmtl" *> newMaterial)

      -- TODO: Expose these parsers for testing purposes (?)
      ambient  = space *> (Ambient  <$> colour)
      diffuse  = space *> (Diffuse  <$> colour)
      specular = space *> (Specular <$> colour)

      mapDiffuse  = space *> (MapDiffuse  <$> word)
      newMaterial = space *> (NewMaterial <$> word)

-- Parser output churners (OBJ) ------------------------------------------------------------------------------------------------------------

-- TODO: Move to separate module (eg. WaveFront.Model)

-- | Creates a mapping between group names and the corresponding bounds ([lower, upper)). Invalid
--   tokens are simply discarded by this function.
--
-- TODO: Figure out how to deal with multiple group names (eg. "g mesh1 nose head")
groupsOf :: [SimpleOBJToken] -> M.Map [T.Text] (Int64, Int64)
groupsOf = buildIndexMapWith . filter notObject
  where
    notObject (Object _) = False
    notObject  _         = True


-- |
objectsOf :: [SimpleOBJToken] -> M.Map [T.Text] (Int64, Int64)
objectsOf = buildIndexMapWith . filter notGroup
  where
    notGroup (Group _) = False
    notGroup  _        = True


-- | Creates a mapping between names (of groups or objects) to face indices
--
-- TODO: Refactor, simplify
--
buildIndexMapWith :: [SimpleOBJToken] -> M.Map [T.Text] (Int64, Int64)
buildIndexMapWith tokens = M.fromList . pairwise zipIndices . reverse . addLastIndex $ foldl update (0, []) $ tokens
  where
    pairwise f xs = zipWith f xs (drop 1 xs)
    addLastIndex (nfaces, groups') = ([], nfaces):groups'
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
facesOf :: [SimpleOBJToken] -> SimpleMTLTable -> [Either T.Text SimpleFace]
facesOf tokens table = reverse . (^._3) . foldl update ("", "", []) $ tokens
  where retrieve lib mat       = M.lookup lib table >>= M.lookup mat
        createFace lib mat ind = case retrieve lib mat of
                                   Nothing -> Left  $ T.concat ["No such material: ", lib, ".", mat]
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
materialsOf :: [SimpleMTLToken] -> M.Map T.Text (Either T.Text SimpleMaterial)
materialsOf tokens = M.fromList . rights $ map createMaterial thegroups
  where
    thegroups = groupBy (((not . isnew) .) . flip const) tokens -- TODO: Refactor this atrocity
    isnew (NewMaterial _) = True  -- TODO: Rename isnew
    isnew  _              = False
    createMaterial (NewMaterial name:attrs) = Right $ (name, fromAttributes attrs)
    createMaterial  attrs                   = Left  $ T.concat ["Free-floating attributes: ", T.pack . show $ attrs]
    fromAttributes  attrs
      | any null colours = Left  $ "Missing colour(s)" -- TODO: More elaborate message (eg. which colour)
      | otherwise        = Right $ Material { ambient=head amb, diffuse=head diff, specular=head spec, texture=listToMaybe [ name | MapDiffuse name <- attrs ] }
      where 
        colours@[diff, spec, amb] = [[ c | (Diffuse  c) <- attrs ],
                                     [ c | (Specular c) <- attrs ],
                                     [ c | (Ambient  c) <- attrs ]]


-- |
-- TODO: Debug information (eg. how many invalid materials were filtered out)
-- TODO: Refactor, simplify
createMTLTable :: [(T.Text, [SimpleMTLToken])] -> SimpleMTLTable
createMTLTable mtls = M.fromList . map (\ (name, tokens) -> (name, M.mapMaybe prune . materialsOf $ tokens)) $ mtls
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
createModel :: SimpleOBJ -> SimpleMTLTable -> SimpleModel
createModel modeldata thematerials = Model { vertices  = [ vec | OBJVertex  vec <- modeldata ],
                                             normals   = [ vec | OBJNormal  vec <- modeldata ],
                                             texcoords = [ vec | OBJTexture vec <- modeldata ],
                                             faces     = map tessellate . rights $ facesOf modeldata thematerials,
                                             groups    = groupsOf  modeldata,
                                             objects   = objectsOf modeldata,
                                             materials = thematerials }


-- (SimpleVertices, SimpleTexCoords,     [Maybe (V3 Double)],  SimpleMaterials)
-- ([V3 Double],    [Maybe (V3 Double)], [Maybe (V3 Double)], [Material Double T.Text])

-- | Extracts vertex, normal, texture and material data from a model
-- TODO: Figure out how to deal with missing indices
-- TODO: Refactor, optimise
modelAttributes :: SimpleModel -> (SimpleVertices, SimpleTexCoords, SimpleNormals, SimpleMaterials)
modelAttributes model = unzip4 $ concat [ map (attributesAt mat) theindices | Face { material=mat, indices=theindices } <- faces model]
  where
    vertexAt   = (vertices model !!)          . fromIntegral . subtract 1 --
    normalAt   = liftM $ (normals   model !!) . fromIntegral . subtract 1 --
    texcoordAt = liftM $ (texcoords model !!) . fromIntegral . subtract 1 --
    attributesAt mat (VertexIndices {ivertex, itexcoord, inormal}) = (vertexAt ivertex, texcoordAt itexcoord, normalAt inormal, mat)
    -- collect (vs, ts, ns, mats)    = (Vertices vs, TexCoords ts, Normals ns, Materials mats)


-- |
-- TODO: Specialise to [[Face]] (?)
-- TODO: Check vertex count (has to be atleast three)
-- TODO: Better names (?)
tessellate :: SimpleFace -> SimpleFace
tessellate face@(Face { indices=ind }) = face { indices=triangles ind }
  where
    triangles (a:rest) = concat $ pairwise (\b c -> [a, b, c]) rest
    pairwise f xs = zipWith f xs (drop 1 xs)


-- |
-- unpackModelAttributes ::


-- |
-- TODO: Deal with empty vertex lists (?)
-- TODO: Refactor
-- boundingbox :: (RealFloat n, Ord n) => Model -> BoundingBox (Vector3D n)
boundingbox :: SimpleModel -> BoundingBox (V3 Double)
boundingbox model = BoundingBox {
                      cornerOf = V3 minx miny minz,
                      sizeOf   = V3 (maxx-minx) (maxy-miny) (maxz-minz) }
  where
    minmax :: (Ord o) => [o] -> (o, o)
    minmax (v:alues) = foldr (\val acc -> (min val (fst acc), max val (snd acc))) (v, v) alues              -- TODO: Factor out

    [(minx, maxx), (miny, maxy), (minz, maxz)] = [ minmax . map (^.f) $ vertices model | f <- [x, y, z]] -- TODO: Make sure the order is right

-- Model queries ---------------------------------------------------------------------------------------------------------------------------

-- |
hasTextures :: Ord s => Model f s i m -> Bool
hasTextures =  not . S.null . textures -- (/= Nothing)


-- | All texture names as a list
-- TODO: Wrap in ;aybe (instead of empty list) (?)
textures :: Ord s => Model f s i m -> S.Set s
textures = S.fromList . catMaybes . map texture . concatMap M.elems . M.elems . materials
