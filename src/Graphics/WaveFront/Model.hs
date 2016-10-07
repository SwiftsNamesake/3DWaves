-- |
-- Module      : Graphics.WaveFront.Model
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, October 2 2016
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- TODO | - 
--        - 

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Extensions
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE FlexibleContexts  #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- Section
--------------------------------------------------------------------------------------------------------------------------------------------
-- TODO: Clean this up
module Graphics.WaveFront.Model (
  BoundingBox(..),
  facesOf,  materialsOf,
  tessellate, bounds,
  hasTextures, textures,
  createModel, createMTLTable,
  fromIndices, fromFaceIndices, diffuseColours
) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import qualified Data.Vector as V
import           Data.Vector ((!?))

import qualified Data.Text   as T
import qualified Data.Map    as M
import qualified Data.Set    as S

import Data.List   (groupBy)
import Data.Maybe  (listToMaybe, catMaybes)

import Data.Int (Int64)

import Linear (V3(..))

import Control.Lens ((^.), (%~), _3)

import Cartesian.Core (BoundingBox(..), fromExtents, x, y, z)

import Graphics.WaveFront.Types
import Graphics.WaveFront.Lenses



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
pairwise :: (a -> a -> b) -> [a] -> [b]
pairwise f xs = zipWith f xs (drop 1 xs)

-- Parser output churners (OBJ) ------------------------------------------------------------------------------------------------------------

-- TODO: Move to separate module (eg. WaveFront.Model)

-- | Creates a mapping between group names and the corresponding bounds ([lower, upper)). Invalid
--   tokens are simply discarded by this function.
--
-- TODO: Figure out how to deal with multiple group names (eg. "g mesh1 nose head")
groupsOf :: [SimpleOBJToken] -> M.Map (S.Set T.Text) (Int64, Int64)
groupsOf = buildIndexMapWith . filter notObject
  where
    notObject (Object _) = False
    notObject  _         = True


-- |
objectsOf :: [SimpleOBJToken] -> M.Map (S.Set T.Text) (Int64, Int64)
objectsOf = buildIndexMapWith . filter notGroup
  where
    notGroup (Group _) = False
    notGroup  _        = True


-- | Creates a mapping between names (of groups or objects) to face indices
--
-- TODO: Refactor, simplify
-- TODO: What happens if the same group or object appears multiple times (is that possible?)
--
buildIndexMapWith :: [SimpleOBJToken] -> M.Map (S.Set T.Text) (Int64, Int64)
buildIndexMapWith tokens = M.fromList . pairwise zipIndices . reverse . addLastIndex $ foldl update (0, []) $ tokens
  where
    addLastIndex (nfaces, groups') = (S.empty, nfaces):groups'
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
-- TODO: Can vertices in the same face have different materials (?)
facesOf :: [SimpleOBJToken] -> SimpleMTLTable -> [Either String SimpleFace]
facesOf tokens table = reverse . (^._3) . foldl update ("", "", []) $ tokens
  where 
    retrieve lib mat       = M.lookup lib table >>= M.lookup mat
    createFace lib mat ind = case retrieve lib mat of
                               Nothing -> Left  $ concat ["No such material: ", T.unpack lib, ".", T.unpack mat]
                               Just m  -> Right $ Face { fIndices=ind, fMaterial=m }
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
materialsOf :: [SimpleMTLToken] -> Either String (M.Map T.Text (SimpleMaterial))
materialsOf tokens = M.fromList <$> sequence (map createMaterial thegroups)
  where
    thegroups = groupBy (((not . isnew) .) . flip const) tokens -- TODO: Refactor this atrocity
    isnew (NewMaterial _) = True  -- TODO: Rename isnew
    isnew  _              = False
    createMaterial (NewMaterial name:attrs) = (name,) <$> fromAttributes attrs
    createMaterial  attrs                   = Left  $ concat ["Free-floating attributes: ", show $ attrs]
    fromAttributes  attrs = case colours of
      Nothing                -> Left  $ "Missing colour(s)" -- TODO: More elaborate message (eg. which colour)
      Just (amb, diff, spec) -> Right $ Material { fAmbient=amb,fDiffuse=diff, fSpecular=spec, fTexture=listToMaybe [ name | MapDiffuse name <- attrs ] }
      where
        -- (diff, spec, amb)
        colours :: Maybe (Colour Double, Colour Double, Colour Double)
        colours = (,,) <$>
                    listToMaybe [ c | (Diffuse  c) <- attrs ] <*>
                    listToMaybe [ c | (Specular c) <- attrs ] <*>
                    listToMaybe [ c | (Ambient  c) <- attrs ]


-- |
-- TODO: Debug information (eg. how many invalid materials were filtered out)
-- TODO: Refactor, simplify
createMTLTable :: [(T.Text, [SimpleMTLToken])] -> Either String (SimpleMTLTable)
createMTLTable mtls = M.fromList <$> mapM (\ (name, tokens) -> (name,) <$> materialsOf tokens) mtls

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
createModel :: SimpleOBJ -> SimpleMTLTable -> Maybe FilePath -> Either String (SimpleModel) -- Either String SimpleModel
createModel tokens materials root = do
    faces' <- sequence $ facesOf tokens materials -- TODO: Don't just filter out the degenerate faces
    return $ Model { fVertices  = V.fromList [ vec | OBJVertex  vec <- tokens ],
                     fNormals   = V.fromList [ vec | OBJNormal  vec <- tokens ],
                     fTexcoords = V.fromList [ vec | OBJTexture vec <- tokens ],
                     fFaces     = packFaces faces',
                     fGroups    = groupsOf  tokens,
                     fObjects   = objectsOf tokens,
                     fMaterials = materials,
                     fRoot      = root }
  where
    packFace :: Face Double T.Text Int64 [] -> Face Double T.Text Int64 V.Vector
    packFace face@(Face { fIndices }) = face { fIndices=V.fromList fIndices } -- indices %~ (_) -- TODO: Type-changing lenses

    packFaces :: [] (Face Double T.Text Int64 []) -> V.Vector (Face Double T.Text Int64 V.Vector)
    packFaces = V.fromList . map (packFace . tessellate)


-- TODO: Specialise to [[Face]] (?)
-- TODO: Check vertex count (has to be atleast three)
-- TODO: Better names (?)
tessellate :: SimpleFace -> SimpleFace
tessellate = indices %~ triangles
  where
    triangles []       = []
    triangles (a:rest) = concat $ pairwise (\b c -> [a, b, c]) rest


-- |
-- TODO: Deal with empty vertex lists (?)
-- TODO: Refactor
-- boundingbox :: (RealFloat n, Ord n) => Model -> BoundingBox (Vector3D n)
-- TODO: Folding over applicative (fold in parallel)
-- TODO: Make sure the order is right
bounds :: (Num f, Ord f, Foldable m, HasVertices (Model f s i m) (m (V3 f))) => Model f s i m -> BoundingBox (V3 f)
bounds model = fromExtents $ (axisBounds $ model^.vertices) <$> V3 x y z
  where
    -- TODO: Factor out 'minmax'
    minmaxBy :: (Ord o, Num o, Foldable m) => (a -> o) -> m a -> (o, o)
    minmaxBy f values = foldr (\val' acc -> let val = f val' in (min val (fst acc), max val (snd acc))) (0, 0) values -- TODO: Factor out

    axisBounds vs axis = minmaxBy (^.axis) vs


-- TODO: Deal with missing values properly
-- TODO: Indexing should be defined in an API function

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Factor out the buffer-building logic
fromIndices :: V.Vector (v Double) -> (VertexIndices Int64 ->  Int64) -> V.Vector (Face Double T.Text Int64 V.Vector) -> V.Vector (Maybe (v Double))
fromIndices data' choose faces' = V.concatMap (fromFaceIndices data' choose) faces'


-- |
fromFaceIndices :: V.Vector (v Double) -> (VertexIndices Int64 ->  Int64) -> Face Double T.Text Int64 V.Vector -> V.Vector (Maybe (v Double))
fromFaceIndices data' choose face' = V.map ((data' !?) . fromIntegral . subtract 1 . choose) . (^.indices) $ face'


-- |
diffuseColours :: V.Vector (Face f s i V.Vector) -> V.Vector (Colour f)
diffuseColours faces' = V.concatMap (\f -> V.replicate (V.length $ f^.indices) (f^.material.diffuse)) faces'

-- Model queries ---------------------------------------------------------------------------------------------------------------------------

-- |
hasTextures :: Ord s => Model f s i m -> Bool
hasTextures =  not . S.null . textures -- (/= Nothing)


-- | All texture names as a list
-- TODO: Wrap in Maybe (instead of empty list) (?)
textures :: Ord s => Model f s i m -> S.Set s
textures = S.fromList . catMaybes . map (^.texture) . concatMap M.elems . M.elems . (^.materials)
