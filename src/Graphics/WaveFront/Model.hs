-- |
-- Module      : Graphics.WaveFront.Model
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, October 2 2016
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- TODO | - Single-pass (eg. consume all tokens only once) for additional performance (?)
--        - 

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Extensions
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE OverloadedLists   #-}



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
import           Data.Vector (Vector, (!?))

import           Data.Text (Text)
import qualified Data.Map  as M
import           Data.Map (Map)
import qualified Data.Set  as S
import           Data.Set (Set)

import Data.List   (groupBy)
import Data.Maybe  (listToMaybe, catMaybes)

import Linear (V3(..))

import Control.Lens ((^.), (.~), (%~), (&), _1, _2, _3)

import Cartesian.Core (BoundingBox(..), fromExtents, x, y, z)

import Graphics.WaveFront.Types
import Graphics.WaveFront.Lenses



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - Factor out and make generic
pairwise :: (a -> a -> b) -> [a] -> [b]
pairwise f xs = zipWith f xs (drop 1 xs)


-- |
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b)  = Right b
maybeToEither a (Nothing) = Left a

-- Parser output churners (OBJ) ------------------------------------------------------------------------------------------------------------

-- TODO | - Move to separate module (eg. WaveFront.Model)

-- | Creates a mapping between group names and the corresponding bounds ([lower, upper)). Invalid
--   tokens are simply discarded by this function.
--
-- TODO | - Figure out how to deal with multiple group names (eg. "g mesh1 nose head")
groupsOf :: (Ord s, Integral i) => [OBJToken f s i m] -> Map (Set s) (i, i)
groupsOf = buildIndexMapWith . filter notObject
  where
    notObject (Object _) = False
    notObject  _         = True


-- |
objectsOf :: (Ord s, Integral i) => [OBJToken f s i m] -> Map (Set s) (i, i)
objectsOf = buildIndexMapWith . filter notGroup
  where
    notGroup (Group _) = False
    notGroup  _        = True


-- | Creates a mapping between names (of groups or objects) to face indices
--
-- TODO | - Refactor, simplify
--        - What happens if the same group or object appears multiple times (is that possible?)
--
buildIndexMapWith :: (Ord s, Integral i) => [OBJToken f s i m] -> Map (Set s) (i, i)
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
-- TODO | - Don't use foldl (?)
--        - Improve naming scheme (lots of primes)
--        - Default material, take 'error-handling' function (?)
--        - Can vertices in the same face have different materials (?)
facesOf :: Ord s => [OBJToken f s i m] -> MTLTable f s -> [Either String (Face f s i m)]
facesOf tokens materials' = reverse . (^._3) . foldl update (Nothing, Nothing, []) $ tokens
  where 
    -- TODO: Keep refactoring...
    update acc@(Just libName, Just matName, faces') (OBJFace indices') = acc & _3 .~ (createFace materials' libName matName indices' : faces')
    update acc@(Nothing,      _,            faces') (OBJFace _)        = acc & _3 .~ (Left "No library selected for face" : faces')
    update acc@(_,            Nothing,      faces') (OBJFace _)        = acc & _3 .~ (Left "No material selected for face" : faces')
    update acc                                      (LibMTL  libName)  = acc & _1 .~ (Just libName)
    update acc                                      (UseMTL  matName)  = acc & _2 .~ (Just matName)
    update acc                                       _                 = acc


-- |
createFace :: Ord s => MTLTable f s -> s -> s -> m (VertexIndices i) -> Either String (Face f s i m)
createFace materials' libName matName indices' = do
  material' <- lookupMaterial materials' libName matName
  Right $ Face { fIndices=indices', fMaterial=material' }


-- |
-- TODO: Specify missing material or library name (would require additional constraints on 's')
-- TODO: Refactor
lookupMaterial :: Ord s => MTLTable f s -> s -> s -> Either String (Material f s)
lookupMaterial materials' libName matName = do
  library <- maybeToEither "No such library" (M.lookup libName materials')
  maybeToEither "No such material" (M.lookup matName library)

-- Parser output churners (MTL) ------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - Refactor, simplify
createMTLTable :: Ord s => [(s, [MTLToken f s])] -> Either String (MTLTable f s)
createMTLTable = fmap M.fromList . mapM (\(name, tokens) -> (name,) <$> materialsOf tokens)


-- | Constructs a map between names and materials. Incomplete material definitions
--   result in an error (Left ...).
--
-- TODO | - Debug information (eg. attributes without an associated material)
--        - Pass in error function (would allow for more flexible error handling) (?)
--        - Deal with duplicated attributes (probably won't crop up in any real situations)
materialsOf :: Ord s => [MTLToken f s] -> Either String (Map s (Material f s))
materialsOf = fmap M.fromList . mapM createMaterial . partitionMaterials


-- | Creates a new (name, material) pair from a stream of MTL tokens.
--   The first token should be a new material name.
createMaterial :: [MTLToken f s] -> Either String (s, Material f s)
createMaterial (NewMaterial name:attrs) = (name,) <$> fromAttributes attrs
createMaterial  attrs                   = Left  $ "Free-floating attributes"


-- |
-- TODO | - Rename (eg. groupMaterials) (?)
partitionMaterials :: [MTLToken f s] -> [[MTLToken f s]]
partitionMaterials = groupBy (\_ b -> not $ isNewMaterial b)
  where
    isNewMaterial (NewMaterial _) = True
    isNewMaterial _               = False


-- | Creates a material
fromAttributes :: [MTLToken f s] -> Either String (Material f s)
fromAttributes attrs = case colours' of
  Nothing                -> Left  $ "Missing colour(s)" -- TODO: More elaborate message (eg. which colour)
  Just (amb, diff, spec) -> Right $ Material { fAmbient=amb,fDiffuse=diff, fSpecular=spec, fTexture=texture' }
  where
    colours' = materialColours attrs
    texture' = listToMaybe [ name | MapDiffuse name <- attrs ]


-- | Tries to extract a diffuse colour, a specular colour, and an ambient colour from a list of MTL tokens
-- TODO | - Should we really require all three colour types (?)
--        - Rename (?)
materialColours :: [MTLToken f s] -> Maybe (Colour f, Colour f, Colour f)
materialColours attrs = (,,) <$>
                          listToMaybe [ c | (Diffuse  c) <- attrs ] <*>
                          listToMaybe [ c | (Specular c) <- attrs ] <*>
                          listToMaybe [ c | (Ambient  c) <- attrs ]

-- API functions ---------------------------------------------------------------------------------------------------------------------------

-- | Constructs a model from a stream of OBJ tokens, a materials table and an optional path to root of the model (used for textures, etc.)
--
-- TODO | - Performance, how are 'copies' of coordinates handled (?)
--        - Performance, one pass (with a fold perhaps)
--
-- I never knew pattern matching in list comprehensions could be used to filter by constructor
createModel :: (Ord s, Integral i) => OBJ f s i [] -> MTLTable f s -> Maybe FilePath -> Either String (Model f s i Vector)
createModel tokens materials root = do
    faces' <- sequence $ facesOf tokens materials
    return $ Model { fVertices  = V.fromList [ vec | OBJVertex  vec <- tokens ],
                     fNormals   = V.fromList [ vec | OBJNormal  vec <- tokens ],
                     fTexcoords = V.fromList [ vec | OBJTexture vec <- tokens ],
                     fFaces     = packFaces faces',
                     fGroups    = groupsOf  tokens,
                     fObjects   = objectsOf tokens,
                     fMaterials = materials,
                     fRoot      = root }
  where
    packFace :: Face f s i [] -> Face f s i Vector
    packFace face@Face{fIndices} = face { fIndices=V.fromList fIndices } -- indices %~ (_) -- TODO: Type-changing lenses

    packFaces :: [] (Face f s i []) -> Vector (Face f s i Vector)
    packFaces = V.fromList . map (packFace . tessellate)


-- |
-- TODO | - Specialise to [[Face]] (?)
--        - Check vertex count (has to be atleast three)
--        - Better names (?)
tessellate :: Face f s i [] -> Face f s i []
tessellate = indices %~ triangles
  where
    triangles []       = []
    triangles (a:rest) = concat $ pairwise (\b c -> [a, b, c]) rest


-- |
-- TODO | - Deal with empty vertex lists (?)
--        - Refactor
--        - Folding over applicative (fold in parallel)
--        - Make sure the order is right
bounds :: (Num f, Ord f, Foldable m, HasVertices (Model f s i m) (m (V3 f))) => Model f s i m -> BoundingBox (V3 f)
bounds model = fromExtents $ axisBounds (model^.vertices) <$> V3 x y z
  where
    -- TODO: Factor out 'minmax'
    minmaxBy :: (Ord o, Num o, Foldable m) => (a -> o) -> m a -> (o, o)
    minmaxBy f values = foldr (\val' acc -> let val = f val' in (min val (fst acc), max val (snd acc))) (0, 0) values -- TODO: Factor out

    axisBounds vs axis = minmaxBy (^.axis) vs


-- TODO: Deal with missing values properly
-- TODO: Indexing should be defined in an API function

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - Factor out the buffer-building logic
fromIndices :: Integral i => V.Vector (v Double) -> (VertexIndices i ->  i) -> V.Vector (Face Double Text i V.Vector) -> V.Vector (Maybe (v Double))
fromIndices data' choose faces' = V.concatMap (fromFaceIndices data' choose) faces'


-- |
fromFaceIndices :: Integral i => V.Vector (v Double) -> (VertexIndices i ->  i) -> Face Double Text i V.Vector -> V.Vector (Maybe (v Double))
fromFaceIndices data' choose face' = V.map ((data' !?) . fromIntegral . subtract 1 . choose) . (^.indices) $ face'


-- |
diffuseColours :: V.Vector (Face f s i V.Vector) -> V.Vector (Colour f)
diffuseColours faces' = V.concatMap (\f -> V.replicate (V.length $ f^.indices) (f^.material.diffuse)) faces'

-- Model queries ---------------------------------------------------------------------------------------------------------------------------

-- |
hasTextures :: Ord s => Model f s i m -> Bool
hasTextures =  not . S.null . textures


-- | All texture names as a list
textures :: Ord s => Model f s i m -> S.Set s
textures = S.fromList . catMaybes . map (^.texture) . concatMap M.elems . M.elems . (^.materials)
