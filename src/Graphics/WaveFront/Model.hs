-- |
-- Module      : Graphics.WaveFront.Model
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2016
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : stable
-- Portability : portable
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
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE OverloadedLists   #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- Section
--------------------------------------------------------------------------------------------------------------------------------------------
-- TODO | - Clean this up
--        - Decide on API
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

import Linear (V2(..), V3(..))

import Control.Lens ((^.), (.~), (%~), (&), _1, _2, _3)

import Cartesian.Core (BoundingBox(..), fromExtents, x, y, z)

import Graphics.WaveFront.Types
import Graphics.WaveFront.Lenses



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------------------------------------------------

-- TODO | - Factor out these combinators

-- | Performs a computation on adjacent pairs in a list
-- TODO | - Factor out and make generic
pairwise :: (a -> a -> b) -> [a] -> [b]
pairwise f xs = zipWith f xs (drop 1 xs)


-- | Convers an Either to a Maybe
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe (Left _)  = Nothing


-- | Converts a Maybe to an Either
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b)  = Right b
maybeToEither a (Nothing) = Left a

-- Parser output churners (OBJ) ------------------------------------------------------------------------------------------------------------

-- TODO | - Move to separate module (eg. WaveFront.Model)

-- | Creates a mapping between group names and the corresponding bounds ([lower, upper)).
--
-- TODO | - Figure out how to deal with multiple group names (eg. "g mesh1 nose head")
--        - Include not just face indices but vertex indices (makes it easier to 'slice' GPU buffers) (maybe in a separate function)
groupsOf :: (Ord s, Integral i) => [OBJToken f s i m] -> Map (Set s) (i, i)
groupsOf = buildIndexMapWith . filter notObject
  where
    notObject (Object _) = False
    notObject  _         = True


-- | Creates a mapping between object names and the corresponding bounds ([lower, upper)).
objectsOf :: (Ord s, Integral i) => [OBJToken f s i m] -> Map (Set s) (i, i)
objectsOf = buildIndexMapWith . filter notGroup
  where
    notGroup (Group _) = False
    notGroup  _        = True


-- | Creates a mapping between names (of groups or objects) to face indices
--
-- TODO | - Refactor, simplify
--        - What happens if the same group or object appears multiple times (is that possible?)
--        - Rename or add function parameter (the -With suffix implies a function parameter)
buildIndexMapWith :: (Ord s, Integral i) => [OBJToken f s i m] -> Map (Set s) (i, i)
buildIndexMapWith = M.fromList . pairwise zipIndices . update 0
  where
    zipIndices (names, low) (_, upp) = (names, (low, upp))
    
    -- TODO | - Separate Group and Object lists
    --        - Rename (?)
    --        - Factor out (might be useful for testing) (?)
    update faceCount []                = [(S.empty, faceCount)]
    update faceCount (Group names:xs)  = (names, faceCount) : update faceCount xs
    update faceCount (Object names:xs) = (names, faceCount) : update faceCount xs
    update faceCount (OBJFace _:xs)    = update (faceCount + 1) xs
    update faceCount (_:xs)            = update faceCount xs


-- | Filters out faces from a stream of OBJTokens and attaches the currently selected material,
--   as defined by the most recent LibMTL and UseMTL tokens.
facesOf :: forall f s i m. Ord s => MTLTable f s -> [OBJToken f s i m] -> [Either String (Face f s i m)]
facesOf materials' = makeFaces Nothing Nothing
  where 
    -- | It's not always rude to make faces
    -- TODO | - Keep refactoring...
    --        - Rename (?)
    makeFaces :: Maybe s -> Maybe s -> [OBJToken f s i m] -> [Either String (Face f s i m)]
    makeFaces _                  _                  []              = []
    makeFaces lib@(Just libName) mat@(Just matName) (OBJFace is:xs) = createFace materials' libName matName is : makeFaces lib mat xs

    makeFaces lib@Nothing        mat                (OBJFace _:xs)  = Left "No library selected for face"      : makeFaces lib mat xs
    makeFaces lib                mat@Nothing        (OBJFace _:xs)  = Left "No material selected for face"     : makeFaces lib mat xs

    makeFaces _                  mat                (LibMTL  libName:xs)  = makeFaces (Just libName) mat xs
    makeFaces lib                _                  (UseMTL  matName:xs)  = makeFaces lib (Just matName) xs

    makeFaces lib                mat                (_:xs)                = makeFaces lib mat xs


-- |
createFace :: Ord s => MTLTable f s -> s -> s -> m (VertexIndices i) -> Either String (Face f s i m)
createFace materials' libName matName indices' = do
  material' <- lookupMaterial materials' libName matName
  Right $ Face { fIndices=indices', fMaterial=material' }


-- | Tries to find a given material in the specified MTL table
-- TODO | - Specify missing material or library name (would require additional constraints on 's')
--        - Refactor
lookupMaterial :: Ord s => MTLTable f s -> s -> s -> Either String (Material f s)
lookupMaterial materials' libName matName = do
  library <- maybeToEither "No such library" (M.lookup libName materials')
  maybeToEither "No such material" (M.lookup matName library)

-- Parser output churners (MTL) ------------------------------------------------------------------------------------------------------------

-- | Constructs an MTL table from a list of (libraryName, token stream) pairs.
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
createMaterial  attrs                   = Left $ "Free-floating attributes"


-- | Breaks a stream of MTL tokens into lists of material definitions
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
    faces' <- sequence $ facesOf materials tokens 
    return $ Model { fVertices  = V.fromList [ vec | OBJVertex   vec <- tokens ],
                     fNormals   = V.fromList [ vec | OBJNormal   vec <- tokens ],
                     fTexcoords = V.fromList [ vec | OBJTexCoord vec <- tokens ],
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


-- | Finds the axis-aligned bounding box of the model
-- TODO | - Deal with empty vertex lists (?)
--        - Refactor
--        - Folding over applicative (fold in parallel)
--        - Make sure the order is right
bounds :: (Num f, Ord f, Foldable m, HasVertices (Model f s i m) (m (V3 f))) => Model f s i m -> BoundingBox (V3 f)
bounds model = fromExtents $ axisBounds (model^.vertices) <$> V3 x y z
  where
    -- TODO | - Factor out 'minmax'
    minmaxBy :: (Ord o, Num o, Foldable m) => (a -> o) -> m a -> (o, o)
    minmaxBy f values = foldr (\val' acc -> let val = f val' in (min val (fst acc), max val (snd acc))) (0, 0) values -- TODO: Factor out

    axisBounds vs axis = minmaxBy (^.axis) vs

-- Orphaned TODOs?

-- TODO | - Deal with missing values properly
--        - Indexing should be defined in an API function

--------------------------------------------------------------------------------------------------------------------------------------------

-- TODO | - Polymorphic indexing and traversing
--        - Profile, optimise
--        - Index buffers


-- | Takes a vector of data, an index function, a choice function, a vector of some type with indices
--   and uses the indices to constructs a new Vector with the data in the original vector.
--
-- TODO | - Factor out the buffer-building logic
--        - Rewrite the above docstring...
fromIndices :: Vector v -> (Vector v -> i -> b) -> (a -> i) -> Vector a -> Vector b
fromIndices data' index choose = V.map (index data' . choose)


-- |
fromFaceIndices :: Integral i => Vector (v f) -> (Vector (v f) -> a -> b) -> (VertexIndices i ->  a) -> Vector (Face f Text i Vector) -> Vector b
fromFaceIndices data' index choose = V.concatMap (fromIndices data' index (choose) . (^.indices))


-- |
-- TODO: Factor out per-vertex logic so we don't have to redefine this function entirely for each colour type
diffuseColours :: Vector (Face f s i Vector) -> Vector (Colour f)
diffuseColours faces' = V.concatMap (\f -> V.replicate (V.length $ f^.indices) (f^.material.diffuse)) faces'

-- TODO | - Do not create intermediate vectors (automatic fusion?)
--        - Allow fallback values (or function), or use Either
--        - Add docstrings

-- |
unindexedVertices :: Model f Text Int Vector -> Maybe (Vector (V3 f))
unindexedVertices model = sequence $ fromFaceIndices (model^.vertices) (index) (^.ivertex) (model^.faces)
  where
    index coords i = coords !? (i-1)

unindexedNormals :: Model f Text Int Vector -> Maybe (Vector (V3 f))
unindexedNormals model = sequence $ fromFaceIndices (model^.normals) (index) (^.inormal) (model^.faces)
  where
    index coords mi = mi >>= \i -> coords !? (i-1)

unindexedTexcoords :: Model f Text Int Vector -> Maybe (Vector (V2 f))
unindexedTexcoords model = sequence $ fromFaceIndices (model^.texcoords) (index) (^.itexcoord) (model^.faces)
  where
    index coords mi = mi >>= \i -> coords !? (i-1)

-- Model queries ---------------------------------------------------------------------------------------------------------------------------

-- TODO: Turn into Lenses/Getters/Isos (?)

-- | Does the model have textures?
hasTextures :: Ord s => Model f s i m -> Bool
hasTextures =  not . S.null . textures


-- | The set of all texture names
textures :: Ord s => Model f s i m -> S.Set s
textures = S.fromList . catMaybes . map (^.texture) . concatMap M.elems . M.elems . (^.materials)
