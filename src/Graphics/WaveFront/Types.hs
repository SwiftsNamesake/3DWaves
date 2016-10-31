-- |
-- Module      : Graphics.WaveFront.Types
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created October 30 2015

-- TODO | -
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE DuplicateRecordFields #-} -- I love GHC 8.0
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveFoldable        #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Graphics.WaveFront.Types where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Functor.Classes (Show1) --Eq1, Show1, showsPrec1, eq1)
import Data.Map as M
import Data.Set as S (Set)
import Linear (V2(..), V3(..))



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------

-- OBJ parser types ------------------------------------------------------------------------------------------------------------------------

-- TODO | - Add strictness annotations (?)


-- | Represents a single (valid) OBJ token
--
-- TODO | - Polymorphic numerical types (?)
--        - Add context, metadata (eg. line numbers, filename) (?)
--        - Naming scheme (added OBJ prefix to prevent name clashes; cf. Face type)
--        - Comment token (preserve comments in parser output or remove them) (?)
--
--        - Cover the entire spec (http://www.martinreddy.net/gfx/3d/OBJ.spec)
--          (and handle unimplemented attributes gracefully)
data OBJToken f s i m = OBJVertex   (V3 f) |
                        OBJNormal   (V3 f) |
                        OBJTexCoord (V2 f) |
                        OBJFace (m (VertexIndices i)) | -- TODO: Associate material with each face, handle absent indices

                        Line i i | -- Line (I'm assuming the arguments are indices to the endpoint vertices)

                        UseMTL s | -- TODO: Rename (eg. 'UseMaterial') (?)
                        LibMTL s | --

                        SmoothGroup (Maybe s) | -- Smooth shading group, or Nothing if it is disabled


                        -- TODO: Use OBJ prefix (?)
                        Group  (Set s) | -- TODO: Do grouped faces have to be consecutive?
                        Object (Set s)   -- TODO: What is the difference between group and object?
                        -- deriving (Show, Eq) -- TODO: Derive Read (?)


-- |
-- TODO: Rename (?)
-- TODO: Use union instead of Maybe (?)
data VertexIndices i = VertexIndices {
  fIvertex   :: i,
  fItexcoord :: Maybe i,
  fInormal   :: Maybe i
} deriving (Show, Eq)


-- | Output type of the OBJ parser.
--
-- TODO | - Rename (?)
--        - Use Integral for line number (?)
--
type OBJ f s i m = m (OBJToken f s i m)

-- MTL parser types ------------------------------------------------------------------------------------------------------------------------

-- | Represents a single (valid) MTL token
--
-- TODO | - Is the alpha channel optional, ignored, disallowed?
--        - Include support for ('Ns', 'Ni', 'd', 'Tr', 'illum')
--        - Assume no colours have an alpha channel, since transparency is handled by the 'd' attribute (?)
data MTLToken f s = Ambient  (Colour f) | -- Ka
                    Diffuse  (Colour f) | -- Kd
                    Specular (Colour f) | -- Ks

                    SpecularExponent f  | -- Ns (TODO: Find out exactly what this entails)

                    Illum Illumination  | -- illum (TODO: Find out what this means)

                    Dissolve f          | -- d (Dissolve; transparency)
                    Refraction f        | -- Ni (Index of refraction; optical_density)

                    MapDiffuse  s | -- map_Kd
                    MapAmbient  s | -- map_Ka
                    NewMaterial s   -- newmtl
                    deriving (Show, Eq)


-- |
-- 0. Color on and Ambient off
-- 1. Color on and Ambient on
-- 2. Highlight on
-- 3. Reflection on and Ray trace on
-- 4. Transparency: Glass on, Reflection: Ray trace on
-- 5. Reflection: Fresnel on and Ray trace on
-- 6. Transparency: Refraction on, Reflection: Fresnel off and Ray trace on
-- 7. Transparency: Refraction on, Reflection: Fresnel on and Ray trace on
-- 8. Reflection on and Ray trace off
-- 9. Transparency: Glass on, Reflection: Ray trace off
-- 10. Casts shadows onto invisible surfaces
type Illumination = Int


-- | Output type of the MTL parser. Currently a list of line number and token (or error string) pairs
-- TODO | - Add type for processed MTL (eg. a map between names and materials)
type MTL f s m = m (MTLToken f s) -- (line number, MTL token, comment)


-- |
type MTLTable f s = M.Map s (M.Map s (Material f s))

-- Model -----------------------------------------------------------------------------------------------------------------------------------

type Vertices  f m   = m (V3 f)
type TexCoords f m   = m (Maybe (V2 f))
type Normals   f m   = m (Maybe (V3 f))
type Materials f s m = m (Material f s)

-- API types -------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - Validation (eg. length ivertices == length == ivertices == length itextures if length isn't 0)
--        - Pack indices in a tuple (eg. indices :: [(Int, Int, Int)]) (?)
--        - Use (String, String) for the names of the mtl file and material instead of Material (?)
--        - Use types so as not to confuse the indices (eg. newtype INormal, newtype ITexcoord)
data Face f s i m = Face {
  fIndices  :: m (VertexIndices i),
  fMaterial :: Material f s
} --deriving (Show, Eq)


-- |
-- TODO | - Use a type from the colour package instead (?)
data Colour f = Colour {
  fRed   :: f,
  fGreen :: f,
  fBlue  :: f,
  fAlpha :: f
} deriving (Show, Eq, Functor, Foldable)


-- |
-- TODO | - Do all materials have an ambient, a diffuse and a specular colour (?)
--        - Support more attributes (entire spec) (?)
--        - Lenses (?)
data Material f s = Material {
  fAmbient  :: Colour f,
  fDiffuse  :: Colour f,
  fSpecular :: Colour f,
  fTexture  :: Maybe s
} deriving (Show, Eq)


-- | Abstract representation of an OBJ model with associated MTL definitions.
--
-- TODO | - Rename (?)
--        - Include metadata, comments, rejected data (?)
--        - Separate type for processed OBJTokens (ie. token + context)
--        - Perform index lookups (?)
--        - Reconsider the types (especially of the materials)
--        - Rename accessor functions (eg. texcoords instead of textures) (?)
--
-- fTextures  :: Set s,
-- data Model f s i m = Model {
data Model f s i m = Model {
  fVertices  :: m (V3 f),
  fNormals   :: m (V3 f),
  fTexcoords :: m (V2 f),
  fFaces     :: m (Face f s i m),
  fMaterials :: MTLTable f s,         -- TODO: Type synonym (?)
  fGroups    :: M.Map (Set s) (i, i), -- TODO: Type synonym
  fObjects   :: M.Map (Set s) (i, i), -- TODO: Type synonym
  fRoot      :: Maybe FilePath        -- This is where we should look for related assets
} -- deriving (Show, Eq)

-- Monomorphic defaults --------------------------------------------------------------------------------------------------------------------


-- Instances -------------------------------------------------------------------------------------------------------------------------------

-- TODO: Use Show1, Eq1, etc. (?)
-- deriving instance (Show1 m) => Show1 (m a)
-- deriving instance (Show1 m) => Show1 (m a)
-- deriving instance (Show1 m) => Show1 (m a)

-- TODO: Clean this up

-- showsPrec1 :: (Show1 f, Show a) => Int -> f a -> ShowS
deriving instance (Show1 m,
                   Show (m f),
                   Show (m (V2 f)),
                   Show (m (V3 f)),
                   Show (m (Face f s i m)),
                   Show (m s),
                   Show f,
                   Show s,
                   Show i) => Show (Model f s i m) --   where showsPrec = showsPrec1

deriving instance (Show1 m,
                   Show (m f),
                   Show (m (VertexIndices i)),
                   Show (m (V3 f)),
                   Show (m s),
                   Show f,
                   Show s,
                   Show i) => Show (Face  f s i m) --   where showsPrec = _

deriving instance (Show1 m,
                   Show (m f),
                   Show (m (VertexIndices i)), 
                   Show (m (V3 f)),
                   Show (m s),
                   Show f,
                   Show s,
                   Show i) => Show (OBJToken f s i m) -- where showsPrec = _