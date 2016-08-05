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




--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Graphics.WaveFront.Types where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import qualified Data.Map as Map
import Cartesian.Space.Types (Vertex3D(..))
import Foreign.Storable



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------

-- OBJ parser types ------------------------------------------------------------------------------------------------------------------------
-- | Represents a single (valid) OBJ token
--
-- TODO: Polymorphic numerical types (?)
-- TODO: Add context, metadata (eg. line numbers, filename) (?)
-- TODO: Naming scheme (added OBJ prefix to prevent name clashes; cf. Face type)
-- TODO: Comment token (preserve comments in parser output or remove them) (?)
data OBJToken m f s i = OBJVertex  f f f |
                        OBJNormal  f f f |
                        OBJTexture f f   |
                        OBJFace (m (i, Maybe i, Maybe i)) | -- TODO: Associate material with each face, handle absent indices

                        UseMTL s | --
                        LibMTL s | -- TODO: Use actual MTL type

                        Group  (m s) |   -- TODO: Do grouped faces have to be consecutive?
                        Object (m s)     -- TODO: What is the difference between group and object?
                        deriving (Eq, Show) -- TODO: Derive Read (?)


-- |
-- TODO: Rename (?)
data OBJNoParse s = OBJComment s | OBJEmpty | OBJNoSuchAttribute s | OBJNoParse s deriving (Show)


-- |
-- TODO: Use error type instead of String, allowing us to distinguish invalid data
--       from eg. comments and blank lines (?)
type OBJRow m f s i = (i, Either (OBJNoParse s) (OBJToken m f s i), s)


-- | Output type of the OBJ parser. Currently a list-like structure of line number and token (or error string) pairs
--
-- TODO: Rename (?)
-- TODO: Use Integral for line number (?)
--
type OBJ m f s i = m (OBJRow m f s i)

-- MTL parser types ------------------------------------------------------------------------------------------------------------------------

-- | Represents a single (valid) MTL token
--
-- TODO: Is the alpha channel optional, ignored, disallowed?
-- TODO: Include support for ('Ns', 'Ni', 'd', 'Tr', 'illum')
--
data MTLToken f s = Ambient  f f f (Maybe f) | -- Ka
                    Diffuse  f f f (Maybe f) | -- Kd
                    Specular f f f (Maybe f) | -- Ks

                    MapDiffuse  s | -- map_Kd
                    NewMaterial s   -- newmtl
                    deriving (Eq, Show)


-- |
-- TODO: Rename (?)
data MTLNoParse s = MTLComment s | MTLEmpty | MTLNoSuchAttribute s | MTLNoParse s deriving (Show)


-- | Output type of the single-row MTL parser.
type MTLRow f s i = (i, Either (MTLNoParse s ) (MTLToken f s), s)


-- | Output type of the MTL parser. Currently a list of line number and token (or error string) pairs
--
-- TODO: Add type for processed MTL (eg. a map between names and materials)
--
type MTL m f s i = m (MTLRow f s i) -- (line number, MTL token, comment)


-- |
type MTLTable f s = Map.Map s (Map.Map s (Material f s))

-- Model -----------------------------------------------------------------------------------------------------------------------------------

-- |
-- data Attributes = Attributes {
  -- vertexdata   :: Vertices
  -- texcoorddata :: TexCoords
-- }

-- newtype Vertices  = Vertices  [Vector Float]
-- newtype TexCoords = TexCoords [Maybe (Point Float)]
-- newtype Normals   = Normals   [Maybe (Vector Float)]
-- newtype Materials = Materials [Material]

type Vertices  m f   = m (Vector f)
type TexCoords m f   = m (Maybe (Point  f))
type Normals   m f   = m (Maybe (Vector f))
type Materials m f s = m (Material f s)

-- API types -------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Validation (eg. length ivertices == length == ivertices == length itextures if length isn't 0)
-- TOOD: Pack indices in a tuple (eg. indices :: [(Int, Int, Int)]) (?)
-- TOOD: Use (String, String) for the names of the mtl file and material instead of Material (?)
-- TODO: Use types so as not to confuse the indices (eg. newtype INormal, newtype ITexcoord)
data Face m f s i = Face { indices :: m (i, Maybe i, Maybe i), material :: Material m f s } deriving (Show)


-- |
data Colour f = Colour { red :: f, green :: f, blue :: f, alpha :: f }


-- |
-- TODO: Do all materials have an ambient, a diffuse and a specular colour (?)
-- TODO: Support more attributes (entire spec) (?)
-- TODO: Lenses (?)
data Material f s = Material { ambient :: Colour f, diffuse :: Colour f, specular :: Colour f, texture :: Maybe s } deriving (Show)


-- | Abstract representation of an OBJ model with associated MTL definitions.
--
-- TODO: Rename (?)
-- TODO: Include metadata, comments, rejected data (?)
-- TODO: Separate type for processed OBJTokens (ie. token + context)
-- TODO: Perform index lookups (?)
-- TODO: Reconsider the types (especially of the materials)
-- TODO: Rename accessor functions (eg. texcoords instead of textures) (?)
--
data Model m f s i = Model { vertices  :: m (Vector f),
                             normals   :: m (Vector f),
                             texcoords :: m (Point  f),
                             faces     :: m f,
                             materials :: MTLTable f s,         -- TODO: Type synonym (?)
                             groups    :: Map.Map (m s) (i, i), -- TODO: Type synonym
                             objects   :: Map.Map (m s) (i, i)  -- TODO: Type synonym
                            } deriving (Show)

-- Foreign ---------------------------------------------------------------------------------------------------------------------------------

-- -- |
-- newtype COBJ = COBJ OBJ
--
--
-- -- |
-- newtype CMTL = CMTL MTL
--
--
-- -- | We
-- instance Storable COBJ where
--   sizeOf    = const 0
--   alignment = const 0
--   peek _    = error "Work in progress"
--   poke _    = error "Work in progress"
--
--
-- -- | We
-- instance Storable CMTL where
--   sizeOf    = const 0
--   alignment = const 0
--   peek _    = error "Work in progress"
--   poke _    = error "Work in progress"
