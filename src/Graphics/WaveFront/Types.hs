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
data OBJToken = OBJVertex  Float Float Float          |
                OBJNormal  Float Float Float          |
                OBJTexture Float Float                |
                OBJFace [(Int, Maybe Int, Maybe Int)] | -- TODO: Associate material with each face, handle absent indices

                UseMTL String | --
                LibMTL String | -- TODO: Use actual MTL type

                Group  [String] |   -- TODO: Do grouped faces have to be consecutive?
                Object [String]     -- TODO: What is the difference between group and object?
                deriving (Eq, Show) -- TODO: Derive Read (?)


-- |
-- TODO: Rename (?)
data OBJNoParse = OBJComment String | OBJEmpty | OBJNoSuchAttribute String | OBJNoParse String deriving (Show)


-- |
-- TODO: Use error type instead of String, allowing us to distinguish invalid data
--       from eg. comments and blank lines (?)
type OBJRow = (Int, Either OBJNoParse OBJToken, String)


-- | Output type of the OBJ parser. Currently a list of line number and token (or error string) pairs
--
-- TODO: Rename (?)
-- TODO: Use Integral for line number (?)
--
type OBJ = [OBJRow]

-- MTL parser types ------------------------------------------------------------------------------------------------------------------------

-- | Represents a single (valid) MTL token
--
-- TODO: Is the alpha channel optional, ignored, disallowed?
-- TODO: Include support for ('Ns', 'Ni', 'd', 'Tr', 'illum')
--
data MTLToken = Ambient  Float Float Float (Maybe Float) | -- Ka
                Diffuse  Float Float Float (Maybe Float) | -- Kd
                Specular Float Float Float (Maybe Float) | -- Ks

                MapDiffuse  String | -- map_Kd
                NewMaterial String   -- newmtl
                deriving (Eq, Show)


-- |
-- TODO: Rename (?)
data MTLNoParse = MTLComment String | MTLEmpty | MTLNoSuchAttribute String | MTLNoParse String deriving (Show)


-- | Output type of the single-row MTL parser.
type MTLRow = (Int, Either MTLNoParse MTLToken, String)


-- | Output type of the MTL parser. Currently a list of line number and token (or error string) pairs
--
-- TODO: Add type for processed MTL (eg. a map between names and materials)
--
type MTL = [MTLRow] -- (line number, MTL token, comment)


-- |
type MTLTable = Map.Map String (Map.Map String Material)

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

type Vertices  = [Vector Float]
type TexCoords = [Maybe (Point Float)]
type Normals   = [Maybe (Vector Float)]
type Materials = [Material]

-- General types ---------------------------------------------------------------------------------------------------------------------------

type Vector num = (num, num, num) -- Queen Vectoria
type Point  num = (num, num)      -- Haskell is no longer Point-free

-- API types -------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Validation (eg. length ivertices == length == ivertices == length itextures if length isn't 0)
-- TOOD: Pack indices in a tuple (eg. indices :: [(Int, Int, Int)]) (?)
-- TOOD: Use (String, String) for the names of the mtl file and material instead of Material (?)
-- TODO: Use types so as not to confuse the indices (eg. newtype INormal, newtype ITexcoord)
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
-- TODO: Rename (?)
-- TODO: Include metadata, comments, rejected data (?)
-- TODO: Separate type for processed OBJTokens (ie. token + context)
-- TODO: Perform index lookups (?)
-- TODO: Reconsider the types (especially of the materials)
-- TODO: Rename accessor functions (eg. texcoords instead of textures) (?)
--
data Model = Model { vertices  :: [Vector Float],
                     normals   :: [Vector Float],
                     texcoords :: [Point  Float],
                     faces     :: [Face],
                     materials :: MTLTable, -- TODO: Type synonym (?)
                     groups    :: Map.Map [String] (Int, Int), -- TODO: Type synonym
                     objects   :: Map.Map [String] (Int, Int)  -- TODO: Type synonym
                   } deriving (Show)


-- |
-- TODO: Use BoundingBox from Cartesian instead
data BoundingBox n = BoundingBox { left :: n, right :: n, top :: n, bottom :: n, front :: n, back :: n }

-- Foreign ---------------------------------------------------------------------------------------------------------------------------------

-- |
newtype COBJ = COBJ OBJ


-- |
newtype CMTL = CMTL MTL


-- | We
instance Storable COBJ where
  sizeOf    = const 0
  alignment = const 0
  peek _    = error "Work in progress"
  poke _    = error "Work in progress"


-- | We
instance Storable CMTL where
  sizeOf    = const 0
  alignment = const 0
  peek _    = error "Work in progress"
  poke _    = error "Work in progress"
