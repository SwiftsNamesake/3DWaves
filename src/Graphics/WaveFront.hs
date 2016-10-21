-- |
-- Module      : Graphics.WaveFront
-- Description : Re-exports public API
-- Copyright   : (c) Jonatan H Sundqvist, 2016
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)

-- TODO | - Logging
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Extensions
--------------------------------------------------------------------------------------------------------------------------------------------



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
-- TODO | - Decide on an API
module Graphics.WaveFront (
  -- ^ OBJ types
  OBJToken(..), VertexIndices(..), OBJ,
  
  -- ^ MTL types
  MTLToken(..), Illumination(..), MTL, MTLTable(..),
  
  -- ^ Model types
  Face(..), Colour(..), Material(..), Model(..),
  
  -- ^ Parsing
  module Graphics.WaveFront.Parse,
  
  -- ^ Model functions
  createModel, tessellate, bounds, fromIndices, fromFaceIndices, diffuseColours, hasTextures, textures,

  -- ^ Loading
  module Load
) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Graphics.WaveFront.Types
import Graphics.WaveFront.Parse
import Graphics.WaveFront.Parse.Common
import Graphics.WaveFront.Model
import qualified Graphics.WaveFront.Load as Load
