-- |
-- Module      : Graphics.WaveFront.Parse.MTL
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, October 2 2016
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)

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
{-# LANGUAGE NamedFieldPuns    #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Graphics.WaveFront.Parse.MTL (
  mtl, row, token,
  ambient, diffuse, specular,
  mapDiffuse, newMaterial
) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
-- import           Data.Int    (Int64)
-- import qualified Data.Map  as M
-- import qualified Data.Set  as S
-- import qualified Data.Text as T
-- import qualified Data.Vector as V

import qualified Data.Attoparsec.Text as Atto

import Control.Applicative ((<$>), (<*), (*>), (<|>))

import Graphics.WaveFront.Parse.Common

import Graphics.WaveFront.Types hiding (ambient, diffuse, specular)



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- MTL parsing -----------------------------------------------------------------------------------------------------------------------------

-- | Produces a list of MTL tokens, with associated line numbers and comments
mtl :: Atto.Parser (SimpleMTL)
mtl = Atto.sepBy row lineSeparator -- <* Atto.endOfInput


-- | Parses a single MTL row.
row :: Atto.Parser (SimpleMTLToken)
row = token <* ignore comment

--------------------------------------------------------------------------------------------------------------------------------------------

-- | Parse an MTL token
-- TODO: How to deal with common prefix (Ka, Kd, Ks) (backtrack?)
token :: Atto.Parser (SimpleMTLToken)
token = (Atto.string "Ka"     *> ambient)     <|>
        (Atto.string "Kd"     *> diffuse)     <|>
        (Atto.string "Ks"     *> specular)    <|>
        (Atto.string "map_Kd" *> mapDiffuse)  <|>
        (Atto.string "newmtl" *> newMaterial)

--------------------------------------------------------------------------------------------------------------------------------------------

-- TODO: Expose these parsers for testing purposes (?)

-- |
ambient :: Atto.Parser (SimpleMTLToken)
ambient = space *> (Ambient  <$> colour)


-- |
diffuse :: Atto.Parser (SimpleMTLToken)
diffuse = space *> (Diffuse  <$> colour)


-- |
specular :: Atto.Parser (SimpleMTLToken)
specular = space *> (Specular <$> colour)


-- |
mapDiffuse :: Atto.Parser (SimpleMTLToken)
mapDiffuse = space *> (MapDiffuse  <$> word)


-- |
newMaterial :: Atto.Parser (SimpleMTLToken)
newMaterial = space *> (NewMaterial <$> word)