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
-- import qualified Data.Map  as M
-- import qualified Data.Set  as S
-- import qualified Data.Vector as V
import Data.Text (Text)

import qualified Data.Attoparsec.Text as Atto

import Control.Applicative ((<$>), (<*), (*>), (<|>))

import Graphics.WaveFront.Parse.Common

import Graphics.WaveFront.Types hiding (ambient, diffuse, specular)



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- MTL parsing -----------------------------------------------------------------------------------------------------------------------------

-- | Produces a list of MTL tokens, with associated line numbers and comments
mtl :: (Fractional f) => Atto.Parser (MTL f Text [])
mtl = Atto.sepBy row lineSeparator -- <* Atto.endOfInput


-- | Parses a single MTL row.
row :: (Fractional f) => Atto.Parser (MTLToken f Text)
row = token <* ignore comment

--------------------------------------------------------------------------------------------------------------------------------------------

-- | Parse an MTL token
-- TODO: How to deal with common prefix (Ka, Kd, Ks) (backtrack?)
token :: (Fractional f) => Atto.Parser (MTLToken f Text)
token = (Atto.string "Ka"     *> ambient)     <|>
        (Atto.string "Kd"     *> diffuse)     <|>
        (Atto.string "Ks"     *> specular)    <|>
        (Atto.string "Ns"     *> specExp)     <|>
        (Atto.string "illum"  *> illum)       <|>
        (Atto.string "Ni"     *> refraction)  <|>
        (Atto.string "d"      *> dissolve)    <|> -- TODO: Handle inverse as well (cf. 'Tr' attribute)
        (Atto.string "map_Kd" *> mapDiffuse)  <|>
        (Atto.string "map_Ka" *> mapAmbient)  <|>
        (Atto.string "newmtl" *> newMaterial)

--------------------------------------------------------------------------------------------------------------------------------------------

-- TODO: Expose these parsers for testing purposes (?)

-- |
ambient :: (Fractional f) => Atto.Parser (MTLToken f s)
ambient = Ambient <$> colour


-- |
diffuse :: (Fractional f) => Atto.Parser (MTLToken f s)
diffuse = Diffuse <$> colour


-- |
specular :: (Fractional f) => Atto.Parser (MTLToken f s)
specular = Specular <$> colour


-- |
specExp :: (Fractional f) => Atto.Parser (MTLToken f s)
specExp = space *> (SpecularExponent <$> Atto.rational)


-- |
illum :: Atto.Parser (MTLToken f s)
illum = space *> (Illum <$> clamped 0 10)


-- |
refraction :: (Fractional f) => Atto.Parser (MTLToken f s)
refraction = space *> (Refraction <$> Atto.rational)


-- |
dissolve :: (Fractional f) => Atto.Parser (MTLToken f s)
dissolve = space *> (Dissolve <$> Atto.rational)


-- |
mapDiffuse :: Atto.Parser (MTLToken f Text)
mapDiffuse = space *> (MapDiffuse <$> name)


-- |
mapAmbient :: Atto.Parser (MTLToken f Text)
mapAmbient = space *> (MapAmbient <$> name)


-- |
newMaterial :: Atto.Parser (MTLToken f Text)
newMaterial = space *> (NewMaterial <$> name)