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
mtl = cutToTheChase *> Atto.sepBy row lineSeparator -- <* Atto.endOfInput


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
ambient :: Atto.Parser (SimpleMTLToken)
ambient = Ambient <$> colour


-- |
diffuse :: Atto.Parser (SimpleMTLToken)
diffuse = Diffuse <$> colour


-- |
specular :: Atto.Parser (SimpleMTLToken)
specular = Specular <$> colour


-- |
specExp :: Atto.Parser (SimpleMTLToken)
specExp = space *> (SpecularExponent <$> Atto.rational)


-- |
illum :: Atto.Parser (SimpleMTLToken)
illum = space *> (Illum <$> clamped 0 10)


-- |
refraction :: Atto.Parser (SimpleMTLToken)
refraction = space *> (Refraction <$> Atto.rational)


-- |
dissolve :: Atto.Parser (SimpleMTLToken)
dissolve = space *> (Dissolve <$> Atto.rational)


-- |
mapDiffuse :: Atto.Parser (SimpleMTLToken)
mapDiffuse = space *> (MapDiffuse  <$> name)


-- |
mapAmbient :: Atto.Parser (SimpleMTLToken)
mapAmbient = space *> (MapAmbient  <$> name)


-- |
newMaterial :: Atto.Parser (SimpleMTLToken)
newMaterial = space *> (NewMaterial <$> name)