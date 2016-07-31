-- |
-- Module      : Graphics.WaveFront.Load
-- Description : Loading (and perhaps writing) OBJ and MTL files
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created July 26 2015

-- TODO | -
--        -

-- SPEC | -
--        -



module Graphics.WaveFront.Load where



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Extensions
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE UnicodeSyntax #-}
-- {-# LANGUAGE TupleSections #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import System.FilePath (splitFileName, (</>))
import System.IO       (hFlush, stdout)

import Data.Either (rights, isLeft)

import Control.Lens ((^.), _2)

import Graphics.WaveFront.Types
import Graphics.WaveFront.Parsers   (parseOBJ, parseMTL, createMTLTable, createModel)
import Graphics.WaveFront.Utilities



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions (IO)
--------------------------------------------------------------------------------------------------------------------------------------------
-- Loading data ----------------------------------------------------------------------------------------------------------------------------
-- |
--
-- TODO: Use bytestrings (?)
--
loadOBJ :: String -> IO (OBJ m f s i)
loadOBJ fn = do
  rawOBJ <- readFile fn    --
  return $ parseOBJ rawOBJ --


-- |
--
-- TODO: Use bytestrings (?)
-- TODO: Merge OBJ and MTL parsers (and plug in format-specific code as needed) (?)
--
loadMTL :: String -> IO MTL
loadMTL fn = do
  rawMTL <- readFile fn    -- Unparsed MTL data (text)
  return $ parseMTL rawMTL --


-- |
-- TODO: Better names (than 'mtls' and 'fns') (?)
-- TODO: Refactor, simplify
-- TODO: Improve path handling (cf. '</>')
-- TODO: Graceful error handling
loadMaterials :: [String] -> IO MTLTable
loadMaterials fns = do
  mtls <- mapM loadMTL fns --
  return . createMTLTable . zip (map (snd . splitFileName) fns) . map tokensOf $ mtls --
  where tokensOf = rights . map (^._2)


-- | Loads an OBJ model from file, including associated materials
-- TODO: Graceful error handling
loadModel :: String -> IO Model
loadModel fn = do
  obj       <- loadOBJ fn
  materials <- loadMaterials [ (fst $ splitFileName fn) </> name | LibMTL name <- rights $ map (^._2) obj ]
  return $ createModel obj materials
  -- where loadWithName name = loadMTL name >>= return . (name,)
