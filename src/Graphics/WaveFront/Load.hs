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

import           Data.Either (rights, isLeft)
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Control.Lens ((^.), _2)
import Control.Applicative ((<$>), (<*>))

import qualified Data.Attoparsec.Text as Atto

import Graphics.WaveFront.Types
import Graphics.WaveFront.Parse (parseOBJ, parseMTL, createMTLTable, createModel)



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions (IO)
--------------------------------------------------------------------------------------------------------------------------------------------

-- Loading data ----------------------------------------------------------------------------------------------------------------------------

-- |
--
-- TODO: Use bytestrings (?)
-- TODO: Deal with IO and parsing errors
loadOBJ :: String -> IO (Either String (OBJ f s i m))
loadOBJ fn = Atto.parseOnly parseOBJ <$> T.readFile fn    --


-- |
--
-- TODO: Use bytestrings (?)
-- TODO: Merge OBJ and MTL parsers (and plug in format-specific code as needed) (?)
-- TODO: Deal with IO and parsing errors
loadMTL :: String -> IO (Either String (MTL f s m))
loadMTL fn = Atto.parseOnly parseMTL <$> T.readFile fn


-- |
-- TODO: Better names (than 'mtls' and 'fns') (?)
-- TODO: Refactor, simplify
-- TODO: Improve path handling (cf. '</>')
-- TODO: Graceful error handling
loadMaterials :: [String] -> IO (MTLTable f s)
loadMaterials fns = createMTLTable . zip (map (snd . splitFileName) fns) . map tokensOf <$> mapM loadMTL fns --
  where
    tokensOf = rights . map (^._2)


-- | Loads an OBJ model from file, including associated materials
-- TODO: Graceful error handling
loadModel :: String -> IO (Model f s i m)
loadModel fn = do
  obj       <- loadOBJ fn
  materials <- loadMaterials [ (fst $ splitFileName fn) </> name | LibMTL name <- rights $ map (^._2) obj ]
  return $ createModel obj materials
  -- where loadWithName name = loadMTL name >>= return . (name,)
