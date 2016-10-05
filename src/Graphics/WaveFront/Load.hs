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

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Control.Applicative ((<$>))
import Control.Monad.Trans.Either

import qualified Data.Attoparsec.Text as Atto

import           Graphics.WaveFront.Types
import qualified Graphics.WaveFront.Parse as Parse
import           Graphics.WaveFront.Model (createMTLTable, createModel)



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions (IO)
--------------------------------------------------------------------------------------------------------------------------------------------

-- Loading data ----------------------------------------------------------------------------------------------------------------------------

-- |
--
-- TODO: Use bytestrings (?)
-- TODO: Deal with IO and parsing errors
obj :: String -> IO (Either String (SimpleOBJ))
obj fn = Atto.parseOnly Parse.obj <$> T.readFile fn    --


-- |
--
-- TODO: Use bytestrings (?)
-- TODO: Merge OBJ and MTL parsers (and plug in format-specific code as needed) (?)
-- TODO: Deal with IO and parsing errors
mtl :: String -> IO (Either String (SimpleMTL))
mtl fn = Atto.parseOnly Parse.mtl <$> T.readFile fn


-- |
-- TODO: Better names (than 'mtls' and 'fns') (?)
-- TODO: Refactor, simplify
-- TODO: Improve path handling (cf. '</>')
-- TODO: Graceful error handling
materials :: [String] -> IO (Either String (SimpleMTLTable))
materials fns = runEitherT $ createTableFromMTLs <$> mapM (EitherT . mtl) fns --
  where
    createTableFromMTLs = createMTLTable . zip (map (T.pack . snd . splitFileName) fns)


-- | Loads an OBJ model from file, including associated materials
-- TODO: Graceful error handling
model :: String -> IO (Either String (SimpleModel))
model fn = runEitherT $ do
  obj       <- EitherT $ obj fn
  materials <- EitherT $ materials [ fst (splitFileName fn) </> T.unpack name | LibMTL name <- obj ]
  return $ createModel obj materials
  -- where loadWithName name = mtl name >>= return . (name,)
