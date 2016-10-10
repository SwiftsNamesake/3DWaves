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

-- TODO | - Logging
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
import System.FilePath (splitFileName, takeDirectory, (</>))

import           Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           Data.Vector (Vector)

import Control.Applicative ((<$>))
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class (lift)

import qualified Data.Attoparsec.Text as Atto

import           Graphics.WaveFront.Types
import qualified Graphics.WaveFront.Parse as Parse
import qualified Graphics.WaveFront.Parse.Common as Parse
import           Graphics.WaveFront.Model (createMTLTable, createModel)



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions (IO)
--------------------------------------------------------------------------------------------------------------------------------------------

-- Loading data ----------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - Use bytestrings (?)
--        - Deal with IO and parsing errors
obj :: (Fractional f, Integral i) => String -> IO (Either String (OBJ f Text i []))
obj fn = runEitherT $ do
  lift $ putStrLn $ "Loading obj file: " ++ fn
  EitherT $ Atto.parseOnly (Parse.wholeFile Parse.obj) <$> T.readFile fn


-- |
-- TODO | - Use bytestrings (?)
--        - Merge OBJ and MTL parsers (and plug in format-specific code as needed) (?)
--        - Deal with IO and parsing errors
mtl :: (Fractional f) => String -> IO (Either String (MTL f Text []))
mtl fn = do
  putStrLn $ "Loading mtl file: " ++ fn
  Atto.parseOnly (Parse.wholeFile Parse.mtl) <$> T.readFile fn


-- |
-- TODO | - Better names (than 'mtls' and 'fns') (?)
--        - Refactor, simplify
--        - Improve path handling (cf. '</>')
--        - Graceful error handling
materials :: (Fractional f) => [FilePath] -> IO (Either String (MTLTable f Text))
materials fns = runEitherT $ do
  tokens <- mapM (EitherT . mtl) fns
  EitherT . return $ createTableFromMTLs tokens
  where
    createTableFromMTLs :: [[MTLToken f Text]] -> Either String (MTLTable f Text)
    createTableFromMTLs = createMTLTable . zip (map (T.pack . snd . splitFileName) fns)


-- | Loads an OBJ model from file, including associated materials
-- TODO | - Graceful error handling
model :: (Fractional f, Integral i) => FilePath -> IO (Either String (Model f Text i Vector))
model fn = runEitherT $ do
  obj       <- EitherT $ obj fn
  materials <- EitherT $ materials [ fst (splitFileName fn) </> T.unpack name | LibMTL name <- obj ]
  EitherT . return $ createModel obj materials (Just $ takeDirectory fn)
  -- where loadWithName name = mtl name >>= return . (name,)
