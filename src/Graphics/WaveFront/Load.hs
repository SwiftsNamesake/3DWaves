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

import qualified Data.Text    as T
import qualified Data.Text.IO as T

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
obj :: String -> IO (Either String (SimpleOBJ))
obj fn = runEitherT $ do
  lift $ putStrLn $ "Loading obj file: " ++ fn
  EitherT $ Atto.parseOnly (parseWholeFile) <$> T.readFile fn
  where
    parseWholeFile = Parse.obj <* Parse.cutToTheChase <* Atto.endOfInput


-- |
-- TODO | -  Use bytestrings (?)
--        -  Merge OBJ and MTL parsers (and plug in format-specific code as needed) (?)
--        -  Deal with IO and parsing errors
mtl :: String -> IO (Either String (SimpleMTL))
mtl fn = do
  putStrLn $ "Loading mtl file: " ++ fn
  Atto.parseOnly (Parse.mtl <* Parse.cutToTheChase <* Atto.endOfInput) <$> T.readFile fn


-- |
-- TODO | - Better names (than 'mtls' and 'fns') (?)
--        - Refactor, simplify
--        - Improve path handling (cf. '</>')
--        - Graceful error handling
materials :: [String] -> IO (Either String (SimpleMTLTable))
materials fns = runEitherT $ do
  tokens <- mapM (EitherT . mtl) fns
  EitherT . return $ createTableFromMTLs tokens
  where
    createTableFromMTLs :: [[SimpleMTLToken]] -> Either String (SimpleMTLTable)
    createTableFromMTLs = createMTLTable . zip (map (T.pack . snd . splitFileName) fns)


-- | Loads an OBJ model from file, including associated materials
-- TODO | - Graceful error handling
model :: String -> IO (Either String (SimpleModel))
model fn = runEitherT $ do
  obj       <- EitherT $ obj fn
  materials <- EitherT $ materials [ fst (splitFileName fn) </> T.unpack name | LibMTL name <- obj ]
  EitherT . return $ createModel obj materials (Just $ takeDirectory fn)
  -- where loadWithName name = mtl name >>= return . (name,)
