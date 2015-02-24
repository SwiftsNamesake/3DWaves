--
-- Wavefront - checks.hs
-- Executable containing checks and tests for the modules in this package
--
-- Jonatan H Sundqvist
-- February 24 2015
--

-- TODO | -
--        -

-- SPEC | -
--        -



module Checks where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Text.Printf (printf)
import Data.Either (rights, lefts)
import System.IO (hFlush, stdout)

import Parsers (loadOBJ, loadMTL)


---------------------------------------------------------------------------------------------------
-- Functions (IO)
---------------------------------------------------------------------------------------------------
-- IO utilities -----------------------------------------------------------------------------------
-- | 
promptContinue :: String -> IO ()
promptContinue prompt = do
  putStr prompt
  hFlush stdout
  getChar
  putChar '\n'



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
-- | 
--
-- TODO: Print culprit lines (✓)
--
main :: IO ()
main = do
  putStrLn "This is where the checks should be."

  let path = "C:/Users/Jonatan/Desktop/Python/experiments/WaveFront/"
  
  flip mapM_ ["queen", "cube"] $ \ fn -> do
    printf "\nParsing OBJ file: %s.obj\n" fn
    model <- loadOBJ $ printf (path ++ "data/%s.obj") fn
    printf "Found %d invalid rows in OBJ file (n comments, m blanks, o errors).\n" . length . lefts $ map snd model

    promptContinue "Press any key to continue..."

    mapM (uncurry $ printf "[%d] %s\n") [ (n, show token) | (n, Right token) <- model ]

    promptContinue "Press any key to continue..."

    printf "\nParsing MTL file: %s.mtl\n" fn
    materials <- loadMTL $ printf (path ++ "data/%s.mtl") fn
    printf "Found %d invalid rows in MTL file (n comments, m blanks, o errors).\n" . length . lefts $ map snd materials
    mapM (uncurry $ printf "[%d] %s\n") [ (n, show token) | (n, Right token) <- materials ]

    promptContinue "Press any key to continue..."