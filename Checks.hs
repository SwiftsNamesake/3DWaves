--
-- Southpaw.Wavefront.Checks
-- Executable containing checks and tests for the modules in this package
--
-- Jonatan H Sundqvist
-- February 24 2015
--

-- TODO | - Use QuickCheck (?)
--        - Full coverage
--        - Benchmarking

-- SPEC | -
--        -



module Southpaw.WaveFront.Checks where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Text.Printf (printf)
import Data.Either (lefts)
import Data.Char (toLower)
import System.IO (hFlush, stdout)

import Control.Monad (forM_, when)

import Southpaw.WaveFront.Parsers (loadOBJ, loadMTL, MTL, OBJ)


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



-- | 
-- 
-- TODO: Refactor (cf. untilM)
-- TODO: Allow flexible feedback
-- TODO: Default return value for invalid replies (?)
-- TODO: Customisable validation (eg. for other languages than English)
--
askYesNo :: String -> IO Bool
askYesNo q = do
  putStr q
  hFlush stdout
  answer <- getLine
  affirmed $ map toLower answer
  where affirmed answer | answer `elem` ["yes", "y", "yeah"] = return True
                        | answer `elem` ["no", "n", "nah"]   = return False
                        | otherwise                          = askYesNo "I don't understand. Answer 'yes' or 'no': "
  -- return [(`elem` ["yes", "y", "yeah"]), (`elem` "no", "n", "nah")]


-- |
askPerformAction :: String -> IO () -> IO ()
askPerformAction q action = do
  affirmed <- askYesNo q
  when affirmed action


-- |
showTokens :: Show a => [(Int, Either String a, String)] -> IO ()
showTokens materials = mapM_ (uncurry $ printf "[%d] %s\n") [ (n, show token) | (n, Right token, comment) <- materials ] -- TODO: cf. line 65



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

  forM_ ["queen", "cube"] $ \ fn -> do
    printf "\nParsing OBJ file: %s.obj\n" fn
    model <- loadOBJ $ printf (path ++ "data/%s.obj") fn
    -- TODO: Utility for partioning a list based on several predicates ([a] -> [a -> Bool] -> [[a]])
    -- TODO: Utilities for displaying output and asking for input
    -- TODO: Oh, the efficiency!
    -- TODO: Less ugly naming convention for monadic functions which ignore the output (cf. mapM_, forM_, etc.)
    let unparsed = lefts $ map second model
    let comments = filter ('#' `elem`) unparsed
    let blanks   = filter null unparsed
    let errors   = length unparsed - (length comments + length blanks)
    printf "Found %d invalid rows in OBJ file (%d comments, %d blanks, %d errors).\n" (length unparsed) (length comments) (length blanks) errors
    when (length unparsed > 0) . askPerformAction "Would you like to see view them (yes/no)? " $ putStrLn "Ok, here they are:" >> mapM_ print unparsed

    promptContinue "Press any key to continue..."

    mapM (uncurry $ printf "[%d] %s\n") [ (n, show token) | (n, Right token, comment) <- model ]
    -- TODO: Print culprit lines (✓)

    promptContinue "Press any key to continue..."

    printf "\nParsing MTL file: %s.mtl\n" fn
    materials <- loadMTL $ printf "%sdata/%s.mtl" path fn
    printf "Found %d invalid rows in MTL file (n comments, m blanks, o errors).\n" . length . lefts $ map second materials
    showTokens materials

    promptContinue "Press any key to continue..."
    where second (_, b, _) = b