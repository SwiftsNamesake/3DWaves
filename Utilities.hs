-- |
-- Module      : Southpaw.WaveFront.Parsers
-- Description : Parsing utilities
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
-- 

-- Created July 15 2015

-- TODO | - 
--        - 

-- SPEC | -
--        -



module Southpaw.WaveFront.Utilities where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Data.List (isPrefixOf)
import Data.Char (isSpace)



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- Parsing utilities ------------------------------------------------------------------------------
-- | Predicate for determining if a String is a comment. Comments are preceded by a '#' and any
--   number of whitespace characters (not including linebreaks). Support for comments at the end
--   of a line has yet to be added.
--
-- TODO: Drop comments at the end of a line (?)
-- TODO: Add stripComment (or extractComment) which consumes a line up until the first '#'.
-- This would allow for tokens and comments to appear on the same line.
isComment :: String -> Bool
isComment = isPrefixOf "#" . dropWhile isSpace


-- | Strips a trailing comment from an MTL or OBJ line.
dropComment :: String -> String
dropComment = takeWhile (/= '#')


-- |
takeComment :: String -> String
takeComment = dropWhile (/= '#')


-- |
withoutComment :: String -> (String -> a) -> (a, String)
withoutComment row parse = let (tokens, comment) = span (/= '#') row in (parse tokens, comment)


-- |
enumerate :: [(token, comment)] -> [(Int, token, comment)]
enumerate = zipWith prepend [1..]
  where prepend n (token, comment) = (n, token, comment)


-- | Splits a string into rows and filters out unimportant elements (empty lines and comments)
-- NOTE: This function is probably obsolete due to comments being included by the parsers
-- TODO: Higher order function for composing predicates
rows :: String -> [String]
rows = filter (not . satisfiesAny [null, isComment]) . lines
  where satisfiesAny predicates x = any ($ x) predicates


-- |
-- TODO: Use readMaybe (?)
-- TODO: Variadic 'unpacking' (or is that sinful?)
-- TODO: More informative error message (?)
-- TODO: Rename (?)
vector :: Read r => (r -> r -> r -> b) -> [String] -> Either String b
vector token (x:y:z:[]) = Right $ token (read x) (read y) (read z) -- TODO: Add back the Maybe wrapper (?)
vector _      _         = Left  $ "Pattern match failed"


-- |
second :: (a, b, c) -> b
second (_, b, _) = b

-- |
third :: (a, b, c) -> c
third (_, _, c) = c
