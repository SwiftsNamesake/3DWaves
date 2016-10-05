-- |
-- Module      : Graphics.WaveFront.Parse.Common
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
{-# LANGUAGE OverloadedStrings #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- Section
--------------------------------------------------------------------------------------------------------------------------------------------
module Graphics.WaveFront.Parse.Common where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import qualified Data.Text as T

import qualified Data.Attoparsec.Text       as Atto

import Control.Applicative (pure, liftA2, (<$>), (<*>), (<*), (*>), (<|>))

import Linear (V2(..), V3(..))

import Graphics.WaveFront.Types



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions (pure)
--------------------------------------------------------------------------------------------------------------------------------------------

-- Jon's little helpers --------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Make sure this is right
-- OBJ rows may be separated by one or more lines of comments and whitespace, or empty lines.
lineSeparator :: Atto.Parser ()
lineSeparator = Atto.skipMany1 $ ignore space *> ignore comment *> Atto.endOfLine


-- |
comment :: Atto.Parser T.Text
comment = Atto.skipSpace *> Atto.char '#' *> Atto.takeTill (\c -> (c == '\r') || (c == '\n')) -- TODO: Is the newline consumed (?)


-- |
-- TODO: Use 'try' to enforce backtracking (?)
optional :: Atto.Parser a -> Atto.Parser (Maybe a)
optional p = Atto.option Nothing (Just <$> p)


-- | Like Atto.skipMany, except it skips one match at the most
ignore :: Atto.Parser a -> Atto.Parser ()
ignore p = optional p *> pure ()


-- | 
atleast :: Int -> Atto.Parser a -> Atto.Parser [a]
atleast n p = liftA2 (++) (Atto.count n p) (Atto.many' p)


-- | Skips atleast one white space character (not including newlines and carriage returns)
space :: Atto.Parser ()
space = Atto.skipMany1 (Atto.satisfy isLinearSpace)


-- |
-- TODO: Unicode awareness (cf. Data.Char.isSpace)
isLinearSpace :: Char -> Bool
isLinearSpace c = (c == ' ') || (c == '\t')


-- |
word :: Atto.Parser T.Text
word = T.pack <$> Atto.many1 Atto.letter


-- | Used primarily for object and group names
-- TODO: Use Unicode groups, make more robust (?)
name :: Atto.Parser T.Text
name = T.pack <$> Atto.many1 (Atto.satisfy $ \c -> (c /= ' ') && (c /= '\t') && (c /= '\r') && (c /= '\n'))


-- |
parenthesised :: Atto.Parser a -> Atto.Parser a
parenthesised p = Atto.char '(' *> p <* Atto.char ')'

-- TODO: Allow scientific notation (?)

-- |
-- TODO: Polymorphic
coord :: Fractional f => Atto.Parser f
coord = space *> (parenthesised Atto.rational <|> Atto.rational)


-- | Parser a single colour channel
-- TODO: Clamp to [0,1] (cf. partial from monadplus) (?)
channel :: Fractional f => Atto.Parser f
channel = space *> (parenthesised Atto.rational <|> Atto.rational)


-- |
colour :: Fractional f => Atto.Parser (Colour f)
colour = Colour <$> channel <*> channel <*> channel <*> Atto.option 1 channel


-- | 
point3D :: Atto.Parser (V3 Double)
point3D = V3 <$> coord <*> coord <*> coord


-- |
point2D :: Atto.Parser (V2 Double)
point2D = V2 <$> coord <*> coord
