-- |
-- Module      : Graphics.WaveFront.Parse.Common
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, October 2 2016
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)

-- TODO | - Fully polymorphic (even in the string and list types) (?)
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
import Data.Text (Text, pack)

import qualified Data.Attoparsec.Text as Atto

import Control.Applicative (pure, liftA2, (<$>), (<*>), (<*), (*>), (<|>))

import Linear (V2(..), V3(..))

import Graphics.WaveFront.Types



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions (pure)
--------------------------------------------------------------------------------------------------------------------------------------------

-- Jon's little helpers --------------------------------------------------------------------------------------------------------------------

-- | Consumes all input, including any leading or trailing comments and whitespace
-- TODO | - Rename (?)
wholeFile :: Atto.Parser a -> Atto.Parser a
wholeFile p = cutToTheChase *> p <* cutToTheChase <* Atto.endOfInput


-- | Skips any leading comments and empty lines
-- TODO | - Rename (?)
--        - Skip whitespace
cutToTheChase :: Atto.Parser ()
cutToTheChase = Atto.skipMany ((comment *> pure ()) <|> (Atto.satisfy isLinearSpace *> pure ()) <|> Atto.endOfLine)


-- | OBJ rows may be separated by one or more lines of comments and whitespace, or empty lines.
-- TODO | - Make sure this is right
lineSeparator :: Atto.Parser ()
lineSeparator = Atto.skipMany1 $ ignore space *> ignore comment *> Atto.endOfLine


-- | Parses a comment (from the '#' to end of the line), possibly preceded by whitespace
-- TODO | - Break out the whitespace part (?)
comment :: Atto.Parser Text
comment = Atto.skipSpace *> Atto.char '#' *> Atto.takeTill (\c -> (c == '\r') || (c == '\n')) -- TODO: Is the newline consumed (?)


-- | Tries the given parser, falls back to 'Nothing' if it fails
-- TODO | - Use 'try' to enforce backtracking (?)
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


-- | Predicate for linear space (eg. whitespace besides newlines)
-- TODO | - Unicode awareness (cf. Data.Char.isSpace)
--        - Come up with a better name (?)
isLinearSpace :: Char -> Bool
isLinearSpace c = (c == ' ') || (c == '\t')


-- | One or more letters (cf. 'Atto.letter' for details)
word :: Atto.Parser Text
word = pack <$> Atto.many1 Atto.letter


-- | Used for texture, material, object and group names (and maybe others that I have yet to think of)
-- TODO | - Use Unicode groups, make more robust (?)
name :: Atto.Parser Text
name = pack <$> Atto.many1 (Atto.satisfy $ \c -> (c /= ' ') && (c /= '\t') && (c /= '\r') && (c /= '\n'))


-- | Parses the strings "off" (False) and "on" (True)
toggle :: Atto.Parser Bool
toggle = (Atto.string "off" *> pure False) <|> (Atto.string "on" *> pure True)


-- | Wraps a parser in a '(' and a ')', with no whitespace in between
parenthesised :: Atto.Parser a -> Atto.Parser a
parenthesised p = Atto.char '(' *> p <* Atto.char ')'


-- TODO | - Allow scientific notation (?)

-- |
coord :: Fractional f => Atto.Parser f
coord = space *> (parenthesised Atto.rational <|> Atto.rational)


-- | A single colour channel
-- TODO | - Clamp to [0,1] (cf. partial from monadplus) (?)
--        - Can channels be parenthesised (?)
channel :: Fractional f => Atto.Parser f
channel = space *> (parenthesised Atto.rational <|> Atto.rational)


-- | A colour with three or four channels (RGB[A])
colour :: Fractional f => Atto.Parser (Colour f)
colour = Colour <$> channel <*> channel <*> channel <*> Atto.option 1 channel


-- | A point in 3D space
point3D :: Fractional f => Atto.Parser (V3 f)
point3D = V3 <$> coord <*> coord <*> coord


-- | A point in 2D space
point2D :: Fractional f => Atto.Parser (V2 f)
point2D = V2 <$> coord <*> coord


-- |
-- TODO | - Clean up and generalise
clamped :: Integral i => i -> i -> Atto.Parser i
clamped lower upper = Atto.decimal >>= \n -> if (lower <= n) && (n <= upper)
                                               then return n
                                               else fail "Number not in range"