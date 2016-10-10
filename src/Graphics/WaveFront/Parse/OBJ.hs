-- |
-- Module      : Graphics.WaveFront.Parse.OBJ
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
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Graphics.WaveFront.Parse.OBJ (
  obj, row, face,
  normal, texture, vertex, object, group,
  lib, use,
  vertexIndices,
) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Text (Text)
-- import qualified Data.Vector as V
import qualified Data.Set as S

import qualified Data.Attoparsec.Text as Atto

import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>))

-- import Linear (V2(..), V3(..))

import Graphics.WaveFront.Parse.Common
import Graphics.WaveFront.Types hiding (texture)



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- OBJ parsing -----------------------------------------------------------------------------------------------------------------------------

-- | This function creates an OBJToken or error for each line in the input data
obj :: (Fractional f, Integral i) => Atto.Parser (OBJ f Text i [])
obj = Atto.sepBy row lineSeparator -- <* Atto.endOfInput


-- | Parses a token given a single valid OBJ row, or an error value if the input is malformed.
--
-- TODO: Correctness (total function, no runtime exceptions)
-- TODO: Handle invalid rows (how to deal with mangled definitions w.r.t indices?)

-- TODO: Named errors (typed?) rather than Nothing (cf. Either) (?)
--       Type for unsupported but valid (according to spec) attributes (?)
--       Type for specific attribute that failed to parse (eg. "f 1/2 0/p 1.5/x")
--
-- TODO: Use ListLike or Monoid (or maybe Indexable, since that's the real requirement) (?)
row :: (Fractional f, Integral i) => Atto.Parser (OBJToken f Text i [])
row = token <* ignore comment -- TODO: Let the separator handle comments (?)


-- |
-- Parses an OBJ token
token :: (Fractional f, Integral i) => Atto.Parser (OBJToken f Text i [])
token = (Atto.string "f"  *> face)    <|>
        (Atto.string "l"  *> line)    <|>
        -- TODO: How to deal with common prefix (v, vn, vt) (backtrack?) (doesn't seem to be a problem)
        (Atto.string "vn" *> normal)  <|>
        (Atto.string "vt" *> texture) <|>
        (Atto.string "v"  *> vertex)  <|>
        (Atto.string "o"  *> object)  <|>
        (Atto.string "g"  *> group)   <|>
        (Atto.string "s"  *> smooth)  <|> -- Smooth shading (TODO: Don't ignore)
        (Atto.string "mtllib" *> lib) <|>
        (Atto.string "usemtl" *> use)

    
-- TODO: Expose these parsers for testing purposes (?)


-- |
face :: Integral i => Atto.Parser (OBJToken f Text i [])
face = OBJFace <$> atleast 3 (space *> vertexIndices)


-- |
line :: Integral i => Atto.Parser (OBJToken f Text i m)
line = Line <$> (space *> Atto.decimal) <*> (space *> Atto.decimal)


-- |
normal :: (Fractional f) => Atto.Parser (OBJToken f Text i m)
normal = OBJNormal <$> point3D


-- |
texture :: (Fractional f) => Atto.Parser (OBJToken f Text i m)
texture = OBJTexture <$> point2D


-- |
vertex :: (Fractional f) =>  Atto.Parser (OBJToken f s i m)
vertex  = OBJVertex <$> point3D


-- |
object :: Atto.Parser (OBJToken f Text i m)
object = Object . S.fromList <$> atleast 1 (space *> name)


-- |
group :: Atto.Parser (OBJToken f Text i m)
group = Group . S.fromList <$> atleast 1 (space *> name)


-- |
smooth :: Atto.Parser (OBJToken f s i m)
smooth = SmoothShading <$> (space *> toggle)


-- |
lib :: Atto.Parser (OBJToken f Text i m)
lib = LibMTL <$> (space *> name)


-- |
use :: Atto.Parser (OBJToken f Text i m)
use = UseMTL <$> (space *> name)


-- | A single vertex definition with indices for vertex position, normal, and texture coordinates
-- TODO: Should the slashes be optional?
-- TODO: PLEASE FIX THIS, FUTURE SELF
-- f Int[/((Int[/Int])|(/Int))]
-- VertexIndices ivert inorm itex
vertexIndices :: Integral i => Atto.Parser (VertexIndices i)
vertexIndices = VertexIndices <$>
                  Atto.decimal <*>
                  ((Atto.char '/' *> (Just <$> Atto.decimal   <|> pure Nothing)) <|> pure Nothing) <*>
                  ((Atto.char '/' *> (Just <$> Atto.decimal)) <|> pure Nothing)
                  -- ((Atto.char '/' *> Atto.decimal) <|> (Atto.char '/' *> pure Nothing *> ))
                  -- optional (Atto.char '/' *> )
                  -- optional (Atto.char '/' *> optional Atto.decimal) <*>
                  -- optional (Atto.char '/' *> optional Atto.decimal)
    