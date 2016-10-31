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


-- | Parses a token given a single valid OBJ row
--
-- TODO | - Correctness (total function, no runtime exceptions)
--        - Handle invalid rows (how to deal with mangled definitions w.r.t indices?)
--        - Use ListLike or Monoid (or maybe Indexable, since that's the real requirement) (?)
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
        (Atto.string "s"  *> smooth)  <|>
        (Atto.string "mtllib" *> lib) <|>
        (Atto.string "usemtl" *> use)

    
-- TODO: Expose these parsers for testing purposes (?)

--------------------------------------------------------------------------------------------------------------------------------------------

-- | Three or more vertex definitions (cf. 'vertexIndices' for details)
face :: Integral i => Atto.Parser (OBJToken f Text i [])
face = OBJFace <$> vertexIndices


-- | A single vertex definition with indices for vertex position, normal, and texture coordinates
--
-- TODO: | - Should the slashes be optional?
--         - Allowed trailing slashes (I'll have to check the spec again) (?)
--
-- f Int[/((Int[/Int])|(/Int))]
vertexIndices :: Integral i => Atto.Parser [VertexIndices i]
vertexIndices = atleast 3 (space *> (ivertex <*> index   <*> index))     <|> -- vi/ti/ni
                atleast 3 (space *> (ivertex <*> nothing <*> skipIndex)) <|> -- vi//ni
                atleast 3 (space *> (ivertex <*> index   <*> nothing))   <|> -- vi/ti
                atleast 3 (space *> (ivertex <*> nothing <*> nothing))       -- vi
  where
    ivertex :: Integral i => Atto.Parser (Maybe i -> Maybe i -> VertexIndices i)
    ivertex = VertexIndices <$> Atto.decimal

    index :: Integral i => Atto.Parser (Maybe i)
    index = Just <$> (Atto.char '/' *> Atto.decimal)
    
    skipIndex :: Integral i => Atto.Parser (Maybe i)
    skipIndex = Atto.char '/' *> index

    nothing :: Atto.Parser (Maybe i)
    nothing = pure Nothing

-- Geometry primitives ---------------------------------------------------------------------------------------------------------------------

-- | Two integers, separated by whitespace
line :: Integral i => Atto.Parser (OBJToken f Text i m)
line = Line <$> (space *> Atto.decimal) <*> (space *> Atto.decimal)

--------------------------------------------------------------------------------------------------------------------------------------------

-- | Three cordinates, separated by whitespace
normal :: (Fractional f) => Atto.Parser (OBJToken f Text i m)
normal = OBJNormal <$> point3D


-- | Two coordinates, separated by whitespace
texture :: (Fractional f) => Atto.Parser (OBJToken f Text i m)
texture = OBJTexCoord <$> point2D


-- | Three coordinates, separated by whitespace
vertex :: (Fractional f) =>  Atto.Parser (OBJToken f s i m)
vertex  = OBJVertex <$> point3D


-- | Object names, separated by whitespace
object :: Atto.Parser (OBJToken f Text i m)
object = Object . S.fromList <$> atleast 1 (space *> name)


-- | Group names, separated by whitespace
group :: Atto.Parser (OBJToken f Text i m)
group = Group . S.fromList <$> atleast 1 (space *> name)


-- | Smoothing group
-- TODO: Refactor
smooth :: Atto.Parser (OBJToken f s i m)
smooth = SmoothGroup <$> ((Atto.string "off" <|> Atto.string "0") *> Nothing) <|> (space *> (Just <$> name))


-- | An MTL library name
lib :: Atto.Parser (OBJToken f Text i m)
lib = LibMTL <$> (space *> name)


-- | An MTL material name
use :: Atto.Parser (OBJToken f Text i m)
use = UseMTL <$> (space *> name)