-- |
-- Module      : Graphics.WaveFront.Parse.OBJ
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
import           Data.Int    (Int64)
-- import qualified Data.Map  as M
-- import qualified Data.Text as T
-- import qualified Data.Vector as V
import qualified Data.Set  as S

import qualified Data.Attoparsec.Text       as Atto

import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>))

-- import Linear (V2(..), V3(..))

import Graphics.WaveFront.Parse.Common
import Graphics.WaveFront.Types hiding (texture)



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- OBJ parsing -----------------------------------------------------------------------------------------------------------------------------

-- | This function creates an OBJToken or error for each line in the input data
obj :: Atto.Parser (SimpleOBJ)
obj = Atto.skipMany ((comment *> pure ()) <|> Atto.endOfLine) *> Atto.sepBy row lineSeparator -- <* Atto.endOfInput


-- | Parses a token given a single valid OBJ row, or an error value if the input is malformed.
--
-- TODO: Correctness (total function, no runtime exceptions)
-- TODO: Handle invalid rows (how to deal with mangled definitions w.r.t indices?)

-- TODO: Named errors (typed?) rather than Nothing (cf. Either) (?)
--       Type for unsupported but valid (according to spec) attributes (?)
--       Type for specific attribute that failed to parse (eg. "f 1/2 0/p 1.5/x")
--
-- TODO: Use ListLike or Monoid (or maybe Indexable, since that's the real requirement) (?)
row :: Atto.Parser (SimpleOBJToken)
row = token <* ignore comment -- TODO: Let the separator handle comments (?)


-- |
-- Parses an OBJ token
token :: Atto.Parser (SimpleOBJToken)
token = (Atto.string "f"  *> face)    <|> --
        -- TODO: How to deal with common prefix (v, vn, vt) (backtrack?)
        (Atto.string "vn" *> normal)  <|>
        (Atto.string "vt" *> texture) <|>
        (Atto.string "v"  *> vertex)  <|>
        (Atto.string "o"  *> object)  <|>
        (Atto.string "g"  *> group)   <|>
        -- Atto.string "s" -- Smooth shading (TODO: Don't ignore)
        (Atto.string "mtllib" *> lib) <|>
        (Atto.string "usemtl" *> use)

    
-- TODO: Expose these parsers for testing purposes (?)


-- |
face :: Atto.Parser (SimpleOBJToken)
face = OBJFace    <$> atleast 3 (space *> vertexIndices)


-- |
normal :: Atto.Parser (SimpleOBJToken)
normal = OBJNormal  <$> point3D


-- |
texture :: Atto.Parser (SimpleOBJToken)
texture = OBJTexture <$> point2D


-- |
vertex :: Atto.Parser (SimpleOBJToken)
vertex  = OBJVertex <$> point3D


-- |
object :: Atto.Parser (SimpleOBJToken)
object = Object . S.fromList <$> atleast 1 (space *> name)


-- |
group :: Atto.Parser (SimpleOBJToken)
group = Group . S.fromList <$> atleast 1 (space *> name)


-- |
lib :: Atto.Parser (SimpleOBJToken)
lib = LibMTL <$> (space *> name)


-- |
use :: Atto.Parser (SimpleOBJToken)
use = UseMTL <$> (space *> word)


-- | A single vertex definition with indices for vertex position, normal, and texture coordinates
-- TODO: Should the slashes be optional?
vertexIndices :: Atto.Parser (VertexIndices Int64)
vertexIndices = VertexIndices <$>
            (Atto.decimal          <* Atto.char '/') <*>
            (optional Atto.decimal <* Atto.char '/') <*>
            (optional Atto.decimal)
    