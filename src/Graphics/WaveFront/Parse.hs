-- |
-- Module      : Graphics.WaveFront.Parse
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, February 8 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--
-- Created February 8 2015
-- Wavefront - Parsers.hs
-- Migrated to separate project on February 21 2015

-- TODO | - Appropriate container types (eg. bytestring, vector)
--        - Grammar specification
--        - Incremental parsing (?)
--        - Improve naming scheme
--          -- Remove 'parse-' prefix, import qualified (eg. 'Parse.obj')
--
--        - Separate MTL and OBJ parsers (?) (...)
--        - Separate parsing, processing, logging, IO and testing (...)
--          -- Proper path handling (eg. include root in MTLTable or not)
--
--        - Additional attributes (lighting, splines, etc.)
--        - FFI (...)
--        - Debugging information (line number, missing file, missing values, etc.) (...)
--        - Proper Haddock coverage, including headers (...)
--        - Model type (✓)
--        - Caching (?)
--        - Performance, profiling, optimisations
--          -- Strict or lazy (eg. with Data.Map) (?)
--          -- Multi-threading
--          -- Appropriate container types
--
--        - PrintfArg instances for the types defined in this module
--        - Reconciling Cabal and hierarchical modules
--        - Dealing with paths in lib statements (requires knowledge of working directories)
--        - Move comments and specification to separate files (eg. README)
--        - Inline comments (for internals, implementation)
--
--        - Full OBJ spec compliance
--          -- Do the usemtl and libmtl statements affect vertices or faces (?)
--
--        - Parser bugs
--          -- Negative coordinates enclosed in parentheses (✓)
--
--        - Decide on a public interface (exports) (API)
--          -- Model will be the main API type
--          -- Processing utils (eg. iterating over model faces; withModelFaces :: ((Material, [(Vertex, Maybe Normalcoords, Maybe Texcoords)]) -> b) -> Model -> [b])
--          -- Export functions for working with the output data (eg. unzipIndices :: [(Int, Int, Int)] -> ([Int], [Int], [Int]))
--          -- Export certain utilities (eg. second, perhaps in another module) (?)

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
-- TODO: Clean this up
module Graphics.WaveFront.Parse (
  module Graphics.WaveFront.Types, -- TODO: Don't export internal types (duh)
  obj, mtl,
  comment, lineSeparator
) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Graphics.WaveFront.Parse.Common
import Graphics.WaveFront.Parse.OBJ (obj)
import Graphics.WaveFront.Parse.MTL (mtl)

import Graphics.WaveFront.Types