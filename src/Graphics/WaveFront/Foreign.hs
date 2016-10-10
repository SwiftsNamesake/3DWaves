-- |
-- Module      : Graphics.WaveFront.Foreign
-- Description : Foreign function interface
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)

-- February 24 2015

-- TODO | - Possible to get rid of newtypes (?)
--        - Decide on an API

-- SPEC | -
--        -



-- TODO: Why do same extensions start with 'X'?
{-# LANGUAGE ForeignFunctionInterface #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Graphics.WaveFront.Foreign where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
-- import System.IO.Unsafe (unsafePerformIO)
-- import Foreign.Storable
-- import qualified Foreign.C as C

import Graphics.WaveFront.Types
-- import qualified Graphics.WaveFront.Parse as Parse



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- -- |
-- -- I feel dirty...
-- parseOBJ :: C.CString -> COBJ
-- parseOBJ = COBJ . Parsers.parseOBJ . unsafePerformIO . C.peekCString
--
-- -- |
-- parseMTL :: C.CString -> CMTL
-- parseMTL = CMTL . Parsers.parseMTL . unsafePerformIO . C.peekCString



-- -- |
-- newtype COBJ = COBJ OBJ
--
--
-- -- |
-- newtype CMTL = CMTL MTL
--
--
-- -- | We
-- instance Storable COBJ where
--   sizeOf    = const 0
--   alignment = const 0
--   peek _    = error "Work in progress"
--   poke _    = error "Work in progress"
--
--
-- -- | We
-- instance Storable CMTL where
--   sizeOf    = const 0
--   alignment = const 0
--   peek _    = error "Work in progress"
--   poke _    = error "Work in progress"


--------------------------------------------------------------------------------------------------------------------------------------------
-- Pure foreign function interface
--------------------------------------------------------------------------------------------------------------------------------------------
-- I feel the urge to make a joke about 'Unacceptable argument in foreign declaration'
-- foreign export ccall parseOBJ :: C.CString -> COBJ
-- foreign export ccall parseMTL :: C.CString -> CMTL
