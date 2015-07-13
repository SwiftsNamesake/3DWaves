--
-- Wavefront - Foreign.hs
-- Foreign function interface
--
-- Jonatan H Sundqvist
-- February 24 2015
--

-- TODO | - Possible to get rid of newtypes
--        - Decide on an API

-- SPEC | -
--        -


-- TODO: Why do same extensions start with 'X'?
{-# LANGUAGE ForeignFunctionInterface #-}



module Southpaw.WaveFront.Foreign where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Foreign.Storable
import qualified Foreign.C as C
import qualified Southpaw.WaveFront.Parsers as Parsers

import System.IO.Unsafe (unsafePerformIO)



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- |
newtype COBJ = COBJ Parsers.OBJ


-- |
newtype CMTL = CMTL Parsers.MTL


-- | We 
instance Storable COBJ where
	sizeOf    = const 0
	alignment = const 0


-- | We 
instance Storable CMTL where
	sizeOf    = const 0
	alignment = const 0



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- |
-- I feel dirty...
parseOBJ :: C.CString -> COBJ
parseOBJ = COBJ . Parsers.parseOBJ . unsafePerformIO . C.peekCString

-- |
parseMTL :: C.CString -> CMTL
parseMTL = CMTL . Parsers.parseMTL . unsafePerformIO . C.peekCString



---------------------------------------------------------------------------------------------------
-- Pure foreign function interface
---------------------------------------------------------------------------------------------------
-- I feel the urge to make a joke about 'Unacceptable argument in foreign declaration'
-- foreign export ccall parseOBJ :: C.CString -> COBJ
-- foreign export ccall parseMTL :: C.CString -> CMTL