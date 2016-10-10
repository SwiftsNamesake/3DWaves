-- |
-- Module      : Graphics.WaveFront.Lenses
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2016
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created July 9 2016

-- TODO | -
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Graphics.WaveFront.Lenses where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Control.Lens (makeLensesWith, abbreviatedFields)

import Graphics.WaveFront.Types



--------------------------------------------------------------------------------------------------------------------------------------------
-- Lenses
--------------------------------------------------------------------------------------------------------------------------------------------
makeLensesWith abbreviatedFields ''VertexIndices
makeLensesWith abbreviatedFields ''Face
makeLensesWith abbreviatedFields ''Colour
makeLensesWith abbreviatedFields ''Material
makeLensesWith abbreviatedFields ''Model