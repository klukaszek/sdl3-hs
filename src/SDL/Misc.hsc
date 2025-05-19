{-# LANGUAGE ForeignFunctionInterface #-}

{-|
Module      : SDL.Misc
Description : Miscellaneous SDL functions
Copyright   : Kyle Lukaszek, 2025
License     : BSD3

This module provides bindings to SDL3 API functions that don't fit elsewhere,
such as opening URLs in the system browser.
-}

module SDL.Misc
  ( -- * URL Handling
    sdlOpenURL
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.Word
import Data.Int
import Control.Monad

import SDL.Stdinc
import SDL.Error

#include <SDL3/SDL_misc.h>

-- | Open a URL/URI in the browser or other appropriate external application.
foreign import ccall "SDL_OpenURL" sdlOpenURLRaw :: CString -> IO Bool

-- | Open a URL/URI in the browser or other appropriate external application.
-- This high-level function wraps the SDL_OpenURL C function, converting the 
-- Haskell String to a C string and handling errors appropriately.
sdlOpenURL :: String -> IO Bool
sdlOpenURL url = withCString url sdlOpenURLRaw
