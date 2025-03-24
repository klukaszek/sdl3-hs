{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : SDL.Version
Description : SDL version querying and management
Copyright   : (c) The SDL3 team
License     : BSD3

SDL provides functionality to query the current SDL version, both as headers
the application was compiled against and as a library the application is linked to.

This module allows you to access the compile-time version of SDL through constants
and macros, as well as retrieve the runtime version and revision of the linked SDL
library. This is particularly useful when linking dynamically to SDL, where the
linked library version might differ from the version used at compile time.

The version information can be used to ensure compatibility or to log details for
debugging purposes.
-}
module SDL.Version
  ( -- * Version Constants
    sdlMajorVersion
  , sdlMinorVersion
  , sdlMicroVersion
  , sdlVersion
  , sdlVersionAtLeast

    -- * Version Functions
  , sdlGetVersion
  , sdlGetRevision
  ) where

#include <SDL3/SDL_version.h>

import Foreign.C.Types
import Foreign.C.String (CString, peekCString)
import Data.Word (Word32)

-- | The current major version of SDL headers (SDL_MAJOR_VERSION).
sdlMajorVersion :: Int
sdlMajorVersion = #const SDL_MAJOR_VERSION

-- | The current minor version of SDL headers (SDL_MINOR_VERSION).
sdlMinorVersion :: Int
sdlMinorVersion = #const SDL_MINOR_VERSION

-- | The current micro (patchlevel) version of SDL headers (SDL_MICRO_VERSION).
sdlMicroVersion :: Int
sdlMicroVersion = #const SDL_MICRO_VERSION

-- | Turns the version numbers into a numeric value (SDL_VERSIONNUM).
-- For example, (1, 2, 3) becomes 1002003.
sdlVersionNum :: Int -> Int -> Int -> Int
sdlVersionNum major minor patch = major * 1000000 + minor * 1000 + patch

-- | The version number for the current SDL version (SDL_VERSION).
sdlVersion :: Int
sdlVersion = sdlVersionNum sdlMajorVersion sdlMinorVersion sdlMicroVersion

-- | Evaluates to True if compiled with SDL at least X.Y.Z (SDL_VERSION_ATLEAST).
sdlVersionAtLeast :: Int -> Int -> Int -> Bool
sdlVersionAtLeast x y z = sdlVersion >= sdlVersionNum x y z

-- | Get the version of SDL that is linked against your program.
--
-- This function returns the version as a single integer (e.g., 3003000 for 3.3.0).
-- It corresponds to SDL_GetVersion in the C API.
--
-- Since this function is available since SDL 3.2.0.
foreign import ccall "SDL_GetVersion"
  sdlGetVersion :: IO CInt

-- | Get the code revision of SDL that is linked against your program.
--
-- Returns an arbitrary string uniquely identifying the exact revision of the SDL library in use.
-- Corresponds to SDL_GetRevision in the C API.
--
-- Since this function is available since SDL 3.2.0.
foreign import ccall "SDL_GetRevision"
  sdlGetRevisionRaw :: IO CString

-- | Haskell wrapper for SDL_GetRevision to return a String.
sdlGetRevision :: IO String
sdlGetRevision = sdlGetRevisionRaw >>= peekCString
