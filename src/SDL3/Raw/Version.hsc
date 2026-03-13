{-# LANGUAGE ForeignFunctionInterface #-}

{-|
Module      : SDL3.Raw.Version
Description : Raw SDL version querying and management
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Low-level bindings for SDL version queries. Prefer 'SDL3.Wrapped.Version' or
'SDL3.Version' unless you need direct FFI access.
-}

module SDL3.Raw.Version
  ( -- * Version Constants
    sdlMajorVersion
  , sdlMinorVersion
  , sdlMicroVersion
  , sdlVersion
  , sdlVersionAtLeast

    -- * Raw Version Functions
  , sdlGetVersionRaw
  , sdlGetRevisionRaw
  ) where

#include <SDL3/SDL_version.h>

import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..))

sdlMajorVersion :: Int
sdlMajorVersion = (#const SDL_MAJOR_VERSION)

sdlMinorVersion :: Int
sdlMinorVersion = (#const SDL_MINOR_VERSION)

sdlMicroVersion :: Int
sdlMicroVersion = (#const SDL_MICRO_VERSION)

sdlVersionNum :: Int -> Int -> Int -> Int
sdlVersionNum major minor patch = major * 1000000 + minor * 1000 + patch

sdlVersion :: Int
sdlVersion = sdlVersionNum sdlMajorVersion sdlMinorVersion sdlMicroVersion

sdlVersionAtLeast :: Int -> Int -> Int -> Bool
sdlVersionAtLeast x y z = sdlVersion >= sdlVersionNum x y z

foreign import ccall "SDL_GetVersion"
  sdlGetVersionRaw :: IO CInt

foreign import ccall "SDL_GetRevision"
  sdlGetRevisionRaw :: IO CString
