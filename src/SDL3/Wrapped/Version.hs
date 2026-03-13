{-|
Module      : SDL3.Wrapped.Version
Description : Wrapped SDL version querying and management
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Ergonomic wrappers over 'SDL3.Raw.Version'.
-}

module SDL3.Wrapped.Version
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

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt)
import qualified SDL3.Raw.Version as Raw

sdlMajorVersion :: Int
sdlMajorVersion = Raw.sdlMajorVersion

sdlMinorVersion :: Int
sdlMinorVersion = Raw.sdlMinorVersion

sdlMicroVersion :: Int
sdlMicroVersion = Raw.sdlMicroVersion

sdlVersion :: Int
sdlVersion = Raw.sdlVersion

sdlVersionAtLeast :: Int -> Int -> Int -> Bool
sdlVersionAtLeast = Raw.sdlVersionAtLeast

sdlGetVersion :: MonadIO m => m CInt
sdlGetVersion = liftIO Raw.sdlGetVersionRaw

sdlGetRevision :: MonadIO m => m String
sdlGetRevision = liftIO $ Raw.sdlGetRevisionRaw >>= peekCString
