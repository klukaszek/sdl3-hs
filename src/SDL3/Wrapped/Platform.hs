-- |
-- Module      : SDL3.Wrapped.Platform
-- Description : Wrapped SDL platform identification
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Ergonomic wrappers over 'SDL3.Raw.Platform'.

module SDL3.Wrapped.Platform
  ( -- * Platform Information
    sdlGetPlatform

    -- * Platform Constants
  , sdlPlatformWindows
  , sdlPlatformMacOS
  , sdlPlatformLinux
  , sdlPlatformIOS
  , sdlPlatformAndroid
  , sdlPlatformTVOS
  , sdlPlatformVisionOS
  , sdlPlatformEmscripten
  , sdlPlatformHaiku
  , sdlPlatformUnix
  , sdlPlatformApple
  , sdlPlatformNetBSD
  , sdlPlatformOpenBSD
  , sdlPlatformFreeBSD
  , sdlPlatformBSDI
  , sdlPlatformOS2
  , sdlPlatformSolaris
  , sdlPlatformCygwin
  , sdlPlatformWin32
  , sdlPlatformXboxOne
  , sdlPlatformXboxSeries
  , sdlPlatformWinGDK
  , sdlPlatformGDK
  , sdlPlatformPSP
  , sdlPlatformPS2
  , sdlPlatformVita
  , sdlPlatform3DS
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.String (peekCString)
import qualified SDL3.Raw.Platform as Raw

sdlGetPlatform :: MonadIO m => m String
sdlGetPlatform = liftIO $ Raw.sdlGetPlatformRaw >>= peekCString

sdlPlatformWindows, sdlPlatformMacOS, sdlPlatformLinux, sdlPlatformIOS, sdlPlatformAndroid, sdlPlatformTVOS, sdlPlatformVisionOS, sdlPlatformEmscripten, sdlPlatformHaiku, sdlPlatformUnix, sdlPlatformApple, sdlPlatformNetBSD, sdlPlatformOpenBSD, sdlPlatformFreeBSD, sdlPlatformBSDI, sdlPlatformOS2, sdlPlatformSolaris, sdlPlatformCygwin, sdlPlatformWin32, sdlPlatformXboxOne, sdlPlatformXboxSeries, sdlPlatformWinGDK, sdlPlatformGDK, sdlPlatformPSP, sdlPlatformPS2, sdlPlatformVita, sdlPlatform3DS :: Bool
sdlPlatformWindows = Raw.sdlPlatformWindows
sdlPlatformMacOS = Raw.sdlPlatformMacOS
sdlPlatformLinux = Raw.sdlPlatformLinux
sdlPlatformIOS = Raw.sdlPlatformIOS
sdlPlatformAndroid = Raw.sdlPlatformAndroid
sdlPlatformTVOS = Raw.sdlPlatformTVOS
sdlPlatformVisionOS = Raw.sdlPlatformVisionOS
sdlPlatformEmscripten = Raw.sdlPlatformEmscripten
sdlPlatformHaiku = Raw.sdlPlatformHaiku
sdlPlatformUnix = Raw.sdlPlatformUnix
sdlPlatformApple = Raw.sdlPlatformApple
sdlPlatformNetBSD = Raw.sdlPlatformNetBSD
sdlPlatformOpenBSD = Raw.sdlPlatformOpenBSD
sdlPlatformFreeBSD = Raw.sdlPlatformFreeBSD
sdlPlatformBSDI = Raw.sdlPlatformBSDI
sdlPlatformOS2 = Raw.sdlPlatformOS2
sdlPlatformSolaris = Raw.sdlPlatformSolaris
sdlPlatformCygwin = Raw.sdlPlatformCygwin
sdlPlatformWin32 = Raw.sdlPlatformWin32
sdlPlatformXboxOne = Raw.sdlPlatformXboxOne
sdlPlatformXboxSeries = Raw.sdlPlatformXboxSeries
sdlPlatformWinGDK = Raw.sdlPlatformWinGDK
sdlPlatformGDK = Raw.sdlPlatformGDK
sdlPlatformPSP = Raw.sdlPlatformPSP
sdlPlatformPS2 = Raw.sdlPlatformPS2
sdlPlatformVita = Raw.sdlPlatformVita
sdlPlatform3DS = Raw.sdlPlatform3DS
