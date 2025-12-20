{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : SDL.Platform
-- Description : SDL platform identification
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- This module provides functionality to identify the application's platform,
-- both at compile time and runtime.
module SDL3.Platform
  ( -- * Platform Information
    sdlGetPlatform,

    -- * Platform Constants
    sdlPlatformWindows,
    sdlPlatformMacOS,
    sdlPlatformLinux,
    sdlPlatformIOS,
    sdlPlatformAndroid,
    sdlPlatformTVOS,
    sdlPlatformVisionOS,
    sdlPlatformEmscripten,
    sdlPlatformHaiku,
    sdlPlatformUnix,
    sdlPlatformApple,
    sdlPlatformNetBSD,
    sdlPlatformOpenBSD,
    sdlPlatformFreeBSD,
    sdlPlatformBSDI,
    sdlPlatformOS2,
    sdlPlatformSolaris,
    sdlPlatformCygwin,
    sdlPlatformWin32,
    sdlPlatformXboxOne,
    sdlPlatformXboxSeries,
    sdlPlatformWinGDK,
    sdlPlatformGDK,
    sdlPlatformPSP,
    sdlPlatformPS2,
    sdlPlatformVita,
    sdlPlatform3DS,
  )
where

import Foreign.C.String (CString, peekCString)

-- | Get the name of the platform.
--
-- Common platform names include:
-- * "Windows"
-- * "macOS"
-- * "Linux"
-- * "iOS"
-- * "Android"
--
-- If the correct platform name is not available, returns a string
-- beginning with "Unknown".
foreign import ccall "SDL_GetPlatform"
  sdlGetPlatform_c :: IO CString

sdlGetPlatform :: IO String
sdlGetPlatform = sdlGetPlatform_c >>= peekCString

-- Platform compile-time constants
#ifdef SDL_PLATFORM_WINDOWS
sdlPlatformWindows = True
#else
sdlPlatformWindows = False
#endif

#ifdef SDL_PLATFORM_MACOS
sdlPlatformMacOS = True
#else
sdlPlatformMacOS = False
#endif

#ifdef SDL_PLATFORM_LINUX
sdlPlatformLinux = True
#else
sdlPlatformLinux = False
#endif

#ifdef SDL_PLATFORM_IOS
sdlPlatformIOS = True
#else
sdlPlatformIOS = False
#endif

#ifdef SDL_PLATFORM_ANDROID
sdlPlatformAndroid = True
#else
sdlPlatformAndroid = False
#endif

#ifdef SDL_PLATFORM_TVOS
sdlPlatformTVOS = True
#else
sdlPlatformTVOS = False
#endif

#ifdef SDL_PLATFORM_VISIONOS
sdlPlatformVisionOS = True
#else
sdlPlatformVisionOS = False
#endif

#ifdef SDL_PLATFORM_EMSCRIPTEN
sdlPlatformEmscripten = True
#else
sdlPlatformEmscripten = False
#endif

#ifdef SDL_PLATFORM_HAIKU
sdlPlatformHaiku = True
#else
sdlPlatformHaiku = False
#endif

#ifdef SDL_PLATFORM_UNIX
sdlPlatformUnix = True
#else
sdlPlatformUnix = False
#endif

#ifdef SDL_PLATFORM_APPLE
sdlPlatformApple = True
#else
sdlPlatformApple = False
#endif

#ifdef SDL_PLATFORM_NETBSD
sdlPlatformNetBSD = True
#else
sdlPlatformNetBSD = False
#endif

#ifdef SDL_PLATFORM_OPENBSD
sdlPlatformOpenBSD = True
#else
sdlPlatformOpenBSD = False
#endif

#ifdef SDL_PLATFORM_FREEBSD
sdlPlatformFreeBSD = True
#else
sdlPlatformFreeBSD = False
#endif

#ifdef SDL_PLATFORM_BSDI
sdlPlatformBSDI = True
#else
sdlPlatformBSDI = False
#endif

#ifdef SDL_PLATFORM_OS2
sdlPlatformOS2 = True
#else
sdlPlatformOS2 = False
#endif

#ifdef SDL_PLATFORM_SOLARIS
sdlPlatformSolaris = True
#else
sdlPlatformSolaris = False
#endif

#ifdef SDL_PLATFORM_CYGWIN
sdlPlatformCygwin = True
#else
sdlPlatformCygwin = False
#endif

#ifdef SDL_PLATFORM_WIN32
sdlPlatformWin32 = True
#else
sdlPlatformWin32 = False
#endif

#ifdef SDL_PLATFORM_XBOXONE
sdlPlatformXboxOne = True
#else
sdlPlatformXboxOne = False
#endif

#ifdef SDL_PLATFORM_XBOXSERIES
sdlPlatformXboxSeries = True
#else
sdlPlatformXboxSeries = False
#endif

#ifdef SDL_PLATFORM_WINGDK
sdlPlatformWinGDK = True
#else
sdlPlatformWinGDK = False
#endif

#ifdef SDL_PLATFORM_GDK
sdlPlatformGDK = True
#else
sdlPlatformGDK = False
#endif

#ifdef SDL_PLATFORM_PSP
sdlPlatformPSP = True
#else
sdlPlatformPSP = False
#endif

#ifdef SDL_PLATFORM_PS2
sdlPlatformPS2 = True
#else
sdlPlatformPS2 = False
#endif

#ifdef SDL_PLATFORM_VITA
sdlPlatformVita = True
#else
sdlPlatformVita = False
#endif

#ifdef SDL_PLATFORM_3DS
sdlPlatform3DS = True
#else
sdlPlatform3DS = False
#endif
