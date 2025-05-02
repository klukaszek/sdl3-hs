{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-} 

-- |
-- Module      : SDL.Video
-- Description : Haskell bindings to SDL_video.h
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- This module provides bindings to the SDL video subsystem, including window
-- management, display handling, and OpenGL context support. Functions in this
-- module must be called from the main thread, as per SDL requirements.

module SDL.Video
  ( -- * Types
    SDLDisplayID(..),
    SDLWindowID(..),
    SDLWindow(..),
    SDLDisplayMode(..),
    SDLDisplayOrientation(..),
    pattern SDL_ORIENTATION_UNKNOWN,
    pattern SDL_ORIENTATION_LANDSCAPE,
    pattern SDL_ORIENTATION_LANDSCAPE_FLIPPED,
    pattern SDL_ORIENTATION_PORTRAIT,
    pattern SDL_ORIENTATION_PORTRAIT_FLIPPED,
    
    SDLWindowFlags(..),
    pattern SDL_WINDOW_FULLSCREEN,
    pattern SDL_WINDOW_OPENGL,
    pattern SDL_WINDOW_HIDDEN,
    pattern SDL_WINDOW_BORDERLESS,
    pattern SDL_WINDOW_RESIZABLE,
    pattern SDL_WINDOW_MINIMIZED,
    pattern SDL_WINDOW_MAXIMIZED,
    pattern SDL_WINDOW_MOUSE_GRABBED,
    pattern SDL_WINDOW_HIGH_PIXEL_DENSITY,
    pattern SDL_WINDOW_ALWAYS_ON_TOP,

    SDLFlashOperation(..),
    pattern SDL_FLASH_CANCEL,
    pattern SDL_FLASH_BRIEFLY,
    pattern SDL_FLASH_UNTIL_FOCUSED,
  
    SDLGLContext,
    SDLSystemTheme(..),
    pattern SDL_SYSTEM_THEME_UNKNOWN,
    pattern SDL_SYSTEM_THEME_LIGHT,
    pattern SDL_SYSTEM_THEME_DARK,

    -- * Window Management
    sdlCreateWindow,
    sdlDestroyWindow,
    sdlSetWindowPosition,
    sdlGetWindowPosition,
    sdlSetWindowSize,
    sdlGetWindowSize,
    sdlGetWindowSizeInPixels,
    sdlShowWindow,
    sdlHideWindow,

    -- * Display Management
    sdlGetNumVideoDrivers,
    sdlGetVideoDriver,
    sdlGetCurrentVideoDriver,
    sdlGetDisplays,
    sdlGetPrimaryDisplay,
    sdlGetDisplayName,
    sdlGetDisplayBounds,

    -- * OpenGL Support
    sdlGLLoadLibrary,
    sdlGLGetProcAddress,
    sdlGLUnloadLibrary,
    sdlGLCreateContext,
    sdlGLMakeCurrent,
    sdlGLSwapWindow,
    sdlGLDestroyContext,
  ) where

#include <SDL3/SDL_video.h>

import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray0, withArrayLen)
import Foreign.Storable (Storable(..))
import Control.Monad (liftM)
import SDL.Rect (SDLRect(..))
import SDL.Pixels (SDLPixelFormat(..), cUIntToPixelFormat, pixelFormatToCUInt)
import SDL.Properties (SDLPropertiesID(..))
import SDL.Surface (SDLSurface)
import Data.Bits

-- | A unique ID for a display.
type SDLDisplayID = CUInt

-- | A unique ID for a window.
type SDLWindowID = CUInt

-- | An opaque handle to an SDL window.
newtype SDLWindow = SDLWindow (Ptr SDLWindow)
  deriving (Show, Eq)

-- | System theme enumeration.
newtype SDLSystemTheme = SDLSystemTheme CInt
  deriving newtype (Show, Eq, Ord, Storable, Enum)

pattern SDL_SYSTEM_THEME_UNKNOWN :: SDLSystemTheme
pattern SDL_SYSTEM_THEME_UNKNOWN = SDLSystemTheme #{const SDL_SYSTEM_THEME_UNKNOWN}
pattern SDL_SYSTEM_THEME_LIGHT :: SDLSystemTheme
pattern SDL_SYSTEM_THEME_LIGHT   = SDLSystemTheme #{const SDL_SYSTEM_THEME_LIGHT}
pattern SDL_SYSTEM_THEME_DARK :: SDLSystemTheme
pattern SDL_SYSTEM_THEME_DARK    = SDLSystemTheme #{const SDL_SYSTEM_THEME_DARK}

-- | Display orientation enumeration.
newtype SDLDisplayOrientation = SDLDisplayOrientation CInt
  deriving newtype (Show, Eq, Ord, Storable, Enum)

pattern SDL_ORIENTATION_UNKNOWN :: SDLDisplayOrientation
pattern SDL_ORIENTATION_UNKNOWN           = SDLDisplayOrientation #{const SDL_ORIENTATION_UNKNOWN}
pattern SDL_ORIENTATION_LANDSCAPE :: SDLDisplayOrientation
pattern SDL_ORIENTATION_LANDSCAPE         = SDLDisplayOrientation #{const SDL_ORIENTATION_LANDSCAPE}
pattern SDL_ORIENTATION_LANDSCAPE_FLIPPED :: SDLDisplayOrientation
pattern SDL_ORIENTATION_LANDSCAPE_FLIPPED = SDLDisplayOrientation #{const SDL_ORIENTATION_LANDSCAPE_FLIPPED}
pattern SDL_ORIENTATION_PORTRAIT :: SDLDisplayOrientation
pattern SDL_ORIENTATION_PORTRAIT          = SDLDisplayOrientation #{const SDL_ORIENTATION_PORTRAIT}
pattern SDL_ORIENTATION_PORTRAIT_FLIPPED :: SDLDisplayOrientation
pattern SDL_ORIENTATION_PORTRAIT_FLIPPED  = SDLDisplayOrientation #{const SDL_ORIENTATION_PORTRAIT_FLIPPED}

-- | Display mode structure.
data SDLDisplayMode = SDLDisplayMode
  { sdlDmDisplayID     :: SDLDisplayID
  , sdlDmFormat        :: SDLPixelFormat
  , sdlDmWidth         :: Int
  , sdlDmHeight        :: Int
  , sdlDmPixelDensity  :: Float
  , sdlDmRefreshRate   :: Float
  } deriving (Show, Eq)

instance Storable SDLDisplayMode where
  sizeOf _ = (# size SDL_DisplayMode )
  alignment _ = (# alignment SDL_DisplayMode )
  peek ptr = do
    rawDid <- (# peek SDL_DisplayMode, displayID ) ptr :: IO CUInt
    let did = rawDid
    fmtVal <- (# peek SDL_DisplayMode, format ) ptr :: IO CUInt
    let fmt = (cUIntToPixelFormat (fromIntegral fmtVal)) :: SDLPixelFormat
    w <- (# peek SDL_DisplayMode, w ) ptr :: IO CInt
    h <- (# peek SDL_DisplayMode, h ) ptr :: IO CInt
    pd <- (# peek SDL_DisplayMode, pixel_density ) ptr :: IO CFloat
    rr <- (# peek SDL_DisplayMode, refresh_rate ) ptr :: IO CFloat
    return $ SDLDisplayMode did fmt (fromIntegral w) (fromIntegral h) (realToFrac pd) (realToFrac rr)

  poke ptr (SDLDisplayMode did fmt w h pd rr) = do
    (# poke SDL_DisplayMode, displayID ) ptr did
    (# poke SDL_DisplayMode, format ) ptr ((pixelFormatToCUInt fmt) :: CUInt)
    (# poke SDL_DisplayMode, w ) ptr (fromIntegral w :: CInt)
    (# poke SDL_DisplayMode, h ) ptr (fromIntegral h :: CInt)
    (# poke SDL_DisplayMode, pixel_density ) ptr (realToFrac pd :: CFloat)
    (# poke SDL_DisplayMode, refresh_rate ) ptr (realToFrac rr :: CFloat)

-- | Window flags (bitfield).
newtype SDLWindowFlags = SDLWindowFlags CUInt
  deriving newtype (Show, Eq, Bits, Num, Storable)

pattern SDL_WINDOW_FULLSCREEN :: SDLWindowFlags
pattern SDL_WINDOW_FULLSCREEN         = SDLWindowFlags #{const SDL_WINDOW_FULLSCREEN}
pattern SDL_WINDOW_OPENGL :: SDLWindowFlags
pattern SDL_WINDOW_OPENGL             = SDLWindowFlags #{const SDL_WINDOW_OPENGL}
pattern SDL_WINDOW_HIDDEN :: SDLWindowFlags
pattern SDL_WINDOW_HIDDEN             = SDLWindowFlags #{const SDL_WINDOW_HIDDEN}
pattern SDL_WINDOW_BORDERLESS :: SDLWindowFlags
pattern SDL_WINDOW_BORDERLESS         = SDLWindowFlags #{const SDL_WINDOW_BORDERLESS}
pattern SDL_WINDOW_RESIZABLE :: SDLWindowFlags
pattern SDL_WINDOW_RESIZABLE          = SDLWindowFlags #{const SDL_WINDOW_RESIZABLE}
pattern SDL_WINDOW_MINIMIZED :: SDLWindowFlags
pattern SDL_WINDOW_MINIMIZED          = SDLWindowFlags #{const SDL_WINDOW_MINIMIZED}
pattern SDL_WINDOW_MAXIMIZED :: SDLWindowFlags
pattern SDL_WINDOW_MAXIMIZED          = SDLWindowFlags #{const SDL_WINDOW_MAXIMIZED}
pattern SDL_WINDOW_MOUSE_GRABBED :: SDLWindowFlags
pattern SDL_WINDOW_MOUSE_GRABBED       = SDLWindowFlags #{const SDL_WINDOW_MOUSE_GRABBED}
pattern SDL_WINDOW_HIGH_PIXEL_DENSITY :: SDLWindowFlags -- Renamed pattern
pattern SDL_WINDOW_HIGH_PIXEL_DENSITY   = SDLWindowFlags #{const SDL_WINDOW_HIGH_PIXEL_DENSITY}
pattern SDL_WINDOW_ALWAYS_ON_TOP :: SDLWindowFlags
pattern SDL_WINDOW_ALWAYS_ON_TOP        = SDLWindowFlags #{const SDL_WINDOW_ALWAYS_ON_TOP}

-- | Flash operation enumeration.
newtype SDLFlashOperation = SDLFlashOperation CInt
  deriving newtype (Show, Eq, Ord, Storable, Enum) -- Added Ord, Storable, Enum

pattern SDL_FLASH_CANCEL :: SDLFlashOperation
pattern SDL_FLASH_CANCEL         = SDLFlashOperation #{const SDL_FLASH_CANCEL}
pattern SDL_FLASH_BRIEFLY :: SDLFlashOperation
pattern SDL_FLASH_BRIEFLY        = SDLFlashOperation #{const SDL_FLASH_BRIEFLY}
pattern SDL_FLASH_UNTIL_FOCUSED :: SDLFlashOperation
pattern SDL_FLASH_UNTIL_FOCUSED  = SDLFlashOperation #{const SDL_FLASH_UNTIL_FOCUSED}

-- | Opaque OpenGL context handle.
newtype SDLGLContext = SDLGLContext (Ptr ())
  deriving (Show, Eq)

-- Helper to convert CInt to Bool
cToBool :: CInt -> Bool
cToBool 0 = False
cToBool _ = True

-- | Get the number of video drivers compiled into SDL.
foreign import ccall "SDL_GetNumVideoDrivers"
  sdlGetNumVideoDrivers_c :: IO CInt

sdlGetNumVideoDrivers :: IO Int
sdlGetNumVideoDrivers = fromIntegral <$> sdlGetNumVideoDrivers_c

-- | Get the name of a built-in video driver.
foreign import ccall "SDL_GetVideoDriver"
  sdlGetVideoDriver_c :: CInt -> IO CString

sdlGetVideoDriver :: Int -> IO String
sdlGetVideoDriver idx = do
  cstr <- sdlGetVideoDriver_c (fromIntegral idx)
  peekCString cstr

-- | Get the name of the currently initialized video driver.
foreign import ccall "SDL_GetCurrentVideoDriver"
  sdlGetCurrentVideoDriver_c :: IO CString

sdlGetCurrentVideoDriver :: IO (Maybe String)
sdlGetCurrentVideoDriver = do
  cstr <- sdlGetCurrentVideoDriver_c
  if cstr == nullPtr
    then return Nothing
    else Just <$> peekCString cstr

-- | Get a list of currently connected displays.
foreign import ccall "SDL_GetDisplays"
  sdlGetDisplays_c :: Ptr CInt -> IO (Ptr SDLDisplayID)

-- For the peekArray0 issue, modify sdlGetDisplays to handle the type conversion:
sdlGetDisplays :: IO [SDLDisplayID]
sdlGetDisplays = alloca $ \countPtr -> do
  ptr <- sdlGetDisplays_c countPtr
  count <- fromIntegral <$> peek countPtr
  if ptr == nullPtr
    then return []
    else do
      rawIDs <- peekArray0 0 (castPtr ptr :: Ptr CUInt)  -- Cast to Ptr CUInt first
      return $ rawIDs

-- | Return the primary display.
foreign import ccall "SDL_GetPrimaryDisplay"
  sdlGetPrimaryDisplay_c :: IO CUInt

sdlGetPrimaryDisplay :: IO (Maybe SDLDisplayID)
sdlGetPrimaryDisplay = do
  did <- sdlGetPrimaryDisplay_c
  return $ if did == 0 then Nothing else Just did

-- | Get the name of a display.
foreign import ccall "SDL_GetDisplayName"
  sdlGetDisplayName_c :: CUInt -> IO CString

sdlGetDisplayName :: SDLDisplayID -> IO (Maybe String)
sdlGetDisplayName did = do
  cstr <- sdlGetDisplayName_c did
  if cstr == nullPtr
    then return Nothing
    else Just <$> peekCString cstr

-- | Get the desktop area represented by a display.
foreign import ccall "SDL_GetDisplayBounds"
  sdlGetDisplayBounds_c :: CUInt -> Ptr SDLRect -> IO CInt

sdlGetDisplayBounds :: SDLDisplayID -> IO (Maybe SDLRect)
sdlGetDisplayBounds did = alloca $ \rectPtr -> do
  result <- sdlGetDisplayBounds_c did rectPtr
  if cToBool result
    then Just <$> peek rectPtr
    else return Nothing

-- | Create a window with the specified title, size, and flags.
foreign import ccall "SDL_CreateWindow"
  sdlCreateWindow_c :: CString -> CInt -> CInt -> CUInt -> IO (Ptr SDLWindow)

sdlCreateWindow :: String -> Int -> Int -> [SDLWindowFlags] -> IO (Maybe SDLWindow)
sdlCreateWindow title w h flags = withCString title $ \cTitle -> do
  let extractFlag (SDLWindowFlags flagVal) = flagVal
  -- Fold using bitwise OR (|.) on the extracted CUInts
  -- Start folding with zeroBits from Data.Bits for the correct CUInt zero
  let combinedFlags = foldr (.|.) zeroBits (map extractFlag flags)
  ptr <- sdlCreateWindow_c cTitle (fromIntegral w) (fromIntegral h) combinedFlags
  return $ if ptr == nullPtr then Nothing else Just (SDLWindow ptr)

-- | Destroy a window.
foreign import ccall "SDL_DestroyWindow"
  sdlDestroyWindow_c :: Ptr SDLWindow -> IO ()

sdlDestroyWindow :: SDLWindow -> IO ()
sdlDestroyWindow (SDLWindow ptr) = sdlDestroyWindow_c ptr

-- | Set the position of a window.
foreign import ccall "SDL_SetWindowPosition"
  sdlSetWindowPosition_c :: Ptr SDLWindow -> CInt -> CInt -> IO CInt

sdlSetWindowPosition :: SDLWindow -> Int -> Int -> IO Bool
sdlSetWindowPosition (SDLWindow ptr) x y =
  cToBool <$> sdlSetWindowPosition_c ptr (fromIntegral x) (fromIntegral y)

-- | Get the position of a window.
foreign import ccall "SDL_GetWindowPosition"
  sdlGetWindowPosition_c :: Ptr SDLWindow -> Ptr CInt -> Ptr CInt -> IO ()

sdlGetWindowPosition :: SDLWindow -> IO (Int, Int)
sdlGetWindowPosition (SDLWindow ptr) =
  alloca $ \xPtr -> alloca $ \yPtr -> do
    sdlGetWindowPosition_c ptr xPtr yPtr
    x <- fromIntegral <$> peek xPtr
    y <- fromIntegral <$> peek yPtr
    return (x, y)

-- | Set the size of a window.
foreign import ccall "SDL_SetWindowSize"
  sdlSetWindowSize_c :: Ptr SDLWindow -> CInt -> CInt -> IO CInt

sdlSetWindowSize :: SDLWindow -> Int -> Int -> IO Bool
sdlSetWindowSize (SDLWindow ptr) w h =
  cToBool <$> sdlSetWindowSize_c ptr (fromIntegral w) (fromIntegral h)

-- | Get the size of a window.
foreign import ccall "SDL_GetWindowSize"
  sdlGetWindowSize_c :: Ptr SDLWindow -> Ptr CInt -> Ptr CInt -> IO ()

sdlGetWindowSize :: SDLWindow -> IO (Int, Int)
sdlGetWindowSize (SDLWindow ptr) =
  alloca $ \wPtr -> alloca $ \hPtr -> do
    sdlGetWindowSize_c ptr wPtr hPtr
    w <- fromIntegral <$> peek wPtr
    h <- fromIntegral <$> peek hPtr
    return (w, h)

-- | Get the size of a window's client area, in pixels.
--   Corresponds to C's SDL_GetWindowSizeInPixels.
--   Returns 'True' on success.
foreign import ccall "SDL_GetWindowSizeInPixels"
  sdlGetWindowSizeInPixels_c :: Ptr SDLWindow -> Ptr CInt -> Ptr CInt -> IO CBool

-- | Get the size of a window's client area in pixels (the drawable size).
--
--   Returns `Just (width, height)` on success, or `Nothing` on failure
--   (e.g., if the window is invalid).
sdlGetWindowSizeInPixels :: SDLWindow -> IO (Maybe (Int, Int))
sdlGetWindowSizeInPixels (SDLWindow ptr) =
  alloca $ \wPtr -> alloca $ \hPtr -> do
    success_c <- sdlGetWindowSizeInPixels_c ptr wPtr hPtr
    -- Compare CBool directly: 0 means false, non-zero means true
    if success_c /= 0
    then do
      w <- fromIntegral <$> peek wPtr
      h <- fromIntegral <$> peek hPtr
      return (Just (w, h))
    else do
      return Nothing

-- | Show a window.
foreign import ccall "SDL_ShowWindow"
  sdlShowWindow_c :: Ptr SDLWindow -> IO CInt

sdlShowWindow :: SDLWindow -> IO Bool
sdlShowWindow (SDLWindow ptr) = cToBool <$> sdlShowWindow_c ptr

-- | Hide a window.
foreign import ccall "SDL_HideWindow"
  sdlHideWindow_c :: Ptr SDLWindow -> IO CInt

sdlHideWindow :: SDLWindow -> IO Bool
sdlHideWindow (SDLWindow ptr) = cToBool <$> sdlHideWindow_c ptr

-- | Load the OpenGL library.
foreign import ccall "SDL_GL_LoadLibrary"
  sdlGLLoadLibrary_c :: CString -> IO CInt

sdlGLLoadLibrary :: Maybe String -> IO Bool
sdlGLLoadLibrary Nothing = cToBool <$> sdlGLLoadLibrary_c nullPtr
sdlGLLoadLibrary (Just path) = withCString path $ \cPath ->
  cToBool <$> sdlGLLoadLibrary_c cPath

-- | Get an OpenGL function by name.
foreign import ccall "SDL_GL_GetProcAddress"
  sdlGLGetProcAddress_c :: CString -> IO (Ptr ())

sdlGLGetProcAddress :: String -> IO (Ptr ())
sdlGLGetProcAddress proc = withCString proc sdlGLGetProcAddress_c

-- | Unload the OpenGL library.
foreign import ccall "SDL_GL_UnloadLibrary"
  sdlGLUnloadLibrary_c :: IO ()

sdlGLUnloadLibrary :: IO ()
sdlGLUnloadLibrary = sdlGLUnloadLibrary_c

-- | Create an OpenGL context for a window.
foreign import ccall "SDL_GL_CreateContext"
  sdlGLCreateContext_c :: Ptr SDLWindow -> IO (Ptr ())

sdlGLCreateContext :: SDLWindow -> IO (Maybe SDLGLContext)
sdlGLCreateContext (SDLWindow ptr) = do
  ctx <- sdlGLCreateContext_c ptr
  return $ if ctx == nullPtr then Nothing else Just (SDLGLContext ctx)

-- | Make an OpenGL context current.
foreign import ccall "SDL_GL_MakeCurrent"
  sdlGLMakeCurrent_c :: Ptr SDLWindow -> Ptr () -> IO CInt

sdlGLMakeCurrent :: SDLWindow -> SDLGLContext -> IO Bool
sdlGLMakeCurrent (SDLWindow win) (SDLGLContext ctx) =
  cToBool <$> sdlGLMakeCurrent_c win ctx

-- | Swap the OpenGL window.
foreign import ccall "SDL_GL_SwapWindow"
  sdlGLSwapWindow_c :: Ptr SDLWindow -> IO CInt

sdlGLSwapWindow :: SDLWindow -> IO Bool
sdlGLSwapWindow (SDLWindow ptr) = cToBool <$> sdlGLSwapWindow_c ptr

-- | Destroy an OpenGL context.
foreign import ccall "SDL_GL_DestroyContext"
  sdlGLDestroyContext_c :: Ptr () -> IO CInt

sdlGLDestroyContext :: SDLGLContext -> IO Bool
sdlGLDestroyContext (SDLGLContext ctx) = cToBool <$> sdlGLDestroyContext_c ctx
