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

module SDL3.Video
  ( -- * Types
    SDLDisplayID,
    SDLWindowID,
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
    pattern SDL_WINDOW_METAL,
    pattern SDL_WINDOW_VULKAN,
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
    -- * Extended SDL3.2+ Video API (STUBS, TODO: Implement)
    sdlGetSystemTheme,
    sdlGetDisplayProperties,
    sdlGetDisplayUsableBounds,
    sdlGetNaturalDisplayOrientation,
    sdlGetCurrentDisplayOrientation,
    sdlGetDisplayContentScale,
    sdlGetFullscreenDisplayModes,
    sdlGetClosestFullscreenDisplayMode,
    sdlGetDesktopDisplayMode,
    sdlGetCurrentDisplayMode,
    sdlGetDisplayForPoint,
    sdlGetDisplayForRect,
    sdlGetDisplayForWindow,
    sdlGetWindowPixelDensity,
    sdlGetWindowDisplayScale,
    sdlSetWindowFullscreenMode,
    sdlGetWindowFullscreenMode,
    sdlGetWindowICCProfile,
    sdlGetWindowPixelFormat,
    sdlGetWindows,
    sdlCreatePopupWindow,
    sdlCreateWindowWithProperties,
    sdlGetWindowID,
    sdlGetWindowFromID,
    sdlGetWindowParent,
    sdlGetWindowProperties,
    sdlGetWindowFlagsEx,
    sdlSetWindowTitleEx,
    sdlGetWindowTitleEx,
    sdlSetWindowIcon,
    sdlGetWindowSafeArea,
    sdlSetWindowAspectRatio,
    sdlGetWindowAspectRatio,
    sdlGetWindowBordersSize,
    sdlSetWindowMinimumSize,
    sdlGetWindowMinimumSize,
    sdlSetWindowMaximumSize,
    sdlGetWindowMaximumSize,
    sdlSetWindowFullscreen,
    sdlSyncWindow
  ) where

#include <SDL3/SDL_video.h>

import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types
import Foreign.Ptr (Ptr, FunPtr, nullPtr, castPtr)
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (peekArray0, withArrayLen)
import Foreign.Storable (Storable(..))
import SDL3.Rect (SDLRect(..), SDLPoint(..))
import SDL3.Pixels (SDLPixelFormat(..), cUIntToPixelFormat, pixelFormatToCUInt)
import SDL3.Properties (SDLPropertiesID)
import SDL3.Surface (SDLSurface)
import Data.Bits
import Data.Word (Word8)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (fromBool, with, maybeWith)

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
    let fmt = (cUIntToPixelFormat fmtVal) :: SDLPixelFormat
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
pattern SDL_WINDOW_METAL :: SDLWindowFlags
pattern SDL_WINDOW_METAL              = SDLWindowFlags #{const SDL_WINDOW_METAL}
pattern SDL_WINDOW_VULKAN :: SDLWindowFlags
pattern SDL_WINDOW_VULKAN             = SDLWindowFlags #{const SDL_WINDOW_VULKAN}
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

--------------------------------------------------------------------------------
-- SDL_video.h window property/action bindings (see SDL_video.txt)
--------------------------------------------------------------------------------

--   Needs direct FFI, simple marshalling.
foreign import ccall unsafe "SDL_SetWindowBordered"
  c_sdlSetWindowBordered :: Ptr SDLWindow -> CInt -> IO CInt

sdlSetWindowBordered :: SDLWindow -> Bool -> IO Bool
sdlSetWindowBordered (SDLWindow ptr) bordered =
  cToBool <$> c_sdlSetWindowBordered ptr (fromBool bordered)

-- | Set whether the window is resizable.
foreign import ccall unsafe "SDL_SetWindowResizable"
  c_sdlSetWindowResizable :: Ptr SDLWindow -> CInt -> IO CInt

sdlSetWindowResizable :: SDLWindow -> Bool -> IO Bool
sdlSetWindowResizable (SDLWindow ptr) resizable =
  cToBool <$> c_sdlSetWindowResizable ptr (fromBool resizable)

-- | Set whether the window is always on top.
foreign import ccall unsafe "SDL_SetWindowAlwaysOnTop"
  c_sdlSetWindowAlwaysOnTop :: Ptr SDLWindow -> CInt -> IO CInt

sdlSetWindowAlwaysOnTop :: SDLWindow -> Bool -> IO Bool
sdlSetWindowAlwaysOnTop (SDLWindow ptr) onTop =
  cToBool <$> c_sdlSetWindowAlwaysOnTop ptr (fromBool onTop)

-- | Raise the window above others.
foreign import ccall unsafe "SDL_RaiseWindow"
  c_sdlRaiseWindow :: Ptr SDLWindow -> IO ()

sdlRaiseWindow :: SDLWindow -> IO ()
sdlRaiseWindow (SDLWindow ptr) = c_sdlRaiseWindow ptr

-- | Maximize the window.
foreign import ccall unsafe "SDL_MaximizeWindow"
  c_sdlMaximizeWindow :: Ptr SDLWindow -> IO ()

sdlMaximizeWindow :: SDLWindow -> IO ()
sdlMaximizeWindow (SDLWindow ptr) = c_sdlMaximizeWindow ptr

-- | Minimize the window.
foreign import ccall unsafe "SDL_MinimizeWindow"
  c_sdlMinimizeWindow :: Ptr SDLWindow -> IO ()

sdlMinimizeWindow :: SDLWindow -> IO ()
sdlMinimizeWindow (SDLWindow ptr) = c_sdlMinimizeWindow ptr

-- | Restore the window.
foreign import ccall unsafe "SDL_RestoreWindow"
  c_sdlRestoreWindow :: Ptr SDLWindow -> IO ()

sdlRestoreWindow :: SDLWindow -> IO ()
sdlRestoreWindow (SDLWindow ptr) = c_sdlRestoreWindow ptr

-- | Set whether the window is in fullscreen mode.
foreign import ccall unsafe "SDL_SetWindowFullscreen"
  c_sdlSetWindowFullscreen :: Ptr SDLWindow -> CInt -> IO CInt

sdlSetWindowFullscreen :: SDLWindow -> Bool -> IO Bool
sdlSetWindowFullscreen (SDLWindow ptr) fullscreen =
  cToBool <$> c_sdlSetWindowFullscreen ptr (fromBool fullscreen)

-- | Sync the window's state with the system.
foreign import ccall unsafe "SDL_SyncWindow"
  c_sdlSyncWindow :: Ptr SDLWindow -> IO ()

sdlSyncWindow :: SDLWindow -> IO ()
sdlSyncWindow (SDLWindow ptr) = c_sdlSyncWindow ptr

-- NOTE: Mark any functions that require cbits wrappers below as you encounter them.

--------------------------------------------------------------------------------
-- SDL3.2+ window surface management (see SDL_video.txt)
--------------------------------------------------------------------------------

-- | Query if a window has a surface.
foreign import ccall unsafe "SDL_WindowHasSurface"
  c_sdlWindowHasSurface :: Ptr SDLWindow -> IO CInt

sdlWindowHasSurface :: SDLWindow -> IO Bool
sdlWindowHasSurface (SDLWindow ptr) = cToBool <$> c_sdlWindowHasSurface ptr

-- | Get the window's surface.
foreign import ccall unsafe "wrapper_SDL_GetWindowSurface"
  c_wrapper_SDL_GetWindowSurface :: Ptr SDLWindow -> IO (Ptr SDLSurface)

sdlGetWindowSurface :: SDLWindow -> IO (Maybe (Ptr SDLSurface))
sdlGetWindowSurface (SDLWindow ptr) = do
  surfPtr <- c_wrapper_SDL_GetWindowSurface ptr
  return $ if surfPtr == nullPtr then Nothing else Just surfPtr

-- | Set the window surface VSync.
foreign import ccall unsafe "SDL_SetWindowSurfaceVSync"
  c_sdlSetWindowSurfaceVSync :: Ptr SDLWindow -> CInt -> IO CInt

sdlSetWindowSurfaceVSync :: SDLWindow -> Int -> IO Bool
sdlSetWindowSurfaceVSync (SDLWindow ptr) vsync =
  cToBool <$> c_sdlSetWindowSurfaceVSync ptr (fromIntegral vsync)

-- | Get the window surface VSync.
foreign import ccall unsafe "SDL_GetWindowSurfaceVSync"
  c_sdlGetWindowSurfaceVSync :: Ptr SDLWindow -> Ptr CInt -> IO CInt

sdlGetWindowSurfaceVSync :: SDLWindow -> IO (Maybe Int)
sdlGetWindowSurfaceVSync (SDLWindow ptr) =
  alloca $ \vsyncPtr -> do
    ok <- c_sdlGetWindowSurfaceVSync ptr vsyncPtr
    if cToBool ok
      then Just . fromIntegral <$> peek vsyncPtr
      else return Nothing

-- | Update the window surface.
foreign import ccall unsafe "SDL_UpdateWindowSurface"
  c_sdlUpdateWindowSurface :: Ptr SDLWindow -> IO CInt

sdlUpdateWindowSurface :: SDLWindow -> IO Bool
sdlUpdateWindowSurface (SDLWindow ptr) =
  cToBool <$> c_sdlUpdateWindowSurface ptr

-- | Update a set of rectangles on the window surface.
foreign import ccall unsafe "wrapper_SDL_UpdateWindowSurfaceRects"
  c_wrapper_SDL_UpdateWindowSurfaceRects :: Ptr SDLWindow -> Ptr SDLRect -> CInt -> IO CInt

sdlUpdateWindowSurfaceRects :: SDLWindow -> [SDLRect] -> IO Bool
sdlUpdateWindowSurfaceRects (SDLWindow ptr) rects =
  withArrayLen rects $ \len rectsPtr ->
    cToBool <$> c_wrapper_SDL_UpdateWindowSurfaceRects ptr rectsPtr (fromIntegral len)

-- | Destroy the window surface.
foreign import ccall unsafe "SDL_DestroyWindowSurface"
  c_sdlDestroyWindowSurface :: Ptr SDLWindow -> IO ()

sdlDestroyWindowSurface :: SDLWindow -> IO ()
sdlDestroyWindowSurface (SDLWindow ptr) = c_sdlDestroyWindowSurface ptr


--------------------------------------------------------------------------------
-- SDL3.2+ window grab, mouse rect, opacity, parent, modal, focus, etc.
--------------------------------------------------------------------------------


-- | Set window keyboard grab.
foreign import ccall unsafe "SDL_SetWindowKeyboardGrab"
  c_sdlSetWindowKeyboardGrab :: Ptr SDLWindow -> CInt -> IO CInt

sdlSetWindowKeyboardGrab :: SDLWindow -> Bool -> IO Bool
sdlSetWindowKeyboardGrab (SDLWindow ptr) grabbed =
  cToBool <$> c_sdlSetWindowKeyboardGrab ptr (fromBool grabbed)

-- | Set window mouse grab.
foreign import ccall unsafe "SDL_SetWindowMouseGrab"
  c_sdlSetWindowMouseGrab :: Ptr SDLWindow -> CInt -> IO CInt

sdlSetWindowMouseGrab :: SDLWindow -> Bool -> IO Bool
sdlSetWindowMouseGrab (SDLWindow ptr) grabbed =
  cToBool <$> c_sdlSetWindowMouseGrab ptr (fromBool grabbed)

-- | Get window keyboard grab.
foreign import ccall unsafe "SDL_GetWindowKeyboardGrab"
  c_sdlGetWindowKeyboardGrab :: Ptr SDLWindow -> IO CInt

sdlGetWindowKeyboardGrab :: SDLWindow -> IO Bool
sdlGetWindowKeyboardGrab (SDLWindow ptr) = cToBool <$> c_sdlGetWindowKeyboardGrab ptr

-- | Get window mouse grab.
foreign import ccall unsafe "SDL_GetWindowMouseGrab"
  c_sdlGetWindowMouseGrab :: Ptr SDLWindow -> IO CInt

sdlGetWindowMouseGrab :: SDLWindow -> IO Bool
sdlGetWindowMouseGrab (SDLWindow ptr) = cToBool <$> c_sdlGetWindowMouseGrab ptr

-- | Get the currently grabbed window.
foreign import ccall unsafe "wrapper_SDL_GetGrabbedWindow"
  c_wrapper_SDL_GetGrabbedWindow :: IO (Ptr SDLWindow)

sdlGetGrabbedWindow :: IO (Maybe SDLWindow)
sdlGetGrabbedWindow = do
  ptr <- c_wrapper_SDL_GetGrabbedWindow
  return $ if ptr == nullPtr then Nothing else Just (SDLWindow ptr)

-- | Set window mouse rect.
foreign import ccall unsafe "wrapper_SDL_SetWindowMouseRect"
  c_wrapper_SDL_SetWindowMouseRect :: Ptr SDLWindow -> Ptr SDLRect -> IO CInt

sdlSetWindowMouseRect :: SDLWindow -> Maybe SDLRect -> IO Bool
sdlSetWindowMouseRect (SDLWindow ptr) mRect =
  maybeWith with mRect $ \rectPtr ->
    cToBool <$> c_wrapper_SDL_SetWindowMouseRect ptr rectPtr

-- | Get window mouse rect.
foreign import ccall unsafe "wrapper_SDL_GetWindowMouseRect"
  c_wrapper_SDL_GetWindowMouseRect :: Ptr SDLWindow -> IO (Ptr SDLRect)

sdlGetWindowMouseRect :: SDLWindow -> IO (Maybe SDLRect)
sdlGetWindowMouseRect (SDLWindow ptr) = do
  rectPtr <- c_wrapper_SDL_GetWindowMouseRect ptr
  if rectPtr == nullPtr then return Nothing else Just <$> peek rectPtr

-- | Set window opacity.
foreign import ccall unsafe "SDL_SetWindowOpacity"
  c_sdlSetWindowOpacity :: Ptr SDLWindow -> CFloat -> IO CInt

sdlSetWindowOpacity :: SDLWindow -> Float -> IO Bool
sdlSetWindowOpacity (SDLWindow ptr) opacity =
  cToBool <$> c_sdlSetWindowOpacity ptr (realToFrac opacity)

-- | Get window opacity.
foreign import ccall unsafe "SDL_GetWindowOpacity"
  c_sdlGetWindowOpacity :: Ptr SDLWindow -> Ptr CFloat -> IO CInt

sdlGetWindowOpacity :: SDLWindow -> IO (Maybe Float)
sdlGetWindowOpacity (SDLWindow ptr) =
  alloca $ \opacityPtr -> do
    ok <- c_sdlGetWindowOpacity ptr opacityPtr
    if cToBool ok
      then Just . realToFrac <$> peek opacityPtr
      else return Nothing

-- | Set window parent.
foreign import ccall unsafe "SDL_SetWindowParent"
  c_sdlSetWindowParent :: Ptr SDLWindow -> Ptr SDLWindow -> IO CInt

sdlSetWindowParent :: SDLWindow -> SDLWindow -> IO Bool
sdlSetWindowParent (SDLWindow ptr) (SDLWindow parentPtr) =
  cToBool <$> c_sdlSetWindowParent ptr parentPtr

-- | Set window modal.
foreign import ccall unsafe "SDL_SetWindowModal"
  c_sdlSetWindowModal :: Ptr SDLWindow -> CInt -> IO CInt

sdlSetWindowModal :: SDLWindow -> Bool -> IO Bool
sdlSetWindowModal (SDLWindow ptr) modal =
  cToBool <$> c_sdlSetWindowModal ptr (fromBool modal)

-- | Set window focusable.
foreign import ccall unsafe "SDL_SetWindowFocusable"
  c_sdlSetWindowFocusable :: Ptr SDLWindow -> CInt -> IO CInt

sdlSetWindowFocusable :: SDLWindow -> Bool -> IO Bool
sdlSetWindowFocusable (SDLWindow ptr) focusable =
  cToBool <$> c_sdlSetWindowFocusable ptr (fromBool focusable)

-- | Show window system menu at (x, y).
foreign import ccall unsafe "SDL_ShowWindowSystemMenu"
  c_sdlShowWindowSystemMenu :: Ptr SDLWindow -> CInt -> CInt -> IO CInt

sdlShowWindowSystemMenu :: SDLWindow -> Int -> Int -> IO Bool
sdlShowWindowSystemMenu (SDLWindow ptr) x y =
  cToBool <$> c_sdlShowWindowSystemMenu ptr (fromIntegral x) (fromIntegral y)

-- | Set window hit test callback.
foreign import ccall unsafe "wrapper_SDL_SetWindowHitTest"
  c_wrapper_SDL_SetWindowHitTest :: Ptr SDLWindow -> FunPtr () -> Ptr () -> IO CInt

sdlSetWindowHitTest :: SDLWindow -> FunPtr () -> Ptr () -> IO Bool
sdlSetWindowHitTest (SDLWindow ptr) cb cbData =
  cToBool <$> c_wrapper_SDL_SetWindowHitTest ptr cb cbData


-- | Set window shape.
foreign import ccall unsafe "SDL_SetWindowShape"
  c_sdlSetWindowShape :: Ptr SDLWindow -> Ptr SDLSurface -> IO CInt

sdlSetWindowShape :: SDLWindow -> Ptr SDLSurface -> IO Bool
sdlSetWindowShape (SDLWindow ptr) surfPtr =
  cToBool <$> c_sdlSetWindowShape ptr surfPtr

-- | Flash window.
foreign import ccall unsafe "SDL_FlashWindow"
  c_sdlFlashWindow :: Ptr SDLWindow -> CInt -> IO CInt

sdlFlashWindow :: SDLWindow -> SDLFlashOperation -> IO Bool
sdlFlashWindow (SDLWindow ptr) (SDLFlashOperation op) =
  cToBool <$> c_sdlFlashWindow ptr op

-- | Set window progress state.
foreign import ccall unsafe "SDL_SetWindowProgressState"
  c_sdlSetWindowProgressState :: Ptr SDLWindow -> CInt -> IO CInt

sdlSetWindowProgressState :: SDLWindow -> CInt -> IO Bool
sdlSetWindowProgressState (SDLWindow ptr) state =
  cToBool <$> c_sdlSetWindowProgressState ptr state

-- | Get window progress state.
foreign import ccall unsafe "SDL_GetWindowProgressState"
  c_sdlGetWindowProgressState :: Ptr SDLWindow -> Ptr CInt -> IO CInt

sdlGetWindowProgressState :: SDLWindow -> IO (Maybe CInt)
sdlGetWindowProgressState (SDLWindow ptr) =
  alloca $ \statePtr -> do
    ok <- c_sdlGetWindowProgressState ptr statePtr
    if cToBool ok
      then Just <$> peek statePtr
      else return Nothing

-- | Set window progress value.
foreign import ccall unsafe "SDL_SetWindowProgressValue"
  c_sdlSetWindowProgressValue :: Ptr SDLWindow -> CFloat -> IO CInt

sdlSetWindowProgressValue :: SDLWindow -> Float -> IO Bool
sdlSetWindowProgressValue (SDLWindow ptr) value =
  cToBool <$> c_sdlSetWindowProgressValue ptr (realToFrac value)

-- | Get window progress value.
foreign import ccall unsafe "SDL_GetWindowProgressValue"
  c_sdlGetWindowProgressValue :: Ptr SDLWindow -> Ptr CFloat -> IO CInt

sdlGetWindowProgressValue :: SDLWindow -> IO (Maybe Float)
sdlGetWindowProgressValue (SDLWindow ptr) =
  alloca $ \valPtr -> do
    ok <- c_sdlGetWindowProgressValue ptr valPtr
    if cToBool ok
      then Just . realToFrac <$> peek valPtr
      else return Nothing

--------------------------------------------------------------------------------
-- SDL3.2+ screensaver and GL/EGL/SwapInterval API (see SDL_video.txt)
--------------------------------------------------------------------------------


-- | Is screensaver enabled?
foreign import ccall unsafe "SDL_ScreenSaverEnabled"
  c_sdlScreenSaverEnabled :: IO CInt

sdlScreenSaverEnabled :: IO Bool
sdlScreenSaverEnabled = cToBool <$> c_sdlScreenSaverEnabled

-- | Enable screensaver.
foreign import ccall unsafe "SDL_EnableScreenSaver"
  c_sdlEnableScreenSaver :: IO ()

sdlEnableScreenSaver :: IO ()
sdlEnableScreenSaver = c_sdlEnableScreenSaver

-- | Disable screensaver.
foreign import ccall unsafe "SDL_DisableScreenSaver"
  c_sdlDisableScreenSaver :: IO ()

sdlDisableScreenSaver :: IO ()
sdlDisableScreenSaver = c_sdlDisableScreenSaver

-- | EGL get proc address.
foreign import ccall unsafe "SDL_EGL_GetProcAddress"
  c_sdlEGLGetProcAddress :: CString -> IO (Ptr ())

sdlEGLGetProcAddress :: String -> IO (Ptr ())
sdlEGLGetProcAddress proc = withCString proc c_sdlEGLGetProcAddress

-- | GL extension supported.
foreign import ccall unsafe "SDL_GL_ExtensionSupported"
  c_sdlGLExtensionSupported :: CString -> IO CInt

sdlGLExtensionSupported :: String -> IO Bool
sdlGLExtensionSupported ext = withCString ext (fmap cToBool . c_sdlGLExtensionSupported)

-- | GL reset attributes.
foreign import ccall unsafe "SDL_GL_ResetAttributes"
  c_sdlGLResetAttributes :: IO ()

sdlGLResetAttributes :: IO ()
sdlGLResetAttributes = c_sdlGLResetAttributes

-- | GL set attribute.
foreign import ccall unsafe "SDL_GL_SetAttribute"
  c_sdlGLSetAttribute :: CInt -> CInt -> IO CInt

sdlGLSetAttribute :: CInt -> CInt -> IO Bool
sdlGLSetAttribute attr value = cToBool <$> c_sdlGLSetAttribute attr value

-- | GL get attribute.
foreign import ccall unsafe "SDL_GL_GetAttribute"
  c_sdlGLGetAttribute :: CInt -> Ptr CInt -> IO CInt

sdlGLGetAttribute :: CInt -> IO (Maybe CInt)
sdlGLGetAttribute attr =
  alloca $ \valPtr -> do
    ok <- c_sdlGLGetAttribute attr valPtr
    if cToBool ok
      then Just <$> peek valPtr
      else return Nothing

-- | GL get current window.
foreign import ccall unsafe "wrapper_SDL_GL_GetCurrentWindow"
  c_wrapper_SDL_GL_GetCurrentWindow :: IO (Ptr SDLWindow)

sdlGLGetCurrentWindow :: IO (Maybe SDLWindow)
sdlGLGetCurrentWindow = do
  ptr <- c_wrapper_SDL_GL_GetCurrentWindow
  return $ if ptr == nullPtr then Nothing else Just (SDLWindow ptr)

-- | GL get current context.
foreign import ccall unsafe "SDL_GL_GetCurrentContext"
  c_sdlGLGetCurrentContext :: IO (Ptr ())

sdlGLGetCurrentContext :: IO (Ptr ())
sdlGLGetCurrentContext = c_sdlGLGetCurrentContext

-- | EGL get current display.
foreign import ccall unsafe "SDL_EGL_GetCurrentDisplay"
  c_sdlEGLGetCurrentDisplay :: IO (Ptr ())

sdlEGLGetCurrentDisplay :: IO (Ptr ())
sdlEGLGetCurrentDisplay = c_sdlEGLGetCurrentDisplay

-- | EGL get current config.
foreign import ccall unsafe "SDL_EGL_GetCurrentConfig"
  c_sdlEGLGetCurrentConfig :: IO (Ptr ())

sdlEGLGetCurrentConfig :: IO (Ptr ())
sdlEGLGetCurrentConfig = c_sdlEGLGetCurrentConfig

-- | EGL get window surface.
foreign import ccall unsafe "SDL_EGL_GetWindowSurface"
  c_sdlEGLGetWindowSurface :: Ptr SDLWindow -> IO (Ptr ())

sdlEGLGetWindowSurface :: SDLWindow -> IO (Ptr ())
sdlEGLGetWindowSurface (SDLWindow ptr) = c_sdlEGLGetWindowSurface ptr

-- | GL set swap interval.
foreign import ccall unsafe "SDL_GL_SetSwapInterval"
  c_sdlGLSetSwapInterval :: CInt -> IO CInt

sdlGLSetSwapInterval :: Int -> IO Bool
sdlGLSetSwapInterval interval = cToBool <$> c_sdlGLSetSwapInterval (fromIntegral interval)

-- | GL get swap interval.
foreign import ccall unsafe "SDL_GL_GetSwapInterval"
  c_sdlGLGetSwapInterval :: Ptr CInt -> IO CInt

sdlGLGetSwapInterval :: IO (Maybe Int)
sdlGLGetSwapInterval =
  alloca $ \intervalPtr -> do
    ok <- c_sdlGLGetSwapInterval intervalPtr
    if cToBool ok
      then Just . fromIntegral <$> peek intervalPtr
      else return Nothing

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
  count <- peek countPtr
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

-- | Get fullscreen display modes for a display.
--
-- This function marshals the C array of pointers to Haskell SDLDisplayMode values.
-- Returns an empty list if none are available.
foreign import ccall unsafe "SDL_GetFullscreenDisplayModes"
  c_sdlGetFullscreenDisplayModes :: CUInt -> Ptr CInt -> IO (Ptr (Ptr SDLDisplayMode))

sdlGetFullscreenDisplayModes :: SDLDisplayID -> IO [SDLDisplayMode]
sdlGetFullscreenDisplayModes did =
  alloca $ \countPtr -> do
    arrPtr <- c_sdlGetFullscreenDisplayModes did countPtr
    count <- fromIntegral <$> peek countPtr
    if arrPtr == nullPtr || count == 0
      then return []
      else do
        ptrs <- peekArray count arrPtr
        mapM peek ptrs

-- | Get the closest fullscreen display mode.
--
-- Fills the provided SDLDisplayMode struct with the closest mode, returns Just mode on success.
foreign import ccall unsafe "SDL_GetClosestFullscreenDisplayMode"
  c_sdlGetClosestFullscreenDisplayMode :: CUInt -> CInt -> CInt -> CFloat -> CInt -> Ptr SDLDisplayMode -> IO CInt

sdlGetClosestFullscreenDisplayMode :: SDLDisplayID -> Int -> Int -> Float -> Bool -> IO (Maybe SDLDisplayMode)
sdlGetClosestFullscreenDisplayMode did w h refreshRate includeHD =
  alloca $ \modePtr -> do
    ok <- c_sdlGetClosestFullscreenDisplayMode did (fromIntegral w) (fromIntegral h) (realToFrac refreshRate) (fromBool includeHD) modePtr
    if cToBool ok
      then Just <$> peek modePtr
      else return Nothing

-- | Get display for a point.
foreign import ccall unsafe "SDL_GetDisplayForPoint"
  c_sdlGetDisplayForPoint :: Ptr SDLPoint -> IO CUInt

sdlGetDisplayForPoint :: SDLPoint -> IO SDLDisplayID
sdlGetDisplayForPoint pt = with pt c_sdlGetDisplayForPoint

-- | Get window ICC profile.
sdlGetWindowICCProfile :: SDLWindow -> IO (Maybe [Word8])
foreign import ccall unsafe "wrapper_SDL_GetWindowICCProfile"
  c_wrapper_SDL_GetWindowICCProfile :: Ptr SDLWindow -> Ptr CSize -> IO (Ptr ())

-- | Get the ICC profile for a window as a list of bytes, or Nothing if unavailable.
-- The returned buffer is copied and must be freed by Haskell.

sdlGetWindowICCProfile (SDLWindow ptr) =
  alloca $ \sizePtr -> do
    bufPtr <- c_wrapper_SDL_GetWindowICCProfile ptr sizePtr
    size <- fromIntegral <$> peek sizePtr
    if bufPtr == nullPtr || size == 0
      then return Nothing
      else do
        bytes <- peekArray size (castPtr bufPtr)
        free bufPtr
        return (Just bytes)

-- | Get all windows.
--
-- Marshals the C array of window pointers into a Haskell list of SDLWindow.
foreign import ccall unsafe "SDL_GetWindows"
  c_sdlGetWindows :: Ptr CInt -> IO (Ptr (Ptr SDLWindow))

sdlGetWindows :: IO [SDLWindow]
sdlGetWindows =
  alloca $ \countPtr -> do
    arrPtr <- c_sdlGetWindows countPtr
    count <- fromIntegral <$> peek countPtr
    if arrPtr == nullPtr || count == 0
      then return []
      else do
        ptrs <- peekArray count arrPtr
        return $ map SDLWindow ptrs


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

--------------------------------------------------------------------------------
-- SDL3.2+ Extended Video API (STUBS)
--------------------------------------------------------------------------------

-- | Get the current system theme.
foreign import ccall unsafe "SDL_GetSystemTheme"
  c_sdlGetSystemTheme :: IO CInt

sdlGetSystemTheme :: IO SDLSystemTheme
sdlGetSystemTheme = SDLSystemTheme <$> c_sdlGetSystemTheme

-- | Get display properties.
foreign import ccall unsafe "SDL_GetDisplayProperties"
  c_sdlGetDisplayProperties :: CUInt -> IO SDLPropertiesID

sdlGetDisplayProperties :: SDLDisplayID -> IO SDLPropertiesID
sdlGetDisplayProperties did = c_sdlGetDisplayProperties did

-- | Get the usable bounds of a display.
foreign import ccall unsafe "SDL_GetDisplayUsableBounds"
  c_sdlGetDisplayUsableBounds :: CUInt -> Ptr SDLRect -> IO CInt

sdlGetDisplayUsableBounds :: SDLDisplayID -> IO (Maybe SDLRect)
sdlGetDisplayUsableBounds did = alloca $ \rectPtr -> do
  ok <- c_sdlGetDisplayUsableBounds did rectPtr
  if cToBool ok then Just <$> peek rectPtr else return Nothing

-- | Get the natural orientation of a display.
foreign import ccall unsafe "SDL_GetNaturalDisplayOrientation"
  c_sdlGetNaturalDisplayOrientation :: CUInt -> IO CInt

sdlGetNaturalDisplayOrientation :: SDLDisplayID -> IO SDLDisplayOrientation
sdlGetNaturalDisplayOrientation did = SDLDisplayOrientation <$> c_sdlGetNaturalDisplayOrientation did

-- | Get the current orientation of a display.
foreign import ccall unsafe "SDL_GetCurrentDisplayOrientation"
  c_sdlGetCurrentDisplayOrientation :: CUInt -> IO CInt

sdlGetCurrentDisplayOrientation :: SDLDisplayID -> IO SDLDisplayOrientation
sdlGetCurrentDisplayOrientation did = SDLDisplayOrientation <$> c_sdlGetCurrentDisplayOrientation did

-- | Get the content scale of a display.
foreign import ccall unsafe "SDL_GetDisplayContentScale"
  c_sdlGetDisplayContentScale :: CUInt -> IO CFloat

sdlGetDisplayContentScale :: SDLDisplayID -> IO Float
sdlGetDisplayContentScale did = realToFrac <$> c_sdlGetDisplayContentScale did

-- | Get the desktop display mode.
foreign import ccall unsafe "SDL_GetDesktopDisplayMode"
  c_sdlGetDesktopDisplayMode :: CUInt -> IO (Ptr SDLDisplayMode)

sdlGetDesktopDisplayMode :: SDLDisplayID -> IO (Maybe SDLDisplayMode)
sdlGetDesktopDisplayMode did = do
  ptr <- c_sdlGetDesktopDisplayMode did
  if ptr == nullPtr then return Nothing else Just <$> peek ptr

-- | Get the current display mode.
foreign import ccall unsafe "SDL_GetCurrentDisplayMode"
  c_sdlGetCurrentDisplayMode :: CUInt -> IO (Ptr SDLDisplayMode)

sdlGetCurrentDisplayMode :: SDLDisplayID -> IO (Maybe SDLDisplayMode)
sdlGetCurrentDisplayMode did = do
  ptr <- c_sdlGetCurrentDisplayMode did
  if ptr == nullPtr then return Nothing else Just <$> peek ptr

-- | Get display for a rect.
foreign import ccall unsafe "SDL_GetDisplayForRect"
  c_sdlGetDisplayForRect :: Ptr SDLRect -> IO CUInt

sdlGetDisplayForRect :: SDLRect -> IO SDLDisplayID
sdlGetDisplayForRect rect = with rect c_sdlGetDisplayForRect

-- | Get display for a window.
foreign import ccall unsafe "SDL_GetDisplayForWindow"
  c_sdlGetDisplayForWindow :: Ptr SDLWindow -> IO CUInt

sdlGetDisplayForWindow :: SDLWindow -> IO SDLDisplayID
sdlGetDisplayForWindow (SDLWindow ptr) = c_sdlGetDisplayForWindow ptr

-- | Get window pixel density.
foreign import ccall unsafe "SDL_GetWindowPixelDensity"
  c_sdlGetWindowPixelDensity :: Ptr SDLWindow -> IO CFloat

sdlGetWindowPixelDensity :: SDLWindow -> IO Float
sdlGetWindowPixelDensity (SDLWindow ptr) = realToFrac <$> c_sdlGetWindowPixelDensity ptr

-- | Get window display scale.
foreign import ccall unsafe "SDL_GetWindowDisplayScale"
  c_sdlGetWindowDisplayScale :: Ptr SDLWindow -> IO CFloat

sdlGetWindowDisplayScale :: SDLWindow -> IO Float
sdlGetWindowDisplayScale (SDLWindow ptr) = realToFrac <$> c_sdlGetWindowDisplayScale ptr

-- | Set window fullscreen mode.
foreign import ccall unsafe "SDL_SetWindowFullscreenMode"
  c_sdlSetWindowFullscreenMode :: Ptr SDLWindow -> Ptr SDLDisplayMode -> IO CInt

sdlSetWindowFullscreenMode :: SDLWindow -> Maybe SDLDisplayMode -> IO Bool
sdlSetWindowFullscreenMode (SDLWindow ptr) mMode =
  maybeWith with mMode $ \modePtr ->
    cToBool <$> c_sdlSetWindowFullscreenMode ptr modePtr

-- | Get window fullscreen mode.
foreign import ccall unsafe "SDL_GetWindowFullscreenMode"
  c_sdlGetWindowFullscreenMode :: Ptr SDLWindow -> IO (Ptr SDLDisplayMode)

sdlGetWindowFullscreenMode :: SDLWindow -> IO (Maybe SDLDisplayMode)
sdlGetWindowFullscreenMode (SDLWindow ptr) = do
  modePtr <- c_sdlGetWindowFullscreenMode ptr
  if modePtr == nullPtr then return Nothing else Just <$> peek modePtr

-- | Get window pixel format.
foreign import ccall unsafe "SDL_GetWindowPixelFormat"
  c_sdlGetWindowPixelFormat :: Ptr SDLWindow -> IO CUInt

sdlGetWindowPixelFormat :: SDLWindow -> IO SDLPixelFormat
sdlGetWindowPixelFormat (SDLWindow ptr) = cUIntToPixelFormat <$> c_sdlGetWindowPixelFormat ptr

-- | Create a popup window.
foreign import ccall unsafe "SDL_CreatePopupWindow"
  c_sdlCreatePopupWindow :: Ptr SDLWindow -> CInt -> CInt -> CInt -> CInt -> CUInt -> IO (Ptr SDLWindow)

sdlCreatePopupWindow :: SDLWindow -> Int -> Int -> Int -> Int -> [SDLWindowFlags] -> IO (Maybe SDLWindow)
sdlCreatePopupWindow (SDLWindow parent) ox oy w h flags = do
  let extractFlag (SDLWindowFlags flagVal) = flagVal
      combinedFlags = foldr (.|.) zeroBits (map extractFlag flags)
  ptr <- c_sdlCreatePopupWindow parent (fromIntegral ox) (fromIntegral oy) (fromIntegral w) (fromIntegral h) combinedFlags
  return $ if ptr == nullPtr then Nothing else Just (SDLWindow ptr)

-- | Create a window with properties.
foreign import ccall unsafe "SDL_CreateWindowWithProperties"
  c_sdlCreateWindowWithProperties :: SDLPropertiesID -> IO (Ptr SDLWindow)

sdlCreateWindowWithProperties :: SDLPropertiesID -> IO (Maybe SDLWindow)
sdlCreateWindowWithProperties props = do
  ptr <- c_sdlCreateWindowWithProperties props
  return $ if ptr == nullPtr then Nothing else Just (SDLWindow ptr)

-- | Get window ID.
foreign import ccall unsafe "SDL_GetWindowID"
  c_sdlGetWindowID :: Ptr SDLWindow -> IO CUInt

sdlGetWindowID :: SDLWindow -> IO SDLWindowID
sdlGetWindowID (SDLWindow ptr) = c_sdlGetWindowID ptr

-- | Get window from ID.
foreign import ccall unsafe "SDL_GetWindowFromID"
  c_sdlGetWindowFromID :: CUInt -> IO (Ptr SDLWindow)

sdlGetWindowFromID :: SDLWindowID -> IO (Maybe SDLWindow)
sdlGetWindowFromID wid = do
  ptr <- c_sdlGetWindowFromID wid
  return $ if ptr == nullPtr then Nothing else Just (SDLWindow ptr)

-- | Get window parent.
foreign import ccall unsafe "SDL_GetWindowParent"
  c_sdlGetWindowParent :: Ptr SDLWindow -> IO (Ptr SDLWindow)

sdlGetWindowParent :: SDLWindow -> IO (Maybe SDLWindow)
sdlGetWindowParent (SDLWindow ptr) = do
  parentPtr <- c_sdlGetWindowParent ptr
  return $ if parentPtr == nullPtr then Nothing else Just (SDLWindow parentPtr)

-- | Get window properties.
foreign import ccall unsafe "SDL_GetWindowProperties"
  c_sdlGetWindowProperties :: Ptr SDLWindow -> IO SDLPropertiesID

sdlGetWindowProperties :: SDLWindow -> IO SDLPropertiesID
sdlGetWindowProperties (SDLWindow ptr) = c_sdlGetWindowProperties ptr

-- | Get window flags (extended).
foreign import ccall unsafe "SDL_GetWindowFlags"
  c_sdlGetWindowFlagsEx :: Ptr SDLWindow -> IO CUInt

sdlGetWindowFlagsEx :: SDLWindow -> IO SDLWindowFlags
sdlGetWindowFlagsEx (SDLWindow ptr) = SDLWindowFlags <$> c_sdlGetWindowFlagsEx ptr

-- | Set window title (extended).
foreign import ccall unsafe "SDL_SetWindowTitle"
  c_sdlSetWindowTitleEx :: Ptr SDLWindow -> CString -> IO CInt

sdlSetWindowTitleEx :: SDLWindow -> String -> IO Bool
sdlSetWindowTitleEx (SDLWindow ptr) title =
  withCString title $ \cstr -> cToBool <$> c_sdlSetWindowTitleEx ptr cstr

-- | Get window title (extended).
foreign import ccall unsafe "SDL_GetWindowTitle"
  c_sdlGetWindowTitleEx :: Ptr SDLWindow -> IO CString

sdlGetWindowTitleEx :: SDLWindow -> IO (Maybe String)
sdlGetWindowTitleEx (SDLWindow ptr) = do
  cstr <- c_sdlGetWindowTitleEx ptr
  if cstr == nullPtr then return Nothing else Just <$> peekCString cstr

-- | Set window icon.
foreign import ccall unsafe "SDL_SetWindowIcon"
  c_sdlSetWindowIcon :: Ptr SDLWindow -> Ptr SDLSurface -> IO CInt

sdlSetWindowIcon :: SDLWindow -> Ptr SDLSurface -> IO Bool
sdlSetWindowIcon (SDLWindow ptr) iconPtr = cToBool <$> c_sdlSetWindowIcon ptr iconPtr

-- | Get window safe area.
foreign import ccall unsafe "SDL_GetWindowSafeArea"
  c_sdlGetWindowSafeArea :: Ptr SDLWindow -> Ptr SDLRect -> IO CInt

sdlGetWindowSafeArea :: SDLWindow -> IO (Maybe SDLRect)
sdlGetWindowSafeArea (SDLWindow ptr) = alloca $ \rectPtr -> do
  ok <- c_sdlGetWindowSafeArea ptr rectPtr
  if cToBool ok then Just <$> peek rectPtr else return Nothing

-- | Set window aspect ratio.
foreign import ccall unsafe "SDL_SetWindowAspectRatio"
  c_sdlSetWindowAspectRatio :: Ptr SDLWindow -> CFloat -> CFloat -> IO CInt

sdlSetWindowAspectRatio :: SDLWindow -> Float -> Float -> IO Bool
sdlSetWindowAspectRatio (SDLWindow ptr) minA maxA =
  cToBool <$> c_sdlSetWindowAspectRatio ptr (realToFrac minA) (realToFrac maxA)

-- | Get window aspect ratio.
foreign import ccall unsafe "SDL_GetWindowAspectRatio"
  c_sdlGetWindowAspectRatio :: Ptr SDLWindow -> Ptr CFloat -> Ptr CFloat -> IO CInt

sdlGetWindowAspectRatio :: SDLWindow -> IO (Maybe (Float, Float))
sdlGetWindowAspectRatio (SDLWindow ptr) =
  alloca $ \minPtr -> alloca $ \maxPtr -> do
    ok <- c_sdlGetWindowAspectRatio ptr minPtr maxPtr
    if cToBool ok
      then do
        minA <- realToFrac <$> peek minPtr
        maxA <- realToFrac <$> peek maxPtr
        return $ Just (minA, maxA)
      else return Nothing

-- | Get window borders size.
foreign import ccall unsafe "SDL_GetWindowBordersSize"
  c_sdlGetWindowBordersSize :: Ptr SDLWindow -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

sdlGetWindowBordersSize :: SDLWindow -> IO (Maybe (Int, Int, Int, Int))
sdlGetWindowBordersSize (SDLWindow ptr) =
  alloca $ \topPtr -> alloca $ \leftPtr -> alloca $ \bottomPtr -> alloca $ \rightPtr -> do
    ok <- c_sdlGetWindowBordersSize ptr topPtr leftPtr bottomPtr rightPtr
    if cToBool ok
      then do
        top <- fromIntegral <$> peek topPtr
        left <- fromIntegral <$> peek leftPtr
        bottom <- fromIntegral <$> peek bottomPtr
        right <- fromIntegral <$> peek rightPtr
        return $ Just (top, left, bottom, right)
      else return Nothing

-- | Set window minimum size.
foreign import ccall unsafe "SDL_SetWindowMinimumSize"
  c_sdlSetWindowMinimumSize :: Ptr SDLWindow -> CInt -> CInt -> IO CInt

sdlSetWindowMinimumSize :: SDLWindow -> Int -> Int -> IO Bool
sdlSetWindowMinimumSize (SDLWindow ptr) minW minH =
  cToBool <$> c_sdlSetWindowMinimumSize ptr (fromIntegral minW) (fromIntegral minH)

-- | Get window minimum size.
foreign import ccall unsafe "SDL_GetWindowMinimumSize"
  c_sdlGetWindowMinimumSize :: Ptr SDLWindow -> Ptr CInt -> Ptr CInt -> IO CInt

sdlGetWindowMinimumSize :: SDLWindow -> IO (Maybe (Int, Int))
sdlGetWindowMinimumSize (SDLWindow ptr) =
  alloca $ \wPtr -> alloca $ \hPtr -> do
    ok <- c_sdlGetWindowMinimumSize ptr wPtr hPtr
    if cToBool ok
      then do
        w <- fromIntegral <$> peek wPtr
        h <- fromIntegral <$> peek hPtr
        return $ Just (w, h)
      else return Nothing

-- | Set window maximum size.
foreign import ccall unsafe "SDL_SetWindowMaximumSize"
  c_sdlSetWindowMaximumSize :: Ptr SDLWindow -> CInt -> CInt -> IO CInt

sdlSetWindowMaximumSize :: SDLWindow -> Int -> Int -> IO Bool
sdlSetWindowMaximumSize (SDLWindow ptr) maxW maxH =
  cToBool <$> c_sdlSetWindowMaximumSize ptr (fromIntegral maxW) (fromIntegral maxH)

-- | Get window maximum size.
foreign import ccall unsafe "SDL_GetWindowMaximumSize"
  c_sdlGetWindowMaximumSize :: Ptr SDLWindow -> Ptr CInt -> Ptr CInt -> IO CInt

sdlGetWindowMaximumSize :: SDLWindow -> IO (Maybe (Int, Int))
sdlGetWindowMaximumSize (SDLWindow ptr) =
  alloca $ \wPtr -> alloca $ \hPtr -> do
    ok <- c_sdlGetWindowMaximumSize ptr wPtr hPtr
    if cToBool ok
      then do
        w <- fromIntegral <$> peek wPtr
        h <- fromIntegral <$> peek hPtr
        return $ Just (w, h)
      else return Nothing
