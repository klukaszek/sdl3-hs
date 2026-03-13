{-# LANGUAGE NoMonomorphismRestriction #-}

module SDL3.Wrapped.Video
  ( module SDL3.Raw.Video
  , sdlCreateWindow
  , sdlDestroyWindow
  , sdlSetWindowPosition
  , sdlGetWindowPosition
  , sdlSetWindowSize
  , sdlGetWindowSize
  , sdlSetWindowFillDocument
  , sdlGetWindowSizeInPixels
  , sdlShowWindow
  , sdlHideWindow
  , sdlGetNumVideoDrivers
  , sdlGetVideoDriver
  , sdlGetCurrentVideoDriver
  , sdlGetDisplays
  , sdlGetPrimaryDisplay
  , sdlGetDisplayName
  , sdlGetDisplayBounds
  , sdlGLLoadLibrary
  , sdlGLGetProcAddress
  , sdlGLUnloadLibrary
  , sdlGLCreateContext
  , sdlGLMakeCurrent
  , sdlGLSwapWindow
  , sdlGLDestroyContext
  , sdlGetSystemTheme
  , sdlGetDisplayProperties
  , sdlGetDisplayUsableBounds
  , sdlGetNaturalDisplayOrientation
  , sdlGetCurrentDisplayOrientation
  , sdlGetDisplayContentScale
  , sdlGetFullscreenDisplayModes
  , sdlGetClosestFullscreenDisplayMode
  , sdlGetDesktopDisplayMode
  , sdlGetCurrentDisplayMode
  , sdlGetDisplayForPoint
  , sdlGetDisplayForRect
  , sdlGetDisplayForWindow
  , sdlGetWindowPixelDensity
  , sdlGetWindowDisplayScale
  , sdlSetWindowFullscreenMode
  , sdlGetWindowFullscreenMode
  , sdlGetWindowICCProfile
  , sdlGetWindowPixelFormat
  , sdlGetWindows
  , sdlCreatePopupWindow
  , sdlCreateWindowWithProperties
  , sdlGetWindowID
  , sdlGetWindowFromID
  , sdlGetWindowParent
  , sdlGetWindowProperties
  , sdlGetWindowFlagsEx
  , sdlSetWindowTitleEx
  , sdlGetWindowTitleEx
  , sdlSetWindowIcon
  , sdlGetWindowSafeArea
  , sdlSetWindowAspectRatio
  , sdlGetWindowAspectRatio
  , sdlGetWindowBordersSize
  , sdlSetWindowMinimumSize
  , sdlGetWindowMinimumSize
  , sdlSetWindowMaximumSize
  , sdlGetWindowMaximumSize
  , sdlSetWindowFullscreen
  , sdlSyncWindow
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import SDL3.Raw.Video hiding
  ( sdlCreatePopupWindow
  , sdlCreateWindow
  , sdlCreateWindowWithProperties
  , sdlDestroyWindow
  , sdlGetClosestFullscreenDisplayMode
  , sdlGetCurrentDisplayMode
  , sdlGetCurrentDisplayOrientation
  , sdlGetCurrentVideoDriver
  , sdlGetDesktopDisplayMode
  , sdlGetDisplayBounds
  , sdlGetDisplayContentScale
  , sdlGetDisplayForPoint
  , sdlGetDisplayForRect
  , sdlGetDisplayForWindow
  , sdlGetDisplayName
  , sdlGetDisplayProperties
  , sdlGetDisplayUsableBounds
  , sdlGetDisplays
  , sdlGetFullscreenDisplayModes
  , sdlGetNaturalDisplayOrientation
  , sdlGetNumVideoDrivers
  , sdlGetPrimaryDisplay
  , sdlGetSystemTheme
  , sdlGetVideoDriver
  , sdlGetWindowAspectRatio
  , sdlGetWindowBordersSize
  , sdlGetWindowFlagsEx
  , sdlGetWindowFromID
  , sdlGetWindowFullscreenMode
  , sdlGetWindowICCProfile
  , sdlGetWindowID
  , sdlGetWindowMaximumSize
  , sdlGetWindowMinimumSize
  , sdlGetWindowParent
  , sdlGetWindowPixelDensity
  , sdlGetWindowPixelFormat
  , sdlGetWindowPosition
  , sdlGetWindowProperties
  , sdlGetWindowSafeArea
  , sdlGetWindowSize
  , sdlGetWindowSizeInPixels
  , sdlGetWindowTitleEx
  , sdlGetWindowDisplayScale
  , sdlGetWindows
  , sdlGLCreateContext
  , sdlGLDestroyContext
  , sdlGLGetProcAddress
  , sdlGLLoadLibrary
  , sdlGLMakeCurrent
  , sdlGLSwapWindow
  , sdlGLUnloadLibrary
  , sdlHideWindow
  , sdlSetWindowAspectRatio
  , sdlSetWindowFillDocument
  , sdlSetWindowFullscreen
  , sdlSetWindowFullscreenMode
  , sdlSetWindowIcon
  , sdlSetWindowMaximumSize
  , sdlSetWindowMinimumSize
  , sdlSetWindowPosition
  , sdlSetWindowSize
  , sdlSetWindowTitleEx
  , sdlShowWindow
  , sdlSyncWindow
  )
import qualified SDL3.Raw.Video as Raw

sdlCreateWindow title w h flags = liftIO $ Raw.sdlCreateWindow title w h flags
sdlDestroyWindow = liftIO . Raw.sdlDestroyWindow
sdlSetWindowPosition window x y = liftIO $ Raw.sdlSetWindowPosition window x y
sdlGetWindowPosition = liftIO . Raw.sdlGetWindowPosition
sdlSetWindowSize window w h = liftIO $ Raw.sdlSetWindowSize window w h
sdlGetWindowSize = liftIO . Raw.sdlGetWindowSize
sdlSetWindowFillDocument window fillDocument = liftIO $ Raw.sdlSetWindowFillDocument window fillDocument
sdlGetWindowSizeInPixels = liftIO . Raw.sdlGetWindowSizeInPixels
sdlShowWindow = liftIO . Raw.sdlShowWindow
sdlHideWindow = liftIO . Raw.sdlHideWindow
sdlGetNumVideoDrivers = liftIO Raw.sdlGetNumVideoDrivers
sdlGetVideoDriver = liftIO . Raw.sdlGetVideoDriver
sdlGetCurrentVideoDriver = liftIO Raw.sdlGetCurrentVideoDriver
sdlGetDisplays = liftIO Raw.sdlGetDisplays
sdlGetPrimaryDisplay = liftIO Raw.sdlGetPrimaryDisplay
sdlGetDisplayName = liftIO . Raw.sdlGetDisplayName
sdlGetDisplayBounds = liftIO . Raw.sdlGetDisplayBounds
sdlGLLoadLibrary = liftIO . Raw.sdlGLLoadLibrary
sdlGLGetProcAddress = liftIO . Raw.sdlGLGetProcAddress
sdlGLUnloadLibrary = liftIO Raw.sdlGLUnloadLibrary
sdlGLCreateContext = liftIO . Raw.sdlGLCreateContext
sdlGLMakeCurrent window context = liftIO $ Raw.sdlGLMakeCurrent window context
sdlGLSwapWindow = liftIO . Raw.sdlGLSwapWindow
sdlGLDestroyContext = liftIO . Raw.sdlGLDestroyContext
sdlGetSystemTheme = liftIO Raw.sdlGetSystemTheme
sdlGetDisplayProperties = liftIO . Raw.sdlGetDisplayProperties
sdlGetDisplayUsableBounds = liftIO . Raw.sdlGetDisplayUsableBounds
sdlGetNaturalDisplayOrientation = liftIO . Raw.sdlGetNaturalDisplayOrientation
sdlGetCurrentDisplayOrientation = liftIO . Raw.sdlGetCurrentDisplayOrientation
sdlGetDisplayContentScale = liftIO . Raw.sdlGetDisplayContentScale
sdlGetFullscreenDisplayModes = liftIO . Raw.sdlGetFullscreenDisplayModes
sdlGetClosestFullscreenDisplayMode did w h refreshRate includeHd = liftIO $ Raw.sdlGetClosestFullscreenDisplayMode did w h refreshRate includeHd
sdlGetDesktopDisplayMode = liftIO . Raw.sdlGetDesktopDisplayMode
sdlGetCurrentDisplayMode = liftIO . Raw.sdlGetCurrentDisplayMode
sdlGetDisplayForPoint = liftIO . Raw.sdlGetDisplayForPoint
sdlGetDisplayForRect = liftIO . Raw.sdlGetDisplayForRect
sdlGetDisplayForWindow = liftIO . Raw.sdlGetDisplayForWindow
sdlGetWindowPixelDensity = liftIO . Raw.sdlGetWindowPixelDensity
sdlGetWindowDisplayScale = liftIO . Raw.sdlGetWindowDisplayScale
sdlSetWindowFullscreenMode window displayMode = liftIO $ Raw.sdlSetWindowFullscreenMode window displayMode
sdlGetWindowFullscreenMode = liftIO . Raw.sdlGetWindowFullscreenMode
sdlGetWindowICCProfile = liftIO . Raw.sdlGetWindowICCProfile
sdlGetWindowPixelFormat = liftIO . Raw.sdlGetWindowPixelFormat
sdlGetWindows = liftIO Raw.sdlGetWindows
sdlCreatePopupWindow parent offsetX offsetY width height flags = liftIO $ Raw.sdlCreatePopupWindow parent offsetX offsetY width height flags
sdlCreateWindowWithProperties = liftIO . Raw.sdlCreateWindowWithProperties
sdlGetWindowID = liftIO . Raw.sdlGetWindowID
sdlGetWindowFromID = liftIO . Raw.sdlGetWindowFromID
sdlGetWindowParent = liftIO . Raw.sdlGetWindowParent
sdlGetWindowProperties = liftIO . Raw.sdlGetWindowProperties
sdlGetWindowFlagsEx = liftIO . Raw.sdlGetWindowFlagsEx
sdlSetWindowTitleEx window title = liftIO $ Raw.sdlSetWindowTitleEx window title
sdlGetWindowTitleEx = liftIO . Raw.sdlGetWindowTitleEx
sdlSetWindowIcon window surface = liftIO $ Raw.sdlSetWindowIcon window surface
sdlGetWindowSafeArea = liftIO . Raw.sdlGetWindowSafeArea
sdlSetWindowAspectRatio window minAspect maxAspect = liftIO $ Raw.sdlSetWindowAspectRatio window minAspect maxAspect
sdlGetWindowAspectRatio = liftIO . Raw.sdlGetWindowAspectRatio
sdlGetWindowBordersSize = liftIO . Raw.sdlGetWindowBordersSize
sdlSetWindowMinimumSize window w h = liftIO $ Raw.sdlSetWindowMinimumSize window w h
sdlGetWindowMinimumSize = liftIO . Raw.sdlGetWindowMinimumSize
sdlSetWindowMaximumSize window w h = liftIO $ Raw.sdlSetWindowMaximumSize window w h
sdlGetWindowMaximumSize = liftIO . Raw.sdlGetWindowMaximumSize
sdlSetWindowFullscreen window fullscreen = liftIO $ Raw.sdlSetWindowFullscreen window fullscreen
sdlSyncWindow = liftIO . Raw.sdlSyncWindow
