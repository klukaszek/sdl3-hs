{-# LANGUAGE CPP #-}

{-|
Module      : SDL.System
Description : Platform-specific SDL API functions
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

SDL provides platform-specific API functions that deal with needs unique to specific operating systems,
which don't fit as generic, platform-independent APIs.

This module offers functions tailored to various platforms (e.g., Windows, Linux, iOS, Android), such as
handling system events, querying device properties, or integrating with platform-specific features.
While most applications can operate without these, they are useful for adding platform-specific polish,
integrating with system components, or addressing platform-unique challenges.
-}

module SDL3.Raw.System
  ( -- * General Device Queries
    sdlIsTablet
  , sdlIsTV
  , SDLSandbox(..)
  , sdlGetSandbox

    -- * Windows-Specific Functions
#ifdef SDL_PLATFORM_WINDOWS
  , SDLWindowsMessageHook
  , wrapSDLWindowsMessageHook
  , sdlSetWindowsMessageHook
#endif
#if defined(SDL_PLATFORM_WIN32) || defined(SDL_PLATFORM_WINGDK)
  , sdlGetDirect3D9AdapterIndex
  , sdlGetDXGIOutputInfo
#endif

    -- * UNIX-Specific Functions
  , SDLX11EventHook
  , wrapSDLX11EventHook
  , sdlSetX11EventHook

    -- * Linux-Specific Functions
#ifdef SDL_PLATFORM_LINUX
  , sdlSetLinuxThreadPriority
  , sdlSetLinuxThreadPriorityAndPolicy
#endif

    -- * iOS-Specific Functions
#ifdef SDL_PLATFORM_IOS
  , SDLiOSAnimationCallback
  , wrapSDLiOSAnimationCallback
  , sdlSetiOSAnimationCallback
  , sdlSetiOSEventPump
#endif

    -- * Android-Specific Functions
#ifdef SDL_PLATFORM_ANDROID
  , sdlGetAndroidJNIEnv
  , sdlGetAndroidActivity
  , sdlGetAndroidSDKVersion
  , sdlIsChromebook
  , sdlIsDeXMode
  , sdlSendAndroidBackButton
  , sdlAndroidExternalStorageRead
  , sdlAndroidExternalStorageWrite
  , sdlGetAndroidInternalStoragePath
  , sdlGetAndroidExternalStorageState
  , sdlGetAndroidExternalStoragePath
  , sdlGetAndroidCachePath
  , SDLRequestAndroidPermissionCallback
  , wrapSDLRequestAndroidPermissionCallback
  , sdlRequestAndroidPermission
  , sdlShowAndroidToast
  , sdlSendAndroidMessage
#endif

    -- * iOS App Delegate Functions
  , sdlOnApplicationWillTerminate
  , sdlOnApplicationDidReceiveMemoryWarning
  , sdlOnApplicationWillEnterBackground
  , sdlOnApplicationDidEnterBackground
  , sdlOnApplicationWillEnterForeground
  , sdlOnApplicationDidEnterForeground
#ifdef SDL_PLATFORM_IOS
  , sdlOnApplicationDidChangeStatusBarOrientation
#endif

    -- * GDK-Specific Functions
#ifdef SDL_PLATFORM_GDK
  , XTaskQueueHandle
  , XUserHandle
  , sdlGetGDKTaskQueue
  , sdlGetGDKDefaultUser
#endif
  ) where

#include <SDL3/SDL_system.h>

import Foreign
import Foreign.C.Types

#if defined(SDL_PLATFORM_WIN32) || defined(SDL_PLATFORM_WINGDK)
import SDL3.Raw.Video (SDLDisplayID)
#endif

-- | Query if the current device is a tablet (SDL_IsTablet).
foreign import ccall "SDL_IsTablet"
  sdlIsTablet :: IO Bool

-- | Query if the current device is a TV (SDL_IsTV).
foreign import ccall "SDL_IsTV"
  sdlIsTV :: IO Bool

-- | Application sandbox environment (SDL_Sandbox).
-- Starts at 0 in C Header so this is fine.
data SDLSandbox
  = SDLSandboxNone             -- ^ SDL_SANDBOX_NONE
  | SDLSandboxUnknownContainer -- ^ SDL_SANDBOX_UNKNOWN_CONTAINER
  | SDLSandboxFlatpak          -- ^ SDL_SANDBOX_FLATPAK
  | SDLSandboxSnap             -- ^ SDL_SANDBOX_SNAP
  | SDLSandboxMacOS            -- ^ SDL_SANDBOX_MACOS
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Get the application sandbox environment (SDL_GetSandbox).
foreign import ccall "SDL_GetSandbox"
  sdlGetSandboxRaw :: IO CInt

-- | Haskell wrapper for SDL_GetSandbox.
sdlGetSandbox :: IO SDLSandbox
sdlGetSandbox = toEnum . fromIntegral <$> sdlGetSandboxRaw

-- Windows-Specific Functions
#ifdef SDL_PLATFORM_WINDOWS

-- | A callback for Windows message hooks (SDL_WindowsMessageHook).
type SDLWindowsMessageHook = Ptr () -> Ptr () -> IO Bool  -- MSG is opaque, treated as Ptr ()

-- | Foreign import for SDL_SetWindowsMessageHook with dynamic wrapper.
foreign import ccall "wrapper"
  wrapSDLWindowsMessageHook :: SDLWindowsMessageHook -> IO (FunPtr SDLWindowsMessageHook)

-- | Set a callback for every Windows message (SDL_SetWindowsMessageHook).
foreign import ccall "SDL_SetWindowsMessageHook"
  sdlSetWindowsMessageHookRaw :: FunPtr SDLWindowsMessageHook -> Ptr () -> IO ()

sdlSetWindowsMessageHook :: FunPtr SDLWindowsMessageHook -> Ptr () -> IO ()
sdlSetWindowsMessageHook = sdlSetWindowsMessageHookRaw

#endif

#if defined(SDL_PLATFORM_WIN32) || defined(SDL_PLATFORM_WINGDK)

-- | Get the D3D9 adapter index for a display (SDL_GetDirect3D9AdapterIndex).
foreign import ccall "SDL_GetDirect3D9AdapterIndex"
  sdlGetDirect3D9AdapterIndex :: SDLDisplayID -> IO Int

-- | Get the DXGI Adapter and Output indices (SDL_GetDXGIOutputInfo).
foreign import ccall "SDL_GetDXGIOutputInfo"
  sdlGetDXGIOutputInfoRaw :: SDLDisplayID -> Ptr CInt -> Ptr CInt -> IO Bool

-- | Haskell wrapper for SDL_GetDXGIOutputInfo.
sdlGetDXGIOutputInfo :: SDLDisplayID -> IO (Maybe (Int, Int))
sdlGetDXGIOutputInfo displayID = alloca $ \adapterPtr -> alloca $ \outputPtr -> do
  success <- sdlGetDXGIOutputInfoRaw displayID adapterPtr outputPtr
  if success
    then do
      adapter <- peek (adapterPtr :: Ptr CInt)
      output <- peek (outputPtr :: Ptr CInt)
      return $ Just (fromIntegral adapter, fromIntegral output)
    else return Nothing

#endif

-- UNIX-Specific Functions

-- | A callback for X11 event hooks (SDL_X11EventHook).
type SDLX11EventHook = Ptr () -> Ptr () -> IO Bool  -- XEvent is opaque, treated as Ptr ()

-- | Foreign import for SDL_SetX11EventHook with dynamic wrapper.
foreign import ccall "wrapper"
  wrapSDLX11EventHook :: SDLX11EventHook -> IO (FunPtr SDLX11EventHook)

-- | Set a callback for every X11 event (SDL_SetX11EventHook).
foreign import ccall "SDL_SetX11EventHook"
  sdlSetX11EventHookRaw :: FunPtr SDLX11EventHook -> Ptr () -> IO ()

sdlSetX11EventHook :: FunPtr SDLX11EventHook -> Ptr () -> IO ()
sdlSetX11EventHook = sdlSetX11EventHookRaw

-- Linux-Specific Functions
#ifdef SDL_PLATFORM_LINUX

-- | Sets the UNIX nice value for a thread (SDL_SetLinuxThreadPriority).
foreign import ccall "SDL_SetLinuxThreadPriority"
  sdlSetLinuxThreadPriority :: Int64 -> Int -> IO Bool

-- | Sets the priority and scheduling policy for a thread (SDL_SetLinuxThreadPriorityAndPolicy).
foreign import ccall "SDL_SetLinuxThreadPriorityAndPolicy"
  sdlSetLinuxThreadPriorityAndPolicy :: Int64 -> Int -> Int -> IO Bool

#endif

-- iOS-Specific Functions
#ifdef SDL_PLATFORM_IOS

-- | Animation callback for iOS (SDL_iOSAnimationCallback).
type SDLiOSAnimationCallback = Ptr () -> IO ()

-- | Foreign import for SDL_SetiOSAnimationCallback with dynamic wrapper.
foreign import ccall "wrapper"
  wrapSDLiOSAnimationCallback :: SDLiOSAnimationCallback -> IO (FunPtr SDLiOSAnimationCallback)

-- | Set the animation callback on Apple iOS (SDL_SetiOSAnimationCallback).
foreign import ccall "SDL_SetiOSAnimationCallback"
  sdlSetiOSAnimationCallbackRaw :: SDLWindow -> Int -> FunPtr SDLiOSAnimationCallback -> Ptr () -> IO Bool

sdlSetiOSAnimationCallback :: SDLWindow -> Int -> FunPtr SDLiOSAnimationCallback -> Ptr () -> IO Bool
sdlSetiOSAnimationCallback = sdlSetiOSAnimationCallbackRaw

-- | Enable or disable the SDL event pump on Apple iOS (SDL_SetiOSEventPump).
foreign import ccall "SDL_SetiOSEventPump"
  sdlSetiOSEventPump :: Bool -> IO ()

#endif

-- Android-Specific Functions
#ifdef SDL_PLATFORM_ANDROID

-- | Get the Android JNI Environment (SDL_GetAndroidJNIEnv).
foreign import ccall "SDL_GetAndroidJNIEnv"
  sdlGetAndroidJNIEnv :: IO (Ptr ())  -- JNIEnv is opaque

-- | Retrieve the Java instance of the Android activity (SDL_GetAndroidActivity).
foreign import ccall "SDL_GetAndroidActivity"
  sdlGetAndroidActivity :: IO (Ptr ())  -- jobject is opaque

-- | Query Android API level (SDL_GetAndroidSDKVersion).
foreign import ccall "SDL_GetAndroidSDKVersion"
  sdlGetAndroidSDKVersion :: IO Int

-- | Query if the application is running on a Chromebook (SDL_IsChromebook).
foreign import ccall "SDL_IsChromebook"
  sdlIsChromebook :: IO Bool

-- | Query if the application is running on a Samsung DeX docking station (SDL_IsDeXMode).
foreign import ccall "SDL_IsDeXMode"
  sdlIsDeXMode :: IO Bool

-- | Trigger the Android system back button behavior (SDL_SendAndroidBackButton).
foreign import ccall "SDL_SendAndroidBackButton"
  sdlSendAndroidBackButton :: IO ()

-- | Android external storage read permission (SDL_ANDROID_EXTERNAL_STORAGE_READ).
sdlAndroidExternalStorageRead :: Word32
sdlAndroidExternalStorageRead = #const SDL_ANDROID_EXTERNAL_STORAGE_READ

-- | Android external storage write permission (SDL_ANDROID_EXTERNAL_STORAGE_WRITE).
sdlAndroidExternalStorageWrite :: Word32
sdlAndroidExternalStorageWrite = #const SDL_ANDROID_EXTERNAL_STORAGE_WRITE

-- | Get the path for internal storage (SDL_GetAndroidInternalStoragePath).
foreign import ccall "SDL_GetAndroidInternalStoragePath"
  sdlGetAndroidInternalStoragePathRaw :: IO CString

sdlGetAndroidInternalStoragePath :: IO CString
sdlGetAndroidInternalStoragePath = sdlGetAndroidInternalStoragePathRaw

-- | Get the current state of external storage (SDL_GetAndroidExternalStorageState).
foreign import ccall "SDL_GetAndroidExternalStorageState"
  sdlGetAndroidExternalStorageState :: IO Word32

-- | Get the path for external storage (SDL_GetAndroidExternalStoragePath).
foreign import ccall "SDL_GetAndroidExternalStoragePath"
  sdlGetAndroidExternalStoragePathRaw :: IO CString

sdlGetAndroidExternalStoragePath :: IO CString
sdlGetAndroidExternalStoragePath = sdlGetAndroidExternalStoragePathRaw

-- | Get the path for caching data (SDL_GetAndroidCachePath).
foreign import ccall "SDL_GetAndroidCachePath"
  sdlGetAndroidCachePathRaw :: IO CString

sdlGetAndroidCachePath :: IO CString
sdlGetAndroidCachePath = sdlGetAndroidCachePathRaw

-- | Callback for Android permission requests (SDL_RequestAndroidPermissionCallback).
type SDLRequestAndroidPermissionCallback = Ptr () -> CString -> Bool -> IO ()

-- | Foreign import for SDL_RequestAndroidPermission with dynamic wrapper.
foreign import ccall "wrapper"
  wrapSDLRequestAndroidPermissionCallback :: SDLRequestAndroidPermissionCallback -> IO (FunPtr SDLRequestAndroidPermissionCallback)

-- | Request Android permissions asynchronously (SDL_RequestAndroidPermission).
foreign import ccall "SDL_RequestAndroidPermission"
  sdlRequestAndroidPermissionRaw :: CString -> FunPtr SDLRequestAndroidPermissionCallback -> Ptr () -> IO Bool

sdlRequestAndroidPermission :: CString -> FunPtr SDLRequestAndroidPermissionCallback -> Ptr () -> IO Bool
sdlRequestAndroidPermission = sdlRequestAndroidPermissionRaw

-- | Show an Android toast notification (SDL_ShowAndroidToast).
foreign import ccall "SDL_ShowAndroidToast"
  sdlShowAndroidToastRaw :: CString -> Int -> Int -> Int -> Int -> IO Bool

sdlShowAndroidToast :: CString -> Int -> Int -> Int -> Int -> IO Bool
sdlShowAndroidToast = sdlShowAndroidToastRaw

-- | Send a user command to SDLActivity (SDL_SendAndroidMessage).
foreign import ccall "SDL_SendAndroidMessage"
  sdlSendAndroidMessage :: Word32 -> Int -> IO Bool

#endif

-- iOS App Delegate Functions

-- | Report onApplicationWillTerminate (SDL_OnApplicationWillTerminate).
foreign import ccall "SDL_OnApplicationWillTerminate"
  sdlOnApplicationWillTerminate :: IO ()

-- | Report onApplicationDidReceiveMemoryWarning (SDL_OnApplicationDidReceiveMemoryWarning).
foreign import ccall "SDL_OnApplicationDidReceiveMemoryWarning"
  sdlOnApplicationDidReceiveMemoryWarning :: IO ()

-- | Report onApplicationWillEnterBackground (SDL_OnApplicationWillEnterBackground).
foreign import ccall "SDL_OnApplicationWillEnterBackground"
  sdlOnApplicationWillEnterBackground :: IO ()

-- | Report onApplicationDidEnterBackground (SDL_OnApplicationDidEnterBackground).
foreign import ccall "SDL_OnApplicationDidEnterBackground"
  sdlOnApplicationDidEnterBackground :: IO ()

-- | Report onApplicationWillEnterForeground (SDL_OnApplicationWillEnterForeground).
foreign import ccall "SDL_OnApplicationWillEnterForeground"
  sdlOnApplicationWillEnterForeground :: IO ()

-- | Report onApplicationDidEnterForeground (SDL_OnApplicationDidEnterForeground).
foreign import ccall "SDL_OnApplicationDidEnterForeground"
  sdlOnApplicationDidEnterForeground :: IO ()

#ifdef SDL_PLATFORM_IOS

-- | Report onApplicationDidChangeStatusBarOrientation (SDL_OnApplicationDidChangeStatusBarOrientation).
foreign import ccall "SDL_OnApplicationDidChangeStatusBarOrientation"
  sdlOnApplicationDidChangeStatusBarOrientation :: IO ()

#endif

-- GDK-Specific Functions
#ifdef SDL_PLATFORM_GDK

-- | Opaque type for XTaskQueueHandle.
type XTaskQueueHandle = Ptr ()

-- | Opaque type for XUserHandle.
type XUserHandle = Ptr ()

-- | Get a reference to the GDK task queue (SDL_GetGDKTaskQueue).
foreign import ccall "SDL_GetGDKTaskQueue"
  sdlGetGDKTaskQueueRaw :: Ptr (Ptr ()) -> IO Bool

-- | Haskell wrapper for SDL_GetGDKTaskQueue.
sdlGetGDKTaskQueue :: IO (Maybe XTaskQueueHandle)
sdlGetGDKTaskQueue = alloca $ \ptr -> do
  success <- sdlGetGDKTaskQueueRaw ptr
  if success
    then Just <$> peek ptr
    else return Nothing

-- | Get the default GDK user handle (SDL_GetGDKDefaultUser).
foreign import ccall "SDL_GetGDKDefaultUser"
  sdlGetGDKDefaultUserRaw :: Ptr (Ptr ()) -> IO Bool

-- | Haskell wrapper for SDL_GetGDKDefaultUser.
sdlGetGDKDefaultUser :: IO (Maybe XUserHandle)
sdlGetGDKDefaultUser = alloca $ \ptr -> do
  success <- sdlGetGDKDefaultUserRaw ptr
  if success
    then Just <$> peek ptr
    else return Nothing

#endif
