{-# LANGUAGE CPP #-}

module SDL3.Wrapped.System
  ( module SDL3.Raw.System
  , sdlIsTablet
  , sdlIsTV
  , sdlGetSandbox
#ifdef SDL_PLATFORM_WINDOWS
  , sdlSetWindowsMessageHook
#endif
#if defined(SDL_PLATFORM_WIN32) || defined(SDL_PLATFORM_WINGDK)
  , sdlGetDXGIOutputInfo
#endif
  , sdlSetX11EventHook
#ifdef SDL_PLATFORM_LINUX
  , sdlSetLinuxThreadPriority
  , sdlSetLinuxThreadPriorityAndPolicy
#endif
#ifdef SDL_PLATFORM_IOS
  , sdlSetiOSAnimationCallback
  , sdlSetiOSEventPump
#endif
#ifdef SDL_PLATFORM_ANDROID
  , sdlGetAndroidInternalStoragePath
  , sdlGetAndroidExternalStoragePath
  , sdlGetAndroidCachePath
  , sdlRequestAndroidPermission
  , sdlShowAndroidToast
#endif
  , sdlOnApplicationWillTerminate
  , sdlOnApplicationDidReceiveMemoryWarning
  , sdlOnApplicationWillEnterBackground
  , sdlOnApplicationDidEnterBackground
  , sdlOnApplicationWillEnterForeground
  , sdlOnApplicationDidEnterForeground
#ifdef SDL_PLATFORM_IOS
  , sdlOnApplicationDidChangeStatusBarOrientation
#endif
#ifdef SDL_PLATFORM_GDK
  , sdlGetGDKTaskQueue
  , sdlGetGDKDefaultUser
#endif
  ) where

#include <SDL3/SDL_system.h>

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import SDL3.Raw.System hiding
  ( sdlIsTablet
  , sdlIsTV
  , sdlGetSandbox
#ifdef SDL_PLATFORM_WINDOWS
  , sdlSetWindowsMessageHook
#endif
#if defined(SDL_PLATFORM_WIN32) || defined(SDL_PLATFORM_WINGDK)
  , sdlGetDXGIOutputInfo
#endif
  , sdlSetX11EventHook
#ifdef SDL_PLATFORM_LINUX
  , sdlSetLinuxThreadPriority
  , sdlSetLinuxThreadPriorityAndPolicy
#endif
#ifdef SDL_PLATFORM_IOS
  , sdlSetiOSAnimationCallback
  , sdlSetiOSEventPump
#endif
#ifdef SDL_PLATFORM_ANDROID
  , sdlGetAndroidInternalStoragePath
  , sdlGetAndroidExternalStoragePath
  , sdlGetAndroidCachePath
  , sdlRequestAndroidPermission
  , sdlShowAndroidToast
#endif
  , sdlOnApplicationWillTerminate
  , sdlOnApplicationDidReceiveMemoryWarning
  , sdlOnApplicationWillEnterBackground
  , sdlOnApplicationDidEnterBackground
  , sdlOnApplicationWillEnterForeground
  , sdlOnApplicationDidEnterForeground
#ifdef SDL_PLATFORM_IOS
  , sdlOnApplicationDidChangeStatusBarOrientation
#endif
#ifdef SDL_PLATFORM_GDK
  , sdlGetGDKTaskQueue
  , sdlGetGDKDefaultUser
#endif
  )
import qualified SDL3.Raw.System as Raw
#if defined(SDL_PLATFORM_WIN32) || defined(SDL_PLATFORM_WINGDK) || defined(SDL_PLATFORM_IOS)
import SDL3.Video
#endif

#ifdef SDL_PLATFORM_WINDOWS
windowsMessageHookRef :: IORef (Maybe (FunPtr Raw.SDLWindowsMessageHook))
windowsMessageHookRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE windowsMessageHookRef #-}
#endif

x11EventHookRef :: IORef (Maybe (FunPtr Raw.SDLX11EventHook))
x11EventHookRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE x11EventHookRef #-}

#ifdef SDL_PLATFORM_IOS
iosAnimationCallbackRefs :: IORef [(Ptr SDLWindow, FunPtr Raw.SDLiOSAnimationCallback)]
iosAnimationCallbackRefs = unsafePerformIO (newIORef [])
{-# NOINLINE iosAnimationCallbackRefs #-}
#endif

peekMaybeCString :: CString -> IO (Maybe String)
peekMaybeCString ptr
  | ptr == nullPtr = pure Nothing
  | otherwise = Just <$> peekCString ptr

replaceManagedCallback :: IORef (Maybe (FunPtr a)) -> FunPtr a -> IO ()
replaceManagedCallback ref funPtr = do
  previous <- atomicModifyIORef' ref (\current -> (Just funPtr, current))
  maybe (pure ()) freeHaskellFunPtr previous

sdlIsTablet :: MonadIO m => m Bool
sdlIsTablet = liftIO Raw.sdlIsTablet

sdlIsTV :: MonadIO m => m Bool
sdlIsTV = liftIO Raw.sdlIsTV

sdlGetSandbox :: MonadIO m => m SDLSandbox
sdlGetSandbox = liftIO Raw.sdlGetSandbox

#ifdef SDL_PLATFORM_WINDOWS
sdlSetWindowsMessageHook :: MonadIO m => SDLWindowsMessageHook -> Ptr () -> m ()
sdlSetWindowsMessageHook callback userdata =
  liftIO $ do
    funPtr <- Raw.wrapSDLWindowsMessageHook callback
    Raw.sdlSetWindowsMessageHook funPtr userdata
    replaceManagedCallback windowsMessageHookRef funPtr
#endif

#if defined(SDL_PLATFORM_WIN32) || defined(SDL_PLATFORM_WINGDK)
sdlGetDXGIOutputInfo :: MonadIO m => SDLDisplayID -> m (Maybe (Int, Int))
sdlGetDXGIOutputInfo = liftIO . Raw.sdlGetDXGIOutputInfo
#endif

sdlSetX11EventHook :: MonadIO m => SDLX11EventHook -> Ptr () -> m ()
sdlSetX11EventHook callback userdata =
  liftIO $ do
    funPtr <- Raw.wrapSDLX11EventHook callback
    Raw.sdlSetX11EventHook funPtr userdata
    replaceManagedCallback x11EventHookRef funPtr

#ifdef SDL_PLATFORM_LINUX
sdlSetLinuxThreadPriority :: MonadIO m => Int64 -> Int -> m Bool
sdlSetLinuxThreadPriority threadId priority =
  liftIO $ Raw.sdlSetLinuxThreadPriority threadId priority

sdlSetLinuxThreadPriorityAndPolicy :: MonadIO m => Int64 -> Int -> Int -> m Bool
sdlSetLinuxThreadPriorityAndPolicy threadId priority policy =
  liftIO $ Raw.sdlSetLinuxThreadPriorityAndPolicy threadId priority policy
#endif

#ifdef SDL_PLATFORM_IOS
sdlSetiOSAnimationCallback :: MonadIO m => SDLWindow -> Int -> SDLiOSAnimationCallback -> Ptr () -> m Bool
sdlSetiOSAnimationCallback window interval callback callbackParam =
  liftIO $ do
    funPtr <- Raw.wrapSDLiOSAnimationCallback callback
    success <- Raw.sdlSetiOSAnimationCallback window interval funPtr callbackParam
    if success
      then do
        let windowPtr = case window of SDLWindow ptr -> ptr
        previous <- atomicModifyIORef' iosAnimationCallbackRefs $ \pairs ->
          let (existing, remaining) = partitionWindow windowPtr pairs
          in ((windowPtr, funPtr) : remaining, existing)
        mapM_ (freeHaskellFunPtr . snd) previous
        pure True
      else do
        freeHaskellFunPtr funPtr
        pure False
  where
    partitionWindow ptr = foldr (\pair@(currentPtr, _) (matches, rest) ->
      if currentPtr == ptr
        then (pair : matches, rest)
        else (matches, pair : rest)) ([], [])

sdlSetiOSEventPump :: MonadIO m => Bool -> m ()
sdlSetiOSEventPump = liftIO . Raw.sdlSetiOSEventPump
#endif

#ifdef SDL_PLATFORM_ANDROID
sdlGetAndroidInternalStoragePath :: MonadIO m => m (Maybe String)
sdlGetAndroidInternalStoragePath = liftIO $ Raw.sdlGetAndroidInternalStoragePath >>= peekMaybeCString

sdlGetAndroidExternalStoragePath :: MonadIO m => m (Maybe String)
sdlGetAndroidExternalStoragePath = liftIO $ Raw.sdlGetAndroidExternalStoragePath >>= peekMaybeCString

sdlGetAndroidCachePath :: MonadIO m => m (Maybe String)
sdlGetAndroidCachePath = liftIO $ Raw.sdlGetAndroidCachePath >>= peekMaybeCString

sdlRequestAndroidPermission
  :: MonadIO m
  => String
  -> (Ptr () -> String -> Bool -> IO ())
  -> Ptr ()
  -> m Bool
sdlRequestAndroidPermission permission callback userdata =
  liftIO $
    withCString permission $ \permissionPtr -> do
      callbackRef <- newIORef Nothing
      let managedCallback ud permissionCString granted = do
            permissionStr <- peekCString permissionCString
            callback ud permissionStr granted
            current <- atomicModifyIORef' callbackRef (\value -> (Nothing, value))
            maybe (pure ()) freeHaskellFunPtr current
      funPtr <- Raw.wrapSDLRequestAndroidPermissionCallback managedCallback
      previous <- atomicModifyIORef' callbackRef (\_ -> (Just funPtr, Nothing))
      maybe (pure ()) freeHaskellFunPtr previous
      success <- Raw.sdlRequestAndroidPermission permissionPtr funPtr userdata
      if success
        then pure True
        else do
          current <- atomicModifyIORef' callbackRef (\value -> (Nothing, value))
          maybe (pure ()) freeHaskellFunPtr current
          pure False

sdlShowAndroidToast :: MonadIO m => String -> Int -> Int -> Int -> Int -> m Bool
sdlShowAndroidToast message duration gravity xoffset yoffset =
  liftIO $
    withCString message $ \messagePtr ->
      Raw.sdlShowAndroidToast messagePtr duration gravity xoffset yoffset
#endif

sdlOnApplicationWillTerminate :: MonadIO m => m ()
sdlOnApplicationWillTerminate = liftIO Raw.sdlOnApplicationWillTerminate

sdlOnApplicationDidReceiveMemoryWarning :: MonadIO m => m ()
sdlOnApplicationDidReceiveMemoryWarning = liftIO Raw.sdlOnApplicationDidReceiveMemoryWarning

sdlOnApplicationWillEnterBackground :: MonadIO m => m ()
sdlOnApplicationWillEnterBackground = liftIO Raw.sdlOnApplicationWillEnterBackground

sdlOnApplicationDidEnterBackground :: MonadIO m => m ()
sdlOnApplicationDidEnterBackground = liftIO Raw.sdlOnApplicationDidEnterBackground

sdlOnApplicationWillEnterForeground :: MonadIO m => m ()
sdlOnApplicationWillEnterForeground = liftIO Raw.sdlOnApplicationWillEnterForeground

sdlOnApplicationDidEnterForeground :: MonadIO m => m ()
sdlOnApplicationDidEnterForeground = liftIO Raw.sdlOnApplicationDidEnterForeground

#ifdef SDL_PLATFORM_IOS
sdlOnApplicationDidChangeStatusBarOrientation :: MonadIO m => m ()
sdlOnApplicationDidChangeStatusBarOrientation = liftIO Raw.sdlOnApplicationDidChangeStatusBarOrientation
#endif

#ifdef SDL_PLATFORM_GDK
sdlGetGDKTaskQueue :: MonadIO m => m (Maybe XTaskQueueHandle)
sdlGetGDKTaskQueue = liftIO Raw.sdlGetGDKTaskQueue

sdlGetGDKDefaultUser :: MonadIO m => m (Maybe XUserHandle)
sdlGetGDKDefaultUser = liftIO Raw.sdlGetGDKDefaultUser
#endif
