{-# LANGUAGE PatternSynonyms #-}

module SDL3.Wrapped.Thread
  ( SDLThread(..)
  , SDLThreadID
  , SDLThreadFunction
  , SDLThreadPriority(..)
  , pattern SDL_THREAD_PRIORITY_LOW
  , pattern SDL_THREAD_PRIORITY_NORMAL
  , pattern SDL_THREAD_PRIORITY_HIGH
  , pattern SDL_THREAD_PRIORITY_TIME_CRITICAL
  , SDLThreadState(..)
  , pattern SDL_THREAD_UNKNOWN
  , pattern SDL_THREAD_ALIVE
  , pattern SDL_THREAD_DETACHED
  , pattern SDL_THREAD_COMPLETE
  , SDLTLSID
  , SDLTLSDestructorCallback
  , pattern SDL_PROP_THREAD_CREATE_ENTRY_FUNCTION
  , pattern SDL_PROP_THREAD_CREATE_NAME
  , pattern SDL_PROP_THREAD_CREATE_USERDATA
  , pattern SDL_PROP_THREAD_CREATE_STACKSIZE
  , sdlCreateThread
  , sdlCreateThreadWithProperties
  , sdlGetThreadName
  , sdlGetCurrentThreadID
  , sdlGetThreadID
  , sdlSetCurrentThreadPriority
  , sdlWaitThread
  , sdlGetThreadState
  , sdlDetachThread
  , sdlGetTLS
  , sdlSetTLS
  , sdlCleanupTLS
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.String (peekCString, withCString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Utils (toBool)
import Foreign.Ptr
import SDL3.Raw.Atomic (SDLAtomicInt(..))
import SDL3.Raw.Properties (SDLPropertiesID)
import SDL3.Raw.Thread
  ( SDLTLSDestructorCallback
  , SDLTLSID
  , SDLThread(..)
  , SDLThreadFunction
  , SDLThreadID
  , SDLThreadPriority(..)
  , SDLThreadState(..)
  , pattern SDL_PROP_THREAD_CREATE_ENTRY_FUNCTION
  , pattern SDL_PROP_THREAD_CREATE_NAME
  , pattern SDL_PROP_THREAD_CREATE_STACKSIZE
  , pattern SDL_PROP_THREAD_CREATE_USERDATA
  , pattern SDL_THREAD_ALIVE
  , pattern SDL_THREAD_COMPLETE
  , pattern SDL_THREAD_DETACHED
  , pattern SDL_THREAD_PRIORITY_HIGH
  , pattern SDL_THREAD_PRIORITY_LOW
  , pattern SDL_THREAD_PRIORITY_NORMAL
  , pattern SDL_THREAD_PRIORITY_TIME_CRITICAL
  , pattern SDL_THREAD_UNKNOWN
  )
import qualified SDL3.Raw.Thread as Raw

sdlCreateThread :: MonadIO m => SDLThreadFunction -> String -> Ptr () -> m (Maybe SDLThread)
sdlCreateThread fn name userData = liftIO $
  withCString name $ \cname -> do
    ptr <- Raw.sdlCreateThreadRaw fn cname userData
    pure $ if ptr == nullPtr then Nothing else Just (SDLThread ptr)

sdlCreateThreadWithProperties :: MonadIO m => SDLPropertiesID -> m (Maybe SDLThread)
sdlCreateThreadWithProperties props = liftIO $ do
  ptr <- Raw.sdlCreateThreadWithPropertiesRaw props
  pure $ if ptr == nullPtr then Nothing else Just (SDLThread ptr)

sdlGetThreadName :: MonadIO m => SDLThread -> m (Maybe String)
sdlGetThreadName (SDLThread threadPtr) = liftIO $ do
  cStr <- Raw.sdlGetThreadNameRaw threadPtr
  if cStr == nullPtr
    then return Nothing
    else Just <$> peekCString cStr

sdlGetCurrentThreadID :: MonadIO m => m SDLThreadID
sdlGetCurrentThreadID = liftIO Raw.sdlGetCurrentThreadIDRaw

sdlGetThreadID :: MonadIO m => SDLThread -> m SDLThreadID
sdlGetThreadID (SDLThread threadPtr) = liftIO $ Raw.sdlGetThreadIDRaw threadPtr

sdlSetCurrentThreadPriority :: MonadIO m => SDLThreadPriority -> m Bool
sdlSetCurrentThreadPriority priority =
  liftIO $ toBool <$> Raw.sdlSetCurrentThreadPriorityRaw (fromIntegral $ fromEnum priority)

sdlWaitThread :: MonadIO m => SDLThread -> Maybe (Ptr CInt) -> m ()
sdlWaitThread (SDLThread threadPtr) mStatusPtr =
  liftIO $ Raw.sdlWaitThreadRaw threadPtr (maybe nullPtr id mStatusPtr)

sdlGetThreadState :: MonadIO m => SDLThread -> m SDLThreadState
sdlGetThreadState (SDLThread threadPtr) =
  liftIO $ toEnum . fromIntegral <$> Raw.sdlGetThreadStateRaw threadPtr

sdlDetachThread :: MonadIO m => SDLThread -> m ()
sdlDetachThread (SDLThread threadPtr) = liftIO $ Raw.sdlDetachThreadRaw threadPtr

sdlGetTLS :: MonadIO m => SDLTLSID -> m (Ptr ())
sdlGetTLS (SDLAtomicInt tlsPtr) = liftIO $ Raw.sdlGetTLSRaw tlsPtr

sdlSetTLS :: MonadIO m => SDLTLSID -> Ptr () -> Maybe SDLTLSDestructorCallback -> m Bool
sdlSetTLS (SDLAtomicInt tlsPtr) value mDestructor =
  liftIO $ toBool <$> Raw.sdlSetTLSRaw tlsPtr value (maybe nullFunPtr id mDestructor)

sdlCleanupTLS :: MonadIO m => m ()
sdlCleanupTLS = liftIO Raw.sdlCleanupTLSRaw
