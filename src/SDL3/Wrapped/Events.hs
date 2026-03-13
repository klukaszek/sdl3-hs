{-# LANGUAGE PatternSynonyms #-}

module SDL3.Wrapped.Events
  ( module SDL3.Raw.Events
  , SDLEventFilterHandle(..)
  , SDLEventWatchHandle(..)
  , sdlPumpEvents
  , sdlPeepEvents
  , sdlHasEvent
  , sdlHasEvents
  , sdlFlushEvent
  , sdlFlushEvents
  , sdlPollEvent
  , sdlWaitEvent
  , sdlWaitEventTimeout
  , sdlPushEvent
  , sdlSetEventFilter
  , sdlGetEventFilter
  , sdlAddEventWatch
  , sdlRemoveEventWatch
  , sdlFilterEvents
  , sdlSetEventEnabled
  , sdlEventEnabled
  , sdlRegisterEvents
  , sdlGetWindowFromEvent
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Word (Word32)
import Foreign.C.Types (CInt)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import System.IO.Unsafe (unsafePerformIO)
import SDL3.Video (SDLWindow)
import SDL3.Raw.Events hiding
  ( mkEventFilter
  , sdlAddEventWatch
  , sdlEventEnabled
  , sdlFilterEvents
  , sdlFlushEvent
  , sdlFlushEvents
  , sdlGetEventFilter
  , sdlGetWindowFromEvent
  , sdlHasEvent
  , sdlHasEvents
  , sdlPeepEvents
  , sdlPollEvent
  , sdlPumpEvents
  , sdlPushEvent
  , sdlRegisterEvents
  , sdlRemoveEventWatch
  , sdlSetEventEnabled
  , sdlSetEventFilter
  , sdlWaitEvent
  , sdlWaitEventTimeout
  , wrapEventFilter
  )
import qualified SDL3.Raw.Events as Raw

data SDLEventFilterHandle = SDLEventFilterHandle
  { eventFilterPtr :: FunPtr Raw.SDLEventFilter
  , eventFilterUserdata :: Ptr ()
  }

data SDLEventWatchHandle = SDLEventWatchHandle
  { eventWatchPtr :: FunPtr Raw.SDLEventFilter
  , eventWatchUserdata :: Ptr ()
  }

eventFilterRef :: IORef (Maybe (FunPtr Raw.SDLEventFilter))
eventFilterRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE eventFilterRef #-}

releaseFilterPtr :: Maybe (FunPtr Raw.SDLEventFilter) -> IO ()
releaseFilterPtr Nothing = pure ()
releaseFilterPtr (Just filterPtr) = freeHaskellFunPtr filterPtr

sdlPumpEvents :: MonadIO m => m ()
sdlPumpEvents = liftIO Raw.sdlPumpEvents

sdlPeepEvents :: MonadIO m => [SDLEvent] -> Int -> SDLEventAction -> Word32 -> Word32 -> m Int
sdlPeepEvents events num action minType maxType =
  liftIO $ Raw.sdlPeepEvents events num action minType maxType

sdlHasEvent :: MonadIO m => Word32 -> m Bool
sdlHasEvent = liftIO . Raw.sdlHasEvent

sdlHasEvents :: MonadIO m => Word32 -> Word32 -> m Bool
sdlHasEvents minType maxType = liftIO $ Raw.sdlHasEvents minType maxType

sdlFlushEvent :: MonadIO m => Word32 -> m ()
sdlFlushEvent = liftIO . Raw.sdlFlushEvent

sdlFlushEvents :: MonadIO m => Word32 -> Word32 -> m ()
sdlFlushEvents minType maxType = liftIO $ Raw.sdlFlushEvents minType maxType

sdlPollEvent :: MonadIO m => m (Maybe SDLEvent)
sdlPollEvent = liftIO Raw.sdlPollEvent

sdlWaitEvent :: MonadIO m => m (Maybe SDLEvent)
sdlWaitEvent = liftIO Raw.sdlWaitEvent

sdlWaitEventTimeout :: MonadIO m => Int -> m (Maybe SDLEvent)
sdlWaitEventTimeout = liftIO . Raw.sdlWaitEventTimeout

sdlPushEvent :: MonadIO m => SDLEvent -> m Bool
sdlPushEvent = liftIO . Raw.sdlPushEvent

sdlSetEventFilter :: MonadIO m => Maybe (SDLEventFilter, Ptr ()) -> m (Maybe SDLEventFilterHandle)
sdlSetEventFilter Nothing =
  liftIO $ do
    Raw.sdlSetEventFilter nullFunPtr nullPtr
    previous <- atomicModifyIORef' eventFilterRef (\current -> (Nothing, current))
    releaseFilterPtr previous
    pure Nothing
sdlSetEventFilter (Just (filt, userdata)) =
  liftIO $ do
    filterPtr <- Raw.wrapEventFilter filt
    Raw.sdlSetEventFilter filterPtr userdata
    previous <- atomicModifyIORef' eventFilterRef (\current -> (Just filterPtr, current))
    releaseFilterPtr previous
    pure $ Just (SDLEventFilterHandle filterPtr userdata)

sdlGetEventFilter :: MonadIO m => m (Maybe SDLEventFilterHandle)
sdlGetEventFilter =
  liftIO $ fmap (\(filterPtr, userdata) -> SDLEventFilterHandle filterPtr userdata) <$> Raw.sdlGetEventFilter

sdlAddEventWatch :: MonadIO m => SDLEventFilter -> Ptr () -> m (Maybe SDLEventWatchHandle)
sdlAddEventWatch filt userdata =
  liftIO $ do
    filterPtr <- Raw.wrapEventFilter filt
    success <- Raw.sdlAddEventWatch filterPtr userdata
    if success
      then pure $ Just (SDLEventWatchHandle filterPtr userdata)
      else do
        freeHaskellFunPtr filterPtr
        pure Nothing

sdlRemoveEventWatch :: MonadIO m => SDLEventWatchHandle -> m ()
sdlRemoveEventWatch (SDLEventWatchHandle filterPtr userdata) =
  liftIO $ do
    Raw.sdlRemoveEventWatch filterPtr userdata
    freeHaskellFunPtr filterPtr

sdlFilterEvents :: MonadIO m => SDLEventFilter -> Ptr () -> m ()
sdlFilterEvents filt userdata =
  liftIO $ do
    filterPtr <- Raw.wrapEventFilter filt
    Raw.sdlFilterEvents filterPtr userdata
    freeHaskellFunPtr filterPtr

sdlSetEventEnabled :: MonadIO m => Word32 -> Bool -> m ()
sdlSetEventEnabled eventType enabled = liftIO $ Raw.sdlSetEventEnabled eventType enabled

sdlEventEnabled :: MonadIO m => Word32 -> m Bool
sdlEventEnabled = liftIO . Raw.sdlEventEnabled

sdlRegisterEvents :: MonadIO m => CInt -> m Word32
sdlRegisterEvents = liftIO . Raw.sdlRegisterEvents

sdlGetWindowFromEvent :: MonadIO m => SDLEvent -> m (Maybe SDLWindow)
sdlGetWindowFromEvent = liftIO . Raw.sdlGetWindowFromEvent
