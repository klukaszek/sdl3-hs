module SDL3.Wrapped.Mutex
  ( SDLMutex
  , withMutexPtr
  , sdlUnsafeFromRawMutex
  , sdlUnsafeToRawMutex
  , sdlCreateMutex
  , sdlLockMutex
  , sdlTryLockMutex
  , sdlUnlockMutex
  , sdlDestroyMutex
  , SDLRWLock
  , withRWLockPtr
  , sdlUnsafeFromRawRWLock
  , sdlUnsafeToRawRWLock
  , sdlCreateRWLock
  , sdlLockRWLockForReading
  , sdlLockRWLockForWriting
  , sdlTryLockRWLockForReading
  , sdlTryLockRWLockForWriting
  , sdlUnlockRWLock
  , sdlDestroyRWLock
  , SDLSemaphore
  , withSemaphorePtr
  , sdlUnsafeFromRawSemaphore
  , sdlUnsafeToRawSemaphore
  , sdlCreateSemaphore
  , sdlDestroySemaphore
  , sdlWaitSemaphore
  , sdlTryWaitSemaphore
  , sdlWaitSemaphoreTimeout
  , sdlSignalSemaphore
  , sdlGetSemaphoreValue
  , SDLCondition
  , withConditionPtr
  , sdlUnsafeFromRawCondition
  , sdlUnsafeToRawCondition
  , sdlCreateCondition
  , sdlDestroyCondition
  , sdlSignalCondition
  , sdlBroadcastCondition
  , sdlWaitCondition
  , sdlWaitConditionTimeout
  , SDLInitStatus(..)
  , SDLInitState(..)
  , sdlShouldInit
  , sdlShouldQuit
  , sdlSetInitialized
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int
import Data.Word
import Foreign.Ptr (Ptr, nullPtr)
import SDL3.Raw.Mutex
  ( SDLInitState(..)
  , SDLInitStatus(..)
  )
import qualified SDL3.Raw.Mutex as Raw

newtype SDLMutex = SDLMutex (Ptr Raw.SDLMutex)
  deriving (Eq)

instance Show SDLMutex where
  show (SDLMutex mutexPtr) = "SDLMutex " ++ show mutexPtr

newtype SDLRWLock = SDLRWLock (Ptr Raw.SDLRWLock)
  deriving (Eq)

instance Show SDLRWLock where
  show (SDLRWLock rwLockPtr) = "SDLRWLock " ++ show rwLockPtr

newtype SDLSemaphore = SDLSemaphore (Ptr Raw.SDLSemaphore)
  deriving (Eq)

instance Show SDLSemaphore where
  show (SDLSemaphore semaphorePtr) = "SDLSemaphore " ++ show semaphorePtr

newtype SDLCondition = SDLCondition (Ptr Raw.SDLCondition)
  deriving (Eq)

instance Show SDLCondition where
  show (SDLCondition conditionPtr) = "SDLCondition " ++ show conditionPtr

sdlUnsafeFromRawMutex :: Ptr Raw.SDLMutex -> Maybe SDLMutex
sdlUnsafeFromRawMutex mutexPtr
  | mutexPtr == nullPtr = Nothing
  | otherwise = Just (SDLMutex mutexPtr)

sdlUnsafeToRawMutex :: SDLMutex -> Ptr Raw.SDLMutex
sdlUnsafeToRawMutex (SDLMutex mutexPtr) = mutexPtr

withMutexPtr :: MonadIO m => SDLMutex -> (Ptr Raw.SDLMutex -> IO a) -> m a
withMutexPtr mutex action = liftIO $ action (sdlUnsafeToRawMutex mutex)

sdlUnsafeFromRawRWLock :: Ptr Raw.SDLRWLock -> Maybe SDLRWLock
sdlUnsafeFromRawRWLock rwLockPtr
  | rwLockPtr == nullPtr = Nothing
  | otherwise = Just (SDLRWLock rwLockPtr)

sdlUnsafeToRawRWLock :: SDLRWLock -> Ptr Raw.SDLRWLock
sdlUnsafeToRawRWLock (SDLRWLock rwLockPtr) = rwLockPtr

withRWLockPtr :: MonadIO m => SDLRWLock -> (Ptr Raw.SDLRWLock -> IO a) -> m a
withRWLockPtr rwLock action = liftIO $ action (sdlUnsafeToRawRWLock rwLock)

sdlUnsafeFromRawSemaphore :: Ptr Raw.SDLSemaphore -> Maybe SDLSemaphore
sdlUnsafeFromRawSemaphore semaphorePtr
  | semaphorePtr == nullPtr = Nothing
  | otherwise = Just (SDLSemaphore semaphorePtr)

sdlUnsafeToRawSemaphore :: SDLSemaphore -> Ptr Raw.SDLSemaphore
sdlUnsafeToRawSemaphore (SDLSemaphore semaphorePtr) = semaphorePtr

withSemaphorePtr :: MonadIO m => SDLSemaphore -> (Ptr Raw.SDLSemaphore -> IO a) -> m a
withSemaphorePtr semaphore action = liftIO $ action (sdlUnsafeToRawSemaphore semaphore)

sdlUnsafeFromRawCondition :: Ptr Raw.SDLCondition -> Maybe SDLCondition
sdlUnsafeFromRawCondition conditionPtr
  | conditionPtr == nullPtr = Nothing
  | otherwise = Just (SDLCondition conditionPtr)

sdlUnsafeToRawCondition :: SDLCondition -> Ptr Raw.SDLCondition
sdlUnsafeToRawCondition (SDLCondition conditionPtr) = conditionPtr

withConditionPtr :: MonadIO m => SDLCondition -> (Ptr Raw.SDLCondition -> IO a) -> m a
withConditionPtr condition action = liftIO $ action (sdlUnsafeToRawCondition condition)

sdlCreateMutex :: MonadIO m => m (Maybe SDLMutex)
sdlCreateMutex = liftIO $ sdlUnsafeFromRawMutex <$> Raw.sdlCreateMutexRaw

sdlLockMutex :: MonadIO m => SDLMutex -> m ()
sdlLockMutex mutex = liftIO $ Raw.sdlLockMutexRaw (sdlUnsafeToRawMutex mutex)

sdlTryLockMutex :: MonadIO m => SDLMutex -> m Bool
sdlTryLockMutex mutex = liftIO $ Raw.sdlTryLockMutexRaw (sdlUnsafeToRawMutex mutex)

sdlUnlockMutex :: MonadIO m => SDLMutex -> m ()
sdlUnlockMutex mutex = liftIO $ Raw.sdlUnlockMutexRaw (sdlUnsafeToRawMutex mutex)

sdlDestroyMutex :: MonadIO m => SDLMutex -> m ()
sdlDestroyMutex mutex = liftIO $ Raw.sdlDestroyMutexRaw (sdlUnsafeToRawMutex mutex)

sdlCreateRWLock :: MonadIO m => m (Maybe SDLRWLock)
sdlCreateRWLock = liftIO $ sdlUnsafeFromRawRWLock <$> Raw.sdlCreateRWLockRaw

sdlLockRWLockForReading :: MonadIO m => SDLRWLock -> m ()
sdlLockRWLockForReading rwLock = liftIO $ Raw.sdlLockRWLockForReadingRaw (sdlUnsafeToRawRWLock rwLock)

sdlLockRWLockForWriting :: MonadIO m => SDLRWLock -> m ()
sdlLockRWLockForWriting rwLock = liftIO $ Raw.sdlLockRWLockForWritingRaw (sdlUnsafeToRawRWLock rwLock)

sdlTryLockRWLockForReading :: MonadIO m => SDLRWLock -> m Bool
sdlTryLockRWLockForReading rwLock = liftIO $ Raw.sdlTryLockRWLockForReadingRaw (sdlUnsafeToRawRWLock rwLock)

sdlTryLockRWLockForWriting :: MonadIO m => SDLRWLock -> m Bool
sdlTryLockRWLockForWriting rwLock = liftIO $ Raw.sdlTryLockRWLockForWritingRaw (sdlUnsafeToRawRWLock rwLock)

sdlUnlockRWLock :: MonadIO m => SDLRWLock -> m ()
sdlUnlockRWLock rwLock = liftIO $ Raw.sdlUnlockRWLockRaw (sdlUnsafeToRawRWLock rwLock)

sdlDestroyRWLock :: MonadIO m => SDLRWLock -> m ()
sdlDestroyRWLock rwLock = liftIO $ Raw.sdlDestroyRWLockRaw (sdlUnsafeToRawRWLock rwLock)

sdlCreateSemaphore :: MonadIO m => Word32 -> m (Maybe SDLSemaphore)
sdlCreateSemaphore value = liftIO $ sdlUnsafeFromRawSemaphore <$> Raw.sdlCreateSemaphoreRaw value

sdlDestroySemaphore :: MonadIO m => SDLSemaphore -> m ()
sdlDestroySemaphore semaphore = liftIO $ Raw.sdlDestroySemaphoreRaw (sdlUnsafeToRawSemaphore semaphore)

sdlWaitSemaphore :: MonadIO m => SDLSemaphore -> m ()
sdlWaitSemaphore semaphore = liftIO $ Raw.sdlWaitSemaphoreRaw (sdlUnsafeToRawSemaphore semaphore)

sdlTryWaitSemaphore :: MonadIO m => SDLSemaphore -> m Bool
sdlTryWaitSemaphore semaphore = liftIO $ Raw.sdlTryWaitSemaphoreRaw (sdlUnsafeToRawSemaphore semaphore)

sdlWaitSemaphoreTimeout :: MonadIO m => SDLSemaphore -> Int32 -> m Bool
sdlWaitSemaphoreTimeout semaphore timeoutMs =
  liftIO $ Raw.sdlWaitSemaphoreTimeoutRaw (sdlUnsafeToRawSemaphore semaphore) timeoutMs

sdlSignalSemaphore :: MonadIO m => SDLSemaphore -> m ()
sdlSignalSemaphore semaphore = liftIO $ Raw.sdlSignalSemaphoreRaw (sdlUnsafeToRawSemaphore semaphore)

sdlGetSemaphoreValue :: MonadIO m => SDLSemaphore -> m Word32
sdlGetSemaphoreValue semaphore = liftIO $ Raw.sdlGetSemaphoreValueRaw (sdlUnsafeToRawSemaphore semaphore)

sdlCreateCondition :: MonadIO m => m (Maybe SDLCondition)
sdlCreateCondition = liftIO $ sdlUnsafeFromRawCondition <$> Raw.sdlCreateConditionRaw

sdlDestroyCondition :: MonadIO m => SDLCondition -> m ()
sdlDestroyCondition condition = liftIO $ Raw.sdlDestroyConditionRaw (sdlUnsafeToRawCondition condition)

sdlSignalCondition :: MonadIO m => SDLCondition -> m ()
sdlSignalCondition condition = liftIO $ Raw.sdlSignalConditionRaw (sdlUnsafeToRawCondition condition)

sdlBroadcastCondition :: MonadIO m => SDLCondition -> m ()
sdlBroadcastCondition condition = liftIO $ Raw.sdlBroadcastConditionRaw (sdlUnsafeToRawCondition condition)

sdlWaitCondition :: MonadIO m => SDLCondition -> SDLMutex -> m ()
sdlWaitCondition condition mutex =
  liftIO $ Raw.sdlWaitConditionRaw (sdlUnsafeToRawCondition condition) (sdlUnsafeToRawMutex mutex)

sdlWaitConditionTimeout :: MonadIO m => SDLCondition -> SDLMutex -> Int32 -> m Bool
sdlWaitConditionTimeout condition mutex timeoutMs =
  liftIO $ Raw.sdlWaitConditionTimeoutRaw
    (sdlUnsafeToRawCondition condition)
    (sdlUnsafeToRawMutex mutex)
    timeoutMs

sdlShouldInit :: MonadIO m => Ptr SDLInitState -> m Bool
sdlShouldInit = liftIO . Raw.sdlShouldInitRaw

sdlShouldQuit :: MonadIO m => Ptr SDLInitState -> m Bool
sdlShouldQuit = liftIO . Raw.sdlShouldQuitRaw

sdlSetInitialized :: MonadIO m => Ptr SDLInitState -> Bool -> m ()
sdlSetInitialized a b = liftIO $ Raw.sdlSetInitializedRaw a b
