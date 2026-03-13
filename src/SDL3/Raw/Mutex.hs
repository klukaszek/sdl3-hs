module SDL3.Raw.Mutex
  ( SDLMutex
  , sdlCreateMutexRaw
  , sdlLockMutexRaw
  , sdlTryLockMutexRaw
  , sdlUnlockMutexRaw
  , sdlDestroyMutexRaw
  , SDLRWLock
  , sdlCreateRWLockRaw
  , sdlLockRWLockForReadingRaw
  , sdlLockRWLockForWritingRaw
  , sdlTryLockRWLockForReadingRaw
  , sdlTryLockRWLockForWritingRaw
  , sdlUnlockRWLockRaw
  , sdlDestroyRWLockRaw
  , SDLSemaphore
  , sdlCreateSemaphoreRaw
  , sdlDestroySemaphoreRaw
  , sdlWaitSemaphoreRaw
  , sdlTryWaitSemaphoreRaw
  , sdlWaitSemaphoreTimeoutRaw
  , sdlSignalSemaphoreRaw
  , sdlGetSemaphoreValueRaw
  , SDLCondition
  , sdlCreateConditionRaw
  , sdlDestroyConditionRaw
  , sdlSignalConditionRaw
  , sdlBroadcastConditionRaw
  , sdlWaitConditionRaw
  , sdlWaitConditionTimeoutRaw
  , SDLInitStatus(..)
  , SDLInitState(..)
  , sdlShouldInitRaw
  , sdlShouldQuitRaw
  , sdlSetInitializedRaw
  ) where

import Data.Int
import Data.Word
import Foreign.Ptr
import SDL3.Raw.Atomic (SDLAtomicInt)
import SDL3.Raw.Thread (SDLThreadID)

data SDLMutex

foreign import ccall "SDL_CreateMutex" sdlCreateMutexRaw :: IO (Ptr SDLMutex)
foreign import ccall "SDL_LockMutex" sdlLockMutexRaw :: Ptr SDLMutex -> IO ()
foreign import ccall "SDL_TryLockMutex" sdlTryLockMutexRaw :: Ptr SDLMutex -> IO Bool
foreign import ccall "SDL_UnlockMutex" sdlUnlockMutexRaw :: Ptr SDLMutex -> IO ()
foreign import ccall "SDL_DestroyMutex" sdlDestroyMutexRaw :: Ptr SDLMutex -> IO ()

data SDLRWLock

foreign import ccall "SDL_CreateRWLock" sdlCreateRWLockRaw :: IO (Ptr SDLRWLock)
foreign import ccall "SDL_LockRWLockForReading" sdlLockRWLockForReadingRaw :: Ptr SDLRWLock -> IO ()
foreign import ccall "SDL_LockRWLockForWriting" sdlLockRWLockForWritingRaw :: Ptr SDLRWLock -> IO ()
foreign import ccall "SDL_TryLockRWLockForReading" sdlTryLockRWLockForReadingRaw :: Ptr SDLRWLock -> IO Bool
foreign import ccall "SDL_TryLockRWLockForWriting" sdlTryLockRWLockForWritingRaw :: Ptr SDLRWLock -> IO Bool
foreign import ccall "SDL_UnlockRWLock" sdlUnlockRWLockRaw :: Ptr SDLRWLock -> IO ()
foreign import ccall "SDL_DestroyRWLock" sdlDestroyRWLockRaw :: Ptr SDLRWLock -> IO ()

data SDLSemaphore

foreign import ccall "SDL_CreateSemaphore" sdlCreateSemaphoreRaw :: Word32 -> IO (Ptr SDLSemaphore)
foreign import ccall "SDL_DestroySemaphore" sdlDestroySemaphoreRaw :: Ptr SDLSemaphore -> IO ()
foreign import ccall "SDL_WaitSemaphore" sdlWaitSemaphoreRaw :: Ptr SDLSemaphore -> IO ()
foreign import ccall "SDL_TryWaitSemaphore" sdlTryWaitSemaphoreRaw :: Ptr SDLSemaphore -> IO Bool
foreign import ccall "SDL_WaitSemaphoreTimeout" sdlWaitSemaphoreTimeoutRaw :: Ptr SDLSemaphore -> Int32 -> IO Bool
foreign import ccall "SDL_SignalSemaphore" sdlSignalSemaphoreRaw :: Ptr SDLSemaphore -> IO ()
foreign import ccall "SDL_GetSemaphoreValue" sdlGetSemaphoreValueRaw :: Ptr SDLSemaphore -> IO Word32

data SDLCondition

foreign import ccall "SDL_CreateCondition" sdlCreateConditionRaw :: IO (Ptr SDLCondition)
foreign import ccall "SDL_DestroyCondition" sdlDestroyConditionRaw :: Ptr SDLCondition -> IO ()
foreign import ccall "SDL_SignalCondition" sdlSignalConditionRaw :: Ptr SDLCondition -> IO ()
foreign import ccall "SDL_BroadcastCondition" sdlBroadcastConditionRaw :: Ptr SDLCondition -> IO ()
foreign import ccall "SDL_WaitCondition" sdlWaitConditionRaw :: Ptr SDLCondition -> Ptr SDLMutex -> IO ()
foreign import ccall "SDL_WaitConditionTimeout" sdlWaitConditionTimeoutRaw :: Ptr SDLCondition -> Ptr SDLMutex -> Int32 -> IO Bool

data SDLInitStatus
  = SDLInitStatusUninitialized
  | SDLInitStatusInitializing
  | SDLInitStatusInitialized
  | SDLInitStatusUninitializing
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data SDLInitState = SDLInitState
  { sdlInitStatus :: SDLAtomicInt
  , sdlInitThread :: SDLThreadID
  , sdlInitReserved :: Ptr ()
  }

foreign import ccall "SDL_ShouldInit" sdlShouldInitRaw :: Ptr SDLInitState -> IO Bool
foreign import ccall "SDL_ShouldQuit" sdlShouldQuitRaw :: Ptr SDLInitState -> IO Bool
foreign import ccall "SDL_SetInitialized" sdlSetInitializedRaw :: Ptr SDLInitState -> Bool -> IO ()
