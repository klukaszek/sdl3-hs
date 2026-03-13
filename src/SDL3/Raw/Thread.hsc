{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

#include <SDL3/SDL_thread.h>
#include <SDL3/SDL_stdinc.h>

module SDL3.Raw.Thread
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
  , sdlCreateThreadRaw
  , sdlCreateThreadWithPropertiesRaw
  , sdlGetThreadNameRaw
  , sdlGetCurrentThreadIDRaw
  , sdlGetThreadIDRaw
  , sdlSetCurrentThreadPriorityRaw
  , sdlWaitThreadRaw
  , sdlGetThreadStateRaw
  , sdlDetachThreadRaw
  , sdlGetTLSRaw
  , sdlSetTLSRaw
  , sdlCleanupTLSRaw
  ) where

import Data.Word
import Foreign.C.String (CString)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable (Storable)
import SDL3.Raw.Atomic (SDLAtomicInt(..))
import SDL3.Raw.Properties (SDLPropertiesID)

data SDL_Thread

newtype SDLThread = SDLThread (Ptr SDL_Thread)
  deriving (Show, Eq)

type SDLThreadID = Word64

type SDLTLSID = SDLAtomicInt

type SDLThreadFunction = FunPtr (Ptr () -> IO CInt)

newtype SDLThreadPriority = SDLThreadPriority CInt
  deriving newtype (Show, Eq, Ord, Storable, Enum)

pattern SDL_THREAD_PRIORITY_LOW = SDLThreadPriority #{const SDL_THREAD_PRIORITY_LOW}
pattern SDL_THREAD_PRIORITY_NORMAL = SDLThreadPriority #{const SDL_THREAD_PRIORITY_NORMAL}
pattern SDL_THREAD_PRIORITY_HIGH = SDLThreadPriority #{const SDL_THREAD_PRIORITY_HIGH}
pattern SDL_THREAD_PRIORITY_TIME_CRITICAL = SDLThreadPriority #{const SDL_THREAD_PRIORITY_TIME_CRITICAL}

newtype SDLThreadState = SDLThreadState CInt
  deriving newtype (Show, Eq, Ord, Storable, Enum)

pattern SDL_THREAD_UNKNOWN = SDLThreadState #{const SDL_THREAD_UNKNOWN}
pattern SDL_THREAD_ALIVE = SDLThreadState #{const SDL_THREAD_ALIVE}
pattern SDL_THREAD_DETACHED = SDLThreadState #{const SDL_THREAD_DETACHED}
pattern SDL_THREAD_COMPLETE = SDLThreadState #{const SDL_THREAD_COMPLETE}

type SDLTLSDestructorCallback = FunPtr (Ptr () -> IO ())

pattern SDL_PROP_THREAD_CREATE_ENTRY_FUNCTION :: String
pattern SDL_PROP_THREAD_CREATE_ENTRY_FUNCTION = "SDL.thread.create.entry_function"

pattern SDL_PROP_THREAD_CREATE_NAME :: String
pattern SDL_PROP_THREAD_CREATE_NAME = "SDL.thread.create.name"

pattern SDL_PROP_THREAD_CREATE_USERDATA :: String
pattern SDL_PROP_THREAD_CREATE_USERDATA = "SDL.thread.create.userdata"

pattern SDL_PROP_THREAD_CREATE_STACKSIZE :: String
pattern SDL_PROP_THREAD_CREATE_STACKSIZE = "SDL.thread.create.stacksize"

foreign import ccall unsafe "wrapper_SDL_CreateThread"
  sdlCreateThreadRaw :: SDLThreadFunction -> CString -> Ptr () -> IO (Ptr SDL_Thread)

foreign import ccall unsafe "wrapper_SDL_CreateThreadWithProperties"
  sdlCreateThreadWithPropertiesRaw :: SDLPropertiesID -> IO (Ptr SDL_Thread)

foreign import ccall unsafe "SDL_GetThreadName"
  sdlGetThreadNameRaw :: Ptr SDL_Thread -> IO CString

foreign import ccall unsafe "SDL_GetCurrentThreadID"
  sdlGetCurrentThreadIDRaw :: IO SDLThreadID

foreign import ccall unsafe "SDL_GetThreadID"
  sdlGetThreadIDRaw :: Ptr SDL_Thread -> IO SDLThreadID

foreign import ccall unsafe "SDL_SetCurrentThreadPriority"
  sdlSetCurrentThreadPriorityRaw :: CInt -> IO CBool

foreign import ccall unsafe "SDL_WaitThread"
  sdlWaitThreadRaw :: Ptr SDL_Thread -> Ptr CInt -> IO ()

foreign import ccall unsafe "SDL_GetThreadState"
  sdlGetThreadStateRaw :: Ptr SDL_Thread -> IO CInt

foreign import ccall unsafe "SDL_DetachThread"
  sdlDetachThreadRaw :: Ptr SDL_Thread -> IO ()

foreign import ccall unsafe "SDL_GetTLS"
  sdlGetTLSRaw :: Ptr SDLAtomicInt -> IO (Ptr ())

foreign import ccall unsafe "SDL_SetTLS"
  sdlSetTLSRaw :: Ptr SDLAtomicInt -> Ptr () -> SDLTLSDestructorCallback -> IO CBool

foreign import ccall unsafe "SDL_CleanupTLS"
  sdlCleanupTLSRaw :: IO ()
