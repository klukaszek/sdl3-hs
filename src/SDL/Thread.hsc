-- SDL/Thread.hsc
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}

{-|
Module      : SDL.Thread
Description : Thread management
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

SDL offers cross-platform thread management functions. These are mostly
concerned with starting threads, setting their priority, and dealing with
their termination.

In addition, there is support for Thread Local Storage (data that is unique
to each thread, but accessed from a single key).

On platforms without thread support (such as Emscripten when built without
pthreads), these functions still exist, but things like 'sdlCreateThread'
will report failure without doing anything.

If you're going to work with threads, you almost certainly need to have a
good understanding of SDL.Mutex as well.
-}

#include <SDL3/SDL_thread.h>
#include <SDL3/SDL_stdinc.h> -- For CBool

module SDL.Thread
  ( -- * Types
    SDLThread(..)
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

    -- * Thread creation and management
  , sdlCreateThread
  , sdlCreateThreadWithProperties
  , sdlGetThreadName
  , sdlGetCurrentThreadID
  , sdlGetThreadID
  , sdlSetCurrentThreadPriority
  , sdlWaitThread
  , sdlGetThreadState
  , sdlDetachThread

    -- * Thread local storage
  , sdlGetTLS
  , sdlSetTLS
  , sdlCleanupTLS

    -- * Thread property identifiers
  , pattern SDL_PROP_THREAD_CREATE_ENTRY_FUNCTION
  , pattern SDL_PROP_THREAD_CREATE_NAME
  , pattern SDL_PROP_THREAD_CREATE_USERDATA
  , pattern SDL_PROP_THREAD_CREATE_STACKSIZE
  ) where

import Foreign.C.Types
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.Ptr
import Foreign.Storable (Storable) -- Added Storable
import Foreign.Marshal.Utils (toBool) -- Added toBool
import Data.Word
import SDL.Atomic (SDLAtomicInt(..)) -- Assuming defined as newtype SDLAtomicInt (Ptr SDL_AtomicInt)
import SDL.Properties (SDLPropertiesID)
import SDL.Stdinc (SDLCall) -- Assuming defined as Ptr () or FunPtr

-- Opaque C struct type
data SDL_Thread

-- | The SDL thread object handle.
newtype SDLThread = SDLThread (Ptr SDL_Thread)
  deriving (Show, Eq)

-- | A unique numeric ID that identifies a thread.
type SDLThreadID = Word64

-- | Thread local storage ID handle. Wraps SDL_AtomicInt.
-- Use pattern matching or accessor if SDLAtomicInt is a record.
type SDLTLSID = SDLAtomicInt -- Keep as is, relies on SDL.Atomic definition

-- | The function passed to 'sdlCreateThread' as the new thread's entry point.
-- The function takes the userdata pointer and returns an exit status code.
type SDLThreadFunction = FunPtr (Ptr () -> IO CInt)

-- | The SDL thread priority enumeration.
newtype SDLThreadPriority = SDLThreadPriority CInt
  deriving newtype (Show, Eq, Ord, Storable, Enum)

pattern SDL_THREAD_PRIORITY_LOW :: SDLThreadPriority
pattern SDL_THREAD_PRIORITY_LOW = SDLThreadPriority #{const SDL_THREAD_PRIORITY_LOW}
pattern SDL_THREAD_PRIORITY_NORMAL :: SDLThreadPriority
pattern SDL_THREAD_PRIORITY_NORMAL = SDLThreadPriority #{const SDL_THREAD_PRIORITY_NORMAL}
pattern SDL_THREAD_PRIORITY_HIGH :: SDLThreadPriority
pattern SDL_THREAD_PRIORITY_HIGH = SDLThreadPriority #{const SDL_THREAD_PRIORITY_HIGH}
pattern SDL_THREAD_PRIORITY_TIME_CRITICAL :: SDLThreadPriority
pattern SDL_THREAD_PRIORITY_TIME_CRITICAL = SDLThreadPriority #{const SDL_THREAD_PRIORITY_TIME_CRITICAL}

-- | The current state of a thread enumeration.
newtype SDLThreadState = SDLThreadState CInt
  deriving newtype (Show, Eq, Ord, Storable, Enum)

pattern SDL_THREAD_UNKNOWN :: SDLThreadState
pattern SDL_THREAD_UNKNOWN   = SDLThreadState #{const SDL_THREAD_UNKNOWN}   -- ^ The thread is not valid
pattern SDL_THREAD_ALIVE :: SDLThreadState
pattern SDL_THREAD_ALIVE     = SDLThreadState #{const SDL_THREAD_ALIVE}     -- ^ The thread is currently running
pattern SDL_THREAD_DETACHED :: SDLThreadState
pattern SDL_THREAD_DETACHED  = SDLThreadState #{const SDL_THREAD_DETACHED}  -- ^ The thread is detached and can't be waited on
pattern SDL_THREAD_COMPLETE :: SDLThreadState
pattern SDL_THREAD_COMPLETE  = SDLThreadState #{const SDL_THREAD_COMPLETE}  -- ^ The thread has finished and should be cleaned up with 'sdlWaitThread'

-- | The callback used to cleanup data passed to 'sdlSetTLS'.
-- The function takes the userdata pointer that needs cleanup.
type SDLTLSDestructorCallback = FunPtr (Ptr () -> IO ())

-- | Thread property identifiers (use pattern synonyms if defined in header)
-- Assuming these are string literals for now
pattern SDL_PROP_THREAD_CREATE_ENTRY_FUNCTION :: String
pattern SDL_PROP_THREAD_CREATE_ENTRY_FUNCTION = "SDL.thread.create.entry_function" -- Check if #{const_str ...} exists
pattern SDL_PROP_THREAD_CREATE_NAME :: String
pattern SDL_PROP_THREAD_CREATE_NAME = "SDL.thread.create.name"
pattern SDL_PROP_THREAD_CREATE_USERDATA :: String
pattern SDL_PROP_THREAD_CREATE_USERDATA = "SDL.thread.create.userdata"
pattern SDL_PROP_THREAD_CREATE_STACKSIZE :: String
pattern SDL_PROP_THREAD_CREATE_STACKSIZE = "SDL.thread.create.stacksize"

-- Internal FFI interface to the actual SDL runtime function
-- FFI import for wrapper_SDL_CreateThread (direct SDL_CreateThread binding)
foreign import ccall unsafe "wrapper_SDL_CreateThread"
  c_sdlCreateThread :: SDLThreadFunction -> CString -> Ptr () -> IO (Ptr SDL_Thread)

foreign import ccall unsafe "SDL_CreateThreadRuntime"
  c_sdlCreateThreadRuntime :: SDLThreadFunction -> CString -> Ptr () -> SDLCall -> SDLCall -> IO (Ptr SDL_Thread)

-- | Create a new thread with a default stack size.
-- Returns Nothing on failure.
-- | Create a new thread with a default stack size (direct wrapper binding).
-- Returns Nothing on failure.
sdlCreateThread :: SDLThreadFunction -> String -> Ptr () -> IO (Maybe SDLThread)
sdlCreateThread fn name userData = do
  withCString name $ \cname -> do
    ptr <- c_sdlCreateThread fn cname userData
    pure $ if ptr == nullPtr then Nothing else Just (SDLThread ptr)

-- Internal FFI interface to the actual SDL runtime function
-- FFI import for wrapper_SDL_CreateThreadWithProperties (direct SDL_CreateThreadWithProperties binding)
foreign import ccall unsafe "wrapper_SDL_CreateThreadWithProperties"
  c_sdlCreateThreadWithProperties :: SDLPropertiesID -> IO (Ptr SDL_Thread)

foreign import ccall unsafe "SDL_CreateThreadWithPropertiesRuntime"
  c_sdlCreateThreadWithPropertiesRuntime :: SDLPropertiesID -> SDLCall -> SDLCall -> IO (Ptr SDL_Thread)

-- | Create a new thread with the specified properties.
-- Returns Nothing on failure.
-- | Create a new thread with the specified properties (direct wrapper binding).
-- Returns Nothing on failure.
sdlCreateThreadWithProperties :: SDLPropertiesID -> IO (Maybe SDLThread)
sdlCreateThreadWithProperties props = do
    ptr <- c_sdlCreateThreadWithProperties props
    pure $ if ptr == nullPtr then Nothing else Just (SDLThread ptr)

-- | Get the thread name as it was specified in 'sdlCreateThread'.
-- Returns Nothing if the thread has no name or is invalid.
foreign import ccall unsafe "SDL_GetThreadName"
  c_sdlGetThreadName :: Ptr SDL_Thread -> IO CString

sdlGetThreadName :: SDLThread -> IO (Maybe String)
sdlGetThreadName (SDLThread threadPtr) = do
  cStr <- c_sdlGetThreadName threadPtr
  if cStr == nullPtr
    then return Nothing
    else Just <$> peekCString cStr

-- | Get the thread identifier for the current thread.
foreign import ccall unsafe "SDL_GetCurrentThreadID"
  sdlGetCurrentThreadID :: IO SDLThreadID -- Direct FFI call

-- | Get the thread identifier for the specified thread.
foreign import ccall unsafe "SDL_GetThreadID"
  c_sdlGetThreadID :: Ptr SDL_Thread -> IO SDLThreadID

sdlGetThreadID :: SDLThread -> IO SDLThreadID
sdlGetThreadID (SDLThread threadPtr) = c_sdlGetThreadID threadPtr

-- | Raw C function to set thread priority.
foreign import ccall unsafe "SDL_SetCurrentThreadPriority"
  c_sdlSetCurrentThreadPriority :: CInt -> IO CBool -- Returns CBool

-- | Set the priority for the current thread.
-- Returns True on success, False on failure.
sdlSetCurrentThreadPriority :: SDLThreadPriority -> IO Bool
sdlSetCurrentThreadPriority priority =
  toBool <$> c_sdlSetCurrentThreadPriority (fromIntegral $ fromEnum priority) -- Use derived Enum + conversion

-- | Wait for a thread to finish.
-- Optionally retrieves the thread's exit status.
foreign import ccall unsafe "SDL_WaitThread"
  c_sdlWaitThread :: Ptr SDL_Thread -> Ptr CInt -> IO ()

sdlWaitThread :: SDLThread -> Maybe (Ptr CInt) -> IO ()
sdlWaitThread (SDLThread threadPtr) mStatusPtr =
  c_sdlWaitThread threadPtr (maybe nullPtr id mStatusPtr)

-- | Raw C function to get thread state.
foreign import ccall unsafe "SDL_GetThreadState"
  c_sdlGetThreadState :: Ptr SDL_Thread -> IO CInt

-- | Get the current state of a thread.
sdlGetThreadState :: SDLThread -> IO SDLThreadState
sdlGetThreadState (SDLThread threadPtr) =
  toEnum . fromIntegral <$> c_sdlGetThreadState threadPtr -- Use derived Enum

-- | Let a thread clean up on exit without intervention.
foreign import ccall unsafe "SDL_DetachThread"
  c_sdlDetachThread :: Ptr SDL_Thread -> IO ()

sdlDetachThread :: SDLThread -> IO ()
sdlDetachThread (SDLThread threadPtr) = c_sdlDetachThread threadPtr

-- | Get the current thread's value associated with a thread local storage ID.
foreign import ccall unsafe "SDL_GetTLS"
  c_sdlGetTLS :: Ptr SDLAtomicInt -> IO (Ptr ()) -- Takes Ptr SDL_AtomicInt

-- Assumes SDLAtomicInt is newtype SDLAtomicInt (Ptr SDL_AtomicInt)
sdlGetTLS :: SDLTLSID -> IO (Ptr ())
sdlGetTLS (SDLAtomicInt tlsPtr) = c_sdlGetTLS tlsPtr

-- | Set the current thread's value associated with a thread local storage ID.
-- Returns True on success, False on failure.
foreign import ccall unsafe "SDL_SetTLS"
  c_sdlSetTLS :: Ptr SDLAtomicInt -> Ptr () -> SDLTLSDestructorCallback -> IO CBool -- Returns CBool

-- Assumes SDLAtomicInt is newtype SDLAtomicInt (Ptr SDL_AtomicInt)
sdlSetTLS :: SDLTLSID -> Ptr () -> Maybe SDLTLSDestructorCallback -> IO Bool
sdlSetTLS (SDLAtomicInt tlsPtr) value mDestructor =
  toBool <$> c_sdlSetTLS tlsPtr value (maybe nullFunPtr id mDestructor)

-- | Cleanup all TLS data for this thread.
foreign import ccall unsafe "SDL_CleanupTLS"
  sdlCleanupTLS :: IO () -- Direct FFI call
