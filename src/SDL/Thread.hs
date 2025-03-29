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

module SDL.Thread
  ( -- * Thread structures and types
    SDLThread
  , SDLThreadID
  , SDLThreadFunction
  , SDLThreadPriority(..)
  , SDLThreadState(..)
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
  , sdlPropThreadCreateEntryFunction
  , sdlPropThreadCreateName
  , sdlPropThreadCreateUserdata
  , sdlPropThreadCreateStacksize
  ) where

import Foreign.C.Types
import Foreign.C.String (CString, withCString)
import Foreign.Ptr
import Data.Word
import Data.Int
import SDL.Atomic (SDLAtomicInt)
import SDL.Properties (SDLPropertiesID(..))
import SDL.Stdinc (SDLCall)

-- | The SDL thread object
data SDLThread

-- | A unique numeric ID that identifies a thread
type SDLThreadID = Word64

-- | Thread local storage ID
type SDLTLSID = SDLAtomicInt

-- | The function passed to 'sdlCreateThread' as the new thread's entry point
type SDLThreadFunction = FunPtr (Ptr () -> IO CInt)

-- | The SDL thread priority
data SDLThreadPriority 
  = SDLThreadPriorityLow
  | SDLThreadPriorityNormal
  | SDLThreadPriorityHigh
  | SDLThreadPriorityTimeCritical
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Convert SDLThreadPriority to CInt for FFI
threadPriorityToCInt :: SDLThreadPriority -> CInt
threadPriorityToCInt SDLThreadPriorityLow = 0
threadPriorityToCInt SDLThreadPriorityNormal = 1
threadPriorityToCInt SDLThreadPriorityHigh = 2
threadPriorityToCInt SDLThreadPriorityTimeCritical = 3

-- | The current state of a thread
data SDLThreadState
  = SDLThreadUnknown     -- ^ The thread is not valid
  | SDLThreadAlive       -- ^ The thread is currently running
  | SDLThreadDetached    -- ^ The thread is detached and can't be waited on
  | SDLThreadComplete    -- ^ The thread has finished and should be cleaned up with 'sdlWaitThread'
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Convert CInt to SDLThreadState for FFI
cIntToThreadState :: CInt -> SDLThreadState
cIntToThreadState 0 = SDLThreadUnknown
cIntToThreadState 1 = SDLThreadAlive
cIntToThreadState 2 = SDLThreadDetached
cIntToThreadState 3 = SDLThreadComplete
cIntToThreadState _ = SDLThreadUnknown  -- Default case for unexpected values

-- | The callback used to cleanup data passed to 'sdlSetTLS'
type SDLTLSDestructorCallback = FunPtr (Ptr () -> IO ())

-- | Thread property identifiers
sdlPropThreadCreateEntryFunction :: String
sdlPropThreadCreateEntryFunction = "SDL.thread.create.entry_function"

sdlPropThreadCreateName :: String
sdlPropThreadCreateName = "SDL.thread.create.name"

sdlPropThreadCreateUserdata :: String
sdlPropThreadCreateUserdata = "SDL.thread.create.userdata"

sdlPropThreadCreateStacksize :: String
sdlPropThreadCreateStacksize = "SDL.thread.create.stacksize"

-- Internal FFI interface to the actual SDL runtime function
foreign import ccall "SDL_CreateThreadRuntime" 
  sdlCreateThreadRuntime :: SDLThreadFunction -> CString -> Ptr a -> SDLCall -> SDLCall -> IO (Ptr SDLThread)

-- | Create a new thread with a default stack size
--
-- This is a convenience function, equivalent to calling
-- 'sdlCreateThreadWithProperties' with the following properties set:
--
-- - 'sdlPropThreadCreateEntryFunction': the thread function
-- - 'sdlPropThreadCreateName': the thread name
-- - 'sdlPropThreadCreateUserdata': the data to pass to the thread function
--
-- @since 3.2.0
sdlCreateThread :: SDLThreadFunction -> String -> Ptr a -> IO (Ptr SDLThread)
sdlCreateThread fn name userData = do
  withCString name $ \cname ->
    sdlCreateThreadRuntime fn cname userData nullPtr nullPtr

-- Internal FFI interface to the actual SDL runtime function
foreign import ccall "SDL_CreateThreadWithPropertiesRuntime"
  sdlCreateThreadWithPropertiesRuntime :: SDLPropertiesID -> SDLCall -> SDLCall -> IO (Ptr SDLThread)

-- | Create a new thread with with the specified properties
--
-- These are the supported properties:
--
-- - 'sdlPropThreadCreateEntryFunction': an 'SDLThreadFunction'
--   value that will be called at the start of the new thread's life.
--   Required.
-- - 'sdlPropThreadCreateName': the name of the new thread, which
--   might be available to debuggers. Optional, defaults to NULL.
-- - 'sdlPropThreadCreateUserdata': an arbitrary app-defined
--   pointer, which is passed to the entry function on the new thread, as its
--   only parameter. Optional, defaults to NULL.
-- - 'sdlPropThreadCreateStacksize': the size, in bytes, of the new
--   thread's stack. Optional, defaults to 0 (system-defined default).
--
-- SDL makes an attempt to report 'sdlPropThreadCreateName' to the
-- system, so that debuggers can display it. Not all platforms support this.
--
-- @since 3.2.0
sdlCreateThreadWithProperties :: SDLPropertiesID -> IO (Ptr SDLThread)
sdlCreateThreadWithProperties props = 
  sdlCreateThreadWithPropertiesRuntime props nullPtr nullPtr

-- | Get the thread name as it was specified in 'sdlCreateThread'
--
-- @since 3.2.0
foreign import ccall "SDL_GetThreadName" 
  sdlGetThreadName :: Ptr SDLThread -> IO CString

-- | Get the thread identifier for the current thread
--
-- This thread identifier is as reported by the underlying operating system.
-- If SDL is running on a platform that does not support threads the return
-- value will always be zero.
--
-- This function also returns a valid thread ID when called from the main
-- thread.
--
-- @since 3.2.0
foreign import ccall "SDL_GetCurrentThreadID"
  sdlGetCurrentThreadID :: IO SDLThreadID

-- | Get the thread identifier for the specified thread
--
-- This thread identifier is as reported by the underlying operating system.
-- If SDL is running on a platform that does not support threads the return
-- value will always be zero.
--
-- @since 3.2.0
foreign import ccall "SDL_GetThreadID"
  sdlGetThreadID :: Ptr SDLThread -> IO SDLThreadID

-- | Raw C function to set thread priority
foreign import ccall "SDL_SetCurrentThreadPriority"
  sdlSetCurrentThreadPriorityRaw :: CInt -> IO Bool

-- | Set the priority for the current thread
--
-- Note that some platforms will not let you alter the priority (or at least,
-- promote the thread to a higher priority) at all, and some require you to be
-- an administrator account. Be prepared for this to fail.
--
-- @since 3.2.0
sdlSetCurrentThreadPriority :: SDLThreadPriority -> IO Bool
sdlSetCurrentThreadPriority priority = sdlSetCurrentThreadPriorityRaw (threadPriorityToCInt priority)

-- | Wait for a thread to finish
--
-- Threads that haven't been detached will remain until this function cleans
-- them up. Not doing so is a resource leak.
--
-- Once a thread has been cleaned up through this function, the 'SDLThread'
-- that references it becomes invalid and should not be referenced again. As
-- such, only one thread may call 'sdlWaitThread' on another.
--
-- The return code from the thread function is placed in the area pointed to
-- by 'status', if 'status' is not NULL.
--
-- You may not wait on a thread that has been used in a call to
-- 'sdlDetachThread'. Use either that function or this one, but not both, or
-- behavior is undefined.
--
-- It is safe to pass a NULL thread to this function; it is a no-op.
--
-- Note that the thread pointer is freed by this function and is not valid
-- afterward.
--
-- @since 3.2.0
foreign import ccall "SDL_WaitThread"
  sdlWaitThread :: Ptr SDLThread -> Ptr CInt -> IO ()

-- | Raw C function to get thread state
foreign import ccall "SDL_GetThreadState"
  sdlGetThreadStateRaw :: Ptr SDLThread -> IO CInt

-- | Get the current state of a thread
--
-- @since 3.2.0
sdlGetThreadState :: Ptr SDLThread -> IO SDLThreadState
sdlGetThreadState thread = do
  stateInt <- sdlGetThreadStateRaw thread
  return $ cIntToThreadState stateInt

-- | Let a thread clean up on exit without intervention
--
-- A thread may be "detached" to signify that it should not remain until
-- another thread has called 'sdlWaitThread' on it. Detaching a thread is
-- useful for long-running threads that nothing needs to synchronize with or
-- further manage. When a detached thread is done, it simply goes away.
--
-- There is no way to recover the return code of a detached thread. If you
-- need this, don't detach the thread and instead use 'sdlWaitThread'.
--
-- Once a thread is detached, you should usually assume the 'SDLThread' isn't
-- safe to reference again, as it will become invalid immediately upon the
-- detached thread's exit, instead of remaining until someone has called
-- 'sdlWaitThread' to finally clean it up. As such, don't detach the same
-- thread more than once.
--
-- If a thread has already exited when passed to 'sdlDetachThread', it will
-- stop waiting for a call to 'sdlWaitThread' and clean up immediately. It is
-- not safe to detach a thread that might be used with 'sdlWaitThread'.
--
-- You may not call 'sdlWaitThread' on a thread that has been detached. Use
-- either that function or this one, but not both, or behavior is undefined.
--
-- It is safe to pass NULL to this function; it is a no-op.
--
-- @since 3.2.0
foreign import ccall "SDL_DetachThread"
  sdlDetachThread :: Ptr SDLThread -> IO ()

-- | Get the current thread's value associated with a thread local storage ID
--
-- @since 3.2.0
foreign import ccall "SDL_GetTLS"
  sdlGetTLS :: Ptr SDLTLSID -> IO (Ptr ())

-- | Set the current thread's value associated with a thread local storage ID
--
-- If the thread local storage ID is not initialized (the value is 0), a new
-- ID will be created in a thread-safe way, so all calls using a pointer to
-- the same ID will refer to the same local storage.
--
-- Note that replacing a value from a previous call to this function on the
-- same thread does _not_ call the previous value's destructor!
--
-- 'destructor' can be NULL; it is assumed that 'value' does not need to be
-- cleaned up if so.
--
-- @since 3.2.0
foreign import ccall "SDL_SetTLS"
  sdlSetTLS :: Ptr SDLTLSID -> Ptr () -> SDLTLSDestructorCallback -> IO Bool

-- | Cleanup all TLS data for this thread
--
-- If you are creating your threads outside of SDL and then calling SDL
-- functions, you should call this function before your thread exits, to
-- properly clean up SDL memory.
--
-- @since 3.2.0
foreign import ccall "SDL_CleanupTLS"
  sdlCleanupTLS :: IO ()
