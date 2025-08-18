-- |
-- Module      : SDL.Mutex
-- Description : Thread synchronization primitives
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- SDL offers several thread synchronization primitives:
--
-- - Mutexes: 'sdlCreateMutex'
-- - Read/Write locks: 'sdlCreateRWLock'
-- - Semaphores: 'sdlCreateSemaphore'
-- - Condition variables: 'sdlCreateCondition'
--
-- SDL also offers a datatype, 'SDLInitState', which can be used to make sure
-- only one thread initializes/deinitializes some resource that several
-- threads might try to use for the first time simultaneously.
module SDL.Mutex
  ( -- * Thread safety annotation wrappers

    -- These are C macros, not exposed directly in Haskell

    -- * Mutex functions
    SDLMutex,
    sdlCreateMutex,
    sdlLockMutex,
    sdlTryLockMutex,
    sdlUnlockMutex,
    sdlDestroyMutex,

    -- * Read/write lock functions
    SDLRWLock,
    sdlCreateRWLock,
    sdlLockRWLockForReading,
    sdlLockRWLockForWriting,
    sdlTryLockRWLockForReading,
    sdlTryLockRWLockForWriting,
    sdlUnlockRWLock,
    sdlDestroyRWLock,

    -- * Semaphore functions
    SDLSemaphore,
    sdlCreateSemaphore,
    sdlDestroySemaphore,
    sdlWaitSemaphore,
    sdlTryWaitSemaphore,
    sdlWaitSemaphoreTimeout,
    sdlSignalSemaphore,
    sdlGetSemaphoreValue,

    -- * Condition variable functions
    SDLCondition,
    sdlCreateCondition,
    sdlDestroyCondition,
    sdlSignalCondition,
    sdlBroadcastCondition,
    sdlWaitCondition,
    sdlWaitConditionTimeout,

    -- * Thread-safe initialization state functions
    SDLInitStatus (..),
    SDLInitState (..),
    sdlShouldInit,
    sdlShouldQuit,
    sdlSetInitialized,
  )
where

import Data.Int
import Data.Word
import Foreign.Ptr
import SDL.Atomic (SDLAtomicInt)
import SDL.Thread (SDLThreadID)

-- | A means to serialize access to a resource between threads
data SDLMutex

-- | Create a new mutex
--
-- All newly-created mutexes begin in the _unlocked_ state.
--
-- Calls to 'sdlLockMutex' will not return while the mutex is locked by
-- another thread. See 'sdlTryLockMutex' to attempt to lock without blocking.
--
-- SDL mutexes are reentrant.
--
-- @since 3.2.0
foreign import ccall "SDL_CreateMutex" sdlCreateMutex :: IO (Ptr SDLMutex)

-- | Lock the mutex
--
-- This will block until the mutex is available, which is to say it is in the
-- unlocked state and the OS has chosen the caller as the next thread to lock
-- it. Of all threads waiting to lock the mutex, only one may do so at a time.
--
-- It is legal for the owning thread to lock an already-locked mutex. It must
-- unlock it the same number of times before it is actually made available for
-- other threads in the system (this is known as a "recursive mutex").
--
-- This function does not fail; if mutex is NULL, it will return immediately
-- having locked nothing. If the mutex is valid, this function will always
-- block until it can lock the mutex, and return with it locked.
--
-- @since 3.2.0
foreign import ccall "SDL_LockMutex" sdlLockMutex :: Ptr SDLMutex -> IO ()

-- | Try to lock a mutex without blocking
--
-- This works just like 'sdlLockMutex', but if the mutex is not available,
-- this function returns False immediately.
--
-- This technique is useful if you need exclusive access to a resource but
-- don't want to wait for it, and will return to it to try again later.
--
-- This function returns True if passed a NULL mutex.
--
-- @since 3.2.0
foreign import ccall "SDL_TryLockMutex" sdlTryLockMutex :: Ptr SDLMutex -> IO Bool

-- | Unlock the mutex
--
-- It is legal for the owning thread to lock an already-locked mutex. It must
-- unlock it the same number of times before it is actually made available for
-- other threads in the system (this is known as a "recursive mutex").
--
-- It is illegal to unlock a mutex that has not been locked by the current
-- thread, and doing so results in undefined behavior.
--
-- @since 3.2.0
foreign import ccall "SDL_UnlockMutex" sdlUnlockMutex :: Ptr SDLMutex -> IO ()

-- | Destroy a mutex created with 'sdlCreateMutex'
--
-- This function must be called on any mutex that is no longer needed. Failure
-- to destroy a mutex will result in a system memory or resource leak. While
-- it is safe to destroy a mutex that is _unlocked_, it is not safe to attempt
-- to destroy a locked mutex, and may result in undefined behavior depending
-- on the platform.
--
-- @since 3.2.0
foreign import ccall "SDL_DestroyMutex" sdlDestroyMutex :: Ptr SDLMutex -> IO ()

-- | A mutex that allows read-only threads to run in parallel
data SDLRWLock

-- | Create a new read/write lock
--
-- A read/write lock is useful for situations where you have multiple threads
-- trying to access a resource that is rarely updated. All threads requesting
-- a read-only lock will be allowed to run in parallel; if a thread requests a
-- write lock, it will be provided exclusive access. This makes it safe for
-- multiple threads to use a resource at the same time if they promise not to
-- change it, and when it has to be changed, the rwlock will serve as a
-- gateway to make sure those changes can be made safely.
--
-- In the right situation, a rwlock can be more efficient than a mutex, which
-- only lets a single thread proceed at a time, even if it won't be modifying
-- the data.
--
-- All newly-created read/write locks begin in the _unlocked_ state.
--
-- Calls to 'sdlLockRWLockForReading' and 'sdlLockRWLockForWriting' will not
-- return while the rwlock is locked _for writing_ by another thread. See
-- 'sdlTryLockRWLockForReading' and 'sdlTryLockRWLockForWriting' to attempt
-- to lock without blocking.
--
-- SDL read/write locks are only recursive for read-only locks! They are not
-- guaranteed to be fair, or provide access in a FIFO manner! They are not
-- guaranteed to favor writers. You may not lock a rwlock for both read-only
-- and write access at the same time from the same thread (so you can't
-- promote your read-only lock to a write lock without unlocking first).
--
-- @since 3.2.0
foreign import ccall "SDL_CreateRWLock" sdlCreateRWLock :: IO (Ptr SDLRWLock)

-- | Lock the read/write lock for _read only_ operations
--
-- This will block until the rwlock is available, which is to say it is not
-- locked for writing by any other thread. Of all threads waiting to lock the
-- rwlock, all may do so at the same time as long as they are requesting
-- read-only access; if a thread wants to lock for writing, only one may do so
-- at a time, and no other threads, read-only or not, may hold the lock at the
-- same time.
--
-- It is legal for the owning thread to lock an already-locked rwlock for
-- reading. It must unlock it the same number of times before it is actually
-- made available for other threads in the system (this is known as a
-- "recursive rwlock").
--
-- Note that locking for writing is not recursive (this is only available to
-- read-only locks).
--
-- It is illegal to request a read-only lock from a thread that already holds
-- the write lock. Doing so results in undefined behavior. Unlock the write
-- lock before requesting a read-only lock. (But, of course, if you have the
-- write lock, you don't need further locks to read in any case.)
--
-- This function does not fail; if rwlock is NULL, it will return immediately
-- having locked nothing. If the rwlock is valid, this function will always
-- block until it can lock the mutex, and return with it locked.
--
-- @since 3.2.0
foreign import ccall "SDL_LockRWLockForReading" sdlLockRWLockForReading :: Ptr SDLRWLock -> IO ()

-- | Lock the read/write lock for _write_ operations
--
-- This will block until the rwlock is available, which is to say it is not
-- locked for reading or writing by any other thread. Only one thread may hold
-- the lock when it requests write access; all other threads, whether they
-- also want to write or only want read-only access, must wait until the
-- writer thread has released the lock.
--
-- It is illegal for the owning thread to lock an already-locked rwlock for
-- writing (read-only may be locked recursively, writing can not). Doing so
-- results in undefined behavior.
--
-- It is illegal to request a write lock from a thread that already holds a
-- read-only lock. Doing so results in undefined behavior. Unlock the
-- read-only lock before requesting a write lock.
--
-- This function does not fail; if rwlock is NULL, it will return immediately
-- having locked nothing. If the rwlock is valid, this function will always
-- block until it can lock the mutex, and return with it locked.
--
-- @since 3.2.0
foreign import ccall "SDL_LockRWLockForWriting" sdlLockRWLockForWriting :: Ptr SDLRWLock -> IO ()

-- | Try to lock a read/write lock _for reading_ without blocking
--
-- This works just like 'sdlLockRWLockForReading', but if the rwlock is not
-- available, then this function returns False immediately.
--
-- This technique is useful if you need access to a resource but don't want to
-- wait for it, and will return to it to try again later.
--
-- Trying to lock for read-only access can succeed if other threads are
-- holding read-only locks, as this won't prevent access.
--
-- This function returns True if passed a NULL rwlock.
--
-- @since 3.2.0
foreign import ccall "SDL_TryLockRWLockForReading" sdlTryLockRWLockForReading :: Ptr SDLRWLock -> IO Bool

-- | Try to lock a read/write lock _for writing_ without blocking
--
-- This works just like 'sdlLockRWLockForWriting', but if the rwlock is not
-- available, then this function returns False immediately.
--
-- This technique is useful if you need exclusive access to a resource but
-- don't want to wait for it, and will return to it to try again later.
--
-- It is illegal for the owning thread to lock an already-locked rwlock for
-- writing (read-only may be locked recursively, writing can not). Doing so
-- results in undefined behavior.
--
-- It is illegal to request a write lock from a thread that already holds a
-- read-only lock. Doing so results in undefined behavior. Unlock the
-- read-only lock before requesting a write lock.
--
-- This function returns True if passed a NULL rwlock.
--
-- @since 3.2.0
foreign import ccall "SDL_TryLockRWLockForWriting" sdlTryLockRWLockForWriting :: Ptr SDLRWLock -> IO Bool

-- | Unlock the read/write lock
--
-- Use this function to unlock the rwlock, whether it was locked for read-only
-- or write operations.
--
-- It is legal for the owning thread to lock an already-locked read-only lock.
-- It must unlock it the same number of times before it is actually made
-- available for other threads in the system (this is known as a "recursive
-- rwlock").
--
-- It is illegal to unlock a rwlock that has not been locked by the current
-- thread, and doing so results in undefined behavior.
--
-- @since 3.2.0
foreign import ccall "SDL_UnlockRWLock" sdlUnlockRWLock :: Ptr SDLRWLock -> IO ()

-- | Destroy a read/write lock created with 'sdlCreateRWLock'
--
-- This function must be called on any read/write lock that is no longer
-- needed. Failure to destroy a rwlock will result in a system memory or
-- resource leak. While it is safe to destroy a rwlock that is _unlocked_, it
-- is not safe to attempt to destroy a locked rwlock, and may result in
-- undefined behavior depending on the platform.
--
-- @since 3.2.0
foreign import ccall "SDL_DestroyRWLock" sdlDestroyRWLock :: Ptr SDLRWLock -> IO ()

-- | A means to manage access to a resource, by count, between threads
data SDLSemaphore

-- | Create a semaphore
--
-- This function creates a new semaphore and initializes it with the value
-- `initial_value`. Each wait operation on the semaphore will atomically
-- decrement the semaphore value and potentially block if the semaphore value
-- is 0. Each post operation will atomically increment the semaphore value and
-- wake waiting threads and allow them to retry the wait operation.
--
-- @since 3.2.0
foreign import ccall "SDL_CreateSemaphore" sdlCreateSemaphore :: Word32 -> IO (Ptr SDLSemaphore)

-- | Destroy a semaphore
--
-- It is not safe to destroy a semaphore if there are threads currently
-- waiting on it.
--
-- @since 3.2.0
foreign import ccall "SDL_DestroySemaphore" sdlDestroySemaphore :: Ptr SDLSemaphore -> IO ()

-- | Wait until a semaphore has a positive value and then decrements it
--
-- This function suspends the calling thread until the semaphore pointed to by
-- `sem` has a positive value, and then atomically decrement the semaphore
-- value.
--
-- This function is the equivalent of calling 'sdlWaitSemaphoreTimeout' with
-- a time length of -1.
--
-- @since 3.2.0
foreign import ccall "SDL_WaitSemaphore" sdlWaitSemaphore :: Ptr SDLSemaphore -> IO ()

-- | See if a semaphore has a positive value and decrement it if it does
--
-- This function checks to see if the semaphore pointed to by `sem` has a
-- positive value and atomically decrements the semaphore value if it does. If
-- the semaphore doesn't have a positive value, the function immediately
-- returns False.
--
-- @since 3.2.0
foreign import ccall "SDL_TryWaitSemaphore" sdlTryWaitSemaphore :: Ptr SDLSemaphore -> IO Bool

-- | Wait until a semaphore has a positive value and then decrements it
--
-- This function suspends the calling thread until either the semaphore
-- pointed to by `sem` has a positive value or the specified time has elapsed.
-- If the call is successful it will atomically decrement the semaphore value.
--
-- @since 3.2.0
foreign import ccall "SDL_WaitSemaphoreTimeout" sdlWaitSemaphoreTimeout :: Ptr SDLSemaphore -> Int32 -> IO Bool

-- | Atomically increment a semaphore's value and wake waiting threads
--
-- @since 3.2.0
foreign import ccall "SDL_SignalSemaphore" sdlSignalSemaphore :: Ptr SDLSemaphore -> IO ()

-- | Get the current value of a semaphore
--
-- @since 3.2.0
foreign import ccall "SDL_GetSemaphoreValue" sdlGetSemaphoreValue :: Ptr SDLSemaphore -> IO Word32

-- | A means to block multiple threads until a condition is satisfied
data SDLCondition

-- | Create a condition variable
--
-- @since 3.2.0
foreign import ccall "SDL_CreateCondition" sdlCreateCondition :: IO (Ptr SDLCondition)

-- | Destroy a condition variable
--
-- @since 3.2.0
foreign import ccall "SDL_DestroyCondition" sdlDestroyCondition :: Ptr SDLCondition -> IO ()

-- | Restart one of the threads that are waiting on the condition variable
--
-- @since 3.2.0
foreign import ccall "SDL_SignalCondition" sdlSignalCondition :: Ptr SDLCondition -> IO ()

-- | Restart all threads that are waiting on the condition variable
--
-- @since 3.2.0
foreign import ccall "SDL_BroadcastCondition" sdlBroadcastCondition :: Ptr SDLCondition -> IO ()

-- | Wait until a condition variable is signaled
--
-- This function unlocks the specified `mutex` and waits for another thread to
-- call 'sdlSignalCondition' or 'sdlBroadcastCondition' on the condition
-- variable `cond`. Once the condition variable is signaled, the mutex is
-- re-locked and the function returns.
--
-- The mutex must be locked before calling this function. Locking the mutex
-- recursively (more than once) is not supported and leads to undefined
-- behavior.
--
-- This function is the equivalent of calling 'sdlWaitConditionTimeout' with
-- a time length of -1.
--
-- @since 3.2.0
foreign import ccall "SDL_WaitCondition" sdlWaitCondition :: Ptr SDLCondition -> Ptr SDLMutex -> IO ()

-- | Wait until a condition variable is signaled or a certain time has passed
--
-- This function unlocks the specified `mutex` and waits for another thread to
-- call 'sdlSignalCondition' or 'sdlBroadcastCondition' on the condition
-- variable `cond`, or for the specified time to elapse. Once the condition
-- variable is signaled or the time elapsed, the mutex is re-locked and the
-- function returns.
--
-- The mutex must be locked before calling this function. Locking the mutex
-- recursively (more than once) is not supported and leads to undefined
-- behavior.
--
-- @since 3.2.0
foreign import ccall "SDL_WaitConditionTimeout" sdlWaitConditionTimeout :: Ptr SDLCondition -> Ptr SDLMutex -> Int32 -> IO Bool

-- | The current status of an SDLInitState structure
data SDLInitStatus
  = -- | Not yet initialized
    SDLInitStatusUninitialized
  | -- | Currently being initialized
    SDLInitStatusInitializing
  | -- | Successfully initialized
    SDLInitStatusInitialized
  | -- | Currently being uninitialized
    SDLInitStatusUninitializing
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | A structure used for thread-safe initialization and shutdown
data SDLInitState = SDLInitState
  { -- | Current status
    sdlInitStatus :: SDLAtomicInt,
    -- | Thread that is initializing
    sdlInitThread :: SDLThreadID,
    -- | Reserved for future use
    sdlInitReserved :: Ptr ()
  }

-- | Return whether initialization should be done
--
-- This function checks the passed in state and if initialization should be
-- done, sets the status to `SDLInitStatusInitializing` and returns True.
-- If another thread is already modifying this state, it will wait until
-- that's done before returning.
--
-- If this function returns True, the calling code must call
-- 'sdlSetInitialized' to complete the initialization.
--
-- @since 3.2.0
foreign import ccall "SDL_ShouldInit" sdlShouldInit :: Ptr SDLInitState -> IO Bool

-- | Return whether cleanup should be done
--
-- This function checks the passed in state and if cleanup should be done,
-- sets the status to `SDLInitStatusUninitializing` and returns True.
--
-- If this function returns True, the calling code must call
-- 'sdlSetInitialized' to complete the cleanup.
--
-- @since 3.2.0
foreign import ccall "SDL_ShouldQuit" sdlShouldQuit :: Ptr SDLInitState -> IO Bool

-- | Finish an initialization state transition
--
-- This function sets the status of the passed in state to
-- `SDLInitStatusInitialized` or `SDLInitStatusUninitialized` and allows
-- any threads waiting for the status to proceed.
--
-- @since 3.2.0
foreign import ccall "SDL_SetInitialized" sdlSetInitialized :: Ptr SDLInitState -> Bool -> IO ()
