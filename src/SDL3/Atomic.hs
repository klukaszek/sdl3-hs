{-|
Module      : SDL.Atomic
Description : Bindings to SDL atomic operations
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

This module provides Haskell bindings to SDL's atomic operations.

IMPORTANT: If you are not an expert in concurrent lockless programming, you
should not be using any functions in this file. You should be protecting
your data structures with full mutexes instead.

***Seriously, here be dragons!***

These operations may or may not actually be implemented using processor
specific atomic operations. When possible they are implemented as true
processor specific atomic operations. When that is not possible the are
implemented using locks that *do* use the available atomic operations.

All of the atomic operations that modify memory are full memory barriers.
-}
module SDL3.Atomic
  (
  -- * Types
    SDLSpinLock
  , SDLAtomicInt(..)
  , SDLAtomicU32(..)
  
  -- * Spinlock Functions
  , sdlTryLockSpinlock
  , sdlLockSpinlock
  , sdlUnlockSpinlock
  
  -- * Memory Barrier Functions
  , sdlCompilerBarrier
  , sdlMemoryBarrierReleaseFunction
  , sdlMemoryBarrierAcquireFunction
  , sdlMemoryBarrierRelease
  , sdlMemoryBarrierAcquire
  , sdlCPUPauseInstruction
  
  -- * Atomic Integer Functions
  , sdlCompareAndSwapAtomicInt
  , sdlSetAtomicInt
  , sdlGetAtomicInt
  , sdlAddAtomicInt
  , sdlAtomicIncRef
  , sdlAtomicDecRef
  
  -- * Atomic Unsigned 32-bit Functions
  , sdlCompareAndSwapAtomicU32
  , sdlSetAtomicU32
  , sdlGetAtomicU32
  
  -- * Atomic Pointer Functions
  , sdlCompareAndSwapAtomicPointer
  , sdlSetAtomicPointer
  , sdlGetAtomicPointer
  ) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import SDL3.Stdinc (SDLBool)
import Data.Word

-- | An atomic spinlock.
type SDLSpinLock = CInt

-- Pointer wrappers
newtype SDLAtomicInt = SDLAtomicInt (Ptr SDLAtomicInt)
  deriving (Show, Eq)
newtype SDLAtomicU32 = SDLAtomicU32 (Ptr SDLAtomicU32) 
  deriving (Show, Eq)

-- Spinlock Functions

-- | Try to lock a spin lock by setting it to a non-zero value.
foreign import ccall unsafe "SDL_TryLockSpinlock"
  sdlTryLockSpinlock :: Ptr SDLSpinLock -> IO SDLBool

-- | Lock a spin lock by setting it to a non-zero value.
foreign import ccall unsafe "SDL_LockSpinlock"
  sdlLockSpinlock :: Ptr SDLSpinLock -> IO ()

-- | Unlock a spin lock by setting it to 0.
foreign import ccall unsafe "SDL_UnlockSpinlock"
  sdlUnlockSpinlock :: Ptr SDLSpinLock -> IO ()

-- Memory Barrier Functions

-- | Mark a compiler barrier.
foreign import ccall unsafe "wrapper_SDL_CompilerBarrier"
  sdlCompilerBarrier :: IO ()

-- | Insert a memory release barrier (function version).
foreign import ccall unsafe "SDL_MemoryBarrierReleaseFunction"
  sdlMemoryBarrierReleaseFunction :: IO ()

-- | Insert a memory acquire barrier (function version).
foreign import ccall unsafe "SDL_MemoryBarrierAcquireFunction"
  sdlMemoryBarrierAcquireFunction :: IO ()

-- | Insert a memory release barrier (macro version).
foreign import ccall unsafe "wrapper_SDL_MemoryBarrierRelease"
  sdlMemoryBarrierRelease :: IO ()

-- | Insert a memory acquire barrier (macro version).
foreign import ccall unsafe "wrapper_SDL_MemoryBarrierAcquire"
  sdlMemoryBarrierAcquire :: IO ()

-- | Insert a CPU-specific "pause" instruction.
foreign import ccall unsafe "wrapper_SDL_CPUPauseInstruction"
  sdlCPUPauseInstruction :: IO ()

-- Atomic Integer Functions

-- | Set an atomic variable to a new value if it is currently an old value.
foreign import ccall unsafe "SDL_CompareAndSwapAtomicInt"
  sdlCompareAndSwapAtomicInt :: Ptr SDLAtomicInt -> CInt -> CInt -> IO SDLBool

-- | Set an atomic variable to a value.
foreign import ccall unsafe "SDL_SetAtomicInt"
  sdlSetAtomicInt :: Ptr SDLAtomicInt -> CInt -> IO CInt

-- | Get the value of an atomic variable.
foreign import ccall unsafe "SDL_GetAtomicInt"
  sdlGetAtomicInt :: Ptr SDLAtomicInt -> IO CInt

-- | Add to an atomic variable.
foreign import ccall unsafe "SDL_AddAtomicInt"
  sdlAddAtomicInt :: Ptr SDLAtomicInt -> CInt -> IO CInt

-- | Increment an atomic variable used as a reference count.
sdlAtomicIncRef :: Ptr SDLAtomicInt -> IO CInt
sdlAtomicIncRef a = sdlAddAtomicInt a 1

-- | Decrement an atomic variable used as a reference count.
sdlAtomicDecRef :: Ptr SDLAtomicInt -> IO Bool
sdlAtomicDecRef a = do
  result <- sdlAddAtomicInt a (-1)
  return (result == 1)

-- Atomic Unsigned 32-bit Functions

-- | Set an atomic variable to a new value if it is currently an old value.
foreign import ccall unsafe "SDL_CompareAndSwapAtomicU32"
  sdlCompareAndSwapAtomicU32 :: Ptr SDLAtomicU32 -> Word32 -> Word32 -> IO SDLBool

-- | Set an atomic variable to a value.
foreign import ccall unsafe "SDL_SetAtomicU32"
  sdlSetAtomicU32 :: Ptr SDLAtomicU32 -> Word32 -> IO Word32

-- | Get the value of an atomic variable.
foreign import ccall unsafe "SDL_GetAtomicU32"
  sdlGetAtomicU32 :: Ptr SDLAtomicU32 -> IO Word32

-- Atomic Pointer Functions

-- | Set a pointer to a new value if it is currently an old value.
foreign import ccall unsafe "SDL_CompareAndSwapAtomicPointer"
  sdlCompareAndSwapAtomicPointer :: Ptr (Ptr a) -> Ptr b -> Ptr c -> IO SDLBool

-- | Set a pointer to a value atomically.
foreign import ccall unsafe "SDL_SetAtomicPointer"
  sdlSetAtomicPointer :: Ptr (Ptr a) -> Ptr b -> IO (Ptr c)

-- | Get the value of a pointer atomically.
foreign import ccall unsafe "SDL_GetAtomicPointer"
  sdlGetAtomicPointer :: Ptr (Ptr a) -> IO (Ptr b)

-- Make the atomic types instances of Storable
instance Storable SDLAtomicInt where
  sizeOf _ = sizeOf (undefined :: CInt)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = SDLAtomicInt <$> peek (castPtr ptr)
  poke ptr (SDLAtomicInt val) = poke (castPtr ptr) val

instance Storable SDLAtomicU32 where
  sizeOf _ = sizeOf (undefined :: Word32)
  alignment _ = alignment (undefined :: Word32)
  peek ptr = SDLAtomicU32 <$> peek (castPtr ptr)
  poke ptr (SDLAtomicU32 val) = poke (castPtr ptr) val
