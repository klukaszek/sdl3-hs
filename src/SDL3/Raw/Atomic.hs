module SDL3.Raw.Atomic
  ( SDLSpinLock
  , SDLAtomicInt(..)
  , SDLAtomicU32(..)
  , sdlTryLockSpinlockRaw
  , sdlLockSpinlockRaw
  , sdlUnlockSpinlockRaw
  , sdlCompilerBarrierRaw
  , sdlMemoryBarrierReleaseFunctionRaw
  , sdlMemoryBarrierAcquireFunctionRaw
  , sdlMemoryBarrierReleaseRaw
  , sdlMemoryBarrierAcquireRaw
  , sdlCPUPauseInstructionRaw
  , sdlCompareAndSwapAtomicIntRaw
  , sdlSetAtomicIntRaw
  , sdlGetAtomicIntRaw
  , sdlAddAtomicIntRaw
  , sdlCompareAndSwapAtomicU32Raw
  , sdlSetAtomicU32Raw
  , sdlGetAtomicU32Raw
  , sdlAddAtomicU32Raw
  , sdlCompareAndSwapAtomicPointerRaw
  , sdlSetAtomicPointerRaw
  , sdlGetAtomicPointerRaw
  ) where

import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import SDL3.Raw.Stdinc (SDLBool)

type SDLSpinLock = CInt

newtype SDLAtomicInt = SDLAtomicInt (Ptr SDLAtomicInt)
  deriving (Show, Eq)

newtype SDLAtomicU32 = SDLAtomicU32 (Ptr SDLAtomicU32)
  deriving (Show, Eq)

foreign import ccall unsafe "SDL_TryLockSpinlock"
  sdlTryLockSpinlockRaw :: Ptr SDLSpinLock -> IO SDLBool

foreign import ccall unsafe "SDL_LockSpinlock"
  sdlLockSpinlockRaw :: Ptr SDLSpinLock -> IO ()

foreign import ccall unsafe "SDL_UnlockSpinlock"
  sdlUnlockSpinlockRaw :: Ptr SDLSpinLock -> IO ()

foreign import ccall unsafe "wrapper_SDL_CompilerBarrier"
  sdlCompilerBarrierRaw :: IO ()

foreign import ccall unsafe "SDL_MemoryBarrierReleaseFunction"
  sdlMemoryBarrierReleaseFunctionRaw :: IO ()

foreign import ccall unsafe "SDL_MemoryBarrierAcquireFunction"
  sdlMemoryBarrierAcquireFunctionRaw :: IO ()

foreign import ccall unsafe "wrapper_SDL_MemoryBarrierRelease"
  sdlMemoryBarrierReleaseRaw :: IO ()

foreign import ccall unsafe "wrapper_SDL_MemoryBarrierAcquire"
  sdlMemoryBarrierAcquireRaw :: IO ()

foreign import ccall unsafe "wrapper_SDL_CPUPauseInstruction"
  sdlCPUPauseInstructionRaw :: IO ()

foreign import ccall unsafe "SDL_CompareAndSwapAtomicInt"
  sdlCompareAndSwapAtomicIntRaw :: Ptr SDLAtomicInt -> CInt -> CInt -> IO SDLBool

foreign import ccall unsafe "SDL_SetAtomicInt"
  sdlSetAtomicIntRaw :: Ptr SDLAtomicInt -> CInt -> IO CInt

foreign import ccall unsafe "SDL_GetAtomicInt"
  sdlGetAtomicIntRaw :: Ptr SDLAtomicInt -> IO CInt

foreign import ccall unsafe "SDL_AddAtomicInt"
  sdlAddAtomicIntRaw :: Ptr SDLAtomicInt -> CInt -> IO CInt

foreign import ccall unsafe "SDL_CompareAndSwapAtomicU32"
  sdlCompareAndSwapAtomicU32Raw :: Ptr SDLAtomicU32 -> Word32 -> Word32 -> IO SDLBool

foreign import ccall unsafe "SDL_SetAtomicU32"
  sdlSetAtomicU32Raw :: Ptr SDLAtomicU32 -> Word32 -> IO Word32

foreign import ccall unsafe "SDL_GetAtomicU32"
  sdlGetAtomicU32Raw :: Ptr SDLAtomicU32 -> IO Word32

foreign import ccall unsafe "SDL_AddAtomicU32"
  sdlAddAtomicU32Raw :: Ptr SDLAtomicU32 -> CInt -> IO Word32

foreign import ccall unsafe "SDL_CompareAndSwapAtomicPointer"
  sdlCompareAndSwapAtomicPointerRaw :: Ptr (Ptr a) -> Ptr b -> Ptr c -> IO SDLBool

foreign import ccall unsafe "SDL_SetAtomicPointer"
  sdlSetAtomicPointerRaw :: Ptr (Ptr a) -> Ptr b -> IO (Ptr c)

foreign import ccall unsafe "SDL_GetAtomicPointer"
  sdlGetAtomicPointerRaw :: Ptr (Ptr a) -> IO (Ptr b)

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
