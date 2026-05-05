module SDL3.Wrapped.Atomic
  ( SDLSpinLock
  , SDLAtomicInt(..)
  , SDLAtomicU32(..)
  , sdlTryLockSpinlock
  , sdlLockSpinlock
  , sdlUnlockSpinlock
  , sdlCompilerBarrier
  , sdlMemoryBarrierReleaseFunction
  , sdlMemoryBarrierAcquireFunction
  , sdlMemoryBarrierRelease
  , sdlMemoryBarrierAcquire
  , sdlCPUPauseInstruction
  , sdlCompareAndSwapAtomicInt
  , sdlSetAtomicInt
  , sdlGetAtomicInt
  , sdlAddAtomicInt
  , sdlAtomicIncRef
  , sdlAtomicDecRef
  , sdlCompareAndSwapAtomicU32
  , sdlSetAtomicU32
  , sdlGetAtomicU32
  , sdlAddAtomicU32
  , sdlCompareAndSwapAtomicPointer
  , sdlSetAtomicPointer
  , sdlGetAtomicPointer
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import SDL3.Raw.Atomic (SDLAtomicInt(..), SDLAtomicU32(..), SDLSpinLock)
import qualified SDL3.Raw.Atomic as Raw
import SDL3.Stdinc (SDLBool)

sdlTryLockSpinlock :: MonadIO m => Ptr SDLSpinLock -> m SDLBool
sdlTryLockSpinlock = liftIO . Raw.sdlTryLockSpinlockRaw

sdlLockSpinlock :: MonadIO m => Ptr SDLSpinLock -> m ()
sdlLockSpinlock = liftIO . Raw.sdlLockSpinlockRaw

sdlUnlockSpinlock :: MonadIO m => Ptr SDLSpinLock -> m ()
sdlUnlockSpinlock = liftIO . Raw.sdlUnlockSpinlockRaw

sdlCompilerBarrier :: MonadIO m => m ()
sdlCompilerBarrier = liftIO Raw.sdlCompilerBarrierRaw

sdlMemoryBarrierReleaseFunction :: MonadIO m => m ()
sdlMemoryBarrierReleaseFunction = liftIO Raw.sdlMemoryBarrierReleaseFunctionRaw

sdlMemoryBarrierAcquireFunction :: MonadIO m => m ()
sdlMemoryBarrierAcquireFunction = liftIO Raw.sdlMemoryBarrierAcquireFunctionRaw

sdlMemoryBarrierRelease :: MonadIO m => m ()
sdlMemoryBarrierRelease = liftIO Raw.sdlMemoryBarrierReleaseRaw

sdlMemoryBarrierAcquire :: MonadIO m => m ()
sdlMemoryBarrierAcquire = liftIO Raw.sdlMemoryBarrierAcquireRaw

sdlCPUPauseInstruction :: MonadIO m => m ()
sdlCPUPauseInstruction = liftIO Raw.sdlCPUPauseInstructionRaw

sdlCompareAndSwapAtomicInt :: MonadIO m => Ptr SDLAtomicInt -> CInt -> CInt -> m SDLBool
sdlCompareAndSwapAtomicInt a b c = liftIO $ Raw.sdlCompareAndSwapAtomicIntRaw a b c

sdlSetAtomicInt :: MonadIO m => Ptr SDLAtomicInt -> CInt -> m CInt
sdlSetAtomicInt a b = liftIO $ Raw.sdlSetAtomicIntRaw a b

sdlGetAtomicInt :: MonadIO m => Ptr SDLAtomicInt -> m CInt
sdlGetAtomicInt = liftIO . Raw.sdlGetAtomicIntRaw

sdlAddAtomicInt :: MonadIO m => Ptr SDLAtomicInt -> CInt -> m CInt
sdlAddAtomicInt a b = liftIO $ Raw.sdlAddAtomicIntRaw a b

sdlAtomicIncRef :: MonadIO m => Ptr SDLAtomicInt -> m CInt
sdlAtomicIncRef a = liftIO $ Raw.sdlAddAtomicIntRaw a 1

sdlAtomicDecRef :: MonadIO m => Ptr SDLAtomicInt -> m Bool
sdlAtomicDecRef a = liftIO $ do
  result <- Raw.sdlAddAtomicIntRaw a (-1)
  return (result == 1)

sdlCompareAndSwapAtomicU32 :: MonadIO m => Ptr SDLAtomicU32 -> Word32 -> Word32 -> m SDLBool
sdlCompareAndSwapAtomicU32 a b c = liftIO $ Raw.sdlCompareAndSwapAtomicU32Raw a b c

sdlSetAtomicU32 :: MonadIO m => Ptr SDLAtomicU32 -> Word32 -> m Word32
sdlSetAtomicU32 a b = liftIO $ Raw.sdlSetAtomicU32Raw a b

sdlGetAtomicU32 :: MonadIO m => Ptr SDLAtomicU32 -> m Word32
sdlGetAtomicU32 = liftIO . Raw.sdlGetAtomicU32Raw

sdlAddAtomicU32 :: MonadIO m => Ptr SDLAtomicU32 -> CInt -> m Word32
sdlAddAtomicU32 a b = liftIO $ Raw.sdlAddAtomicU32Raw a b

sdlCompareAndSwapAtomicPointer :: MonadIO m => Ptr (Ptr a) -> Ptr b -> Ptr c -> m SDLBool
sdlCompareAndSwapAtomicPointer a b c = liftIO $ Raw.sdlCompareAndSwapAtomicPointerRaw a b c

sdlSetAtomicPointer :: MonadIO m => Ptr (Ptr a) -> Ptr b -> m (Ptr c)
sdlSetAtomicPointer a b = liftIO $ Raw.sdlSetAtomicPointerRaw a b

sdlGetAtomicPointer :: MonadIO m => Ptr (Ptr a) -> m (Ptr b)
sdlGetAtomicPointer = liftIO . Raw.sdlGetAtomicPointerRaw
