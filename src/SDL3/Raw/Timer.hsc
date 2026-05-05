{-# LANGUAGE ForeignFunctionInterface #-}

{-|
Module      : SDL3.Raw.Timer
Description : Raw SDL time management utilities
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Low-level timer bindings. Prefer 'SDL3.Wrapped.Timer' or 'SDL3.Timer' unless
you need direct FFI access.
-}

module SDL3.Raw.Timer
  ( -- * Time Constants
    sdlMsPerSecond
  , sdlUsPerSecond
  , sdlNsPerSecond
  , sdlNsPerMs
  , sdlNsPerUs

    -- * Conversion Functions
  , sdlSecondsToNs
  , sdlNsToSeconds
  , sdlMsToNs
  , sdlNsToMs
  , sdlUsToNs
  , sdlNsToUs

    -- * Timer Types
  , SDLTimerID
  , SDLTimerCallback
  , SDLNSTimerCallback

    -- * Raw Timer Functions
  , sdlGetTicksRaw
  , sdlGetTicksNSRaw
  , sdlGetPerformanceCounterRaw
  , sdlGetPerformanceFrequencyRaw
  , sdlDelayRaw
  , sdlDelayNSRaw
  , sdlDelayPreciseRaw
  , wrapSDLTimerCallback
  , wrapSDLNSTimerCallback
  , sdlAddTimerRaw
  , sdlAddTimerNSRaw
  , sdlRemoveTimerRaw
  ) where

#include <SDL3/SDL_timer.h>

import Data.Word (Word32, Word64)
import Foreign.Ptr (FunPtr, Ptr)

sdlMsPerSecond :: Word32
sdlMsPerSecond = (#const SDL_MS_PER_SECOND)

sdlUsPerSecond :: Word32
sdlUsPerSecond = (#const SDL_US_PER_SECOND)

sdlNsPerSecond :: Word64
sdlNsPerSecond = (#const SDL_NS_PER_SECOND)

sdlNsPerMs :: Word32
sdlNsPerMs = (#const SDL_NS_PER_MS)

sdlNsPerUs :: Word32
sdlNsPerUs = (#const SDL_NS_PER_US)

sdlSecondsToNs :: Word64 -> Word64
sdlSecondsToNs s = s * sdlNsPerSecond

sdlNsToSeconds :: Word64 -> Word64
sdlNsToSeconds ns = ns `div` sdlNsPerSecond

sdlMsToNs :: Word64 -> Word64
sdlMsToNs ms = ms * fromIntegral sdlNsPerMs

sdlNsToMs :: Word64 -> Word64
sdlNsToMs ns = ns `div` fromIntegral sdlNsPerMs

sdlUsToNs :: Word64 -> Word64
sdlUsToNs us = us * fromIntegral sdlNsPerUs

sdlNsToUs :: Word64 -> Word64
sdlNsToUs ns = ns `div` fromIntegral sdlNsPerUs

type SDLTimerID = Word32

type SDLTimerCallback = Ptr () -> SDLTimerID -> Word32 -> IO Word32

type SDLNSTimerCallback = Ptr () -> SDLTimerID -> Word64 -> IO Word64

foreign import ccall "SDL_GetTicks"
  sdlGetTicksRaw :: IO Word64

foreign import ccall "SDL_GetTicksNS"
  sdlGetTicksNSRaw :: IO Word64

foreign import ccall "SDL_GetPerformanceCounter"
  sdlGetPerformanceCounterRaw :: IO Word64

foreign import ccall "SDL_GetPerformanceFrequency"
  sdlGetPerformanceFrequencyRaw :: IO Word64

foreign import ccall "SDL_Delay"
  sdlDelayRaw :: Word32 -> IO ()

foreign import ccall "SDL_DelayNS"
  sdlDelayNSRaw :: Word64 -> IO ()

foreign import ccall "SDL_DelayPrecise"
  sdlDelayPreciseRaw :: Word64 -> IO ()

foreign import ccall "wrapper"
  wrapSDLTimerCallback :: SDLTimerCallback -> IO (FunPtr SDLTimerCallback)

foreign import ccall "wrapper"
  wrapSDLNSTimerCallback :: SDLNSTimerCallback -> IO (FunPtr SDLNSTimerCallback)

foreign import ccall "SDL_AddTimer"
  sdlAddTimerRaw :: Word32 -> FunPtr SDLTimerCallback -> Ptr () -> IO SDLTimerID

foreign import ccall "SDL_AddTimerNS"
  sdlAddTimerNSRaw :: Word64 -> FunPtr SDLNSTimerCallback -> Ptr () -> IO SDLTimerID

foreign import ccall "SDL_RemoveTimer"
  sdlRemoveTimerRaw :: SDLTimerID -> IO Bool
