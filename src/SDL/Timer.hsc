{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : SDL.Timer
Description : SDL time management utilities
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

SDL provides time management functionality for dealing with small durations of time.

This module offers tools for measuring elapsed time (e.g., 'sdlGetTicks', 'sdlGetPerformanceCounter'),
delaying execution (e.g., 'sdlDelay', 'sdlDelayNS'), and scheduling callbacks to fire after a specified
time has elapsed (e.g., 'sdlAddTimer'). It is distinct from calendar time management, which is handled
elsewhere in SDL.

These utilities are useful for timing operations in games or applications, such as controlling frame
rates, profiling performance, or triggering events after delays.
-}

module SDL.Timer
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

    -- * Time Query Functions
  , sdlGetTicks
  , sdlGetTicksNS
  , sdlGetPerformanceCounter
  , sdlGetPerformanceFrequency

    -- * Delay Functions
  , sdlDelay
  , sdlDelayNS
  , sdlDelayPrecise

    -- * Timer Types
  , SDLTimerID
  , SDLTimerCallback
  , SDLNSTimerCallback

    -- * Timer Management Functions
  , sdlAddTimer
  , sdlAddTimerNS
  , sdlRemoveTimer
  ) where

#include <SDL3/SDL_timer.h>

import Foreign.C.Types
import Foreign.Ptr (Ptr, FunPtr, nullPtr, freeHaskellFunPtr)
import Foreign.C.String (peekCString)
import Data.Word (Word32, Word64)

-- | Number of milliseconds in a second (SDL_MS_PER_SECOND).
sdlMsPerSecond :: Int
sdlMsPerSecond = #const SDL_MS_PER_SECOND

-- | Number of microseconds in a second (SDL_US_PER_SECOND).
sdlUsPerSecond :: Int
sdlUsPerSecond = #const SDL_US_PER_SECOND

-- | Number of nanoseconds in a second (SDL_NS_PER_SECOND).
sdlNsPerSecond :: Word64
sdlNsPerSecond = #const SDL_NS_PER_SECOND

-- | Number of nanoseconds in a millisecond (SDL_NS_PER_MS).
sdlNsPerMs :: Int
sdlNsPerMs = #const SDL_NS_PER_MS

-- | Number of nanoseconds in a microsecond (SDL_NS_PER_US).
sdlNsPerUs :: Int
sdlNsPerUs = #const SDL_NS_PER_US

-- | Convert seconds to nanoseconds (SDL_SECONDS_TO_NS).
sdlSecondsToNs :: Word64 -> Word64
sdlSecondsToNs s = s * sdlNsPerSecond

-- | Convert nanoseconds to seconds (SDL_NS_TO_SECONDS).
sdlNsToSeconds :: Word64 -> Word64
sdlNsToSeconds ns = ns `div` sdlNsPerSecond

-- | Convert milliseconds to nanoseconds (SDL_MS_TO_NS).
sdlMsToNs :: Word64 -> Word64
sdlMsToNs ms = ms * fromIntegral sdlNsPerMs

-- | Convert nanoseconds to milliseconds (SDL_NS_TO_MS).
sdlNsToMs :: Word64 -> Word64
sdlNsToMs ns = ns `div` fromIntegral sdlNsPerMs

-- | Convert microseconds to nanoseconds (SDL_US_TO_NS).
sdlUsToNs :: Word64 -> Word64
sdlUsToNs us = us * fromIntegral sdlNsPerUs

-- | Convert nanoseconds to microseconds (SDL_NS_TO_US).
sdlNsToUs :: Word64 -> Word64
sdlNsToUs ns = ns `div` fromIntegral sdlNsPerUs

-- | Get the number of milliseconds since SDL library initialization (SDL_GetTicks).
foreign import ccall "SDL_GetTicks"
  sdlGetTicks :: IO Word64

-- | Get the number of nanoseconds since SDL library initialization (SDL_GetTicksNS).
foreign import ccall "SDL_GetTicksNS"
  sdlGetTicksNS :: IO Word64

-- | Get the current value of the high resolution counter (SDL_GetPerformanceCounter).
foreign import ccall "SDL_GetPerformanceCounter"
  sdlGetPerformanceCounter :: IO Word64

-- | Get the count per second of the high resolution counter (SDL_GetPerformanceFrequency).
foreign import ccall "SDL_GetPerformanceFrequency"
  sdlGetPerformanceFrequency :: IO Word64

-- | Wait a specified number of milliseconds before returning (SDL_Delay).
foreign import ccall "SDL_Delay"
  sdlDelay :: Word32 -> IO ()

-- | Wait a specified number of nanoseconds before returning (SDL_DelayNS).
foreign import ccall "SDL_DelayNS"
  sdlDelayNS :: Word64 -> IO ()

-- | Wait a specified number of nanoseconds with higher precision (SDL_DelayPrecise).
foreign import ccall "SDL_DelayPrecise"
  sdlDelayPrecise :: Word64 -> IO ()

-- | Definition of the timer ID type (SDL_TimerID).
type SDLTimerID = Word32

-- | Function prototype for the millisecond timer callback (SDL_TimerCallback).
type SDLTimerCallback = Ptr () -> SDLTimerID -> Word32 -> IO Word32

-- | Function prototype for the nanosecond timer callback (SDL_NSTimerCallback).
type SDLNSTimerCallback = Ptr () -> SDLTimerID -> Word64 -> IO Word64

-- | Foreign import for SDL_AddTimer with dynamic wrapper.
foreign import ccall "wrapper"
  wrapSDLTimerCallback :: SDLTimerCallback -> IO (FunPtr SDLTimerCallback)

-- | Foreign import for SDL_AddTimerNS with dynamic wrapper.
foreign import ccall "wrapper"
  wrapSDLNSTimerCallback :: SDLNSTimerCallback -> IO (FunPtr SDLNSTimerCallback)

-- | Call a callback function at a future time in milliseconds (SDL_AddTimer).
foreign import ccall "SDL_AddTimer"
  sdlAddTimerRaw :: Word32 -> FunPtr SDLTimerCallback -> Ptr () -> IO SDLTimerID

-- | Haskell wrapper for SDL_AddTimer.
sdlAddTimer :: Word32 -> SDLTimerCallback -> Ptr () -> IO SDLTimerID
sdlAddTimer interval callback userdata = do
  funPtr <- wrapSDLTimerCallback callback
  timerID <- sdlAddTimerRaw interval funPtr userdata
  if timerID == 0
    then freeHaskellFunPtr funPtr >> return 0
    else return timerID

-- | Call a callback function at a future time in nanoseconds (SDL_AddTimerNS).
foreign import ccall "SDL_AddTimerNS"
  sdlAddTimerNSRaw :: Word64 -> FunPtr SDLNSTimerCallback -> Ptr () -> IO SDLTimerID

-- | Haskell wrapper for SDL_AddTimerNS.
sdlAddTimerNS :: Word64 -> SDLNSTimerCallback -> Ptr () -> IO SDLTimerID
sdlAddTimerNS interval callback userdata = do
  funPtr <- wrapSDLNSTimerCallback callback
  timerID <- sdlAddTimerNSRaw interval funPtr userdata
  if timerID == 0
    then freeHaskellFunPtr funPtr >> return 0
    else return timerID

-- | Remove a timer created with SDL_AddTimer or SDL_AddTimerNS (SDL_RemoveTimer).
foreign import ccall "SDL_RemoveTimer"
  sdlRemoveTimer :: SDLTimerID -> IO Bool
