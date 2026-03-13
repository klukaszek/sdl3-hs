{-|
Module      : SDL3.Wrapped.Timer
Description : Wrapped SDL time management utilities
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Ergonomic wrappers over 'SDL3.Raw.Timer'.
-}

module SDL3.Wrapped.Timer
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

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32, Word64)
import Foreign.Ptr (Ptr, freeHaskellFunPtr)
import SDL3.Raw.Timer
  ( SDLNSTimerCallback
  , SDLTimerCallback
  , SDLTimerID
  , sdlMsPerSecond
  , sdlNsPerMs
  , sdlNsPerSecond
  , sdlNsPerUs
  , sdlNsToMs
  , sdlNsToSeconds
  , sdlNsToUs
  , sdlSecondsToNs
  , sdlUsPerSecond
  , sdlUsToNs
  , sdlMsToNs
  )
import qualified SDL3.Raw.Timer as Raw

sdlGetTicks :: MonadIO m => m Word64
sdlGetTicks = liftIO Raw.sdlGetTicksRaw

sdlGetTicksNS :: MonadIO m => m Word64
sdlGetTicksNS = liftIO Raw.sdlGetTicksNSRaw

sdlGetPerformanceCounter :: MonadIO m => m Word64
sdlGetPerformanceCounter = liftIO Raw.sdlGetPerformanceCounterRaw

sdlGetPerformanceFrequency :: MonadIO m => m Word64
sdlGetPerformanceFrequency = liftIO Raw.sdlGetPerformanceFrequencyRaw

sdlDelay :: MonadIO m => Word32 -> m ()
sdlDelay ms = liftIO $ Raw.sdlDelayRaw ms

sdlDelayNS :: MonadIO m => Word64 -> m ()
sdlDelayNS ns = liftIO $ Raw.sdlDelayNSRaw ns

sdlDelayPrecise :: MonadIO m => Word64 -> m ()
sdlDelayPrecise ns = liftIO $ Raw.sdlDelayPreciseRaw ns

sdlAddTimer :: MonadIO m => Word32 -> SDLTimerCallback -> Ptr () -> m SDLTimerID
sdlAddTimer interval callback userdata = liftIO $ do
  funPtr <- Raw.wrapSDLTimerCallback callback
  timerID <- Raw.sdlAddTimerRaw interval funPtr userdata
  if timerID == 0
    then freeHaskellFunPtr funPtr >> return 0
    else return timerID

sdlAddTimerNS :: MonadIO m => Word64 -> SDLNSTimerCallback -> Ptr () -> m SDLTimerID
sdlAddTimerNS interval callback userdata = liftIO $ do
  funPtr <- Raw.wrapSDLNSTimerCallback callback
  timerID <- Raw.sdlAddTimerNSRaw interval funPtr userdata
  if timerID == 0
    then freeHaskellFunPtr funPtr >> return 0
    else return timerID

sdlRemoveTimer :: MonadIO m => SDLTimerID -> m Bool
sdlRemoveTimer timerID = liftIO $ Raw.sdlRemoveTimerRaw timerID
