{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : SDL3.Wrapped.Power
-- Description : Wrapped power management functions for SDL3
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Ergonomic wrappers over 'SDL3.Raw.Power'.

module SDL3.Wrapped.Power
  ( SDLPowerState(..)
  , pattern SDL_POWERSTATE_ERROR
  , pattern SDL_POWERSTATE_UNKNOWN
  , pattern SDL_POWERSTATE_ON_BATTERY
  , pattern SDL_POWERSTATE_NO_BATTERY
  , pattern SDL_POWERSTATE_CHARGING
  , pattern SDL_POWERSTATE_CHARGED
  , sdlGetPowerInfo
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign (Ptr, nullPtr)
import Foreign.C.Types (CInt)
import SDL3.Raw.Power
  ( SDLPowerState(..)
  , pattern SDL_POWERSTATE_CHARGED
  , pattern SDL_POWERSTATE_CHARGING
  , pattern SDL_POWERSTATE_ERROR
  , pattern SDL_POWERSTATE_NO_BATTERY
  , pattern SDL_POWERSTATE_ON_BATTERY
  , pattern SDL_POWERSTATE_UNKNOWN
  )
import qualified SDL3.Raw.Power as Raw

sdlGetPowerInfo :: MonadIO m => Maybe (Ptr CInt) -> Maybe (Ptr CInt) -> m (Maybe SDLPowerState)
sdlGetPowerInfo mSecondsPtr mPercentPtr = liftIO $ do
  let secPtr = maybe nullPtr id mSecondsPtr
      pctPtr = maybe nullPtr id mPercentPtr
  stateCInt <- Raw.sdlGetPowerInfoRaw secPtr pctPtr
  let state = toEnum (fromIntegral stateCInt) :: SDLPowerState
  return $ if state == SDL_POWERSTATE_ERROR then Nothing else Just state
