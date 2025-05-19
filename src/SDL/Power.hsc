-- SDL/Power.hsc
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : SDL.Power
-- Description : Power management functions for SDL3.
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- This module provides Haskell bindings to the SDL3 power management functionality.

#include <SDL3/SDL_power.h>

module SDL.Power
  (
    -- * Types
    SDLPowerState(..)

    -- * Patterns / Constants
  , pattern SDL_POWERSTATE_ERROR
  , pattern SDL_POWERSTATE_UNKNOWN
  , pattern SDL_POWERSTATE_ON_BATTERY
  , pattern SDL_POWERSTATE_NO_BATTERY
  , pattern SDL_POWERSTATE_CHARGING
  , pattern SDL_POWERSTATE_CHARGED

    -- * Functions
  , sdlGetPowerInfo
  ) where

import Foreign (Ptr, Storable, nullPtr)
import Foreign.C.Types (CInt(..))

-- | The basic state for the system's power supply.
newtype SDLPowerState = SDLPowerState CInt
  deriving newtype (Eq, Ord, Storable, Enum) -- Deriving common instances

-- Define pattern synonyms using the C constants
pattern SDL_POWERSTATE_ERROR :: SDLPowerState
pattern SDL_POWERSTATE_ERROR     = SDLPowerState (#{const SDL_POWERSTATE_ERROR})
pattern SDL_POWERSTATE_UNKNOWN :: SDLPowerState
pattern SDL_POWERSTATE_UNKNOWN   = SDLPowerState #{const SDL_POWERSTATE_UNKNOWN}
pattern SDL_POWERSTATE_ON_BATTERY :: SDLPowerState
pattern SDL_POWERSTATE_ON_BATTERY = SDLPowerState #{const SDL_POWERSTATE_ON_BATTERY}
pattern SDL_POWERSTATE_NO_BATTERY :: SDLPowerState
pattern SDL_POWERSTATE_NO_BATTERY = SDLPowerState #{const SDL_POWERSTATE_NO_BATTERY}
pattern SDL_POWERSTATE_CHARGING :: SDLPowerState
pattern SDL_POWERSTATE_CHARGING  = SDLPowerState #{const SDL_POWERSTATE_CHARGING}
pattern SDL_POWERSTATE_CHARGED :: SDLPowerState
pattern SDL_POWERSTATE_CHARGED   = SDLPowerState #{const SDL_POWERSTATE_CHARGED}

-- Provide a more informative Show instance using patterns
instance Show SDLPowerState where
  show ps
    | ps == SDL_POWERSTATE_ERROR     = "SDL_POWERSTATE_ERROR"
    | ps == SDL_POWERSTATE_UNKNOWN   = "SDL_POWERSTATE_UNKNOWN"
    | ps == SDL_POWERSTATE_ON_BATTERY = "SDL_POWERSTATE_ON_BATTERY"
    | ps == SDL_POWERSTATE_NO_BATTERY = "SDL_POWERSTATE_NO_BATTERY"
    | ps == SDL_POWERSTATE_CHARGING  = "SDL_POWERSTATE_CHARGING"
    | ps == SDL_POWERSTATE_CHARGED   = "SDL_POWERSTATE_CHARGED"
    | otherwise                    = "SDLPowerState " ++ show n -- Fallback
    where (SDLPowerState n) = ps -- Unpack for the fallback case

-- FFI Import using c_ prefix convention
foreign import ccall unsafe "SDL_GetPowerInfo"
  c_sdlGetPowerInfo :: Ptr CInt -> Ptr CInt -> IO CInt -- Return raw CInt

-- Haskell Wrapper
-- | Get the current power supply details.
-- Returns the power state and optionally fills in seconds and percent of battery life remaining.
-- Passing Nothing for the Ptr arguments means you don't want that value.
-- Returns Nothing if SDL reports an error state.
sdlGetPowerInfo :: Maybe (Ptr CInt) {-^ Pointer to store seconds remaining, or Nothing -} ->
                   Maybe (Ptr CInt) {-^ Pointer to store percentage remaining, or Nothing -} ->
                   IO (Maybe SDLPowerState) {-^ Power state, or Nothing on error -}
sdlGetPowerInfo mSecondsPtr mPercentPtr = do
  let secPtr = maybe nullPtr id mSecondsPtr
      pctPtr = maybe nullPtr id mPercentPtr
  stateCInt <- c_sdlGetPowerInfo secPtr pctPtr
  let state = toEnum (fromIntegral stateCInt) :: SDLPowerState
  return $ if state == SDL_POWERSTATE_ERROR then Nothing else Just state
