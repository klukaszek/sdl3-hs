{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- SDL/Power.hsc
{-|
Module      : SDL.Power
Description : Power management functions for SDL3.
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

This module provides Haskell bindings to the SDL3 power management functionality.
-}

module SDL.Power
  ( 
    -- * Power State
    SDLPowerState(..)
  , sdlGetPowerInfo
  ) where

#include <SDL3/SDL_power.h>

import Foreign
import Foreign.C

-- | The basic state for the system's power supply.
newtype SDLPowerState = SDLPowerState CInt
  deriving (Eq, Bits, Enum)

instance Show SDLPowerState where
  show (SDLPowerState n) = case n of
    (-1) -> "SDL_POWERSTATE_ERROR"
    0    -> "SDL_POWERSTATE_UNKNOWN"
    1    -> "SDL_POWERSTATE_ON_BATTERY"
    2    -> "SDL_POWERSTATE_NO_BATTERY"
    3    -> "SDL_POWERSTATE_CHARGING"
    4    -> "SDL_POWERSTATE_CHARGED"
    _    -> "SDLPowerState " ++ show n  -- Fallback for unknown values

#{enum SDLPowerState, SDLPowerState
 , sdlPowerStateERROR     = SDL_POWERSTATE_ERROR
 , sdlPowerStateUNKNOWN   = SDL_POWERSTATE_UNKNOWN
 , sdlPowerStateONBATTERY = SDL_POWERSTATE_ON_BATTERY
 , sdlPowerStateNOBATTERY = SDL_POWERSTATE_NO_BATTERY
 , sdlPowerStateCHARGING  = SDL_POWERSTATE_CHARGING
 , sdlPowerStateCHARGED   = SDL_POWERSTATE_CHARGED
 }

-- FFI Import

foreign import ccall unsafe "SDL_GetPowerInfo" sdlGetPowerInfo_ :: Ptr CInt -> Ptr CInt -> IO SDLPowerState

-- Haskell Wrapper

-- | Get the current power supply details.
-- Returns the power state and optionally fills in seconds and percent of battery life remaining.
sdlGetPowerInfo :: Maybe (Ptr CInt) -> Maybe (Ptr CInt) -> IO (Maybe SDLPowerState)
sdlGetPowerInfo secondsPtr percentPtr = do
  let secPtr = maybe nullPtr id secondsPtr
      pctPtr = maybe nullPtr id percentPtr
  state <- sdlGetPowerInfo_ secPtr pctPtr
  return $ if state == sdlPowerStateERROR then Nothing else Just state
