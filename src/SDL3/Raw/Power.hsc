{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : SDL3.Raw.Power
-- Description : Raw power management functions for SDL3
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Low-level power bindings. Prefer 'SDL3.Wrapped.Power' or 'SDL3.Power' unless
-- you need direct FFI access.

#include <SDL3/SDL_power.h>

module SDL3.Raw.Power
  ( SDLPowerState(..)
  , pattern SDL_POWERSTATE_ERROR
  , pattern SDL_POWERSTATE_UNKNOWN
  , pattern SDL_POWERSTATE_ON_BATTERY
  , pattern SDL_POWERSTATE_NO_BATTERY
  , pattern SDL_POWERSTATE_CHARGING
  , pattern SDL_POWERSTATE_CHARGED
  , sdlGetPowerInfoRaw
  ) where

import Foreign (Ptr, Storable)
import Foreign.C.Types (CInt(..))

newtype SDLPowerState = SDLPowerState CInt
  deriving newtype (Eq, Ord, Storable, Enum)

pattern SDL_POWERSTATE_ERROR = SDLPowerState (#const SDL_POWERSTATE_ERROR)
pattern SDL_POWERSTATE_UNKNOWN = SDLPowerState (#const SDL_POWERSTATE_UNKNOWN)
pattern SDL_POWERSTATE_ON_BATTERY = SDLPowerState (#const SDL_POWERSTATE_ON_BATTERY)
pattern SDL_POWERSTATE_NO_BATTERY = SDLPowerState (#const SDL_POWERSTATE_NO_BATTERY)
pattern SDL_POWERSTATE_CHARGING = SDLPowerState (#const SDL_POWERSTATE_CHARGING)
pattern SDL_POWERSTATE_CHARGED = SDLPowerState (#const SDL_POWERSTATE_CHARGED)

instance Show SDLPowerState where
  show ps
    | ps == SDL_POWERSTATE_ERROR = "SDL_POWERSTATE_ERROR"
    | ps == SDL_POWERSTATE_UNKNOWN = "SDL_POWERSTATE_UNKNOWN"
    | ps == SDL_POWERSTATE_ON_BATTERY = "SDL_POWERSTATE_ON_BATTERY"
    | ps == SDL_POWERSTATE_NO_BATTERY = "SDL_POWERSTATE_NO_BATTERY"
    | ps == SDL_POWERSTATE_CHARGING = "SDL_POWERSTATE_CHARGING"
    | ps == SDL_POWERSTATE_CHARGED = "SDL_POWERSTATE_CHARGED"
    | otherwise = "SDLPowerState " ++ show n
    where
      SDLPowerState n = ps

foreign import ccall unsafe "SDL_GetPowerInfo"
  sdlGetPowerInfoRaw :: Ptr CInt -> Ptr CInt -> IO CInt
