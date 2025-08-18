-- SDL/Pen.hsc
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : SDL.Pen
-- Description : SDL pen event handling functions
-- Copyright   : Kyle Lukaszek, 2025
-- License     : BSD3
--
-- This module provides bindings to the SDL3 pen API, allowing Haskell applications
-- to handle pressure-sensitive pen input (stylus and/or eraser) for devices such as
-- drawing tablets and suitably equipped mobile/tablet devices.
--
-- To use these functions, SDL must be initialized and the application should handle
-- SDL_EVENT_PEN_* events.

#include <SDL3/SDL_pen.h>

module SDL.Pen
  ( -- * Types
    SDLPenID
  , SDLPenInputFlags(..)
  , SDLPenAxis(..)

    -- * Constants / Patterns
  , pattern SDL_PEN_MOUSEID
  , pattern SDL_PEN_TOUCHID
  , pattern SDL_PEN_INPUT_DOWN
  , pattern SDL_PEN_INPUT_BUTTON_1
  , pattern SDL_PEN_INPUT_BUTTON_2
  , pattern SDL_PEN_INPUT_BUTTON_3
  , pattern SDL_PEN_INPUT_BUTTON_4
  , pattern SDL_PEN_INPUT_BUTTON_5
  , pattern SDL_PEN_INPUT_ERASER_TIP
  , pattern SDL_PEN_AXIS_PRESSURE
  , pattern SDL_PEN_AXIS_XTILT
  , pattern SDL_PEN_AXIS_YTILT
  , pattern SDL_PEN_AXIS_DISTANCE
  , pattern SDL_PEN_AXIS_ROTATION
  , pattern SDL_PEN_AXIS_SLIDER
  , pattern SDL_PEN_AXIS_TANGENTIAL_PRESSURE
  , pattern SDL_PEN_AXIS_COUNT
  ) where

import Foreign.C.Types (CInt(..))
import Foreign.Storable (Storable)
import Data.Word (Word32)
import Data.Bits (Bits)
import SDL.Mouse (SDLMouseID)
import SDL.Touch (SDLTouchID)

-- | SDL pen instance ID.
-- Zero signifies an invalid/null device.
-- Using a type alias as it's just an identifier.
type SDLPenID = Word32

-- | Pen input flags as reported in pen events' pen_state field.
-- Wraps a Word32 as the C #defines are flags (bitmasks).
newtype SDLPenInputFlags = SDLPenInputFlags Word32
  deriving newtype (Show, Eq, Bits, Num, Storable) -- Added Num for convenience with bitwise OR

pattern SDL_PEN_INPUT_DOWN :: SDLPenInputFlags
pattern SDL_PEN_INPUT_DOWN = SDLPenInputFlags #{const SDL_PEN_INPUT_DOWN}
pattern SDL_PEN_INPUT_BUTTON_1 :: SDLPenInputFlags
pattern SDL_PEN_INPUT_BUTTON_1 = SDLPenInputFlags #{const SDL_PEN_INPUT_BUTTON_1}
pattern SDL_PEN_INPUT_BUTTON_2 :: SDLPenInputFlags
pattern SDL_PEN_INPUT_BUTTON_2 = SDLPenInputFlags #{const SDL_PEN_INPUT_BUTTON_2}
pattern SDL_PEN_INPUT_BUTTON_3 :: SDLPenInputFlags
pattern SDL_PEN_INPUT_BUTTON_3 = SDLPenInputFlags #{const SDL_PEN_INPUT_BUTTON_3}
pattern SDL_PEN_INPUT_BUTTON_4 :: SDLPenInputFlags
pattern SDL_PEN_INPUT_BUTTON_4 = SDLPenInputFlags #{const SDL_PEN_INPUT_BUTTON_4}
pattern SDL_PEN_INPUT_BUTTON_5 :: SDLPenInputFlags
pattern SDL_PEN_INPUT_BUTTON_5 = SDLPenInputFlags #{const SDL_PEN_INPUT_BUTTON_5}
pattern SDL_PEN_INPUT_ERASER_TIP :: SDLPenInputFlags
pattern SDL_PEN_INPUT_ERASER_TIP = SDLPenInputFlags #{const SDL_PEN_INPUT_ERASER_TIP}

-- | Pen axis indices for SDL_PenAxisEvent.
-- Wraps CInt as the C type is a standard enum.
newtype SDLPenAxis = SDLPenAxis CInt
  deriving newtype (Show, Eq, Ord, Storable, Enum)

pattern SDL_PEN_AXIS_PRESSURE :: SDLPenAxis
pattern SDL_PEN_AXIS_PRESSURE = SDLPenAxis #{const SDL_PEN_AXIS_PRESSURE}
pattern SDL_PEN_AXIS_XTILT :: SDLPenAxis
pattern SDL_PEN_AXIS_XTILT = SDLPenAxis #{const SDL_PEN_AXIS_XTILT}
pattern SDL_PEN_AXIS_YTILT :: SDLPenAxis
pattern SDL_PEN_AXIS_YTILT = SDLPenAxis #{const SDL_PEN_AXIS_YTILT}
pattern SDL_PEN_AXIS_DISTANCE :: SDLPenAxis
pattern SDL_PEN_AXIS_DISTANCE = SDLPenAxis #{const SDL_PEN_AXIS_DISTANCE}
pattern SDL_PEN_AXIS_ROTATION :: SDLPenAxis
pattern SDL_PEN_AXIS_ROTATION = SDLPenAxis #{const SDL_PEN_AXIS_ROTATION}
pattern SDL_PEN_AXIS_SLIDER :: SDLPenAxis
pattern SDL_PEN_AXIS_SLIDER = SDLPenAxis #{const SDL_PEN_AXIS_SLIDER}
pattern SDL_PEN_AXIS_TANGENTIAL_PRESSURE :: SDLPenAxis
pattern SDL_PEN_AXIS_TANGENTIAL_PRESSURE = SDLPenAxis #{const SDL_PEN_AXIS_TANGENTIAL_PRESSURE}
pattern SDL_PEN_AXIS_COUNT :: SDLPenAxis
pattern SDL_PEN_AXIS_COUNT = SDLPenAxis #{const SDL_PEN_AXIS_COUNT}

-- | Mouse ID for mouse events simulated with pen input.
pattern SDL_PEN_MOUSEID :: SDLMouseID
pattern SDL_PEN_MOUSEID = #{const SDL_PEN_MOUSEID}

-- | Touch ID for touch events simulated with pen input.
pattern SDL_PEN_TOUCHID :: SDLTouchID
pattern SDL_PEN_TOUCHID = #{const SDL_PEN_TOUCHID}
