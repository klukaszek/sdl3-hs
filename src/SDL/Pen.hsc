{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : SDL.Pen
Description : SDL pen event handling functions
Copyright   : Kyle Lukaszek, 2025
License     : BSD3

This module provides bindings to the SDL3 pen API, allowing Haskell applications
to handle pressure-sensitive pen input (stylus and/or eraser) for devices such as
drawing tablets and suitably equipped mobile/tablet devices.

To use these functions, SDL must be initialized and the application should handle
SDL_EVENT_PEN_* events.
-}

module SDL.Pen
  ( -- * Types
    SDLPenID(..)
  , SDLPenInputFlags(..)
  , SDLPenAxis(..)
  
    -- * Constants
  , sdlPenMouseID
  , sdlPenTouchID
  , sdlPenInputDown
  , sdlPenInputButton1
  , sdlPenInputButton2
  , sdlPenInputButton3
  , sdlPenInputButton4
  , sdlPenInputButton5
  , sdlPenInputEraserTip
  ) where

import Foreign
import Foreign.C.Types
import Data.Word
import Data.Bits
import SDL.Mouse
import SDL.Touch

#include <SDL3/SDL_pen.h>

-- | SDL pen instance ID
-- Zero signifies an invalid/null device
type SDLPenID = Word32

-- | Pen input flags as reported in pen events' pen_state field
newtype SDLPenInputFlags = SDLPenInputFlags Word32
  deriving (Show, Eq, Bits)

-- | Pen axis indices for SDL_PenAxisEvent
data SDLPenAxis
  = SDL_PEN_AXIS_PRESSURE            -- ^ Pen pressure (0 to 1.0)
  | SDL_PEN_AXIS_XTILT              -- ^ Horizontal tilt (-90.0 to 90.0)
  | SDL_PEN_AXIS_YTILT              -- ^ Vertical tilt (-90.0 to 90.0)
  | SDL_PEN_AXIS_DISTANCE           -- ^ Distance to surface (0.0 to 1.0)
  | SDL_PEN_AXIS_ROTATION           -- ^ Barrel rotation (-180.0 to 179.9)
  | SDL_PEN_AXIS_SLIDER             -- ^ Finger wheel/slider (0 to 1.0)
  | SDL_PEN_AXIS_TANGENTIAL_PRESSURE -- ^ Barrel pressure
  | SDL_PEN_AXIS_COUNT              -- ^ Total known pen axes
  deriving (Show, Eq, Enum)

-- | Mouse ID for mouse events simulated with pen input
sdlPenMouseID :: SDLMouseID
sdlPenMouseID = 0xFFFFFFFE

-- | Touch ID for touch events simulated with pen input
sdlPenTouchID :: SDLTouchID
sdlPenTouchID = 0xFFFFFFFFFFFFFFFE

-- | Pen input flag constants
sdlPenInputDown :: SDLPenInputFlags
sdlPenInputDown = SDLPenInputFlags #{const SDL_PEN_INPUT_DOWN}

sdlPenInputButton1 :: SDLPenInputFlags
sdlPenInputButton1 = SDLPenInputFlags #{const SDL_PEN_INPUT_BUTTON_1}

sdlPenInputButton2 :: SDLPenInputFlags
sdlPenInputButton2 = SDLPenInputFlags #{const SDL_PEN_INPUT_BUTTON_2}

sdlPenInputButton3 :: SDLPenInputFlags
sdlPenInputButton3 = SDLPenInputFlags #{const SDL_PEN_INPUT_BUTTON_3}

sdlPenInputButton4 :: SDLPenInputFlags
sdlPenInputButton4 = SDLPenInputFlags #{const SDL_PEN_INPUT_BUTTON_4}

sdlPenInputButton5 :: SDLPenInputFlags
sdlPenInputButton5 = SDLPenInputFlags #{const SDL_PEN_INPUT_BUTTON_5}

sdlPenInputEraserTip :: SDLPenInputFlags
sdlPenInputEraserTip = SDLPenInputFlags #{const SDL_PEN_INPUT_ERASER_TIP}
