{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

#include <SDL3/SDL_pen.h>

module SDL3.Raw.Pen
  ( SDLPenID
  , SDLPenInputFlags(..)
  , SDLPenAxis(..)
  , pattern SDL_PEN_MOUSEID
  , pattern SDL_PEN_TOUCHID
  , pattern SDL_PEN_INPUT_DOWN
  , pattern SDL_PEN_INPUT_BUTTON_1
  , pattern SDL_PEN_INPUT_BUTTON_2
  , pattern SDL_PEN_INPUT_BUTTON_3
  , pattern SDL_PEN_INPUT_BUTTON_4
  , pattern SDL_PEN_INPUT_BUTTON_5
  , pattern SDL_PEN_INPUT_ERASER_TIP
  , pattern SDL_PEN_INPUT_IN_PROXIMITY
  , SDLPenDeviceType(..)
  , pattern SDL_PEN_DEVICE_TYPE_INVALID
  , pattern SDL_PEN_DEVICE_TYPE_UNKNOWN
  , pattern SDL_PEN_DEVICE_TYPE_DIRECT
  , pattern SDL_PEN_DEVICE_TYPE_INDIRECT
  , sdlGetPenDeviceTypeRaw
  , pattern SDL_PEN_AXIS_PRESSURE
  , pattern SDL_PEN_AXIS_XTILT
  , pattern SDL_PEN_AXIS_YTILT
  , pattern SDL_PEN_AXIS_DISTANCE
  , pattern SDL_PEN_AXIS_ROTATION
  , pattern SDL_PEN_AXIS_SLIDER
  , pattern SDL_PEN_AXIS_TANGENTIAL_PRESSURE
  , pattern SDL_PEN_AXIS_COUNT
  ) where

import Data.Bits (Bits)
import Data.Word (Word32)
import Foreign.C.Types (CInt(..))
import Foreign.Storable (Storable)
import SDL3.Raw.Mouse (SDLMouseID)
import SDL3.Raw.Touch (SDLTouchID)

type SDLPenID = Word32

newtype SDLPenInputFlags = SDLPenInputFlags Word32
  deriving newtype (Show, Eq, Bits, Num, Storable)

pattern SDL_PEN_INPUT_DOWN = SDLPenInputFlags #{const SDL_PEN_INPUT_DOWN}
pattern SDL_PEN_INPUT_BUTTON_1 = SDLPenInputFlags #{const SDL_PEN_INPUT_BUTTON_1}
pattern SDL_PEN_INPUT_BUTTON_2 = SDLPenInputFlags #{const SDL_PEN_INPUT_BUTTON_2}
pattern SDL_PEN_INPUT_BUTTON_3 = SDLPenInputFlags #{const SDL_PEN_INPUT_BUTTON_3}
pattern SDL_PEN_INPUT_BUTTON_4 = SDLPenInputFlags #{const SDL_PEN_INPUT_BUTTON_4}
pattern SDL_PEN_INPUT_BUTTON_5 = SDLPenInputFlags #{const SDL_PEN_INPUT_BUTTON_5}
pattern SDL_PEN_INPUT_ERASER_TIP = SDLPenInputFlags #{const SDL_PEN_INPUT_ERASER_TIP}
pattern SDL_PEN_INPUT_IN_PROXIMITY = SDLPenInputFlags #{const SDL_PEN_INPUT_IN_PROXIMITY}

newtype SDLPenDeviceType = SDLPenDeviceType CInt
  deriving newtype (Show, Eq, Ord, Storable, Enum)

pattern SDL_PEN_DEVICE_TYPE_INVALID = SDLPenDeviceType (-1)
pattern SDL_PEN_DEVICE_TYPE_UNKNOWN = SDLPenDeviceType #{const SDL_PEN_DEVICE_TYPE_UNKNOWN}
pattern SDL_PEN_DEVICE_TYPE_DIRECT = SDLPenDeviceType #{const SDL_PEN_DEVICE_TYPE_DIRECT}
pattern SDL_PEN_DEVICE_TYPE_INDIRECT = SDLPenDeviceType #{const SDL_PEN_DEVICE_TYPE_INDIRECT}

foreign import ccall unsafe "SDL_GetPenDeviceType"
  sdlGetPenDeviceTypeRaw :: SDLPenID -> IO SDLPenDeviceType

newtype SDLPenAxis = SDLPenAxis CInt
  deriving newtype (Show, Eq, Ord, Storable, Enum)

pattern SDL_PEN_AXIS_PRESSURE = SDLPenAxis #{const SDL_PEN_AXIS_PRESSURE}
pattern SDL_PEN_AXIS_XTILT = SDLPenAxis #{const SDL_PEN_AXIS_XTILT}
pattern SDL_PEN_AXIS_YTILT = SDLPenAxis #{const SDL_PEN_AXIS_YTILT}
pattern SDL_PEN_AXIS_DISTANCE = SDLPenAxis #{const SDL_PEN_AXIS_DISTANCE}
pattern SDL_PEN_AXIS_ROTATION = SDLPenAxis #{const SDL_PEN_AXIS_ROTATION}
pattern SDL_PEN_AXIS_SLIDER = SDLPenAxis #{const SDL_PEN_AXIS_SLIDER}
pattern SDL_PEN_AXIS_TANGENTIAL_PRESSURE = SDLPenAxis #{const SDL_PEN_AXIS_TANGENTIAL_PRESSURE}
pattern SDL_PEN_AXIS_COUNT = SDLPenAxis #{const SDL_PEN_AXIS_COUNT}

pattern SDL_PEN_MOUSEID :: SDLMouseID
pattern SDL_PEN_MOUSEID = #{const SDL_PEN_MOUSEID}

pattern SDL_PEN_TOUCHID :: SDLTouchID
pattern SDL_PEN_TOUCHID = #{const SDL_PEN_TOUCHID}
