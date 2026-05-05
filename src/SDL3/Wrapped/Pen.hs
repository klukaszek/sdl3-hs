{-# LANGUAGE PatternSynonyms #-}

module SDL3.Wrapped.Pen
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
  , sdlGetPenDeviceType
  , pattern SDL_PEN_AXIS_PRESSURE
  , pattern SDL_PEN_AXIS_XTILT
  , pattern SDL_PEN_AXIS_YTILT
  , pattern SDL_PEN_AXIS_DISTANCE
  , pattern SDL_PEN_AXIS_ROTATION
  , pattern SDL_PEN_AXIS_SLIDER
  , pattern SDL_PEN_AXIS_TANGENTIAL_PRESSURE
  , pattern SDL_PEN_AXIS_COUNT
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import SDL3.Raw.Pen
  ( SDLPenAxis(..)
  , SDLPenDeviceType(..)
  , SDLPenID
  , SDLPenInputFlags(..)
  , pattern SDL_PEN_AXIS_COUNT
  , pattern SDL_PEN_AXIS_DISTANCE
  , pattern SDL_PEN_AXIS_PRESSURE
  , pattern SDL_PEN_AXIS_ROTATION
  , pattern SDL_PEN_AXIS_SLIDER
  , pattern SDL_PEN_AXIS_TANGENTIAL_PRESSURE
  , pattern SDL_PEN_AXIS_XTILT
  , pattern SDL_PEN_AXIS_YTILT
  , pattern SDL_PEN_DEVICE_TYPE_DIRECT
  , pattern SDL_PEN_DEVICE_TYPE_INDIRECT
  , pattern SDL_PEN_DEVICE_TYPE_INVALID
  , pattern SDL_PEN_DEVICE_TYPE_UNKNOWN
  , pattern SDL_PEN_INPUT_BUTTON_1
  , pattern SDL_PEN_INPUT_BUTTON_2
  , pattern SDL_PEN_INPUT_BUTTON_3
  , pattern SDL_PEN_INPUT_BUTTON_4
  , pattern SDL_PEN_INPUT_BUTTON_5
  , pattern SDL_PEN_INPUT_DOWN
  , pattern SDL_PEN_INPUT_ERASER_TIP
  , pattern SDL_PEN_INPUT_IN_PROXIMITY
  , pattern SDL_PEN_MOUSEID
  , pattern SDL_PEN_TOUCHID
  )
import qualified SDL3.Raw.Pen as Raw

sdlGetPenDeviceType :: MonadIO m => SDLPenID -> m SDLPenDeviceType
sdlGetPenDeviceType = liftIO . Raw.sdlGetPenDeviceTypeRaw
