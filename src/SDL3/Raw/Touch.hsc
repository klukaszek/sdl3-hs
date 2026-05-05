{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

#include <SDL3/SDL_touch.h>

module SDL3.Raw.Touch
  ( SDLTouchID
  , SDLFingerID
  , SDLTouchDeviceType(..)
  , pattern SDL_TOUCH_DEVICE_INVALID
  , pattern SDL_TOUCH_DEVICE_DIRECT
  , pattern SDL_TOUCH_DEVICE_INDIRECT_ABSOLUTE
  , pattern SDL_TOUCH_DEVICE_INDIRECT_RELATIVE
  , SDLFinger(..)
  , pattern SDL_TOUCH_MOUSEID
  , pattern SDL_MOUSE_TOUCHID
  , sdlGetTouchDevicesRaw
  , sdlGetTouchDeviceNameRaw
  , sdlGetTouchDeviceTypeRaw
  , sdlGetTouchFingersRaw
  ) where

import Data.Word (Word64)
import Foreign.C.String (CString)
import Foreign.C.Types
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import SDL3.Raw.Mouse (SDLMouseID)

type SDLTouchID = Word64
type SDLFingerID = Word64

newtype SDLTouchDeviceType = SDLTouchDeviceType CInt
  deriving newtype (Show, Eq, Ord, Storable, Enum, Bounded)

pattern SDL_TOUCH_DEVICE_INVALID = SDLTouchDeviceType (#const SDL_TOUCH_DEVICE_INVALID)
pattern SDL_TOUCH_DEVICE_DIRECT = SDLTouchDeviceType (#const SDL_TOUCH_DEVICE_DIRECT)
pattern SDL_TOUCH_DEVICE_INDIRECT_ABSOLUTE = SDLTouchDeviceType (#const SDL_TOUCH_DEVICE_INDIRECT_ABSOLUTE)
pattern SDL_TOUCH_DEVICE_INDIRECT_RELATIVE = SDLTouchDeviceType (#const SDL_TOUCH_DEVICE_INDIRECT_RELATIVE)

data SDLFinger = SDLFinger
  { fingerID :: SDLFingerID
  , fingerX :: CFloat
  , fingerY :: CFloat
  , fingerPressure :: CFloat
  } deriving (Eq, Show)

instance Storable SDLFinger where
  sizeOf _ = #{size SDL_Finger}
  alignment _ = #{alignment SDL_Finger}
  peek ptr = do
    fingerID <- #{peek SDL_Finger, id} ptr
    fingerX <- #{peek SDL_Finger, x} ptr
    fingerY <- #{peek SDL_Finger, y} ptr
    fingerPressure <- #{peek SDL_Finger, pressure} ptr
    return SDLFinger {..}
  poke ptr SDLFinger {..} = do
    #{poke SDL_Finger, id} ptr fingerID
    #{poke SDL_Finger, x} ptr fingerX
    #{poke SDL_Finger, y} ptr fingerY
    #{poke SDL_Finger, pressure} ptr fingerPressure

pattern SDL_TOUCH_MOUSEID :: SDLMouseID
pattern SDL_TOUCH_MOUSEID = #const SDL_TOUCH_MOUSEID

pattern SDL_MOUSE_TOUCHID :: SDLTouchID
pattern SDL_MOUSE_TOUCHID = #const SDL_MOUSE_TOUCHID

foreign import ccall unsafe "SDL_GetTouchDevices"
  sdlGetTouchDevicesRaw :: Ptr CInt -> IO (Ptr SDLTouchID)

foreign import ccall unsafe "SDL_GetTouchDeviceName"
  sdlGetTouchDeviceNameRaw :: SDLTouchID -> IO CString

foreign import ccall unsafe "SDL_GetTouchDeviceType"
  sdlGetTouchDeviceTypeRaw :: SDLTouchID -> IO CInt

foreign import ccall unsafe "SDL_GetTouchFingers"
  sdlGetTouchFingersRaw :: SDLTouchID -> Ptr CInt -> IO (Ptr (Ptr SDLFinger))
