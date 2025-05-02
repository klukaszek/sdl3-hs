{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : SDL.Joystick
Description : SDL joystick management
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

SDL joystick support provides low-level access to joystick devices. This module
offers functions to manage joystick devices, query their properties, and handle
input from axes, buttons, hats, and other features.

Basic usage:
- Initialize the subsystem (SDL_INIT_JOYSTICK)
- Open a joystick using 'sdlOpenJoystick'
- Poll or handle events with 'sdlUpdateJoysticks'
- Close with 'sdlCloseJoystick'

For a higher-level interface, consider the SDL Gamepad API.
-}

module SDL.Joystick
  ( -- * Types
    SDLJoystick(..)
  , SDLJoystickID(..)
  , SDLJoystickType(..)
  , SDLJoystickConnectionState(..)
  , SDLVirtualJoystickTouchpadDesc(..)
  , SDLVirtualJoystickSensorDesc(..)
  , SDLVirtualJoystickDesc(..)

    -- * Constants
  , sdlJoystickAxisMax
  , sdlJoystickAxisMin
  , sdlHatCentered
  , sdlHatUp
  , sdlHatRight
  , sdlHatDown
  , sdlHatLeft
  , sdlHatRightUp
  , sdlHatRightDown
  , sdlHatLeftUp
  , sdlHatLeftDown

    -- * Joystick Functions
  , sdlLockJoysticks
  , sdlUnlockJoysticks
  , sdlHasJoystick
  , sdlGetJoysticks
  , sdlGetJoystickNameForID
  , sdlGetJoystickPathForID
  , sdlGetJoystickPlayerIndexForID
  , sdlGetJoystickGUIDForID
  , sdlGetJoystickVendorForID
  , sdlGetJoystickProductForID
  , sdlGetJoystickProductVersionForID
  , sdlGetJoystickTypeForID
  , sdlOpenJoystick
  , sdlGetJoystickFromID
  , sdlGetJoystickFromPlayerIndex
  , sdlAttachVirtualJoystick
  , sdlDetachVirtualJoystick
  , sdlIsJoystickVirtual
  , sdlSetJoystickVirtualAxis
  , sdlSetJoystickVirtualBall
  , sdlSetJoystickVirtualButton
  , sdlSetJoystickVirtualHat
  , sdlSetJoystickVirtualTouchpad
  , sdlSendJoystickVirtualSensorData
  , sdlGetJoystickProperties
  , sdlGetJoystickName
  , sdlGetJoystickPath
  , sdlGetJoystickPlayerIndex
  , sdlSetJoystickPlayerIndex
  , sdlGetJoystickGUID
  , sdlGetJoystickVendor
  , sdlGetJoystickProduct
  , sdlGetJoystickProductVersion
  , sdlGetJoystickFirmwareVersion
  , sdlGetJoystickSerial
  , sdlGetJoystickType
  , sdlGetJoystickGUIDInfo
  , sdlJoystickConnected
  , sdlGetJoystickID
  , sdlGetNumJoystickAxes
  , sdlGetNumJoystickBalls
  , sdlGetNumJoystickHats
  , sdlGetNumJoystickButtons
  , sdlSetJoystickEventsEnabled
  , sdlJoystickEventsEnabled
  , sdlUpdateJoysticks
  , sdlGetJoystickAxis
  , sdlGetJoystickAxisInitialState
  , sdlGetJoystickBall
  , sdlGetJoystickHat
  , sdlGetJoystickButton
  , sdlRumbleJoystick
  , sdlRumbleJoystickTriggers
  , sdlSetJoystickLED
  , sdlSendJoystickEffect
  , sdlCloseJoystick
  , sdlGetJoystickConnectionState
  , sdlGetJoystickPowerInfo
  ) where

#include <SDL3/SDL_joystick.h>

import Foreign (Ptr, nullPtr, FunPtr, castPtr, plusPtr, toBool)
import Foreign.C.Types (CBool(..), CInt(..), CFloat(..), CUInt(..))
import Foreign.C.String (CString, peekCString, newCString)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (peekArray, pokeArray, newArray)
import Data.Word
import Data.Int (Int32, Int16)
import SDL.Sensor (SDLSensorType(..))
import SDL.Power (SDLPowerState(..))
import SDL.GUID (SDLGUID(..))
import SDL.Properties (SDLPropertiesID(..))
import Data.Bits

-- | Opaque type representing a joystick device
newtype SDLJoystick = SDLJoystick { unSDLJoystick :: Ptr SDLJoystick }
  deriving (Eq)

-- | Unique ID for a joystick
newtype SDLJoystickID = SDLJoystickID Word32
  deriving (Eq, Show)

instance Storable SDLJoystickID where
  sizeOf _ = sizeOf (undefined :: Word32)
  alignment _ = alignment (undefined :: Word32)
  peek ptr = SDLJoystickID <$> peek (castPtr ptr :: Ptr Word32)
  poke ptr (SDLJoystickID w) = poke (castPtr ptr :: Ptr Word32) w

-- | Joystick type enumeration
data SDLJoystickType
  = SDL_JOYSTICK_TYPE_UNKNOWN
  | SDL_JOYSTICK_TYPE_GAMEPAD
  | SDL_JOYSTICK_TYPE_WHEEL
  | SDL_JOYSTICK_TYPE_ARCADE_STICK
  | SDL_JOYSTICK_TYPE_FLIGHT_STICK
  | SDL_JOYSTICK_TYPE_DANCE_PAD
  | SDL_JOYSTICK_TYPE_GUITAR
  | SDL_JOYSTICK_TYPE_DRUM_KIT
  | SDL_JOYSTICK_TYPE_ARCADE_PAD
  | SDL_JOYSTICK_TYPE_THROTTLE
  | SDL_JOYSTICK_TYPE_COUNT
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Joystick connection state
data SDLJoystickConnectionState
  = SDLJoystickConnectionInvalid
  | SDLJoystickConnectionUnknown
  | SDLJoystickConnectionWired
  | SDLJoystickConnectionWireless
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Virtual joystick touchpad description
data SDLVirtualJoystickTouchpadDesc = SDLVirtualJoystickTouchpadDesc
  { touchpadNFingers :: Word16
  } deriving (Eq, Show)

instance Storable SDLVirtualJoystickTouchpadDesc where
  sizeOf _ = #size SDL_VirtualJoystickTouchpadDesc
  alignment _ = #alignment SDL_VirtualJoystickTouchpadDesc
  peek ptr = SDLVirtualJoystickTouchpadDesc
    <$> (#peek SDL_VirtualJoystickTouchpadDesc, nfingers) ptr
  poke ptr (SDLVirtualJoystickTouchpadDesc nf) = do
    (#poke SDL_VirtualJoystickTouchpadDesc, nfingers) ptr nf
    pokeArray (castPtr $ (#ptr SDL_VirtualJoystickTouchpadDesc, padding) ptr) [0, 0, 0 :: Word16]

-- | Virtual joystick sensor description
data SDLVirtualJoystickSensorDesc = SDLVirtualJoystickSensorDesc
  { sensorType :: SDLSensorType
  , sensorRate :: Float
  } deriving (Eq, Show)

instance Storable SDLVirtualJoystickSensorDesc where
  sizeOf _ = #size SDL_VirtualJoystickSensorDesc
  alignment _ = #alignment SDL_VirtualJoystickSensorDesc
  peek ptr = SDLVirtualJoystickSensorDesc
    <$> (#peek SDL_VirtualJoystickSensorDesc, type) ptr
    <*> (#peek SDL_VirtualJoystickSensorDesc, rate) ptr
  poke ptr (SDLVirtualJoystickSensorDesc typ rate) = do
    (#poke SDL_VirtualJoystickSensorDesc, type) ptr typ
    (#poke SDL_VirtualJoystickSensorDesc, rate) ptr rate

-- | Virtual joystick description
data SDLVirtualJoystickDesc = SDLVirtualJoystickDesc
  { virtualVersion      :: Word32
  , virtualType         :: SDLJoystickType
  , virtualVendorID     :: Word16
  , virtualProductID    :: Word16
  , virtualNAxes        :: Word16
  , virtualNButtons     :: Word16
  , virtualNBalls       :: Word16
  , virtualNHats        :: Word16
  , virtualNTouchpads   :: Word16
  , virtualNSensors     :: Word16
  , virtualButtonMask   :: Word32
  , virtualAxisMask     :: Word32
  , virtualName         :: String
  , virtualTouchpads    :: [SDLVirtualJoystickTouchpadDesc]
  , virtualSensors      :: [SDLVirtualJoystickSensorDesc]
  , virtualUserdata     :: Ptr ()
  , virtualUpdate       :: FunPtr (Ptr () -> IO ())
  , virtualSetPlayerIdx :: FunPtr (Ptr () -> CInt -> IO ())
  , virtualRumble       :: FunPtr (Ptr () -> Word16 -> Word16 -> IO CBool)
  , virtualRumbleTrig   :: FunPtr (Ptr () -> Word16 -> Word16 -> IO CBool)
  , virtualSetLED       :: FunPtr (Ptr () -> Word8 -> Word8 -> Word8 -> IO CBool)
  , virtualSendEffect   :: FunPtr (Ptr () -> Ptr () -> CInt -> IO CBool)
  , virtualSetSensors   :: FunPtr (Ptr () -> CBool -> IO CBool)
  , virtualCleanup      :: FunPtr (Ptr () -> IO ())
  } deriving (Eq, Show)

instance Storable SDLVirtualJoystickDesc where
  sizeOf _ = #size SDL_VirtualJoystickDesc
  alignment _ = #alignment SDL_VirtualJoystickDesc
  peek ptr = error "SDLVirtualJoystickDesc peek not implemented" -- Complex pointer handling required
  poke ptr (SDLVirtualJoystickDesc ver typ vid pid nax nbtn nball nhat ntouch nsens btnMask axisMask name touch sens ud upd setPl rum rumTrig led eff sensEn clean) = do
    (#poke SDL_VirtualJoystickDesc, version) ptr ver
    (#poke SDL_VirtualJoystickDesc, type) ptr (fromEnum typ)
    (#poke SDL_VirtualJoystickDesc, padding) ptr (0 :: Word16)
    (#poke SDL_VirtualJoystickDesc, vendor_id) ptr vid
    (#poke SDL_VirtualJoystickDesc, product_id) ptr pid
    (#poke SDL_VirtualJoystickDesc, naxes) ptr nax
    (#poke SDL_VirtualJoystickDesc, nbuttons) ptr nbtn
    (#poke SDL_VirtualJoystickDesc, nballs) ptr nball
    (#poke SDL_VirtualJoystickDesc, nhats) ptr nhat
    (#poke SDL_VirtualJoystickDesc, ntouchpads) ptr ntouch
    (#poke SDL_VirtualJoystickDesc, nsensors) ptr nsens
    pokeArray (castPtr $ (#ptr SDL_VirtualJoystickDesc, padding2) ptr) [0, 0 :: Word16]
    (#poke SDL_VirtualJoystickDesc, button_mask) ptr btnMask
    (#poke SDL_VirtualJoystickDesc, axis_mask) ptr axisMask
    (#poke SDL_VirtualJoystickDesc, name) ptr =<< newCString name
    (#poke SDL_VirtualJoystickDesc, touchpads) ptr =<< newArray touch
    (#poke SDL_VirtualJoystickDesc, sensors) ptr =<< newArray sens
    (#poke SDL_VirtualJoystickDesc, userdata) ptr ud
    (#poke SDL_VirtualJoystickDesc, Update) ptr upd
    (#poke SDL_VirtualJoystickDesc, SetPlayerIndex) ptr setPl
    (#poke SDL_VirtualJoystickDesc, Rumble) ptr rum
    (#poke SDL_VirtualJoystickDesc, RumbleTriggers) ptr rumTrig
    (#poke SDL_VirtualJoystickDesc, SetLED) ptr led
    (#poke SDL_VirtualJoystickDesc, SendEffect) ptr eff
    (#poke SDL_VirtualJoystickDesc, SetSensorsEnabled) ptr sensEn
    (#poke SDL_VirtualJoystickDesc, Cleanup) ptr clean

-- | Constants
sdlJoystickAxisMax :: Int16
sdlJoystickAxisMax = #const SDL_JOYSTICK_AXIS_MAX

sdlJoystickAxisMin :: Int16
sdlJoystickAxisMin = #const SDL_JOYSTICK_AXIS_MIN

sdlHatCentered :: Word8
sdlHatCentered = #const SDL_HAT_CENTERED

sdlHatUp :: Word8
sdlHatUp = #const SDL_HAT_UP

sdlHatRight :: Word8
sdlHatRight = #const SDL_HAT_RIGHT

sdlHatDown :: Word8
sdlHatDown = #const SDL_HAT_DOWN

sdlHatLeft :: Word8
sdlHatLeft = #const SDL_HAT_LEFT

sdlHatRightUp :: Word8
sdlHatRightUp = #const SDL_HAT_RIGHTUP

sdlHatRightDown :: Word8
sdlHatRightDown = #const SDL_HAT_RIGHTDOWN

sdlHatLeftUp :: Word8
sdlHatLeftUp = #const SDL_HAT_LEFTUP

sdlHatLeftDown :: Word8
sdlHatLeftDown = #const SDL_HAT_LEFTDOWN

-- | Lock joystick API
foreign import ccall "SDL_LockJoysticks"
  sdlLockJoysticks :: IO ()

-- | Unlock joystick API
foreign import ccall "SDL_UnlockJoysticks"
  sdlUnlockJoysticks :: IO ()

-- | Check if any joystick is connected
foreign import ccall "SDL_HasJoystick"
  sdlHasJoystick :: IO CBool

-- | Get list of connected joysticks
foreign import ccall "SDL_GetJoysticks"
  sdlGetJoysticksRaw :: Ptr CInt -> IO (Ptr Word32)

sdlGetJoysticks :: IO [SDLJoystickID]
sdlGetJoysticks = alloca $ \countPtr -> do
  ptr <- sdlGetJoysticksRaw countPtr
  count <- peek countPtr
  ids <- peekArray (fromIntegral count) ptr
  free ptr
  return $ map SDLJoystickID ids

-- | Get joystick name by ID
foreign import ccall "SDL_GetJoystickNameForID"
  sdlGetJoystickNameForID :: SDLJoystickID -> IO CString

-- | Get joystick path by ID
foreign import ccall "SDL_GetJoystickPathForID"
  sdlGetJoystickPathForID :: SDLJoystickID -> IO CString

-- | Get player index by ID
foreign import ccall "SDL_GetJoystickPlayerIndexForID"
  sdlGetJoystickPlayerIndexForID :: SDLJoystickID -> IO CInt

-- | Get GUID by ID
foreign import ccall "SDL_GetJoystickGUIDForID"
  sdlGetJoystickGUIDForID :: SDLJoystickID -> IO (Ptr SDLGUID)

-- | Get vendor by ID
foreign import ccall "SDL_GetJoystickVendorForID"
  sdlGetJoystickVendorForID :: SDLJoystickID -> IO Word16

-- | Get product by ID
foreign import ccall "SDL_GetJoystickProductForID"
  sdlGetJoystickProductForID :: SDLJoystickID -> IO Word16

-- | Get product version by ID
foreign import ccall "SDL_GetJoystickProductVersionForID"
  sdlGetJoystickProductVersionForID :: SDLJoystickID -> IO Word16

-- | Get joystick type by ID
foreign import ccall "SDL_GetJoystickTypeForID"
  sdlGetJoystickTypeForID :: SDLJoystickID -> IO CInt

-- | Open a joystick
foreign import ccall "SDL_OpenJoystick"
  sdlOpenJoystickRaw :: SDLJoystickID -> IO (Ptr SDLJoystick)

sdlOpenJoystick :: SDLJoystickID -> IO (Maybe SDLJoystick)
sdlOpenJoystick id = do
  ptr <- sdlOpenJoystickRaw id
  return $ if ptr == nullPtr then Nothing else Just (SDLJoystick ptr)

-- | Get joystick from ID
foreign import ccall "SDL_GetJoystickFromID"
  sdlGetJoystickFromID :: SDLJoystickID -> IO (Ptr SDLJoystick)

-- | Get joystick from player index
foreign import ccall "SDL_GetJoystickFromPlayerIndex"
  sdlGetJoystickFromPlayerIndex :: CInt -> IO (Ptr SDLJoystick)

-- | Attach virtual joystick
foreign import ccall "SDL_AttachVirtualJoystick"
  sdlAttachVirtualJoystick :: Ptr SDLVirtualJoystickDesc -> IO SDLJoystickID

-- | Detach virtual joystick
foreign import ccall "SDL_DetachVirtualJoystick"
  sdlDetachVirtualJoystick :: SDLJoystickID -> IO CBool

-- | Check if joystick is virtual
foreign import ccall "SDL_IsJoystickVirtual"
  sdlIsJoystickVirtual :: SDLJoystickID -> IO CBool

-- | Set virtual joystick axis
foreign import ccall "SDL_SetJoystickVirtualAxis"
  sdlSetJoystickVirtualAxis :: Ptr SDLJoystick -> CInt -> Int16 -> IO CBool

-- | Set virtual joystick ball
foreign import ccall "SDL_SetJoystickVirtualBall"
  sdlSetJoystickVirtualBall :: Ptr SDLJoystick -> CInt -> Int16 -> Int16 -> IO CBool

-- | Set virtual joystick button
foreign import ccall "SDL_SetJoystickVirtualButton"
  sdlSetJoystickVirtualButton :: Ptr SDLJoystick -> CInt -> CBool -> IO CBool

-- | Set virtual joystick hat
foreign import ccall "SDL_SetJoystickVirtualHat"
  sdlSetJoystickVirtualHat :: Ptr SDLJoystick -> CInt -> Word8 -> IO CBool

-- | Set virtual joystick touchpad
foreign import ccall "SDL_SetJoystickVirtualTouchpad"
  sdlSetJoystickVirtualTouchpad :: Ptr SDLJoystick -> CInt -> CInt -> CBool -> CFloat -> CFloat -> CFloat -> IO CBool

-- | Send virtual joystick sensor data
foreign import ccall "SDL_SendJoystickVirtualSensorData"
  sdlSendJoystickVirtualSensorData_ :: Ptr SDLJoystick -> CInt -> Word64 -> Ptr CFloat -> CInt -> IO CBool

sdlSendJoystickVirtualSensorData :: SDLJoystick -> SDLSensorType -> Word64 -> [Float] -> IO Bool
sdlSendJoystickVirtualSensorData (SDLJoystick joy) (SDLSensorType sensorType) timestamp values = do
  let numValues = length values
  valuePtr <- newArray $ map realToFrac values  -- Convert [Float] to [CFloat]
  result <- sdlSendJoystickVirtualSensorData_ joy sensorType timestamp valuePtr (fromIntegral numValues)
  free valuePtr
  return $ toBool result

-- | Get joystick properties
foreign import ccall "SDL_GetJoystickProperties"
  sdlGetJoystickProperties :: Ptr SDLJoystick -> IO SDLPropertiesID

-- | Get joystick name
foreign import ccall "SDL_GetJoystickName"
  sdlGetJoystickName :: Ptr SDLJoystick -> IO CString

-- | Get joystick path
foreign import ccall "SDL_GetJoystickPath"
  sdlGetJoystickPath :: Ptr SDLJoystick -> IO CString

-- | Get player index
foreign import ccall "SDL_GetJoystickPlayerIndex"
  sdlGetJoystickPlayerIndex :: Ptr SDLJoystick -> IO CInt

-- | Set player index
foreign import ccall "SDL_SetJoystickPlayerIndex"
  sdlSetJoystickPlayerIndex :: Ptr SDLJoystick -> CInt -> IO CBool

-- | Get joystick GUID
foreign import ccall "SDL_GetJoystickGUID"
  sdlGetJoystickGUID :: Ptr SDLJoystick -> IO (Ptr SDLGUID)

-- | Get joystick vendor
foreign import ccall "SDL_GetJoystickVendor"
  sdlGetJoystickVendor :: Ptr SDLJoystick -> IO Word16

-- | Get joystick product
foreign import ccall "SDL_GetJoystickProduct"
  sdlGetJoystickProduct :: Ptr SDLJoystick -> IO Word16

-- | Get joystick product version
foreign import ccall "SDL_GetJoystickProductVersion"
  sdlGetJoystickProductVersion :: Ptr SDLJoystick -> IO Word16

-- | Get joystick firmware version
foreign import ccall "SDL_GetJoystickFirmwareVersion"
  sdlGetJoystickFirmwareVersion :: Ptr SDLJoystick -> IO Word16

-- | Get joystick serial
foreign import ccall "SDL_GetJoystickSerial"
  sdlGetJoystickSerial :: Ptr SDLJoystick -> IO CString

-- | Get joystick type
foreign import ccall "SDL_GetJoystickType"
  sdlGetJoystickType :: Ptr SDLJoystick -> IO CInt

-- | Get GUID info
foreign import ccall "SDL_GetJoystickGUIDInfo"
  sdlGetJoystickGUIDInfo :: Ptr SDLGUID -> Ptr Word16 -> Ptr Word16 -> Ptr Word16 -> Ptr Word16 -> IO ()

-- | Check if joystick is connected
foreign import ccall "SDL_JoystickConnected"
  sdlJoystickConnected :: Ptr SDLJoystick -> IO CBool

-- | Get joystick ID
foreign import ccall "SDL_GetJoystickID"
  sdlGetJoystickID :: Ptr SDLJoystick -> IO SDLJoystickID

-- | Get number of axes
foreign import ccall "SDL_GetNumJoystickAxes"
  sdlGetNumJoystickAxes :: Ptr SDLJoystick -> IO CInt

-- | Get number of balls
foreign import ccall "SDL_GetNumJoystickBalls"
  sdlGetNumJoystickBalls :: Ptr SDLJoystick -> IO CInt

-- | Get number of hats
foreign import ccall "SDL_GetNumJoystickHats"
  sdlGetNumJoystickHats :: Ptr SDLJoystick -> IO CInt

-- | Get number of buttons
foreign import ccall "SDL_GetNumJoystickButtons"
  sdlGetNumJoystickButtons :: Ptr SDLJoystick -> IO CInt

-- | Enable/disable joystick events
foreign import ccall "SDL_SetJoystickEventsEnabled"
  sdlSetJoystickEventsEnabled :: CBool -> IO ()

-- | Check if joystick events are enabled
foreign import ccall "SDL_JoystickEventsEnabled"
  sdlJoystickEventsEnabled :: IO CBool

-- | Update joystick state
foreign import ccall "SDL_UpdateJoysticks"
  sdlUpdateJoysticks :: IO ()

-- | Get joystick axis
foreign import ccall "SDL_GetJoystickAxis"
  sdlGetJoystickAxis :: Ptr SDLJoystick -> CInt -> IO Int16

-- | Get joystick axis initial state
foreign import ccall "SDL_GetJoystickAxisInitialState"
  sdlGetJoystickAxisInitialState :: Ptr SDLJoystick -> CInt -> Ptr Int16 -> IO CBool

-- | Get joystick ball
foreign import ccall "SDL_GetJoystickBall"
  sdlGetJoystickBall :: Ptr SDLJoystick -> CInt -> Ptr CInt -> Ptr CInt -> IO CBool

-- | Get joystick hat
foreign import ccall "SDL_GetJoystickHat"
  sdlGetJoystickHat :: Ptr SDLJoystick -> CInt -> IO Word8

-- | Get joystick button
foreign import ccall "SDL_GetJoystickButton"
  sdlGetJoystickButton :: Ptr SDLJoystick -> CInt -> IO CBool

-- | Rumble joystick
foreign import ccall "SDL_RumbleJoystick"
  sdlRumbleJoystick :: Ptr SDLJoystick -> Word16 -> Word16 -> Word32 -> IO CBool

-- | Rumble joystick triggers
foreign import ccall "SDL_RumbleJoystickTriggers"
  sdlRumbleJoystickTriggers :: Ptr SDLJoystick -> Word16 -> Word16 -> Word32 -> IO CBool

-- | Set joystick LED
foreign import ccall "SDL_SetJoystickLED"
  sdlSetJoystickLED :: Ptr SDLJoystick -> Word8 -> Word8 -> Word8 -> IO CBool

-- | Send joystick effect
foreign import ccall "SDL_SendJoystickEffect"
  sdlSendJoystickEffect :: Ptr SDLJoystick -> Ptr () -> CInt -> IO CBool

-- | Close joystick
foreign import ccall "SDL_CloseJoystick"
  sdlCloseJoystick :: Ptr SDLJoystick -> IO ()

-- | Get joystick connection state
foreign import ccall "SDL_GetJoystickConnectionState"
  sdlGetJoystickConnectionState :: Ptr SDLJoystick -> IO CInt

-- | Get joystick power info
foreign import ccall "SDL_GetJoystickPowerInfo"
  sdlGetJoystickPowerInfo :: Ptr SDLJoystick -> Ptr CInt -> IO SDLPowerState
