{-|
Module      : SDL.Gamepad
Description : SDL gamepad management
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

This software is provided 'as-is', without any express or implied
warranty. In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
   claim that you wrote the original software. If you use this software
   in a product, an acknowledgment in the product documentation would be
   appreciated but is not required.
2. Altered source versions must be plainly marked as such, and must not be
   misrepresented as being the original software.
3. This notice may not be removed or altered from any source distribution.

SDL provides a gamepad API on top of the lower-level joystick functionality,
offering a standardized way to interact with console-style gamepads. This
module includes functions to manage gamepads, query their properties, and
handle input from buttons, axes, and additional features like rumble and LEDs.

Basic usage:
- Initialize the subsystem with sdlInit(InitGamepad)
- Open a gamepad using 'sdlOpenGamepad'
- Query state with 'sdlGetGamepadAxis' or 'sdlGetGamepadButton'
- Close with 'sdlCloseGamepad'
-}

module SDL.Gamepad
  ( -- * Types
    SDLGamepad(..)
  , SDLGamepadType(..)
  , SDLGamepadButton(..)
  , SDLGamepadButtonLabel(..)
  , SDLGamepadAxis(..)
  , SDLGamepadBindingType(..)
  , SDLGamepadBinding(..)

    -- * Constants
  , sdlPropGamepadCapMonoLEDBoolean
  , sdlPropGamepadCapRGBLEDBoolean
  , sdlPropGamepadCapPlayerLEDBoolean
  , sdlPropGamepadCapRumbleBoolean
  , sdlPropGamepadCapTriggerRumbleBoolean

    -- * Gamepad Functions
  , sdlAddGamepadMapping
  , sdlAddGamepadMappingsFromIO
  , sdlAddGamepadMappingsFromFile
  , sdlReloadGamepadMappings
  , sdlGetGamepadMappings
  , sdlGetGamepadMappingForGUID
  , sdlGetGamepadMapping
  , sdlSetGamepadMapping
  , sdlHasGamepad
  , sdlGetGamepads
  , sdlIsGamepad
  , sdlGetGamepadNameForID
  , sdlGetGamepadPathForID
  , sdlGetGamepadPlayerIndexForID
  , sdlGetGamepadGUIDForID
  , sdlGetGamepadVendorForID
  , sdlGetGamepadProductForID
  , sdlGetGamepadProductVersionForID
  , sdlGetGamepadTypeForID
  , sdlGetRealGamepadTypeForID
  , sdlGetGamepadMappingForID
  , sdlOpenGamepad
  , sdlGetGamepadFromID
  , sdlGetGamepadFromPlayerIndex
  , sdlGetGamepadProperties
  , sdlGetGamepadID
  , sdlGetGamepadName
  , sdlGetGamepadPath
  , sdlGetGamepadType
  , sdlGetRealGamepadType
  , sdlGetGamepadPlayerIndex
  , sdlSetGamepadPlayerIndex
  , sdlGetGamepadVendor
  , sdlGetGamepadProduct
  , sdlGetGamepadProductVersion
  , sdlGetGamepadFirmwareVersion
  , sdlGetGamepadSerial
  , sdlGetGamepadSteamHandle
  , sdlGetGamepadConnectionState
  , sdlGetGamepadPowerInfo
  , sdlGamepadConnected
  , sdlGetGamepadJoystick
  , sdlSetGamepadEventsEnabled
  , sdlGamepadEventsEnabled
  , sdlGetGamepadBindings
  , sdlUpdateGamepads
  , sdlGetGamepadTypeFromString
  , sdlGetGamepadStringForType
  , sdlGetGamepadAxisFromString
  , sdlGetGamepadStringForAxis
  , sdlGamepadHasAxis
  , sdlGetGamepadAxis
  , sdlGetGamepadButtonFromString
  , sdlGetGamepadStringForButton
  , sdlGamepadHasButton
  , sdlGetGamepadButton
  , sdlGetGamepadButtonLabelForType
  , sdlGetGamepadButtonLabel
  , sdlGetNumGamepadTouchpads
  , sdlGetNumGamepadTouchpadFingers
  , sdlGetGamepadTouchpadFinger
  , sdlGamepadHasSensor
  , sdlSetGamepadSensorEnabled
  , sdlGamepadSensorEnabled
  , sdlGetGamepadSensorDataRate
  , sdlGetGamepadSensorData
  , sdlRumbleGamepad
  , sdlRumbleGamepadTriggers
  , sdlSetGamepadLED
  , sdlSendGamepadEffect
  , sdlCloseGamepad
  , sdlGetGamepadAppleSFSymbolsNameForButton
  , sdlGetGamepadAppleSFSymbolsNameForAxis
  ) where

#include <SDL3/SDL_gamepad.h>

import Foreign (Ptr, nullPtr, FunPtr, castPtr, peekArray, pokeArray, withArray, peek, poke, toBool, plusPtr, with, fromBool)
import Foreign.C.Types (CBool(..), CInt(..), CFloat(..), CUInt(..), CULLong(..))
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (alloca, free)
import Data.Word (Word32, Word16, Word8)
import Data.Int (Int32, Int16)
import SDL.GUID (SDLGUID(..))
import SDL.Joystick (SDLJoystick(..), SDLJoystickID(..), SDLJoystickConnectionState(..))
import SDL.Power (SDLPowerState(..))
import SDL.Properties (SDLPropertiesID(..))
import SDL.Sensor (SDLSensorType(..))
import SDL.IOStream (SDLIOStream)

-- | Opaque type representing a gamepad
newtype SDLGamepad = SDLGamepad { unSDLGamepad :: Ptr SDLGamepad }
  deriving (Eq, Show)

-- | Standard gamepad types
data SDLGamepadType
  = SDLGamepadTypeUnknown
  | SDLGamepadTypeStandard
  | SDLGamepadTypeXbox360
  | SDLGamepadTypeXboxOne
  | SDLGamepadTypePS3
  | SDLGamepadTypePS4
  | SDLGamepadTypePS5
  | SDLGamepadTypeNintendoSwitchPro
  | SDLGamepadTypeNintendoSwitchJoyconLeft
  | SDLGamepadTypeNintendoSwitchJoyconRight
  | SDLGamepadTypeNintendoSwitchJoyconPair
  | SDLGamepadTypeCount
  deriving (Eq, Show, Enum)

-- | Gamepad buttons
data SDLGamepadButton
  = SDLGamepadButtonInvalid
  | SDLGamepadButtonSouth
  | SDLGamepadButtonEast
  | SDLGamepadButtonWest
  | SDLGamepadButtonNorth
  | SDLGamepadButtonBack
  | SDLGamepadButtonGuide
  | SDLGamepadButtonStart
  | SDLGamepadButtonLeftStick
  | SDLGamepadButtonRightStick
  | SDLGamepadButtonLeftShoulder
  | SDLGamepadButtonRightShoulder
  | SDLGamepadButtonDPadUp
  | SDLGamepadButtonDPadDown
  | SDLGamepadButtonDPadLeft
  | SDLGamepadButtonDPadRight
  | SDLGamepadButtonMisc1
  | SDLGamepadButtonRightPaddle1
  | SDLGamepadButtonLeftPaddle1
  | SDLGamepadButtonRightPaddle2
  | SDLGamepadButtonLeftPaddle2
  | SDLGamepadButtonTouchpad
  | SDLGamepadButtonMisc2
  | SDLGamepadButtonMisc3
  | SDLGamepadButtonMisc4
  | SDLGamepadButtonMisc5
  | SDLGamepadButtonMisc6
  | SDLGamepadButtonCount
  deriving (Eq, Show, Enum)

-- | Gamepad button labels
data SDLGamepadButtonLabel
  = SDLGamepadButtonLabelUnknown
  | SDLGamepadButtonLabelA
  | SDLGamepadButtonLabelB
  | SDLGamepadButtonLabelX
  | SDLGamepadButtonLabelY
  | SDLGamepadButtonLabelCross
  | SDLGamepadButtonLabelCircle
  | SDLGamepadButtonLabelSquare
  | SDLGamepadButtonLabelTriangle
  deriving (Eq, Show, Enum)

-- | Gamepad axes
data SDLGamepadAxis
  = SDLGamepadAxisInvalid
  | SDLGamepadAxisLeftX
  | SDLGamepadAxisLeftY
  | SDLGamepadAxisRightX
  | SDLGamepadAxisRightY
  | SDLGamepadAxisLeftTrigger
  | SDLGamepadAxisRightTrigger
  | SDLGamepadAxisCount
  deriving (Eq, Show, Enum)

-- | Types of gamepad control bindings
data SDLGamepadBindingType
  = SDLGamepadBindTypeNone
  | SDLGamepadBindTypeButton
  | SDLGamepadBindTypeAxis
  | SDLGamepadBindTypeHat
  deriving (Eq, Show, Enum)

-- | A mapping between joystick input and gamepad control
data SDLGamepadBinding = SDLGamepadBinding
  { bindingInputType :: SDLGamepadBindingType
  , bindingInput     :: SDLGamepadBindingInput
  , bindingOutputType :: SDLGamepadBindingType
  , bindingOutput    :: SDLGamepadBindingOutput
  } deriving (Eq, Show)

data SDLGamepadBindingInput
  = BindingInputButton Int
  | BindingInputAxis Int Int Int
  | BindingInputHat Int Int
  | BindingInputNone
  deriving (Eq, Show)

data SDLGamepadBindingOutput
  = BindingOutputButton SDLGamepadButton
  | BindingOutputAxis SDLGamepadAxis Int Int
  | BindingOutputNone
  deriving (Eq, Show)

instance Storable SDLGamepadBinding where
  sizeOf _ = #size SDL_GamepadBinding
  alignment _ = #alignment SDL_GamepadBinding
  peek ptr = do
    inputType <- toEnum . fromIntegral <$> ((#peek SDL_GamepadBinding, input_type) ptr :: IO CInt)
    outputType <- toEnum . fromIntegral <$> ((#peek SDL_GamepadBinding, output_type) ptr :: IO CInt)
    input <- case inputType of
      SDLGamepadBindTypeButton -> BindingInputButton <$> (#peek SDL_GamepadBinding, input.button) ptr
      SDLGamepadBindTypeAxis -> do
        axis <- (#peek SDL_GamepadBinding, input.axis.axis) ptr
        min' <- (#peek SDL_GamepadBinding, input.axis.axis_min) ptr
        max' <- (#peek SDL_GamepadBinding, input.axis.axis_max) ptr
        return $ BindingInputAxis axis min' max'
      SDLGamepadBindTypeHat -> do
        hat <- (#peek SDL_GamepadBinding, input.hat.hat) ptr
        mask <- (#peek SDL_GamepadBinding, input.hat.hat_mask) ptr
        return $ BindingInputHat hat mask
      _ -> pure BindingInputNone
    output <- case outputType of
      SDLGamepadBindTypeButton -> do
        btn <- ((#peek SDL_GamepadBinding, output.button) ptr :: IO CInt)
        return $ BindingOutputButton (toEnum $ fromIntegral btn)
      SDLGamepadBindTypeAxis -> do
        axis <- toEnum . fromIntegral <$> ((#peek SDL_GamepadBinding, output.axis.axis) ptr :: IO CInt )
        min' <- (#peek SDL_GamepadBinding, output.axis.axis_min) ptr
        max' <- (#peek SDL_GamepadBinding, output.axis.axis_max) ptr
        return $ BindingOutputAxis axis min' max'
      _ -> pure BindingOutputNone
    return $ SDLGamepadBinding inputType input outputType output
  poke ptr (SDLGamepadBinding iType i oType o) = do
    (#poke SDL_GamepadBinding, input_type) ptr ((fromIntegral $ fromEnum iType) :: CInt)
    (#poke SDL_GamepadBinding, output_type) ptr ((fromIntegral $ fromEnum oType) :: CInt)
    case i of
      BindingInputButton btn -> (#poke SDL_GamepadBinding, input.button) ptr btn
      BindingInputAxis ax min' max' -> do
        (#poke SDL_GamepadBinding, input.axis.axis) ptr ax
        (#poke SDL_GamepadBinding, input.axis.axis_min) ptr min'
        (#poke SDL_GamepadBinding, input.axis.axis_max) ptr max'
      BindingInputHat hat mask -> do
        (#poke SDL_GamepadBinding, input.hat.hat) ptr hat
        (#poke SDL_GamepadBinding, input.hat.hat_mask) ptr mask
      BindingInputNone -> return ()
    case o of
      BindingOutputButton btn -> (#poke SDL_GamepadBinding, output.button) ptr ((fromIntegral $ fromEnum btn) :: CInt)
      BindingOutputAxis ax min' max' -> do
        (#poke SDL_GamepadBinding, output.axis.axis) ptr ((fromIntegral $ fromEnum ax) :: CInt)
        (#poke SDL_GamepadBinding, output.axis.axis_min) ptr min'
        (#poke SDL_GamepadBinding, output.axis.axis_max) ptr max'
      BindingOutputNone -> return ()

-- | Constants
sdlPropGamepadCapMonoLEDBoolean :: String
sdlPropGamepadCapMonoLEDBoolean = #const_str SDL_PROP_GAMEPAD_CAP_MONO_LED_BOOLEAN

sdlPropGamepadCapRGBLEDBoolean :: String
sdlPropGamepadCapRGBLEDBoolean = #const_str SDL_PROP_GAMEPAD_CAP_RGB_LED_BOOLEAN

sdlPropGamepadCapPlayerLEDBoolean :: String
sdlPropGamepadCapPlayerLEDBoolean = #const_str SDL_PROP_GAMEPAD_CAP_PLAYER_LED_BOOLEAN

sdlPropGamepadCapRumbleBoolean :: String
sdlPropGamepadCapRumbleBoolean = #const_str SDL_PROP_GAMEPAD_CAP_RUMBLE_BOOLEAN

sdlPropGamepadCapTriggerRumbleBoolean :: String
sdlPropGamepadCapTriggerRumbleBoolean = #const_str SDL_PROP_GAMEPAD_CAP_TRIGGER_RUMBLE_BOOLEAN

-- | Add a gamepad mapping
foreign import ccall "SDL_AddGamepadMapping"
  sdlAddGamepadMapping :: CString -> IO CInt

-- | Load gamepad mappings from an SDL_IOStream
foreign import ccall "SDL_AddGamepadMappingsFromIO"
  sdlAddGamepadMappingsFromIO :: Ptr SDLIOStream -> CBool -> IO CInt

-- | Load gamepad mappings from a file
foreign import ccall "SDL_AddGamepadMappingsFromFile"
  sdlAddGamepadMappingsFromFile :: CString -> IO CInt

-- | Reinitialize gamepad mapping database
foreign import ccall "SDL_ReloadGamepadMappings"
  sdlReloadGamepadMappings_ :: IO CBool

sdlReloadGamepadMappings :: IO Bool
sdlReloadGamepadMappings = toBool <$> sdlReloadGamepadMappings_

-- | Get current gamepad mappings
foreign import ccall "SDL_GetGamepadMappings"
  sdlGetGamepadMappingsRaw :: Ptr CInt -> IO (Ptr CString)

sdlGetGamepadMappings :: IO [String]
sdlGetGamepadMappings = alloca $ \countPtr -> do
  ptr <- sdlGetGamepadMappingsRaw countPtr
  count <- peek countPtr
  strs <- peekArray (fromIntegral count + 1) ptr >>= mapM peekCString
  free ptr
  return $ takeWhile (not . null) strs

-- | Get gamepad mapping for GUID
foreign import ccall "SDL_GetGamepadMappingForGUID"
  sdlGetGamepadMappingForGUIDRaw :: Ptr SDLGUID -> IO CString

sdlGetGamepadMappingForGUID :: SDLGUID -> IO (Maybe String)
sdlGetGamepadMappingForGUID guid = with guid $ \ptr -> do
  cstr <- sdlGetGamepadMappingForGUIDRaw ptr
  if cstr == nullPtr then return Nothing else Just <$> peekCString cstr

-- | Get current mapping of a gamepad
foreign import ccall "SDL_GetGamepadMapping"
  sdlGetGamepadMappingRaw :: Ptr SDLGamepad -> IO CString

sdlGetGamepadMapping :: SDLGamepad -> IO (Maybe String)
sdlGetGamepadMapping (SDLGamepad ptr) = do
  cstr <- sdlGetGamepadMappingRaw ptr
  if cstr == nullPtr then return Nothing else Just <$> peekCString cstr

-- | Set gamepad mapping
foreign import ccall "SDL_SetGamepadMapping"
  sdlSetGamepadMapping_ :: Word32 -> CString -> IO CBool

sdlSetGamepadMapping :: SDLJoystickID -> Maybe String -> IO Bool
sdlSetGamepadMapping (SDLJoystickID jid) mapping = withCString (maybe "" id mapping) $ \cstr ->
  toBool <$> sdlSetGamepadMapping_ jid cstr

-- | Check if any gamepads are connected
foreign import ccall "SDL_HasGamepad"
  sdlHasGamepad_ :: IO CBool

sdlHasGamepad :: IO Bool
sdlHasGamepad = toBool <$> sdlHasGamepad_

-- | Get list of connected gamepads
foreign import ccall "SDL_GetGamepads"
  sdlGetGamepadsRaw :: Ptr CInt -> IO (Ptr SDLJoystickID)

sdlGetGamepads :: IO [SDLJoystickID]
sdlGetGamepads = alloca $ \countPtr -> do
  ptr <- sdlGetGamepadsRaw countPtr
  count <- peek countPtr
  ids <- peekArray (fromIntegral count) ptr
  free ptr
  return ids

-- | Check if a joystick is a gamepad
foreign import ccall "SDL_IsGamepad"
  sdlIsGamepad_ :: Word32 -> IO CBool

sdlIsGamepad :: SDLJoystickID -> IO Bool
sdlIsGamepad (SDLJoystickID jid) = toBool <$> sdlIsGamepad_ jid

-- | Get gamepad name by ID
foreign import ccall "SDL_GetGamepadNameForID"
  sdlGetGamepadNameForID_ :: Word32 -> IO CString

sdlGetGamepadNameForID :: SDLJoystickID -> IO (Maybe String)
sdlGetGamepadNameForID (SDLJoystickID jid) = do
  cstr <- sdlGetGamepadNameForID_ jid
  if cstr == nullPtr then return Nothing else Just <$> peekCString cstr

-- | Get gamepad path by ID
foreign import ccall "SDL_GetGamepadPathForID"
  sdlGetGamepadPathForID_ :: Word32 -> IO CString

sdlGetGamepadPathForID :: SDLJoystickID -> IO (Maybe String)
sdlGetGamepadPathForID (SDLJoystickID jid) = do
  cstr <- sdlGetGamepadPathForID_ jid
  if cstr == nullPtr then return Nothing else Just <$> peekCString cstr

-- | Get player index by ID
foreign import ccall "SDL_GetGamepadPlayerIndexForID"
  sdlGetGamepadPlayerIndexForID_ :: Word32 -> IO CInt

sdlGetGamepadPlayerIndexForID :: SDLJoystickID -> IO Int
sdlGetGamepadPlayerIndexForID (SDLJoystickID jid) = do
  fromIntegral <$> sdlGetGamepadPlayerIndexForID_ jid
  
-- | Get GUID by ID
foreign import ccall "SDL_GetGamepadGUIDForID"
  sdlGetGamepadGUIDForID :: Word32 -> IO (Ptr SDLGUID)

-- | Get vendor ID by ID
foreign import ccall "SDL_GetGamepadVendorForID"
  sdlGetGamepadVendorForID_ :: Word32 -> IO Word16

sdlGetGamepadVendorForID :: SDLJoystickID -> IO Word16
sdlGetGamepadVendorForID (SDLJoystickID jid) = do
  sdlGetGamepadVendorForID_ jid

-- | Get product ID by ID
foreign import ccall "SDL_GetGamepadProductForID"
  sdlGetGamepadProductForID_ :: Word32 -> IO Word16

sdlGetGamepadProductForID :: SDLJoystickID -> IO Word16
sdlGetGamepadProductForID (SDLJoystickID jid) = do
  sdlGetGamepadProductForID_ jid

-- | Get product version by ID
foreign import ccall "SDL_GetGamepadProductVersionForID"
  sdlGetGamepadProductVersionForID_ :: Word32 -> IO Word16

sdlGetGamepadProductVersionForID :: SDLJoystickID -> IO Word16
sdlGetGamepadProductVersionForID (SDLJoystickID jid) = do
  sdlGetGamepadProductVersionForID_ jid

-- | Get gamepad type by ID
foreign import ccall "SDL_GetGamepadTypeForID"
  sdlGetGamepadTypeForID_ :: Word32 -> IO CInt

sdlGetGamepadTypeForID :: SDLJoystickID -> IO SDLGamepadType
sdlGetGamepadTypeForID (SDLJoystickID jid) = toEnum . fromIntegral <$> sdlGetGamepadTypeForID_ jid

-- | Get real gamepad type by ID
foreign import ccall "SDL_GetRealGamepadTypeForID"
  sdlGetRealGamepadTypeForID_ :: Word32 -> IO CInt

sdlGetRealGamepadTypeForID :: SDLJoystickID -> IO SDLGamepadType
sdlGetRealGamepadTypeForID (SDLJoystickID jid) = toEnum . fromIntegral <$> sdlGetRealGamepadTypeForID_ jid

-- | Get gamepad mapping by ID
foreign import ccall "SDL_GetGamepadMappingForID"
  sdlGetGamepadMappingForID_ :: Word32 -> IO CString

sdlGetGamepadMappingForID :: SDLJoystickID -> IO (Maybe String)
sdlGetGamepadMappingForID (SDLJoystickID jid) = do
  cstr <- sdlGetGamepadMappingForID_ jid
  if cstr == nullPtr then return Nothing else Just <$> peekCString cstr

-- | Open a gamepad
foreign import ccall "SDL_OpenGamepad"
  sdlOpenGamepadRaw :: Word32 -> IO (Ptr SDLGamepad)

sdlOpenGamepad :: SDLJoystickID -> IO (Maybe SDLGamepad)
sdlOpenGamepad (SDLJoystickID jid) = do
  ptr <- sdlOpenGamepadRaw jid
  return $ if ptr == nullPtr then Nothing else Just (SDLGamepad ptr)

-- | Get gamepad from ID
foreign import ccall "SDL_GetGamepadFromID"
  sdlGetGamepadFromIDRaw :: Word32 -> IO (Ptr SDLGamepad)

sdlGetGamepadFromID :: SDLJoystickID -> IO (Maybe SDLGamepad)
sdlGetGamepadFromID (SDLJoystickID jid) = do
  ptr <- sdlGetGamepadFromIDRaw jid
  return $ if ptr == nullPtr then Nothing else Just (SDLGamepad ptr)

-- | Get gamepad from player index
foreign import ccall "SDL_GetGamepadFromPlayerIndex"
  sdlGetGamepadFromPlayerIndexRaw :: CInt -> IO (Ptr SDLGamepad)

sdlGetGamepadFromPlayerIndex :: Int -> IO (Maybe SDLGamepad)
sdlGetGamepadFromPlayerIndex idx = do
  ptr <- sdlGetGamepadFromPlayerIndexRaw (fromIntegral idx)
  return $ if ptr == nullPtr then Nothing else Just (SDLGamepad ptr)

-- | Get gamepad properties
foreign import ccall "SDL_GetGamepadProperties"
  sdlGetGamepadProperties :: Ptr SDLGamepad -> IO SDLPropertiesID

-- | Get gamepad instance ID
foreign import ccall "SDL_GetGamepadID"
  sdlGetGamepadID_ :: Ptr SDLGamepad -> IO Word32

sdlGetGamepadID :: Ptr SDLGamepad -> IO SDLJoystickID
sdlGetGamepadID ptr = fmap SDLJoystickID (sdlGetGamepadID_ ptr)

-- | Get gamepad name
foreign import ccall "SDL_GetGamepadName"
  sdlGetGamepadNameRaw :: Ptr SDLGamepad -> IO CString

sdlGetGamepadName :: SDLGamepad -> IO (Maybe String)
sdlGetGamepadName (SDLGamepad ptr) = do
  cstr <- sdlGetGamepadNameRaw ptr
  if cstr == nullPtr then return Nothing else Just <$> peekCString cstr

-- | Get gamepad path
foreign import ccall "SDL_GetGamepadPath"
  sdlGetGamepadPathRaw :: Ptr SDLGamepad -> IO CString

sdlGetGamepadPath :: SDLGamepad -> IO (Maybe String)
sdlGetGamepadPath (SDLGamepad ptr) = do
  cstr <- sdlGetGamepadPathRaw ptr
  if cstr == nullPtr then return Nothing else Just <$> peekCString cstr

-- | Get gamepad type
foreign import ccall "SDL_GetGamepadType"
  sdlGetGamepadType_ :: Ptr SDLGamepad -> IO CInt

sdlGetGamepadType :: SDLGamepad -> IO SDLGamepadType
sdlGetGamepadType (SDLGamepad ptr) = toEnum . fromIntegral <$> sdlGetGamepadType_ ptr

-- | Get real gamepad type
foreign import ccall "SDL_GetRealGamepadType"
  sdlGetRealGamepadType_ :: Ptr SDLGamepad -> IO CInt

sdlGetRealGamepadType :: SDLGamepad -> IO SDLGamepadType
sdlGetRealGamepadType (SDLGamepad ptr) = toEnum . fromIntegral <$> sdlGetRealGamepadType_ ptr

-- | Get player index
foreign import ccall "SDL_GetGamepadPlayerIndex"
  sdlGetGamepadPlayerIndex :: Ptr SDLGamepad -> IO CInt

-- | Set player index
foreign import ccall "SDL_SetGamepadPlayerIndex"
  sdlSetGamepadPlayerIndex_ :: Ptr SDLGamepad -> CInt -> IO CBool

sdlSetGamepadPlayerIndex :: SDLGamepad -> Int -> IO Bool
sdlSetGamepadPlayerIndex (SDLGamepad ptr) idx = toBool <$> sdlSetGamepadPlayerIndex_ ptr (fromIntegral idx)

-- | Get vendor ID
foreign import ccall "SDL_GetGamepadVendor"
  sdlGetGamepadVendor :: Ptr SDLGamepad -> IO Word16

-- | Get product ID
foreign import ccall "SDL_GetGamepadProduct"
  sdlGetGamepadProduct :: Ptr SDLGamepad -> IO Word16

-- | Get product version
foreign import ccall "SDL_GetGamepadProductVersion"
  sdlGetGamepadProductVersion :: Ptr SDLGamepad -> IO Word16

-- | Get firmware version
foreign import ccall "SDL_GetGamepadFirmwareVersion"
  sdlGetGamepadFirmwareVersion :: Ptr SDLGamepad -> IO Word16

-- | Get serial number
foreign import ccall "SDL_GetGamepadSerial"
  sdlGetGamepadSerialRaw :: Ptr SDLGamepad -> IO CString

sdlGetGamepadSerial :: SDLGamepad -> IO (Maybe String)
sdlGetGamepadSerial (SDLGamepad ptr) = do
  cstr <- sdlGetGamepadSerialRaw ptr
  if cstr == nullPtr then return Nothing else Just <$> peekCString cstr

-- | Get Steam Input handle
foreign import ccall "SDL_GetGamepadSteamHandle"
  sdlGetGamepadSteamHandle :: Ptr SDLGamepad -> IO CULLong

-- | Get connection state
foreign import ccall "SDL_GetGamepadConnectionState"
  sdlGetGamepadConnectionState_ :: Ptr SDLGamepad -> IO CInt

sdlGetGamepadConnectionState :: SDLGamepad -> IO SDLJoystickConnectionState
sdlGetGamepadConnectionState (SDLGamepad ptr) = toEnum . fromIntegral <$> sdlGetGamepadConnectionState_ ptr

-- | Get power info
foreign import ccall "SDL_GetGamepadPowerInfo"
  sdlGetGamepadPowerInfoRaw :: Ptr SDLGamepad -> Ptr CInt -> IO CInt

sdlGetGamepadPowerInfo :: SDLGamepad -> IO (SDLPowerState, Maybe Int)
sdlGetGamepadPowerInfo (SDLGamepad ptr) = alloca $ \percentPtr -> do
  state <- sdlGetGamepadPowerInfoRaw ptr percentPtr
  percent <- peek percentPtr
  return (toEnum $ fromIntegral state, if percent < 0 then Nothing else Just $ fromIntegral percent)

-- | Check if gamepad is connected
foreign import ccall "SDL_GamepadConnected"
  sdlGamepadConnected_ :: Ptr SDLGamepad -> IO CBool

sdlGamepadConnected :: SDLGamepad -> IO Bool
sdlGamepadConnected (SDLGamepad ptr) = toBool <$> sdlGamepadConnected_ ptr

-- | Get underlying joystick
foreign import ccall "SDL_GetGamepadJoystick"
  sdlGetGamepadJoystickRaw :: Ptr SDLGamepad -> IO (Ptr SDLJoystick)

sdlGetGamepadJoystick :: SDLGamepad -> IO SDLJoystick
sdlGetGamepadJoystick (SDLGamepad ptr) = SDLJoystick <$> sdlGetGamepadJoystickRaw ptr

-- | Set gamepad event processing
foreign import ccall "SDL_SetGamepadEventsEnabled"
  sdlSetGamepadEventsEnabled :: CBool -> IO ()

-- | Query gamepad event processing
foreign import ccall "SDL_GamepadEventsEnabled"
  sdlGamepadEventsEnabled_ :: IO CBool

sdlGamepadEventsEnabled :: IO Bool
sdlGamepadEventsEnabled = toBool <$> sdlGamepadEventsEnabled_

-- | Get gamepad bindings
foreign import ccall "SDL_GetGamepadBindings"
  sdlGetGamepadBindingsRaw :: Ptr SDLGamepad -> Ptr CInt -> IO (Ptr (Ptr SDLGamepadBinding))

sdlGetGamepadBindings :: SDLGamepad -> IO [SDLGamepadBinding]
sdlGetGamepadBindings (SDLGamepad ptr) = alloca $ \countPtr -> do
  bindingsPtr <- sdlGetGamepadBindingsRaw ptr countPtr
  count <- peek countPtr
  bindings <- peekArray (fromIntegral count) bindingsPtr >>= mapM peek
  free bindingsPtr
  return bindings

-- | Update gamepads
foreign import ccall "SDL_UpdateGamepads"
  sdlUpdateGamepads :: IO ()

-- | Convert string to gamepad type
foreign import ccall "SDL_GetGamepadTypeFromString"
  sdlGetGamepadTypeFromString_ :: CString -> IO CInt

sdlGetGamepadTypeFromString :: String -> IO SDLGamepadType
sdlGetGamepadTypeFromString str = withCString str $ \cstr ->
  toEnum . fromIntegral <$> sdlGetGamepadTypeFromString_ cstr

-- | Convert gamepad type to string
foreign import ccall "SDL_GetGamepadStringForType"
  sdlGetGamepadStringForTypeRaw :: CInt -> IO CString

sdlGetGamepadStringForType :: SDLGamepadType -> IO (Maybe String)
sdlGetGamepadStringForType typ = do
  cstr <- sdlGetGamepadStringForTypeRaw (fromIntegral $ fromEnum typ)
  if cstr == nullPtr then return Nothing else Just <$> peekCString cstr

-- | Convert string to gamepad axis
foreign import ccall "SDL_GetGamepadAxisFromString"
  sdlGetGamepadAxisFromString_ :: CString -> IO CInt

sdlGetGamepadAxisFromString :: String -> IO SDLGamepadAxis
sdlGetGamepadAxisFromString str = withCString str $ \cstr ->
  toEnum . fromIntegral <$> sdlGetGamepadAxisFromString_ cstr

-- | Convert gamepad axis to string
foreign import ccall "SDL_GetGamepadStringForAxis"
  sdlGetGamepadStringForAxisRaw :: CInt -> IO CString

sdlGetGamepadStringForAxis :: SDLGamepadAxis -> IO (Maybe String)
sdlGetGamepadStringForAxis axis = do
  cstr <- sdlGetGamepadStringForAxisRaw (fromIntegral $ fromEnum axis)
  if cstr == nullPtr then return Nothing else Just <$> peekCString cstr

-- | Check if gamepad has axis
foreign import ccall "SDL_GamepadHasAxis"
  sdlGamepadHasAxis_ :: Ptr SDLGamepad -> CInt -> IO CBool

sdlGamepadHasAxis :: SDLGamepad -> SDLGamepadAxis -> IO Bool
sdlGamepadHasAxis (SDLGamepad ptr) axis = toBool <$> sdlGamepadHasAxis_ ptr (fromIntegral $ fromEnum axis)

-- | Get axis state
foreign import ccall "SDL_GetGamepadAxis"
  sdlGetGamepadAxis_ :: Ptr SDLGamepad -> CInt -> IO Int16

sdlGetGamepadAxis :: SDLGamepad -> SDLGamepadAxis -> IO Int16
sdlGetGamepadAxis (SDLGamepad ptr) axis = sdlGetGamepadAxis_ ptr (fromIntegral $ fromEnum axis)

-- | Convert string to gamepad button
foreign import ccall "SDL_GetGamepadButtonFromString"
  sdlGetGamepadButtonFromString_ :: CString -> IO CInt

sdlGetGamepadButtonFromString :: String -> IO SDLGamepadButton
sdlGetGamepadButtonFromString str = withCString str $ \cstr ->
  toEnum . fromIntegral <$> sdlGetGamepadButtonFromString_ cstr

-- | Convert gamepad button to string
foreign import ccall "SDL_GetGamepadStringForButton"
  sdlGetGamepadStringForButtonRaw :: CInt -> IO CString

sdlGetGamepadStringForButton :: SDLGamepadButton -> IO (Maybe String)
sdlGetGamepadStringForButton btn = do
  cstr <- sdlGetGamepadStringForButtonRaw (fromIntegral $ fromEnum btn)
  if cstr == nullPtr then return Nothing else Just <$> peekCString cstr

-- | Check if gamepad has button
foreign import ccall "SDL_GamepadHasButton"
  sdlGamepadHasButton_ :: Ptr SDLGamepad -> CInt -> IO CBool

sdlGamepadHasButton :: SDLGamepad -> SDLGamepadButton -> IO Bool
sdlGamepadHasButton (SDLGamepad ptr) btn = toBool <$> sdlGamepadHasButton_ ptr (fromIntegral $ fromEnum btn)

-- | Get button state
foreign import ccall "SDL_GetGamepadButton"
  sdlGetGamepadButton_ :: Ptr SDLGamepad -> CInt -> IO CBool

sdlGetGamepadButton :: SDLGamepad -> SDLGamepadButton -> IO Bool
sdlGetGamepadButton (SDLGamepad ptr) btn = toBool <$> sdlGetGamepadButton_ ptr (fromIntegral $ fromEnum btn)

-- | Get button label for type
foreign import ccall "SDL_GetGamepadButtonLabelForType"
  sdlGetGamepadButtonLabelForType_ :: CInt -> CInt -> IO CInt

sdlGetGamepadButtonLabelForType :: SDLGamepadType -> SDLGamepadButton -> IO SDLGamepadButtonLabel
sdlGetGamepadButtonLabelForType typ btn =
  toEnum . fromIntegral <$> sdlGetGamepadButtonLabelForType_ (fromIntegral $ fromEnum typ) (fromIntegral $ fromEnum btn)

-- | Get button label
foreign import ccall "SDL_GetGamepadButtonLabel"
  sdlGetGamepadButtonLabel_ :: Ptr SDLGamepad -> CInt -> IO CInt

sdlGetGamepadButtonLabel :: SDLGamepad -> SDLGamepadButton -> IO SDLGamepadButtonLabel
sdlGetGamepadButtonLabel (SDLGamepad ptr) btn =
  toEnum . fromIntegral <$> sdlGetGamepadButtonLabel_ ptr (fromIntegral $ fromEnum btn)

-- | Get number of touchpads
foreign import ccall "SDL_GetNumGamepadTouchpads"
  sdlGetNumGamepadTouchpads :: Ptr SDLGamepad -> IO CInt

-- | Get number of touchpad fingers
foreign import ccall "SDL_GetNumGamepadTouchpadFingers"
  sdlGetNumGamepadTouchpadFingers :: Ptr SDLGamepad -> CInt -> IO CInt

-- | Get touchpad finger state
foreign import ccall "SDL_GetGamepadTouchpadFinger"
  sdlGetGamepadTouchpadFinger_ :: Ptr SDLGamepad -> CInt -> CInt -> Ptr CBool -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CBool

sdlGetGamepadTouchpadFinger :: SDLGamepad -> Int -> Int -> IO (Bool, Maybe Float, Maybe Float, Maybe Float)
sdlGetGamepadTouchpadFinger (SDLGamepad ptr) touchpad finger =
  alloca $ \downPtr ->
  alloca $ \xPtr ->
  alloca $ \yPtr ->
  alloca $ \pressurePtr -> do
    success <- sdlGetGamepadTouchpadFinger_ ptr (fromIntegral touchpad) (fromIntegral finger) downPtr xPtr yPtr pressurePtr
    if not $ toBool success
      then return (False, Nothing, Nothing, Nothing)
      else do
        down <- toBool <$> peek downPtr
        x <- realToFrac <$> peek xPtr  -- Convert CFloat to Float
        y <- realToFrac <$> peek yPtr  -- Convert CFloat to Float
        pressure <- realToFrac <$> peek pressurePtr  -- Convert CFloat to Float
        return (down, Just x, Just y, Just pressure)

-- | Check if gamepad has sensor
foreign import ccall "SDL_GamepadHasSensor"
  sdlGamepadHasSensor_ :: Ptr SDLGamepad -> CInt -> IO CBool

sdlGamepadHasSensor :: SDLGamepad -> SDLSensorType -> IO Bool
sdlGamepadHasSensor (SDLGamepad ptr) (SDLSensorType typ) = toBool <$> sdlGamepadHasSensor_ ptr typ

-- | Enable/disable sensor
foreign import ccall "SDL_SetGamepadSensorEnabled"
  sdlSetGamepadSensorEnabled_ :: Ptr SDLGamepad -> CInt -> CBool -> IO CBool

sdlSetGamepadSensorEnabled :: SDLGamepad -> SDLSensorType -> Bool -> IO Bool
sdlSetGamepadSensorEnabled (SDLGamepad ptr) (SDLSensorType typ) enabled =
  toBool <$> sdlSetGamepadSensorEnabled_ ptr typ (fromBool enabled)

-- | Check if sensor is enabled
foreign import ccall "SDL_GamepadSensorEnabled"
  sdlGamepadSensorEnabled_ :: Ptr SDLGamepad -> CInt -> IO CBool

sdlGamepadSensorEnabled :: SDLGamepad -> SDLSensorType -> IO Bool
sdlGamepadSensorEnabled (SDLGamepad ptr) (SDLSensorType typ) =
  toBool <$> sdlGamepadSensorEnabled_ ptr typ

-- | Get sensor data rate
foreign import ccall "SDL_GetGamepadSensorDataRate"
  sdlGetGamepadSensorDataRate :: Ptr SDLGamepad -> CInt -> IO CFloat

-- | Get sensor data
foreign import ccall "SDL_GetGamepadSensorData"
  sdlGetGamepadSensorData_ :: Ptr SDLGamepad -> CInt -> Ptr CFloat -> CInt -> IO CBool

sdlGetGamepadSensorData :: SDLGamepad -> SDLSensorType -> Int -> IO (Maybe [Float])
sdlGetGamepadSensorData (SDLGamepad ptr) (SDLSensorType typ) numValues = withArray (replicate numValues 0) $ \dataPtr -> do
  success <- sdlGetGamepadSensorData_ ptr typ dataPtr (fromIntegral numValues)
  if toBool success
    then Just . map realToFrac <$> peekArray numValues dataPtr
    else return Nothing

-- | Rumble gamepad
foreign import ccall "SDL_RumbleGamepad"
  sdlRumbleGamepad_ :: Ptr SDLGamepad -> Word16 -> Word16 -> Word32 -> IO CBool

sdlRumbleGamepad :: SDLGamepad -> Word16 -> Word16 -> Word32 -> IO Bool
sdlRumbleGamepad (SDLGamepad ptr) low high duration =
  toBool <$> sdlRumbleGamepad_ ptr low high duration

-- | Rumble gamepad triggers
foreign import ccall "SDL_RumbleGamepadTriggers"
  sdlRumbleGamepadTriggers_ :: Ptr SDLGamepad -> Word16 -> Word16 -> Word32 -> IO CBool

sdlRumbleGamepadTriggers :: SDLGamepad -> Word16 -> Word16 -> Word32 -> IO Bool
sdlRumbleGamepadTriggers (SDLGamepad ptr) left right duration =
  toBool <$> sdlRumbleGamepadTriggers_ ptr left right duration

-- | Set gamepad LED
foreign import ccall "SDL_SetGamepadLED"
  sdlSetGamepadLED_ :: Ptr SDLGamepad -> Word8 -> Word8 -> Word8 -> IO CBool

sdlSetGamepadLED :: SDLGamepad -> Word8 -> Word8 -> Word8 -> IO Bool
sdlSetGamepadLED (SDLGamepad ptr) r g b = toBool <$> sdlSetGamepadLED_ ptr r g b

-- | Send gamepad effect
foreign import ccall "SDL_SendGamepadEffect"
  sdlSendGamepadEffect_ :: Ptr SDLGamepad -> Ptr () -> CInt -> IO CBool

sdlSendGamepadEffect :: SDLGamepad -> Ptr () -> Int -> IO Bool
sdlSendGamepadEffect (SDLGamepad ptr) dataPtr size =
  toBool <$> sdlSendGamepadEffect_ ptr dataPtr (fromIntegral size)

-- | Close gamepad
foreign import ccall "SDL_CloseGamepad"
  sdlCloseGamepad :: Ptr SDLGamepad -> IO ()

-- | Get Apple SF Symbols name for button
foreign import ccall "SDL_GetGamepadAppleSFSymbolsNameForButton"
  sdlGetGamepadAppleSFSymbolsNameForButtonRaw :: Ptr SDLGamepad -> CInt -> IO CString

sdlGetGamepadAppleSFSymbolsNameForButton :: SDLGamepad -> SDLGamepadButton -> IO (Maybe String)
sdlGetGamepadAppleSFSymbolsNameForButton (SDLGamepad ptr) btn = do
  cstr <- sdlGetGamepadAppleSFSymbolsNameForButtonRaw ptr (fromIntegral $ fromEnum btn)
  if cstr == nullPtr then return Nothing else Just <$> peekCString cstr

-- | Get Apple SF Symbols name for axis
foreign import ccall "SDL_GetGamepadAppleSFSymbolsNameForAxis"
  sdlGetGamepadAppleSFSymbolsNameForAxisRaw :: Ptr SDLGamepad -> CInt -> IO CString

sdlGetGamepadAppleSFSymbolsNameForAxis :: SDLGamepad -> SDLGamepadAxis -> IO (Maybe String)
sdlGetGamepadAppleSFSymbolsNameForAxis (SDLGamepad ptr) axis = do
  cstr <- sdlGetGamepadAppleSFSymbolsNameForAxisRaw ptr (fromIntegral $ fromEnum axis)
  if cstr == nullPtr then return Nothing else Just <$> peekCString cstr
