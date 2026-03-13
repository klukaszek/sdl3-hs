module SDL3.Wrapped.Gamepad
  ( module SDL3.Raw.Gamepad
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

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int16)
import Data.Word (Word8, Word16, Word32)
import Foreign.C.String (CString)
import Foreign.C.Types (CBool, CChar, CFloat, CInt, CULLong)
import Foreign.Ptr (Ptr)
import SDL3.IOStream (SDLIOStream, sdlUnsafeToRawIOStream)
import SDL3.Joystick (SDLJoystick, SDLJoystickConnectionState, SDLJoystickID)
import SDL3.Raw.Gamepad hiding
  ( sdlAddGamepadMapping
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
  )
import SDL3.Raw.GUID (SDLGUID)
import SDL3.Raw.Power (SDLPowerState)
import SDL3.Raw.Properties (SDLPropertiesID)
import SDL3.Raw.Sensor (SDLSensorType)
import qualified SDL3.Raw.Gamepad as Raw

sdlAddGamepadMapping :: MonadIO m => CString -> m CInt
sdlAddGamepadMapping = liftIO . Raw.sdlAddGamepadMapping

sdlAddGamepadMappingsFromIO :: MonadIO m => SDLIOStream -> CBool -> m CInt
sdlAddGamepadMappingsFromIO io closeIO =
  liftIO $ Raw.sdlAddGamepadMappingsFromIO (sdlUnsafeToRawIOStream io) closeIO

sdlAddGamepadMappingsFromFile :: MonadIO m => CString -> m CInt
sdlAddGamepadMappingsFromFile = liftIO . Raw.sdlAddGamepadMappingsFromFile

sdlReloadGamepadMappings :: MonadIO m => m Bool
sdlReloadGamepadMappings = liftIO Raw.sdlReloadGamepadMappings

sdlGetGamepadMappings :: MonadIO m => m [String]
sdlGetGamepadMappings = liftIO Raw.sdlGetGamepadMappings

sdlGetGamepadMappingForGUID :: MonadIO m => SDLGUID -> m (Maybe String)
sdlGetGamepadMappingForGUID = liftIO . Raw.sdlGetGamepadMappingForGUID

sdlGetGamepadMapping :: MonadIO m => SDLGamepad -> m (Maybe String)
sdlGetGamepadMapping = liftIO . Raw.sdlGetGamepadMapping

sdlSetGamepadMapping :: MonadIO m => SDLJoystickID -> Maybe String -> m Bool
sdlSetGamepadMapping jid = liftIO . Raw.sdlSetGamepadMapping jid

sdlHasGamepad :: MonadIO m => m Bool
sdlHasGamepad = liftIO Raw.sdlHasGamepad

sdlGetGamepads :: MonadIO m => m [SDLJoystickID]
sdlGetGamepads = liftIO Raw.sdlGetGamepads

sdlIsGamepad :: MonadIO m => SDLJoystickID -> m Bool
sdlIsGamepad = liftIO . Raw.sdlIsGamepad

sdlGetGamepadNameForID :: MonadIO m => SDLJoystickID -> m (Maybe String)
sdlGetGamepadNameForID = liftIO . Raw.sdlGetGamepadNameForID

sdlGetGamepadPathForID :: MonadIO m => SDLJoystickID -> m (Maybe String)
sdlGetGamepadPathForID = liftIO . Raw.sdlGetGamepadPathForID

sdlGetGamepadPlayerIndexForID :: MonadIO m => SDLJoystickID -> m Int
sdlGetGamepadPlayerIndexForID = liftIO . Raw.sdlGetGamepadPlayerIndexForID

sdlGetGamepadGUIDForID :: MonadIO m => Word32 -> m (Ptr SDLGUID)
sdlGetGamepadGUIDForID = liftIO . Raw.sdlGetGamepadGUIDForID

sdlGetGamepadVendorForID :: MonadIO m => SDLJoystickID -> m Word16
sdlGetGamepadVendorForID = liftIO . Raw.sdlGetGamepadVendorForID

sdlGetGamepadProductForID :: MonadIO m => SDLJoystickID -> m Word16
sdlGetGamepadProductForID = liftIO . Raw.sdlGetGamepadProductForID

sdlGetGamepadProductVersionForID :: MonadIO m => SDLJoystickID -> m Word16
sdlGetGamepadProductVersionForID = liftIO . Raw.sdlGetGamepadProductVersionForID

sdlGetGamepadTypeForID :: MonadIO m => SDLJoystickID -> m SDLGamepadType
sdlGetGamepadTypeForID = liftIO . Raw.sdlGetGamepadTypeForID

sdlGetRealGamepadTypeForID :: MonadIO m => SDLJoystickID -> m SDLGamepadType
sdlGetRealGamepadTypeForID = liftIO . Raw.sdlGetRealGamepadTypeForID

sdlGetGamepadMappingForID :: MonadIO m => SDLJoystickID -> m (Maybe String)
sdlGetGamepadMappingForID = liftIO . Raw.sdlGetGamepadMappingForID

sdlOpenGamepad :: MonadIO m => SDLJoystickID -> m (Maybe SDLGamepad)
sdlOpenGamepad = liftIO . Raw.sdlOpenGamepad

sdlGetGamepadFromID :: MonadIO m => SDLJoystickID -> m (Maybe SDLGamepad)
sdlGetGamepadFromID = liftIO . Raw.sdlGetGamepadFromID

sdlGetGamepadFromPlayerIndex :: MonadIO m => Int -> m (Maybe SDLGamepad)
sdlGetGamepadFromPlayerIndex = liftIO . Raw.sdlGetGamepadFromPlayerIndex

sdlGetGamepadProperties :: MonadIO m => SDLGamepad -> m SDLPropertiesID
sdlGetGamepadProperties (SDLGamepad gamepadPtr) = liftIO $ Raw.sdlGetGamepadProperties gamepadPtr

sdlGetGamepadID :: MonadIO m => SDLGamepad -> m SDLJoystickID
sdlGetGamepadID (SDLGamepad gamepadPtr) = liftIO $ Raw.sdlGetGamepadID gamepadPtr

sdlGetGamepadName :: MonadIO m => SDLGamepad -> m (Maybe String)
sdlGetGamepadName = liftIO . Raw.sdlGetGamepadName

sdlGetGamepadPath :: MonadIO m => SDLGamepad -> m (Maybe String)
sdlGetGamepadPath = liftIO . Raw.sdlGetGamepadPath

sdlGetGamepadType :: MonadIO m => SDLGamepad -> m SDLGamepadType
sdlGetGamepadType = liftIO . Raw.sdlGetGamepadType

sdlGetRealGamepadType :: MonadIO m => SDLGamepad -> m SDLGamepadType
sdlGetRealGamepadType = liftIO . Raw.sdlGetRealGamepadType

sdlGetGamepadPlayerIndex :: MonadIO m => SDLGamepad -> m CInt
sdlGetGamepadPlayerIndex (SDLGamepad gamepadPtr) = liftIO $ Raw.sdlGetGamepadPlayerIndex gamepadPtr

sdlSetGamepadPlayerIndex :: MonadIO m => SDLGamepad -> Int -> m Bool
sdlSetGamepadPlayerIndex gamepad = liftIO . Raw.sdlSetGamepadPlayerIndex gamepad

sdlGetGamepadVendor :: MonadIO m => SDLGamepad -> m Word16
sdlGetGamepadVendor (SDLGamepad gamepadPtr) = liftIO $ Raw.sdlGetGamepadVendor gamepadPtr

sdlGetGamepadProduct :: MonadIO m => SDLGamepad -> m Word16
sdlGetGamepadProduct (SDLGamepad gamepadPtr) = liftIO $ Raw.sdlGetGamepadProduct gamepadPtr

sdlGetGamepadProductVersion :: MonadIO m => SDLGamepad -> m Word16
sdlGetGamepadProductVersion (SDLGamepad gamepadPtr) =
  liftIO $ Raw.sdlGetGamepadProductVersion gamepadPtr

sdlGetGamepadFirmwareVersion :: MonadIO m => SDLGamepad -> m Word16
sdlGetGamepadFirmwareVersion (SDLGamepad gamepadPtr) =
  liftIO $ Raw.sdlGetGamepadFirmwareVersion gamepadPtr

sdlGetGamepadSerial :: MonadIO m => SDLGamepad -> m (Maybe String)
sdlGetGamepadSerial = liftIO . Raw.sdlGetGamepadSerial

sdlGetGamepadSteamHandle :: MonadIO m => SDLGamepad -> m CULLong
sdlGetGamepadSteamHandle (SDLGamepad gamepadPtr) = liftIO $ Raw.sdlGetGamepadSteamHandle gamepadPtr

sdlGetGamepadConnectionState :: MonadIO m => SDLGamepad -> m SDLJoystickConnectionState
sdlGetGamepadConnectionState = liftIO . Raw.sdlGetGamepadConnectionState

sdlGetGamepadPowerInfo :: MonadIO m => SDLGamepad -> m (SDLPowerState, Maybe Int)
sdlGetGamepadPowerInfo = liftIO . Raw.sdlGetGamepadPowerInfo

sdlGamepadConnected :: MonadIO m => SDLGamepad -> m Bool
sdlGamepadConnected = liftIO . Raw.sdlGamepadConnected

sdlGetGamepadJoystick :: MonadIO m => SDLGamepad -> m SDLJoystick
sdlGetGamepadJoystick = liftIO . Raw.sdlGetGamepadJoystick

sdlSetGamepadEventsEnabled :: MonadIO m => CBool -> m ()
sdlSetGamepadEventsEnabled = liftIO . Raw.sdlSetGamepadEventsEnabled

sdlGamepadEventsEnabled :: MonadIO m => m Bool
sdlGamepadEventsEnabled = liftIO Raw.sdlGamepadEventsEnabled

sdlGetGamepadBindings :: MonadIO m => SDLGamepad -> m [SDLGamepadBinding]
sdlGetGamepadBindings = liftIO . Raw.sdlGetGamepadBindings

sdlUpdateGamepads :: MonadIO m => m ()
sdlUpdateGamepads = liftIO Raw.sdlUpdateGamepads

sdlGetGamepadTypeFromString :: MonadIO m => String -> m SDLGamepadType
sdlGetGamepadTypeFromString = liftIO . Raw.sdlGetGamepadTypeFromString

sdlGetGamepadStringForType :: MonadIO m => SDLGamepadType -> m (Maybe String)
sdlGetGamepadStringForType = liftIO . Raw.sdlGetGamepadStringForType

sdlGetGamepadAxisFromString :: MonadIO m => String -> m SDLGamepadAxis
sdlGetGamepadAxisFromString = liftIO . Raw.sdlGetGamepadAxisFromString

sdlGetGamepadStringForAxis :: MonadIO m => SDLGamepadAxis -> m (Maybe String)
sdlGetGamepadStringForAxis = liftIO . Raw.sdlGetGamepadStringForAxis

sdlGamepadHasAxis :: MonadIO m => SDLGamepad -> SDLGamepadAxis -> m Bool
sdlGamepadHasAxis gamepad = liftIO . Raw.sdlGamepadHasAxis gamepad

sdlGetGamepadAxis :: MonadIO m => SDLGamepad -> SDLGamepadAxis -> m Int16
sdlGetGamepadAxis gamepad = liftIO . Raw.sdlGetGamepadAxis gamepad

sdlGetGamepadButtonFromString :: MonadIO m => String -> m SDLGamepadButton
sdlGetGamepadButtonFromString = liftIO . Raw.sdlGetGamepadButtonFromString

sdlGetGamepadStringForButton :: MonadIO m => SDLGamepadButton -> m (Maybe String)
sdlGetGamepadStringForButton = liftIO . Raw.sdlGetGamepadStringForButton

sdlGamepadHasButton :: MonadIO m => SDLGamepad -> SDLGamepadButton -> m Bool
sdlGamepadHasButton gamepad = liftIO . Raw.sdlGamepadHasButton gamepad

sdlGetGamepadButton :: MonadIO m => SDLGamepad -> SDLGamepadButton -> m Bool
sdlGetGamepadButton gamepad = liftIO . Raw.sdlGetGamepadButton gamepad

sdlGetGamepadButtonLabelForType :: MonadIO m => SDLGamepadType -> SDLGamepadButton -> m SDLGamepadButtonLabel
sdlGetGamepadButtonLabelForType typ = liftIO . Raw.sdlGetGamepadButtonLabelForType typ

sdlGetGamepadButtonLabel :: MonadIO m => SDLGamepad -> SDLGamepadButton -> m SDLGamepadButtonLabel
sdlGetGamepadButtonLabel gamepad = liftIO . Raw.sdlGetGamepadButtonLabel gamepad

sdlGetNumGamepadTouchpads :: MonadIO m => SDLGamepad -> m CInt
sdlGetNumGamepadTouchpads (SDLGamepad gamepadPtr) = liftIO $ Raw.sdlGetNumGamepadTouchpads gamepadPtr

sdlGetNumGamepadTouchpadFingers :: MonadIO m => SDLGamepad -> CInt -> m CInt
sdlGetNumGamepadTouchpadFingers (SDLGamepad gamepadPtr) =
  liftIO . Raw.sdlGetNumGamepadTouchpadFingers gamepadPtr

sdlGetGamepadTouchpadFinger :: MonadIO m => SDLGamepad -> Int -> Int -> m (Bool, Maybe Float, Maybe Float, Maybe Float)
sdlGetGamepadTouchpadFinger gamepad touchpad = liftIO . Raw.sdlGetGamepadTouchpadFinger gamepad touchpad

sdlGamepadHasSensor :: MonadIO m => SDLGamepad -> SDLSensorType -> m Bool
sdlGamepadHasSensor gamepad = liftIO . Raw.sdlGamepadHasSensor gamepad

sdlSetGamepadSensorEnabled :: MonadIO m => SDLGamepad -> SDLSensorType -> Bool -> m Bool
sdlSetGamepadSensorEnabled gamepad sensorType enabled =
  liftIO $ Raw.sdlSetGamepadSensorEnabled gamepad sensorType enabled

sdlGamepadSensorEnabled :: MonadIO m => SDLGamepad -> SDLSensorType -> m Bool
sdlGamepadSensorEnabled gamepad = liftIO . Raw.sdlGamepadSensorEnabled gamepad

sdlGetGamepadSensorDataRate :: MonadIO m => SDLGamepad -> CInt -> m CFloat
sdlGetGamepadSensorDataRate (SDLGamepad gamepadPtr) =
  liftIO . Raw.sdlGetGamepadSensorDataRate gamepadPtr

sdlGetGamepadSensorData :: MonadIO m => SDLGamepad -> SDLSensorType -> Int -> m (Maybe [Float])
sdlGetGamepadSensorData gamepad sensorType = liftIO . Raw.sdlGetGamepadSensorData gamepad sensorType

sdlRumbleGamepad :: MonadIO m => SDLGamepad -> Word16 -> Word16 -> Word32 -> m Bool
sdlRumbleGamepad gamepad low high = liftIO . Raw.sdlRumbleGamepad gamepad low high

sdlRumbleGamepadTriggers :: MonadIO m => SDLGamepad -> Word16 -> Word16 -> Word32 -> m Bool
sdlRumbleGamepadTriggers gamepad left right = liftIO . Raw.sdlRumbleGamepadTriggers gamepad left right

sdlSetGamepadLED :: MonadIO m => SDLGamepad -> Word8 -> Word8 -> Word8 -> m Bool
sdlSetGamepadLED gamepad r g = liftIO . Raw.sdlSetGamepadLED gamepad r g

sdlSendGamepadEffect :: MonadIO m => SDLGamepad -> Ptr () -> Int -> m Bool
sdlSendGamepadEffect gamepad effectPtr = liftIO . Raw.sdlSendGamepadEffect gamepad effectPtr

sdlCloseGamepad :: MonadIO m => SDLGamepad -> m ()
sdlCloseGamepad = liftIO . Raw.sdlCloseGamepad

sdlGetGamepadAppleSFSymbolsNameForButton :: MonadIO m => SDLGamepad -> SDLGamepadButton -> m (Maybe String)
sdlGetGamepadAppleSFSymbolsNameForButton gamepad =
  liftIO . Raw.sdlGetGamepadAppleSFSymbolsNameForButton gamepad

sdlGetGamepadAppleSFSymbolsNameForAxis :: MonadIO m => SDLGamepad -> SDLGamepadAxis -> m (Maybe String)
sdlGetGamepadAppleSFSymbolsNameForAxis gamepad =
  liftIO . Raw.sdlGetGamepadAppleSFSymbolsNameForAxis gamepad
