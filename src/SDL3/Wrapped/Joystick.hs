module SDL3.Wrapped.Joystick
  ( module SDL3.Raw.Joystick
  , SDLVirtualJoystickDesc(..)
  , sdlHasJoystick
  , sdlGetJoysticks
  , sdlGetJoystickNameForID
  , sdlGetJoystickPathForID
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
  , sdlGetJoystickName
  , sdlGetJoystickPath
  , sdlSetJoystickPlayerIndex
  , sdlJoystickConnected
  , sdlSetJoystickEventsEnabled
  , sdlJoystickEventsEnabled
  , sdlGetJoystickAxisInitialState
  , sdlGetJoystickBall
  , sdlGetJoystickButton
  , sdlRumbleJoystick
  , sdlRumbleJoystickTriggers
  , sdlSetJoystickLED
  , sdlSendJoystickEffect
  , sdlGetJoystickConnectionState
  , sdlGetJoystickPowerInfo
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int16)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CBool, CFloat, CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.Storable (Storable, peek, poke)
import SDL3.Raw.Joystick hiding
  ( SDLVirtualJoystickDesc(..)
  , sdlHasJoystick
  , sdlGetJoysticks
  , sdlGetJoystickNameForID
  , sdlGetJoystickPathForID
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
  , sdlGetJoystickName
  , sdlGetJoystickPath
  , sdlSetJoystickPlayerIndex
  , sdlJoystickConnected
  , sdlSetJoystickEventsEnabled
  , sdlJoystickEventsEnabled
  , sdlGetJoystickAxisInitialState
  , sdlGetJoystickBall
  , sdlGetJoystickButton
  , sdlRumbleJoystick
  , sdlRumbleJoystickTriggers
  , sdlSetJoystickLED
  , sdlSendJoystickEffect
  , sdlGetJoystickConnectionState
  , sdlGetJoystickPowerInfo
  )
import qualified SDL3.Raw.Joystick as Raw
import SDL3.Power (SDLPowerState)
import SDL3.Sensor (SDLSensorType)

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

cbool :: CBool -> Bool
cbool = (/= 0)

peekMaybeCString :: CString -> IO (Maybe String)
peekMaybeCString ptr
  | ptr == nullPtr = pure Nothing
  | otherwise = Just <$> peekCString ptr

toMaybeJoystick :: Ptr SDLJoystick -> Maybe SDLJoystick
toMaybeJoystick ptr
  | ptr == nullPtr = Nothing
  | otherwise = Just (SDLJoystick ptr)

withArrayOrNull :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withArrayOrNull [] action = action nullPtr
withArrayOrNull values action = withArray values action

withVirtualJoystickDesc :: SDLVirtualJoystickDesc -> (Ptr Raw.SDLVirtualJoystickDesc -> IO a) -> IO a
withVirtualJoystickDesc desc action =
  withCString (virtualName desc) $ \namePtr ->
    withArrayOrNull (virtualTouchpads desc) $ \touchpadPtr ->
      withArrayOrNull (virtualSensors desc) $ \sensorPtr ->
        alloca $ \descPtr -> do
          let rawDesc = Raw.SDLVirtualJoystickDesc
                (virtualVersion desc)
                (virtualType desc)
                (virtualVendorID desc)
                (virtualProductID desc)
                (virtualNAxes desc)
                (virtualNButtons desc)
                (virtualNBalls desc)
                (virtualNHats desc)
                (virtualNTouchpads desc)
                (virtualNSensors desc)
                (virtualButtonMask desc)
                (virtualAxisMask desc)
                namePtr
                touchpadPtr
                sensorPtr
                (virtualUserdata desc)
                (virtualUpdate desc)
                (virtualSetPlayerIdx desc)
                (virtualRumble desc)
                (virtualRumbleTrig desc)
                (virtualSetLED desc)
                (virtualSendEffect desc)
                (virtualSetSensors desc)
                (virtualCleanup desc)
          poke descPtr rawDesc
          action descPtr

sdlHasJoystick :: MonadIO m => m Bool
sdlHasJoystick = liftIO $ cbool <$> Raw.sdlHasJoystick

sdlGetJoysticks :: MonadIO m => m [SDLJoystickID]
sdlGetJoysticks = liftIO Raw.sdlGetJoysticks

sdlGetJoystickNameForID :: MonadIO m => SDLJoystickID -> m (Maybe String)
sdlGetJoystickNameForID joystickID = liftIO $ Raw.sdlGetJoystickNameForID joystickID >>= peekMaybeCString

sdlGetJoystickPathForID :: MonadIO m => SDLJoystickID -> m (Maybe String)
sdlGetJoystickPathForID joystickID = liftIO $ Raw.sdlGetJoystickPathForID joystickID >>= peekMaybeCString

sdlOpenJoystick :: MonadIO m => SDLJoystickID -> m (Maybe SDLJoystick)
sdlOpenJoystick = liftIO . Raw.sdlOpenJoystick

sdlGetJoystickFromID :: MonadIO m => SDLJoystickID -> m (Maybe SDLJoystick)
sdlGetJoystickFromID joystickID = liftIO $ toMaybeJoystick <$> Raw.sdlGetJoystickFromID joystickID

sdlGetJoystickFromPlayerIndex :: MonadIO m => CInt -> m (Maybe SDLJoystick)
sdlGetJoystickFromPlayerIndex playerIndex = liftIO $ toMaybeJoystick <$> Raw.sdlGetJoystickFromPlayerIndex playerIndex

sdlAttachVirtualJoystick :: MonadIO m => SDLVirtualJoystickDesc -> m SDLJoystickID
sdlAttachVirtualJoystick desc = liftIO $ withVirtualJoystickDesc desc Raw.sdlAttachVirtualJoystick

sdlDetachVirtualJoystick :: MonadIO m => SDLJoystickID -> m Bool
sdlDetachVirtualJoystick = liftIO . fmap cbool . Raw.sdlDetachVirtualJoystick

sdlIsJoystickVirtual :: MonadIO m => SDLJoystickID -> m Bool
sdlIsJoystickVirtual = liftIO . fmap cbool . Raw.sdlIsJoystickVirtual

sdlSetJoystickVirtualAxis :: MonadIO m => SDLJoystick -> CInt -> Int16 -> m Bool
sdlSetJoystickVirtualAxis (SDLJoystick joystickPtr) axis value =
  liftIO $ cbool <$> Raw.sdlSetJoystickVirtualAxis joystickPtr axis value

sdlSetJoystickVirtualBall :: MonadIO m => SDLJoystick -> CInt -> Int16 -> Int16 -> m Bool
sdlSetJoystickVirtualBall (SDLJoystick joystickPtr) ball dx dy =
  liftIO $ cbool <$> Raw.sdlSetJoystickVirtualBall joystickPtr ball dx dy

sdlSetJoystickVirtualButton :: MonadIO m => SDLJoystick -> CInt -> Bool -> m Bool
sdlSetJoystickVirtualButton (SDLJoystick joystickPtr) button pressed =
  liftIO $ cbool <$> Raw.sdlSetJoystickVirtualButton joystickPtr button (if pressed then 1 else 0)

sdlSetJoystickVirtualHat :: MonadIO m => SDLJoystick -> CInt -> Word8 -> m Bool
sdlSetJoystickVirtualHat (SDLJoystick joystickPtr) hat value =
  liftIO $ cbool <$> Raw.sdlSetJoystickVirtualHat joystickPtr hat value

sdlSetJoystickVirtualTouchpad :: MonadIO m => SDLJoystick -> CInt -> CInt -> Bool -> Float -> Float -> Float -> m Bool
sdlSetJoystickVirtualTouchpad (SDLJoystick joystickPtr) touchpad finger down x y pressure =
  liftIO $
    cbool <$> Raw.sdlSetJoystickVirtualTouchpad joystickPtr touchpad finger (if down then 1 else 0)
      (realToFrac x) (realToFrac y) (realToFrac pressure)

sdlSendJoystickVirtualSensorData :: MonadIO m => SDLJoystick -> SDLSensorType -> Word64 -> [Float] -> m Bool
sdlSendJoystickVirtualSensorData joystick sensorType timestamp values =
  liftIO $ Raw.sdlSendJoystickVirtualSensorData joystick sensorType timestamp values

sdlGetJoystickName :: MonadIO m => SDLJoystick -> m (Maybe String)
sdlGetJoystickName (SDLJoystick joystickPtr) = liftIO $ Raw.sdlGetJoystickName joystickPtr >>= peekMaybeCString

sdlGetJoystickPath :: MonadIO m => SDLJoystick -> m (Maybe String)
sdlGetJoystickPath (SDLJoystick joystickPtr) = liftIO $ Raw.sdlGetJoystickPath joystickPtr >>= peekMaybeCString

sdlSetJoystickPlayerIndex :: MonadIO m => SDLJoystick -> CInt -> m Bool
sdlSetJoystickPlayerIndex (SDLJoystick joystickPtr) playerIndex =
  liftIO $ cbool <$> Raw.sdlSetJoystickPlayerIndex joystickPtr playerIndex

sdlJoystickConnected :: MonadIO m => SDLJoystick -> m Bool
sdlJoystickConnected (SDLJoystick joystickPtr) =
  liftIO $ cbool <$> Raw.sdlJoystickConnected joystickPtr

sdlSetJoystickEventsEnabled :: MonadIO m => Bool -> m ()
sdlSetJoystickEventsEnabled enabled =
  liftIO $ Raw.sdlSetJoystickEventsEnabled (if enabled then 1 else 0)

sdlJoystickEventsEnabled :: MonadIO m => m Bool
sdlJoystickEventsEnabled = liftIO $ cbool <$> Raw.sdlJoystickEventsEnabled

sdlGetJoystickAxisInitialState :: MonadIO m => SDLJoystick -> CInt -> m (Maybe Int16)
sdlGetJoystickAxisInitialState (SDLJoystick joystickPtr) axis =
  liftIO $ alloca $ \valuePtr -> do
    hasValue <- Raw.sdlGetJoystickAxisInitialState joystickPtr axis valuePtr
    if cbool hasValue
      then Just <$> peek valuePtr
      else pure Nothing

sdlGetJoystickBall :: MonadIO m => SDLJoystick -> CInt -> m (Maybe (CInt, CInt))
sdlGetJoystickBall (SDLJoystick joystickPtr) ball =
  liftIO $ alloca $ \dxPtr ->
    alloca $ \dyPtr -> do
      ok <- Raw.sdlGetJoystickBall joystickPtr ball dxPtr dyPtr
      if cbool ok
        then do
          dx <- peek dxPtr
          dy <- peek dyPtr
          pure (Just (dx, dy))
        else pure Nothing

sdlGetJoystickButton :: MonadIO m => SDLJoystick -> CInt -> m Bool
sdlGetJoystickButton (SDLJoystick joystickPtr) button =
  liftIO $ cbool <$> Raw.sdlGetJoystickButton joystickPtr button

sdlRumbleJoystick :: MonadIO m => SDLJoystick -> Word16 -> Word16 -> Word32 -> m Bool
sdlRumbleJoystick (SDLJoystick joystickPtr) lowFrequency highFrequency duration =
  liftIO $ cbool <$> Raw.sdlRumbleJoystick joystickPtr lowFrequency highFrequency duration

sdlRumbleJoystickTriggers :: MonadIO m => SDLJoystick -> Word16 -> Word16 -> Word32 -> m Bool
sdlRumbleJoystickTriggers (SDLJoystick joystickPtr) leftRumble rightRumble duration =
  liftIO $ cbool <$> Raw.sdlRumbleJoystickTriggers joystickPtr leftRumble rightRumble duration

sdlSetJoystickLED :: MonadIO m => SDLJoystick -> Word8 -> Word8 -> Word8 -> m Bool
sdlSetJoystickLED (SDLJoystick joystickPtr) red green blue =
  liftIO $ cbool <$> Raw.sdlSetJoystickLED joystickPtr red green blue

sdlSendJoystickEffect :: MonadIO m => SDLJoystick -> Ptr () -> CInt -> m Bool
sdlSendJoystickEffect (SDLJoystick joystickPtr) effect effectLength =
  liftIO $ cbool <$> Raw.sdlSendJoystickEffect joystickPtr effect effectLength

sdlGetJoystickConnectionState :: MonadIO m => SDLJoystick -> m SDLJoystickConnectionState
sdlGetJoystickConnectionState (SDLJoystick joystickPtr) =
  liftIO $ SDLJoystickConnectionState <$> Raw.sdlGetJoystickConnectionState joystickPtr

sdlGetJoystickPowerInfo :: MonadIO m => SDLJoystick -> m (SDLPowerState, CInt)
sdlGetJoystickPowerInfo (SDLJoystick joystickPtr) =
  liftIO $ alloca $ \percentPtr -> do
    powerState <- Raw.sdlGetJoystickPowerInfo joystickPtr percentPtr
    batteryPercent <- peek percentPtr
    pure (powerState, batteryPercent)
