{-# LANGUAGE ForeignFunctionInterface #-}

{-|
Module      : SDL.Sensor
Description : SDL sensor management functions
Copyright   : Kyle Lukaszek, 2025
License     : BSD3

This module provides bindings to the SDL3 sensor API, allowing Haskell applications
to interact with various sensors such as accelerometers and gyroscopes on supported platforms.

To use these functions, SDL_Init() must have been called with the SDL_INIT_SENSOR flag.
-}

module SDL.Sensor
  ( -- * Types
    SDLSensor(..)
  , SDLSensorID(..)
  , SDLSensorType(..)
  , sdlStandardGravity
  
    -- * Sensor Management
  , sdlGetSensors
  , sdlGetSensorNameForID
  , sdlGetSensorTypeForID
  , sdlGetSensorNonPortableTypeForID
  , sdlOpenSensor
  , sdlGetSensorFromID
  , sdlGetSensorProperties
  , sdlGetSensorName
  , sdlGetSensorType
  , sdlGetSensorNonPortableType
  , sdlGetSensorID
  , sdlGetSensorData
  , sdlCloseSensor
  , sdlUpdateSensors
  ) where

import Foreign hiding (free)
import Foreign.C.Types
import Foreign.C.String
import Data.Word
import Control.Monad

import SDL.Stdinc
import SDL.Error
import SDL.Properties

#include <SDL3/SDL_sensor.h>

-- | An opaque handle representing an open SDL sensor
newtype SDLSensor = SDLSensor { unSDLSensor :: Ptr SDLSensor }
  deriving (Show, Eq)

-- | A unique ID for a sensor
type SDLSensorID = Word32

-- | Standard gravity constant for accelerometer sensors (9.80665 m/s²)
sdlStandardGravity :: Float
sdlStandardGravity = 9.80665

-- | Different types of sensors supported by SDL
data SDLSensorType
  = SDL_SENSOR_INVALID    -- ^ Invalid sensor
  | SDL_SENSOR_UNKNOWN    -- ^ Unknown sensor type
  | SDL_SENSOR_ACCEL      -- ^ Accelerometer
  | SDL_SENSOR_GYRO       -- ^ Gyroscope
  | SDL_SENSOR_ACCEL_L    -- ^ Left Joy-Con/Nunchuk accelerometer
  | SDL_SENSOR_GYRO_L     -- ^ Left Joy-Con gyroscope
  | SDL_SENSOR_ACCEL_R    -- ^ Right Joy-Con accelerometer
  | SDL_SENSOR_GYRO_R     -- ^ Right Joy-Con gyroscope
  deriving (Show, Eq, Enum)

instance Storable SDLSensorType where
  sizeOf _ = sizeOf (undefined :: CInt)  -- SDL_SensorType is an int in C
  alignment _ = alignment (undefined :: CInt)
  peek ptr = do
    val <- peek (castPtr ptr :: Ptr CInt)
    return $ toEnum $ fromIntegral val
  poke ptr val = poke (castPtr ptr :: Ptr CInt) (fromIntegral $ fromEnum val)

-- | Get a list of currently connected sensors
foreign import ccall "SDL_GetSensors" sdlGetSensors_ :: Ptr CInt -> IO (Ptr Word32)

sdlGetSensors :: IO (Maybe [SDLSensorID])
sdlGetSensors = do
  alloca $ \countPtr -> do
    sensorArray <- sdlGetSensors_ countPtr
    if sensorArray == nullPtr
      then return Nothing
      else do
        count <- peek countPtr
        sensors <- peekArray (fromIntegral count) sensorArray
        free sensorArray
        return $ Just sensors

-- | Get the name of a sensor by its ID
foreign import ccall "SDL_GetSensorNameForID" sdlGetSensorNameForID_ :: Word32 -> IO CString

sdlGetSensorNameForID :: SDLSensorID -> IO (Maybe String)
sdlGetSensorNameForID sensorId = do
  cStr <- sdlGetSensorNameForID_ sensorId
  if cStr == nullPtr
    then return Nothing
    else Just <$> peekCString cStr

-- | Get the type of a sensor by its ID
foreign import ccall "SDL_GetSensorTypeForID" sdlGetSensorTypeForID_ :: SDLSensorID -> IO CInt

sdlGetSensorTypeForID :: SDLSensorID -> IO SDLSensorType
sdlGetSensorTypeForID sensorId = do
  sensorType <- sdlGetSensorTypeForID_ sensorId
  return $ toEnum (fromIntegral sensorType)

-- | Get the platform-dependent type of a sensor by its ID
foreign import ccall "SDL_GetSensorNonPortableTypeForID" sdlGetSensorNonPortableTypeForID :: SDLSensorID -> IO CInt

-- | Open a sensor for use
foreign import ccall "SDL_OpenSensor" sdlOpenSensor_ :: SDLSensorID -> IO (Ptr SDLSensor)

sdlOpenSensor :: SDLSensorID -> IO (Maybe SDLSensor)
sdlOpenSensor sensorId = do
  sensor <- sdlOpenSensor_ sensorId
  if sensor == nullPtr
    then return Nothing
    else return $ Just $ SDLSensor sensor

-- | Get a sensor from its ID
foreign import ccall "SDL_GetSensorFromID" sdlGetSensorFromID_ :: SDLSensorID -> IO (Ptr SDLSensor)

sdlGetSensorFromID :: SDLSensorID -> IO (Maybe SDLSensor)
sdlGetSensorFromID sensorId = do
  sensor <- sdlGetSensorFromID_ sensorId
  if sensor == nullPtr
    then return Nothing
    else return $ Just $ SDLSensor sensor

-- | Get the properties associated with a sensor
foreign import ccall "SDL_GetSensorProperties" sdlGetSensorProperties_ :: Ptr SDLSensor -> IO Word32

sdlGetSensorProperties :: SDLSensor -> IO SDLPropertiesID
sdlGetSensorProperties (SDLSensor sensor) = do
  props <- sdlGetSensorProperties_ sensor
  return $ props

-- | Get the name of an opened sensor
foreign import ccall "SDL_GetSensorName" sdlGetSensorName_ :: Ptr SDLSensor -> IO CString

sdlGetSensorName :: SDLSensor -> IO (Maybe String)
sdlGetSensorName (SDLSensor sensor) = do
  cStr <- sdlGetSensorName_ sensor
  if cStr == nullPtr
    then return Nothing
    else Just <$> peekCString cStr

-- | Get the type of an opened sensor
foreign import ccall "SDL_GetSensorType" sdlGetSensorType_ :: Ptr SDLSensor -> IO CInt

sdlGetSensorType :: SDLSensor -> IO SDLSensorType
sdlGetSensorType (SDLSensor sensor) = do
  sensorType <- sdlGetSensorType_ sensor
  return $ toEnum (fromIntegral sensorType)

-- | Get the platform-dependent type of an opened sensor
foreign import ccall "SDL_GetSensorNonPortableType" sdlGetSensorNonPortableType :: Ptr SDLSensor -> IO CInt

-- | Get the instance ID of an opened sensor
foreign import ccall "SDL_GetSensorID" sdlGetSensorID :: Ptr SDLSensor -> IO SDLSensorID

-- | Get the current state of an opened sensor
foreign import ccall "SDL_GetSensorData" sdlGetSensorData_ :: Ptr SDLSensor -> Ptr CFloat -> CInt -> IO SDLBool

sdlGetSensorData :: SDLSensor -> Int -> IO (Maybe [Float])
sdlGetSensorData (SDLSensor sensor) numValues = do
  allocaArray numValues $ \dataPtr -> do
    success <- sdlGetSensorData_ sensor dataPtr (fromIntegral numValues)
    if success == sdlTrue  -- Convert SDLBool to Haskell Bool
      then do
        values <- peekArray numValues dataPtr
        return $ Just $ map realToFrac values
      else return Nothing

-- | Close a previously opened sensor
foreign import ccall "SDL_CloseSensor" sdlCloseSensor_ :: Ptr SDLSensor -> IO ()

sdlCloseSensor :: SDLSensor -> IO ()
sdlCloseSensor (SDLSensor sensor) = sdlCloseSensor_ sensor

-- | Update the current state of all open sensors
foreign import ccall "SDL_UpdateSensors" sdlUpdateSensors :: IO ()
