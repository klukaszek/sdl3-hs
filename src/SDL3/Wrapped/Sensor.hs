{-# LANGUAGE PatternSynonyms #-}

module SDL3.Wrapped.Sensor
  ( SDLSensor(..)
  , SDLSensorID
  , SDLSensorType(..)
  , pattern SDL_SENSOR_INVALID
  , pattern SDL_SENSOR_UNKNOWN
  , pattern SDL_SENSOR_ACCEL
  , pattern SDL_SENSOR_GYRO
  , pattern SDL_SENSOR_ACCEL_L
  , pattern SDL_SENSOR_GYRO_L
  , pattern SDL_SENSOR_ACCEL_R
  , pattern SDL_SENSOR_GYRO_R
  , sdlStandardGravity
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

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign hiding (free)
import Foreign.C.String
import Foreign.C.Types
import SDL3.Raw.Properties (SDLPropertiesID)
import SDL3.Raw.Sensor
  ( SDLSensor(..)
  , SDLSensorID
  , SDLSensorType(..)
  , pattern SDL_SENSOR_ACCEL
  , pattern SDL_SENSOR_ACCEL_L
  , pattern SDL_SENSOR_ACCEL_R
  , pattern SDL_SENSOR_GYRO
  , pattern SDL_SENSOR_GYRO_L
  , pattern SDL_SENSOR_GYRO_R
  , pattern SDL_SENSOR_INVALID
  , pattern SDL_SENSOR_UNKNOWN
  , sdlStandardGravity
  )
import qualified SDL3.Raw.Sensor as Raw
import SDL3.Stdinc

sdlGetSensors :: MonadIO m => m (Maybe [SDLSensorID])
sdlGetSensors = liftIO $
  alloca $ \countPtr -> do
    sensorArray <- Raw.sdlGetSensorsRaw countPtr
    if sensorArray == nullPtr
      then return Nothing
      else do
        count <- peek countPtr
        sensors <- peekArray (fromIntegral count) sensorArray
        free sensorArray
        return $ Just sensors

sdlGetSensorNameForID :: MonadIO m => SDLSensorID -> m (Maybe String)
sdlGetSensorNameForID sensorId = liftIO $ do
  cStr <- Raw.sdlGetSensorNameForIDRaw sensorId
  if cStr == nullPtr
    then return Nothing
    else Just <$> peekCString cStr

sdlGetSensorTypeForID :: MonadIO m => SDLSensorID -> m SDLSensorType
sdlGetSensorTypeForID sensorId = liftIO $ do
  sensorType <- Raw.sdlGetSensorTypeForIDRaw sensorId
  return (fromIntegral sensorType)

sdlGetSensorNonPortableTypeForID :: MonadIO m => SDLSensorID -> m CInt
sdlGetSensorNonPortableTypeForID = liftIO . Raw.sdlGetSensorNonPortableTypeForIDRaw

sdlOpenSensor :: MonadIO m => SDLSensorID -> m (Maybe SDLSensor)
sdlOpenSensor sensorId = liftIO $ do
  sensor <- Raw.sdlOpenSensorRaw sensorId
  if sensor == nullPtr
    then return Nothing
    else return $ Just $ SDLSensor sensor

sdlGetSensorFromID :: MonadIO m => SDLSensorID -> m (Maybe SDLSensor)
sdlGetSensorFromID sensorId = liftIO $ do
  sensor <- Raw.sdlGetSensorFromIDRaw sensorId
  if sensor == nullPtr
    then return Nothing
    else return $ Just $ SDLSensor sensor

sdlGetSensorProperties :: MonadIO m => SDLSensor -> m SDLPropertiesID
sdlGetSensorProperties (SDLSensor sensor) = liftIO $
  Raw.sdlGetSensorPropertiesRaw sensor

sdlGetSensorName :: MonadIO m => SDLSensor -> m (Maybe String)
sdlGetSensorName (SDLSensor sensor) = liftIO $ do
  cStr <- Raw.sdlGetSensorNameRaw sensor
  if cStr == nullPtr
    then return Nothing
    else Just <$> peekCString cStr

sdlGetSensorType :: MonadIO m => SDLSensor -> m SDLSensorType
sdlGetSensorType (SDLSensor sensor) = liftIO $ do
  sensorType <- Raw.sdlGetSensorTypeRaw sensor
  return (fromIntegral sensorType)

sdlGetSensorNonPortableType :: MonadIO m => Ptr SDLSensor -> m CInt
sdlGetSensorNonPortableType = liftIO . Raw.sdlGetSensorNonPortableTypeRaw

sdlGetSensorID :: MonadIO m => Ptr SDLSensor -> m SDLSensorID
sdlGetSensorID = liftIO . Raw.sdlGetSensorIDRaw

sdlGetSensorData :: MonadIO m => SDLSensor -> Int -> m (Maybe [Float])
sdlGetSensorData (SDLSensor sensor) numValues = liftIO $
  allocaArray numValues $ \dataPtr -> do
    success <- Raw.sdlGetSensorDataRaw sensor dataPtr (fromIntegral numValues)
    if success == sdlTrue
      then do
        values <- peekArray numValues dataPtr
        return $ Just $ map realToFrac values
      else return Nothing

sdlCloseSensor :: MonadIO m => SDLSensor -> m ()
sdlCloseSensor (SDLSensor sensor) = liftIO $
  Raw.sdlCloseSensorRaw sensor

sdlUpdateSensors :: MonadIO m => m ()
sdlUpdateSensors = liftIO Raw.sdlUpdateSensorsRaw
