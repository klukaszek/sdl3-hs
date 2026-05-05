{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

#include <SDL3/SDL_sensor.h>

module SDL3.Raw.Sensor
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
  , sdlGetSensorsRaw
  , sdlGetSensorNameForIDRaw
  , sdlGetSensorTypeForIDRaw
  , sdlGetSensorNonPortableTypeForIDRaw
  , sdlOpenSensorRaw
  , sdlGetSensorFromIDRaw
  , sdlGetSensorPropertiesRaw
  , sdlGetSensorNameRaw
  , sdlGetSensorTypeRaw
  , sdlGetSensorNonPortableTypeRaw
  , sdlGetSensorIDRaw
  , sdlGetSensorDataRaw
  , sdlCloseSensorRaw
  , sdlUpdateSensorsRaw
  ) where

import Foreign hiding (free)
import Foreign.C.String
import Foreign.C.Types
import SDL3.Raw.Properties (SDLPropertiesID)
import SDL3.Raw.Stdinc

newtype SDLSensor = SDLSensor {unSDLSensor :: Ptr SDLSensor}
  deriving (Show, Eq)

type SDLSensorID = Word32

sdlStandardGravity :: Float
sdlStandardGravity = 9.80665

newtype SDLSensorType = SDLSensorType CInt
  deriving newtype (Show, Eq, Ord, Storable, Num)

pattern SDL_SENSOR_INVALID = SDLSensorType (#{const SDL_SENSOR_INVALID})
pattern SDL_SENSOR_UNKNOWN = SDLSensorType #{const SDL_SENSOR_UNKNOWN}
pattern SDL_SENSOR_ACCEL = SDLSensorType #{const SDL_SENSOR_ACCEL}
pattern SDL_SENSOR_GYRO = SDLSensorType #{const SDL_SENSOR_GYRO}
pattern SDL_SENSOR_ACCEL_L = SDLSensorType #{const SDL_SENSOR_ACCEL_L}
pattern SDL_SENSOR_GYRO_L = SDLSensorType #{const SDL_SENSOR_GYRO_L}
pattern SDL_SENSOR_ACCEL_R = SDLSensorType #{const SDL_SENSOR_ACCEL_R}
pattern SDL_SENSOR_GYRO_R = SDLSensorType #{const SDL_SENSOR_GYRO_R}

foreign import ccall "SDL_GetSensors"
  sdlGetSensorsRaw :: Ptr CInt -> IO (Ptr Word32)

foreign import ccall "SDL_GetSensorNameForID"
  sdlGetSensorNameForIDRaw :: Word32 -> IO CString

foreign import ccall "SDL_GetSensorTypeForID"
  sdlGetSensorTypeForIDRaw :: SDLSensorID -> IO CInt

foreign import ccall "SDL_GetSensorNonPortableTypeForID"
  sdlGetSensorNonPortableTypeForIDRaw :: SDLSensorID -> IO CInt

foreign import ccall "SDL_OpenSensor"
  sdlOpenSensorRaw :: SDLSensorID -> IO (Ptr SDLSensor)

foreign import ccall "SDL_GetSensorFromID"
  sdlGetSensorFromIDRaw :: SDLSensorID -> IO (Ptr SDLSensor)

foreign import ccall "SDL_GetSensorProperties"
  sdlGetSensorPropertiesRaw :: Ptr SDLSensor -> IO SDLPropertiesID

foreign import ccall "SDL_GetSensorName"
  sdlGetSensorNameRaw :: Ptr SDLSensor -> IO CString

foreign import ccall "SDL_GetSensorType"
  sdlGetSensorTypeRaw :: Ptr SDLSensor -> IO CInt

foreign import ccall "SDL_GetSensorNonPortableType"
  sdlGetSensorNonPortableTypeRaw :: Ptr SDLSensor -> IO CInt

foreign import ccall "SDL_GetSensorID"
  sdlGetSensorIDRaw :: Ptr SDLSensor -> IO SDLSensorID

foreign import ccall "SDL_GetSensorData"
  sdlGetSensorDataRaw :: Ptr SDLSensor -> Ptr CFloat -> CInt -> IO SDLBool

foreign import ccall "SDL_CloseSensor"
  sdlCloseSensorRaw :: Ptr SDLSensor -> IO ()

foreign import ccall "SDL_UpdateSensors"
  sdlUpdateSensorsRaw :: IO ()
