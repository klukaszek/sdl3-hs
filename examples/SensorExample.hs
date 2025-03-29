module Main where

import SDL
import Control.Monad
import System.Exit (exitFailure, exitSuccess)
import Foreign.Ptr (nullPtr)

main :: IO ()
main = do
  -- Initialize SDL with sensor subsystem
  initSuccess <- sdlInit [InitSensor]
  
  unless initSuccess $ do
    sdlLog "Failed to initialize SDL with sensor support!"
    exitFailure
  
  sdlLog "SDL initialized with sensor support"
  
  -- Get list of available sensors
  maybeSensors <- sdlGetSensors
  case maybeSensors of
    Nothing -> do
      sdlLog "No sensors found or error occurred"
      cleanupAndExit
    Just sensorIds -> do
      sdlLog $ "Found " ++ show (length sensorIds) ++ " sensor(s)"
      
      -- Process each sensor
      forM_ sensorIds $ \sensorId -> do
        sdlLog $ "\nProcessing sensor ID: " ++ show sensorId
        
        -- Get sensor name
        maybeName <- sdlGetSensorNameForID sensorId
        sdlLog $ "Name: " ++ maybe "Unknown" id maybeName
        
        -- Get sensor type
        sensorType <- sdlGetSensorTypeForID sensorId
        sdlLog $ "Type: " ++ show sensorType
        
        -- Open the sensor
        maybeSensor <- sdlOpenSensor sensorId
        case maybeSensor of
          Nothing -> sdlLog "Failed to open sensor"
          Just sensor -> do
            sdlLog "Successfully opened sensor"
            
            -- Get some sensor data (assuming 3 values for accel/gyro)
            maybeData <- sdlGetSensorData sensor 3
            case maybeData of
              Nothing -> sdlLog "Failed to get sensor data"
              Just values -> do
                sdlLog "Sensor data:"
                case sensorType of
                  SDL_SENSOR_ACCEL -> do
                    sdlLog $ "X acceleration: " ++ show (values !! 0) ++ " m/s²"
                    sdlLog $ "Y acceleration: " ++ show (values !! 1) ++ " m/s²"
                    sdlLog $ "Z acceleration: " ++ show (values !! 2) ++ " m/s²"
                  SDL_SENSOR_GYRO -> do
                    sdlLog $ "X rotation: " ++ show (values !! 0) ++ " rad/s"
                    sdlLog $ "Y rotation: " ++ show (values !! 1) ++ " rad/s"
                    sdlLog $ "Z rotation: " ++ show (values !! 2) ++ " rad/s"
                  _ -> sdlLog $ "Raw values: " ++ show values
            
            -- Close the sensor
            sdlCloseSensor sensor
            sdlLog "Sensor closed"
  
  cleanupAndExit

-- Helper function to cleanup and exit
cleanupAndExit :: IO ()
cleanupAndExit = do
  sdlLog "\nShutting down SDL..."
  sdlQuit
  sdlLog "Application terminated successfully"
  exitSuccess

-- Helper function to display sensor type
showSensorType :: SDLSensorType -> String
showSensorType t = case t of
  SDL_SENSOR_INVALID -> "Invalid"
  SDL_SENSOR_UNKNOWN -> "Unknown"
  SDL_SENSOR_ACCEL   -> "Accelerometer"
  SDL_SENSOR_GYRO    -> "Gyroscope"
  SDL_SENSOR_ACCEL_L -> "Left Joy-Con Accelerometer"
  SDL_SENSOR_GYRO_L  -> "Left Joy-Con Gyroscope"
  SDL_SENSOR_ACCEL_R -> "Right Joy-Con Accelerometer"
  SDL_SENSOR_GYRO_R  -> "Right Joy-Con Gyroscope"
