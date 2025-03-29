module Main where

import SDL
import Control.Monad
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  -- Check compiled version
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  when (sdlVersionAtLeast 3 2 0) $
    sdlLog "Compiled with at least SDL 3.2.0"

  -- Get linked version
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  -- Initialize SDL with camera subsystem
  initSuccess <- sdlInit [InitCamera]
  unless initSuccess $ do
    sdlLog "Failed to initialize SDL with camera support!"
    err <- sdlGetError
    sdlLog $ "Error: " ++ err
    exitFailure

  -- Check initialized subsystems
  initializedSystems <- sdlWasInit []
  sdlLog "Initialized subsystems:"
  mapM_ printSubsystem initializedSystems

  -- List available cameras
  sdlLog "Enumerating cameras..."
  cameras <- sdlGetCameras
  if null cameras
    then sdlLog "No cameras found."
    else do
      sdlLog $ "Found " ++ show (length cameras) ++ " camera(s):"
      forM_ cameras $ \camId -> do
        mName <- sdlGetCameraName camId
        pos <- sdlGetCameraPosition camId
        sdlLog $ "  - ID: " ++ show camId ++ 
                   ", Name: " ++ maybe "Unknown" id mName ++ 
                   ", Position: " ++ show pos

      -- Open the first camera
      let firstCamera = head cameras
      sdlLog $ "Opening camera with ID " ++ show firstCamera ++ "..."
      mCamera <- sdlOpenCamera firstCamera Nothing
      case mCamera of
        Nothing -> do
          sdlLog "Failed to open camera!"
          err <- sdlGetError
          sdlLog $ "Error: " ++ err
        Just camera -> do
          sdlLog "Camera opened successfully."
          
          -- Check permission state
          permState <- sdlGetCameraPermissionState camera
          sdlLog $ "Permission state: " ++ 
                     case permState of
                       1  -> "Approved"
                       -1 -> "Denied"
                       _  -> "Pending"

          -- Get camera format
          mFormat <- sdlGetCameraFormat camera
          case mFormat of
            Nothing -> sdlLog "Failed to get camera format."
            Just fmt -> sdlLog $ "Camera format: " ++ show fmt

          -- Close the camera
          sdlCloseCamera camera
          sdlLog "Camera closed."

  -- Quit SDL
  sdlQuit
  sdlLog "SDL shutdown complete."
  exitSuccess

-- Helper function to print subsystem names
printSubsystem :: InitFlag -> IO ()
printSubsystem flag = sdlLog $ "  - " ++ case flag of
  InitAudio    -> "Audio"
  InitVideo    -> "Video"
  InitJoystick -> "Joystick"
  InitHaptic   -> "Haptic"
  InitGamepad  -> "Gamepad"
  InitEvents   -> "Events"
  InitSensor   -> "Sensor"
  InitCamera   -> "Camera"
  _            -> "Unknown subsystem"
