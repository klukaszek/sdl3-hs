module Main where

import SDL
import SDL.Camera
import Control.Monad
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  -- Check compiled version
  putStrLn $ "Compiled SDL Version: " ++ show sdlVersion
  when (sdlVersionAtLeast 3 2 0) $
    putStrLn "Compiled with at least SDL 3.2.0"

  -- Get linked version
  linkedVersion <- sdlGetVersion
  putStrLn $ "Linked SDL Version: " ++ show linkedVersion

  -- Initialize SDL with camera subsystem
  initSuccess <- sdlInit [InitCamera]
  unless initSuccess $ do
    putStrLn "Failed to initialize SDL with camera support!"
    err <- sdlGetError
    putStrLn $ "Error: " ++ err
    exitFailure

  -- Check initialized subsystems
  initializedSystems <- sdlWasInit []
  putStrLn "Initialized subsystems:"
  mapM_ printSubsystem initializedSystems

  -- List available cameras
  putStrLn "Enumerating cameras..."
  cameras <- sdlGetCameras
  if null cameras
    then putStrLn "No cameras found."
    else do
      putStrLn $ "Found " ++ show (length cameras) ++ " camera(s):"
      forM_ cameras $ \camId -> do
        mName <- sdlGetCameraName camId
        pos <- sdlGetCameraPosition camId
        putStrLn $ "  - ID: " ++ show camId ++ 
                   ", Name: " ++ maybe "Unknown" id mName ++ 
                   ", Position: " ++ show pos

      -- Open the first camera
      let firstCamera = head cameras
      putStrLn $ "Opening camera with ID " ++ show firstCamera ++ "..."
      mCamera <- sdlOpenCamera firstCamera Nothing
      case mCamera of
        Nothing -> do
          putStrLn "Failed to open camera!"
          err <- sdlGetError
          putStrLn $ "Error: " ++ err
        Just camera -> do
          putStrLn "Camera opened successfully."
          
          -- Check permission state
          permState <- sdlGetCameraPermissionState camera
          putStrLn $ "Permission state: " ++ 
                     case permState of
                       1  -> "Approved"
                       -1 -> "Denied"
                       _  -> "Pending"

          -- Get camera format
          mFormat <- sdlGetCameraFormat camera
          case mFormat of
            Nothing -> putStrLn "Failed to get camera format."
            Just fmt -> putStrLn $ "Camera format: " ++ show fmt

          -- Close the camera
          sdlCloseCamera camera
          putStrLn "Camera closed."

  -- Quit SDL
  sdlQuit
  putStrLn "SDL shutdown complete."
  exitSuccess

-- Helper function to print subsystem names
printSubsystem :: InitFlag -> IO ()
printSubsystem flag = putStrLn $ "  - " ++ case flag of
  InitAudio    -> "Audio"
  InitVideo    -> "Video"
  InitJoystick -> "Joystick"
  InitHaptic   -> "Haptic"
  InitGamepad  -> "Gamepad"
  InitEvents   -> "Events"
  InitSensor   -> "Sensor"
  InitCamera   -> "Camera"
  _            -> "Unknown subsystem"
