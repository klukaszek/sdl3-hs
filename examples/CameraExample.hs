module Main where

import Control.Monad
import SDL
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  -- Check compiled version
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  when (sdlVersionAtLeast 3 3 0) $
    sdlLog "Compiled with at least SDL 3.3.0"

  -- Get linked version
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  -- Initialize SDL with camera subsystem
  initSuccess <- sdlInit [SDL_INIT_CAMERA]
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
        sdlLog $
          "  - ID: "
            ++ show camId
            ++ ", Name: "
            ++ maybe "Unknown" id mName
            ++ ", Position: "
            ++ show pos

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
          sdlLog $
            "Permission state: "
              ++ case permState of
                1 -> "Approved"
                -1 -> "Denied"
                _ -> "Pending"

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
printSubsystem :: SDLInitFlags -> IO ()
printSubsystem flag =
  sdlLog $
    "  - " ++ case flag of
      SDL_INIT_AUDIO -> "Audio"
      SDL_INIT_VIDEO -> "Video"
      SDL_INIT_JOYSTICK -> "Joystick"
      SDL_INIT_HAPTIC -> "Haptic"
      SDL_INIT_GAMEPAD -> "Gamepad"
      SDL_INIT_EVENTS -> "Events"
      SDL_INIT_SENSOR -> "Sensor"
      SDL_INIT_CAMERA -> "Camera"
      _ -> "Unknown subsystem"
