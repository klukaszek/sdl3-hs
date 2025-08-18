module Main where

import Control.Monad (unless, when)
import Foreign.Ptr (nullPtr)
import SDL
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  -- Initialize SDL with basic subsystems
  initSuccess <- sdlInit [SDL_INIT_VIDEO, SDL_INIT_EVENTS]
  unless initSuccess $ do
    sdlLog "Failed to initialize SDL!"
    exitFailure

  -- Set basic app metadata
  appMetadataSuccess <-
    sdlSetAppMetadata
      "SDL Filesystem Demo"
      "1.0.0"
      "com.example.filesystemdemo"
  unless appMetadataSuccess $ do
    sdlLog "Failed to set app metadata!"

  -- Get and print the base path
  basePath <- sdlGetBasePath
  sdlLog $ "Base Path: " ++ maybe "Not available" id basePath

  -- Get and print the preferences path
  prefPath <- sdlGetPrefPath "ExampleOrg" "FilesystemDemo"
  case prefPath of
    Nothing -> sdlLog "Failed to get preferences path!"
    Just path -> do
      sdlLog $ "Preferences Path: " ++ path

      -- Create a test directory in the preferences path
      let testDir = path ++ "test_dir"
      dirCreated <- sdlCreateDirectory testDir
      sdlLog $ "Created test directory: " ++ show dirCreated

      -- List contents of the preferences path
      dirContents <- sdlGlobDirectory path Nothing (SDLGlobFlags 0)
      sdlLog "Contents of preferences path:"
      case dirContents of
        Nothing -> sdlLog "  Failed to list directory contents!"
        Just contents -> mapM_ (sdlLog . ("  - " ++)) contents

  -- Get and print the user's Documents folder
  docsPath <- sdlGetUserFolder SDL_FOLDER_DOCUMENTS
  sdlLog $ "Documents Path: " ++ maybe "Not available" id docsPath

  -- Clean up and quit
  sdlQuit
  sdlLog "Application terminated successfully"
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
