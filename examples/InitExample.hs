module Main where

import SDL
import Control.Monad
import System.Exit (exitFailure, exitSuccess)
import Foreign.Ptr (nullPtr)

main :: IO ()
main = do
  -- Check compiled version
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  when (sdlVersionAtLeast 3 3 0) $
    sdlLog "Compiled with at least SDL 3.3.0"

  -- Get linked version
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  -- Get revision
  revision <- sdlGetRevision
  sdlLog $ "SDL Revision: " ++ revision

  -- Set some basic metadata about our application
  appMetadataSuccess <- sdlSetAppMetadata 
    "SDL3 Haskell!"
    "1.0.0"
    "com.example.sdlapp"
  
  unless appMetadataSuccess $ do
    sdlLog "Failed to set app metadata!"
  
  -- Add some additional metadata
  _ <- sdlSetAppMetadataProperty propAppMetadataCreator "Kyle Lukaszek"
  _ <- sdlSetAppMetadataProperty propAppMetadataCopyright "Copyright (c) 2025"
  _ <- sdlSetAppMetadataProperty propAppMetadataType "Demo"
  
  -- Print some metadata to verify it was set
  name <- sdlGetAppMetadataProperty propAppMetadataName
  sdlLog $ "App Name: " ++ maybe "Not set" id name
  
  -- Initialize SDL with video and events subsystems
  initSuccess <- sdlInit [SDL_INIT_VIDEO]
  
  unless initSuccess $ do
    sdlLog "Failed to initialize SDL!"
    exitFailure
  
  -- Check what subsystems are initialized
  initializedSystems <- sdlWasInit []
  sdlLog "Initialized subsystems:"
  mapM_ printSubsystem initializedSystems
  
  sdlLog "Would create window here..."
  sdlLog "Application running..."
  
  -- For the main thread callback, use a simpler callback
  -- that doesn't try to print to avoid potential issues
  isMain <- sdlIsMainThread
  sdlLog $ "Are we on the main thread? " ++ show isMain
  
  -- If we're already on the main thread, running on main thread is redundant
  -- but we'll do it for demonstration
  if isMain
    then sdlLog "Already on main thread, calling directly:"
    else sdlLog "Running callback on main thread:"
  
  -- Use the simpler callback that doesn't do IO operations
  _ <- sdlRunOnMainThread (\_ -> pure ()) nullPtr True
  sdlLog "Callback completed"
  
  sdlLog "Shutting down SDL..."
  sdlQuit
 
  sdlLog "Application terminated successfully"
  exitSuccess

-- Helper function to print subsystem names
printSubsystem :: SDLInitFlags -> IO ()
printSubsystem flag = sdlLog $ "  - " ++ case flag of
  SDL_INIT_AUDIO    -> "Audio"
  SDL_INIT_VIDEO    -> "Video"
  SDL_INIT_JOYSTICK -> "Joystick"
  SDL_INIT_HAPTIC   -> "Haptic"
  SDL_INIT_GAMEPAD  -> "Gamepad"
  SDL_INIT_EVENTS   -> "Events"
  SDL_INIT_SENSOR   -> "Sensor"
  SDL_INIT_CAMERA   -> "Camera"
  _            -> "Unknown subsystem"
