module Main where

import SDL
import Prelude hiding (init)
import Control.Monad
import System.Exit (exitFailure, exitSuccess)
import Foreign.Ptr (nullPtr)

main :: IO ()
main = do
  -- Check compiled version
  putStrLn $ "Compiled SDL Version: " ++ show sdlVersion
  when (sdlVersionAtLeast 3 2 0) $
    putStrLn "Compiled with at least SDL 3.2.0"

  -- Get linked version
  linkedVersion <- sdlGetVersion
  putStrLn $ "Linked SDL Version: " ++ show linkedVersion

  -- Get revision
  revision <- sdlGetRevision
  putStrLn $ "SDL Revision: " ++ revision

  -- Set some basic metadata about our application
  appMetadataSuccess <- setAppMetadata 
    "SDL3 Haskell!"
    "1.0.0"
    "com.example.sdlapp"
  
  unless appMetadataSuccess $ do
    putStrLn "Failed to set app metadata!"
  
  -- Add some additional metadata
  _ <- setAppMetadataProperty propAppMetadataCreator "Kyle Lukaszek"
  _ <- setAppMetadataProperty propAppMetadataCopyright "Copyright (c) 2025"
  _ <- setAppMetadataProperty propAppMetadataType "Demo"
  
  -- Print some metadata to verify it was set
  name <- getAppMetadataProperty propAppMetadataName
  putStrLn $ "App Name: " ++ maybe "Not set" id name
  
  -- Initialize SDL with video and events subsystems
  initSuccess <- init [InitVideo]
  
  unless initSuccess $ do
    putStrLn "Failed to initialize SDL!"
    exitFailure
  
  -- Check what subsystems are initialized
  initializedSystems <- wasInit []
  putStrLn "Initialized subsystems:"
  mapM_ printSubsystem initializedSystems
  
  putStrLn "Would create window here..."
  putStrLn "Application running..."
  
  -- For the main thread callback, let's use a simpler callback
  -- that doesn't try to print to avoid potential issues
  isMain <- isMainThread
  putStrLn $ "Are we on the main thread? " ++ show isMain
  
  -- If we're already on the main thread, running on main thread is redundant
  -- but we'll do it for demonstration
  if isMain
    then putStrLn "Already on main thread, calling directly:"
    else putStrLn "Running callback on main thread:"
  
  -- Use the simpler callback that doesn't do IO operations
  _ <- runOnMainThread (\_ -> pure ()) nullPtr True
  putStrLn "Callback completed"
  
  -- When done, clean up and quit
  putStrLn "Shutting down SDL..."
  quit
 
  putStrLn "Application terminated successfully"
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

