module Main where

import SDL
import Control.Monad
import Foreign.Marshal.Alloc (alloca, mallocBytes)
import Foreign.Storable (poke, peek)
import System.Exit (exitFailure, exitSuccess)

-- Check power state
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

  -- Set a hint before initialization
  putStrLn "Setting hint SDL_VIDEO_ALLOW_SCREENSAVER to enable screensaver..."
  success <- sdlSetHint sdlHintVideoAllowScreensaver "1"
  if success
    then putStrLn "Hint set successfully."
    else do
      putStrLn "Failed to set hint!"
      err <- sdlGetError
      putStrLn $ "Error: " ++ err


  -- Initialize SDL with video and events subsystems
  initSuccess <- sdlInit [InitVideo]
  unless initSuccess $ do
    putStrLn "Failed to initialize SDL!"
    exitFailure

  -- Check initialized subsystems
  initializedSystems <- sdlWasInit []
  putStrLn "Initialized subsystems:"
  mapM_ printSubsystem initializedSystems

  -- Query the hint
  mHintValue <- sdlGetHint sdlHintVideoAllowScreensaver
  putStrLn $ "SDL_VIDEO_ALLOW_SCREENSAVER value: " ++ maybe "Not set" id mHintValue

  sdlQuit


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
