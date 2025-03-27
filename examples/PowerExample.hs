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

  -- Initialize SDL with video and events subsystems
  initSuccess <- sdlInit [InitVideo]
  unless initSuccess $ do
    putStrLn "Failed to initialize SDL!"
    exitFailure

  -- Check initialized subsystems
  initializedSystems <- sdlWasInit []
  putStrLn "Initialized subsystems:"
  mapM_ printSubsystem initializedSystems

  putStrLn "Checking power state..."
  alloca $ \secondsPtr ->
    alloca $ \percentPtr -> do
      mState <- sdlGetPowerInfo (Just secondsPtr) (Just percentPtr)
      case mState of
        Nothing -> do
          putStrLn "Failed to get power info!"
          err <- sdlGetError
          putStrLn $ "Error: " ++ err
        Just state -> do
          seconds <- peek secondsPtr
          percent <- peek percentPtr
          putStrLn $ "Power State: " ++ show state
          putStrLn $ "Seconds remaining: " ++ (if seconds == -1 then "Unknown" else show seconds)
          putStrLn $ "Percent remaining: " ++ (if percent == -1 then "Unknown" else show percent ++ "%")

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
