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
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  when (sdlVersionAtLeast 3 3 0) $
    sdlLog "Compiled with at least SDL 3.3.0"

  -- Get linked version
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  -- Get revision
  revision <- sdlGetRevision
  sdlLog $ "SDL Revision: " ++ revision

  -- Set a hint before initialization
  sdlLog "Setting hint SDL_VIDEO_ALLOW_SCREENSAVER to enable screensaver..."
  success <- sdlSetHint sdlHintVideoAllowScreensaver "1"
  if success
    then sdlLog "Hint set successfully."
    else do
      sdlLog "Failed to set hint!"
      err <- sdlGetError
      sdlLog $ "Error: " ++ err


  -- Initialize SDL with video and events subsystems
  initSuccess <- sdlInit [SDL_INIT_VIDEO, SDL_INIT_EVENTS]
  unless initSuccess $ do
    sdlLog "Failed to initialize SDL!"
    exitFailure

  -- Check initialized subsystems
  initializedSystems <- sdlWasInit []
  sdlLog "Initialized subsystems:"
  mapM_ printSubsystem initializedSystems

  -- Query the hint
  mHintValue <- sdlGetHint sdlHintVideoAllowScreensaver
  sdlLog $ "SDL_VIDEO_ALLOW_SCREENSAVER value: " ++ maybe "Not set" id mHintValue

  sdlQuit

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
