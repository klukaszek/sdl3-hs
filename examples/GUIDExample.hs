module Main where

import Control.Monad
import Data.Word (Word8)
import SDL3
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  -- Basic SDL initialization from your example
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  when (sdlVersionAtLeast 3 3 0) $
    sdlLog "Compiled with at least SDL 3.3.0"

  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  revision <- sdlGetRevision
  sdlLog $ "SDL Revision: " ++ revision

  initSuccess <- sdlInit [SDL_INIT_VIDEO, SDL_INIT_GAMEPAD]
  unless initSuccess $ do
    sdlLog "Failed to initialize SDL!"
    exitFailure

  -- GUID example section
  sdlLog "\n=== GUID Example ==="

  -- Create a sample GUID (normally this would come from an SDL input device)
  let sampleBytes = take 16 $ cycle [0x12, 0x34, 0x56, 0x78] :: [Word8]
      sampleGUID = SDLGUID sampleBytes

  sdlLog "Original GUID bytes:"
  print $ unSDLGUID sampleGUID

  -- Convert GUID to string
  guidStr <- sdlGUIDToString sampleGUID
  sdlLog $ "GUID as string: " ++ guidStr

  -- Convert string back to GUID
  guidBack <- sdlStringToGUID guidStr
  sdlLog "Converted back to GUID bytes:"
  print $ unSDLGUID guidBack

  -- Verify roundtrip conversion
  let isMatch = unSDLGUID sampleGUID == unSDLGUID guidBack
  sdlLog $ "Roundtrip conversion successful: " ++ show isMatch

  -- Example with an invalid string
  sdlLog "\nTesting with invalid GUID string:"
  invalidGuid <- sdlStringToGUID "not-a-valid-guid"
  sdlLog "Result of invalid string conversion:"
  print $ unSDLGUID invalidGuid

  -- Clean up
  sdlLog "\nShutting down SDL..."
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
