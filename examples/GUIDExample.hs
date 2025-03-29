module Main where

import SDL
import SDL.GUID
import Control.Monad
import System.Exit (exitFailure, exitSuccess)
import Foreign.Ptr (nullPtr)
import Data.Word (Word8)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  -- Basic SDL initialization from your example
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  when (sdlVersionAtLeast 3 2 0) $
    sdlLog "Compiled with at least SDL 3.2.0"

  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  revision <- sdlGetRevision
  sdlLog $ "SDL Revision: " ++ revision

  initSuccess <- sdlInit [InitVideo, InitGamepad]
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

-- Helper function to print subsystem names (from your example)
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
