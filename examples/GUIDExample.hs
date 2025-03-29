module Main where

import SDL
import SDL.Guid
import Control.Monad
import System.Exit (exitFailure, exitSuccess)
import Foreign.Ptr (nullPtr)
import Data.Word (Word8)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  -- Basic SDL initialization from your example
  putStrLn $ "Compiled SDL Version: " ++ show sdlVersion
  when (sdlVersionAtLeast 3 2 0) $
    putStrLn "Compiled with at least SDL 3.2.0"

  linkedVersion <- sdlGetVersion
  putStrLn $ "Linked SDL Version: " ++ show linkedVersion

  revision <- sdlGetRevision
  putStrLn $ "SDL Revision: " ++ revision

  initSuccess <- sdlInit [InitVideo, InitGamepad]
  unless initSuccess $ do
    putStrLn "Failed to initialize SDL!"
    exitFailure

  -- GUID example section
  putStrLn "\n=== GUID Example ==="

  -- Create a sample GUID (normally this would come from an SDL input device)
  let sampleBytes = take 16 $ cycle [0x12, 0x34, 0x56, 0x78] :: [Word8]
      sampleGUID = SDLGUID sampleBytes

  putStrLn "Original GUID bytes:"
  print $ unSDLGUID sampleGUID

  -- Convert GUID to string
  guidStr <- sdlGUIDToString sampleGUID
  putStrLn $ "GUID as string: " ++ guidStr

  -- Convert string back to GUID
  guidBack <- sdlStringToGUID guidStr
  putStrLn "Converted back to GUID bytes:"
  print $ unSDLGUID guidBack

  -- Verify roundtrip conversion
  let isMatch = unSDLGUID sampleGUID == unSDLGUID guidBack
  putStrLn $ "Roundtrip conversion successful: " ++ show isMatch

  -- Example with an invalid string
  putStrLn "\nTesting with invalid GUID string:"
  invalidGuid <- sdlStringToGUID "not-a-valid-guid"
  putStrLn "Result of invalid string conversion:"
  print $ unSDLGUID invalidGuid

  -- Clean up
  putStrLn "\nShutting down SDL..."
  sdlQuit

  putStrLn "Application terminated successfully"
  exitSuccess

-- Helper function to print subsystem names (from your example)
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
