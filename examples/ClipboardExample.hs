module Main where

import SDL
import Control.Monad (when, unless)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  -- Initialize SDL with video and events subsystems
  initSuccess <- sdlInit [InitVideo]
  unless initSuccess $ do
    putStrLn "Failed to initialize SDL!"
    exitFailure

  -- Check initialized subsystems
  initializedSystems <- sdlWasInit []
  putStrLn "Initialized subsystems:"
  mapM_ printSubsystem initializedSystems

  -- Set some text to the clipboard
  success <- sdlSetClipboardText "Hello, Clipboard!"
  when success $ putStrLn "Successfully set clipboard text"

  -- Check if clipboard has text
  hasText <- sdlHasClipboardText
  putStrLn $ "Clipboard has text: " ++ show hasText

  -- Retrieve text from clipboard
  clipboardContent <- sdlGetClipboardText
  case clipboardContent of
    Just text -> putStrLn $ "Clipboard contents: " ++ text
    Nothing -> putStrLn "Failed to retrieve clipboard text"

  -- Demonstrate primary selection (on systems that support it)
  _ <- sdlSetPrimarySelectionText "Primary Selection Text"
  primaryText <- sdlGetPrimarySelectionText
  case primaryText of
    Just text -> putStrLn $ "Primary selection: " ++ text
    Nothing -> putStrLn "Failed to retrieve primary selection"

  -- Get clipboard MIME types
  mimeTypes <- sdlGetClipboardMimeTypes
  putStrLn "Available MIME types:"
  mapM_ putStrLn mimeTypes

  sdlQuit
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
