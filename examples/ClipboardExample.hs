module Main where

import SDL
import Control.Monad (when, unless)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  -- Initialize SDL with video and events subsystems
  initSuccess <- sdlInit [InitVideo]
  unless initSuccess $ do
    sdlLog "Failed to initialize SDL!"
    exitFailure

  -- Check initialized subsystems
  initializedSystems <- sdlWasInit []
  sdlLog "Initialized subsystems:"
  mapM_ printSubsystem initializedSystems

  -- Set some text to the clipboard
  success <- sdlSetClipboardText "Hello, Clipboard!"
  when success $ sdlLog "Successfully set clipboard text"

  -- Check if clipboard has text
  hasText <- sdlHasClipboardText
  sdlLog $ "Clipboard has text: " ++ show hasText

  -- Retrieve text from clipboard
  clipboardContent <- sdlGetClipboardText
  case clipboardContent of
    Just text -> sdlLog $ "Clipboard contents: " ++ text
    Nothing -> sdlLog "Failed to retrieve clipboard text"

  -- Demonstrate primary selection (on systems that support it)
  _ <- sdlSetPrimarySelectionText "Primary Selection Text"
  primaryText <- sdlGetPrimarySelectionText
  case primaryText of
    Just text -> sdlLog $ "Primary selection: " ++ text
    Nothing -> sdlLog "Failed to retrieve primary selection"

  -- Get clipboard MIME types
  mimeTypes <- sdlGetClipboardMimeTypes
  sdlLog "Available MIME types:"
  mapM_ sdlLog mimeTypes

  sdlQuit
  exitSuccess

-- Helper function to print subsystem names
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
