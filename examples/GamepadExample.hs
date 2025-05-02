module Main where

import SDL hiding (abs)
import Control.Monad
import System.Exit (exitFailure, exitSuccess)
import Foreign.C.String (peekCString)
import Data.Word (Word16, Word8)
import Data.Char (chr)
import Control.Concurrent (threadDelay)
import Foreign.Ptr (nullPtr)

main :: IO ()
main = do
    -- Basic SDL initialization
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  when (sdlVersionAtLeast 3 2 0) $
    sdlLog "Compiled with at least SDL 3.2.0"

  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  revision <- sdlGetRevision
  sdlLog $ "SDL Revision: " ++ revision

  initSuccess <- sdlInit [InitVideo, InitJoystick, InitHaptic, InitGamepad]
  unless initSuccess $ do
    sdlLog "Failed to initialize SDL with Joystick and Haptic subsystems!"
    exitFailure

  initializedSystems <- sdlWasInit []
  sdlLog "Initialized subsystems:"
  mapM_ printSubsystem initializedSystems

  sdlLog "SDL initialized with Gamepad subsystem"

  -- Check if any gamepads are connected
  hasGamepad <- sdlHasGamepad
  if not hasGamepad
  then do
    sdlLog "No gamepads detected. Please connect a gamepad and try again."
    sdlQuit
    exitSuccess
  else sdlLog "Gamepad detected!"

  -- Get list of connected gamepads
  gamepads <- sdlGetGamepads
  sdlLog $ "Number of gamepads found: " ++ show (length gamepads)

  -- Test the first gamepad
  case gamepads of
    [] -> sdlLog "No gamepads available to test."
    (jid:_) -> do
      sdlLog $ "Testing gamepad with ID: " ++ show jid
      -- Check if joystick is a gamepad
      isGamepad <- sdlIsGamepad jid
      sdlLog $ "Is this device a gamepad? " ++ show isGamepad

      -- Get gamepad info before opening
      mName <- sdlGetGamepadNameForID jid
      sdlLog $ "Gamepad name: " ++ maybe "Unknown" id mName

      mPath <- sdlGetGamepadPathForID jid
      sdlLog $ "Gamepad path: " ++ maybe "Unknown" id mPath

      playerIndex <- sdlGetGamepadPlayerIndexForID jid
      sdlLog $ "Player index: " ++ show playerIndex

      vendor <- sdlGetGamepadVendorForID jid
      sdlLog $ "Vendor ID: 0x" ++ showHex vendor

      product <- sdlGetGamepadProductForID jid
      sdlLog $ "Product ID: 0x" ++ showHex product

      version <- sdlGetGamepadProductVersionForID jid
      sdlLog $ "Product version: " ++ show version

      gamepadType <- sdlGetGamepadTypeForID jid
      sdlLog $ "Gamepad type: " ++ show gamepadType

      realType <- sdlGetRealGamepadTypeForID jid
      sdlLog $ "Real gamepad type: " ++ show realType

      -- Open the gamepad
      mgamepad <- sdlOpenGamepad jid
      case mgamepad of
        Nothing -> do
          sdlLog "Failed to open gamepad!"
          exitFailure
        Just gamepad -> do
          sdlLog "Gamepad opened successfully."
      
          -- Get more detailed info now that the gamepad is open
          mName' <- sdlGetGamepadName gamepad
          sdlLog $ "Confirmed name: " ++ maybe "Unknown" id mName'
      
          -- Check for specific buttons and axes
          forM_ [SDL_GAMEPAD_BUTTON_SOUTH, SDL_GAMEPAD_BUTTON_EAST, SDL_GAMEPAD_BUTTON_WEST, SDL_GAMEPAD_BUTTON_NORTH ] $ \btn -> do
            hasButton <- sdlGamepadHasButton gamepad btn
            when hasButton $ do
              btnLabel <- sdlGetGamepadButtonLabel gamepad btn
              sdlLog $ "Has button " ++ show btn ++ " with label: " ++ show btnLabel

          forM_ [SDL_GAMEPAD_AXIS_LEFTX, SDL_GAMEPAD_AXIS_LEFTY, SDL_GAMEPAD_AXIS_RIGHTX, SDL_GAMEPAD_AXIS_RIGHTY] $ \axis -> do
            hasAxis <- sdlGamepadHasAxis gamepad axis
            when hasAxis $ do
              mAxisName <- sdlGetGamepadStringForAxis axis
              sdlLog $ "Has axis " ++ show axis ++ maybe "" (\n -> " (" ++ n ++ ")") mAxisName
      
          -- Test rumble if available
          let rumbleTest = do
                sdlLog "Testing rumble (low=16384, high=32768) for 1 second..."
                rumbleSuccess <- sdlRumbleGamepad gamepad 16384 32768 1000
                if rumbleSuccess
                  then sdlLog "Rumble started successfully."
                  else sdlLog "Failed to start rumble."
                threadDelay 1000000  -- Wait 1 second
      
          -- Test trigger rumble if available
          let triggerRumbleTest = do
                sdlLog "Testing trigger rumble (left=16384, right=32768) for 1 second..."
                rumbleSuccess <- sdlRumbleGamepadTriggers gamepad 16384 32768 1000
                if rumbleSuccess
                  then sdlLog "Trigger rumble started successfully."
                  else sdlLog "Failed to start trigger rumble."
                threadDelay 1000000  -- Wait 1 second
      
          -- Test LED if available
          let ledTest = do
                sdlLog "Testing LED (setting to red)..."
                ledSuccess <- sdlSetGamepadLED gamepad 255 0 0
                if ledSuccess
                  then sdlLog "LED set successfully."
                  else sdlLog "Failed to set LED."
      
          -- Run tests
          rumbleTest
          triggerRumbleTest
          ledTest
      
          -- Real-time input monitoring
          sdlLog "\nMonitoring gamepad input for 5 seconds..."
          sdlLog "Press buttons or move sticks to see values..."
      
          forM_ [1..5] $ \_ -> do
            sdlUpdateGamepads  -- Update gamepad state
        
          -- Check some button states
          forM_ [SDL_GAMEPAD_BUTTON_SOUTH, SDL_GAMEPAD_BUTTON_EAST, SDL_GAMEPAD_BUTTON_WEST, SDL_GAMEPAD_BUTTON_NORTH] $ \btn -> do
            pressed <- sdlGetGamepadButton gamepad btn
            when pressed $ sdlLog $ "Button " ++ show btn ++ " is pressed!"
        
          -- Check some axis values
          forM_ [SDL_GAMEPAD_AXIS_LEFTX, SDL_GAMEPAD_AXIS_LEFTY, SDL_GAMEPAD_AXIS_RIGHTX, SDL_GAMEPAD_AXIS_RIGHTY] $ \axis -> do
            value <- sdlGetGamepadAxis gamepad axis
            when (abs (fromIntegral value) > 8000) $ 
              sdlLog $ "Axis " ++ show axis ++ " value: " ++ show value
        
          threadDelay 100000  -- 100ms delay between updates
      
          -- Close gamepad
          sdlCloseGamepad gamepad
          sdlLog "Gamepad closed."

  -- Clean up
  sdlLog "Shutting down SDL..."
  sdlQuit
  sdlLog "Application terminated successfully"
  exitSuccess

-- Helper to show hex values
showHex :: Word16 -> String
showHex 0 = "0000"
showHex n = pad 4 (showHex' n)
  where
    showHex' :: Word16 -> String
    showHex' 0 = ""
    showHex' n = showHex' (n `div` 16) ++ [hexDigit (n `mod` 16)]

    hexDigit :: Word16 -> Char
    hexDigit n 
      | n < 10    = chr (fromEnum '0' + fromIntegral n)
      | otherwise = chr (fromEnum 'a' + fromIntegral (n - 10))

    pad :: Int -> String -> String
    pad n s = replicate (n - length s) '0' ++ s


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
