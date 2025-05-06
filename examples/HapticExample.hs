module Main where

import SDL
import SDL.GUID
import SDL.Joystick
import SDL.Haptic
import Control.Monad
import System.Exit (exitFailure, exitSuccess)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types (CFloat(..))
import Foreign.C.String (peekCString)
import Data.Word (Word8, Word32)
import Data.Maybe (fromMaybe, isJust)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  -- Basic SDL initialization
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  when (sdlVersionAtLeast 3 3 0) $
    sdlLog "Compiled with at least SDL 3.3.0"

  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  revision <- sdlGetRevision
  sdlLog $ "SDL Revision: " ++ revision

  initSuccess <- sdlInit [SDL_INIT_VIDEO, SDL_INIT_JOYSTICK, SDL_INIT_HAPTIC, SDL_INIT_GAMEPAD]
  unless initSuccess $ do
    sdlLog "Failed to initialize SDL with Joystick and Haptic subsystems!"
    exitFailure

  -- Check what subsystems are initialized
  initializedSystems <- sdlWasInit []
  sdlLog "Initialized subsystems:"
  mapM_ printSubsystem initializedSystems

  -- === Joystick Testing ===
  sdlLog "\n=== Joystick Testing ==="
  joysticks <- sdlGetJoysticks
  sdlLog $ "Number of joysticks found: " ++ show (length joysticks)

  case joysticks of
    [] -> sdlLog "No joysticks available to test."
    (jid:_) -> do
      sdlLog $ "Testing joystick with ID: " ++ show jid

      -- Open the joystick
      mjoy <- sdlOpenJoystick jid
      case mjoy of
        Nothing -> do
          sdlLog "Failed to open joystick!"
          exitFailure
        Just joy -> do
          sdlLog "Joystick opened successfully."

          -- Query joystick properties
          name <- peekCString =<< sdlGetJoystickName (unSDLJoystick joy)
          sdlLog $ "Joystick name: " ++ name

          axes <- sdlGetNumJoystickAxes (unSDLJoystick joy)
          sdlLog $ "Number of axes: " ++ show axes
          when (axes < 0) $ sdlLog "Failed to get number of axes!"

          buttons <- sdlGetNumJoystickButtons (unSDLJoystick joy)
          sdlLog $ "Number of buttons: " ++ show buttons
          when (buttons < 0) $ sdlLog "Failed to get number of buttons!"

          hats <- sdlGetNumJoystickHats (unSDLJoystick joy)
          sdlLog $ "Number of hats: " ++ show hats
          when (hats < 0) $ sdlLog "Failed to get number of hats!"

          -- Test axis reading (assuming at least one axis)
          when (axes > 0) $ do
            axisVal <- sdlGetJoystickAxis (unSDLJoystick joy) 0
            sdlLog $ "Axis 0 value: " ++ show axisVal

          -- Close joystick
          sdlCloseJoystick (unSDLJoystick joy)
          sdlLog "Joystick closed."

  -- === Haptic Testing ===
  sdlLog "\n=== Haptic Testing ==="
  haptics <- sdlGetHaptics
  sdlLog $ "Number of haptic devices found: " ++ show (length haptics)

  case haptics of
    [] -> sdlLog "No haptic devices available to test."
    (hid:_) -> do
      sdlLog $ "Testing haptic device with ID: " ++ show hid

      -- Open the haptic device
      mhaptic <- sdlOpenHaptic hid
      case mhaptic of
        Nothing -> do
          sdlLog "Failed to open haptic device!"
          exitFailure
        Just haptic -> do
          sdlLog "Haptic device opened successfully."

          -- Query haptic properties
          name <- peekCString =<< sdlGetHapticName haptic
          sdlLog $ "Haptic device name: " ++ name

          maxEffects <- sdlGetMaxHapticEffects haptic
          sdlLog $ "Max effects: " ++ show maxEffects
          when (maxEffects < 0) $ sdlLog "Failed to get max effects!"

          features <- sdlGetHapticFeatures haptic
          sdlLog $ "Supported features (bitmask): " ++ show features

          -- Test simple rumble if supported
          rumbleSupported <- sdlHapticRumbleSupported haptic
          sdlLog $ "Rumble supported: " ++ show rumbleSupported

          when rumbleSupported $ do
            initSuccess <- sdlInitHapticRumble haptic
            unless initSuccess $ do
              sdlLog "Failed to initialize rumble!"
              exitFailure

            sdlLog "Playing rumble effect (50% strength for 1 second)..."
            playSuccess <- sdlPlayHapticRumble haptic 0.5 1000
            unless playSuccess $ do
              sdlLog "Failed to play rumble!"
              exitFailure

            threadDelay 1000000  -- Wait 1 second for effect to complete
            sdlLog "Stopping rumble..."
            stopSuccess <- sdlStopHapticRumble haptic
            unless stopSuccess $ sdlLog "Failed to stop rumble!"

          -- Close haptic device
          sdlCloseHaptic haptic
          sdlLog "Haptic device closed."

    -- Clean up
  sdlLog "\nShutting down SDL..."
  sdlQuit

  sdlLog "Application terminated successfully"
  exitSuccess


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
