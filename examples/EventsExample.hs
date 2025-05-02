{-|
Example     : SDL.Events
Description : SDL Window and Keyboard Event Example
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3
|-}

module Main where

import SDL
import Control.Monad (unless, when)
import System.Exit (exitFailure, exitSuccess)
import Foreign.Ptr (nullPtr)
import Data.IORef
import Data.Word (Word32, Word64)
import Text.Printf (printf)

main :: IO ()
main = do
  -- Check compiled version
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  when (sdlVersionAtLeast 3 2 0) $ sdlLog "Compiled with at least SDL 3.2.0"

  -- Get linked version
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  -- Initialize SDL
  initSuccess <- sdlInit [InitVideo, InitEvents]
  unless initSuccess $ do
    sdlLog "Failed to initialize SDL!"
    exitFailure

  -- Check initialized subsystems
  initializedSystems <- sdlWasInit []
  sdlLog "Initialized subsystems:"
  mapM_ printSubsystem initializedSystems

  -- Create a window
  window <- sdlCreateWindow "SDL3 Haskell Event Loop" 800 600 [SDL_WINDOW_RESIZABLE]
  case window of
    Nothing -> do
      sdlLog "Failed to create window!"
      sdlQuit
      exitFailure
    Just win -> do
      sdlLog "Window created successfully!"

      -- Start event loop with initial time
      startTime <- sdlGetPerformanceCounter
      freq <- sdlGetPerformanceFrequency
      deltaTimeRef <- newIORef 0.0
      eventLoop win startTime freq deltaTimeRef

      -- Cleanup
      sdlDestroyWindow win
      sdlLog "Window destroyed."

  sdlLog "Shutting down SDL..."
  sdlQuit
  sdlLog "Application terminated successfully"
  exitSuccess

-- | Main event loop that tracks FPS and processes events
eventLoop :: SDLWindow -> Word64 -> Word64 -> IORef Double -> IO ()
eventLoop window lastTime freq deltaTimeRef = do
  currentTime <- sdlGetPerformanceCounter
  let deltaTime = fromIntegral (currentTime - lastTime) / fromIntegral freq * 1000.0

  -- Store the new deltaTime
  writeIORef deltaTimeRef deltaTime
  
  -- Event handling
  sdlPumpEvents
  maybeEvent <- sdlPollEvent
  shouldQuit <- case maybeEvent of
    Nothing -> return False
    Just event -> handleEvent event deltaTimeRef

  unless shouldQuit $ eventLoop window currentTime freq deltaTimeRef

-- | Handle SDL events
handleEvent :: SDLEvent -> IORef Double -> IO Bool
handleEvent event deltaTimeRef = case event of
  SDLEventQuit _ -> do
    sdlLog "Quit event received."
    return True
  SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ scancode _ _ _ down _) | down -> do
    deltaTime <- readIORef deltaTimeRef  -- Read delta time
    sdlLog $ printf "Key event received. Delta Time: %.3f ms" deltaTime
    return $ scancode == SDL_SCANCODE_Q
  _ -> return False

-- | Helper function to print subsystem names
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

-- Function to log the frame time in milliseconds with decimal precision
logFrameTime :: Double -> IO ()
logFrameTime frameTime = sdlLog $ printf "Frame time: %.4f ms" frameTime

