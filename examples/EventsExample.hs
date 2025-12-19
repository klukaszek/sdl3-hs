module Main where

import Control.Monad (unless, when)
import Data.IORef
import Data.Word (Word64)
import SDL3
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

main :: IO ()
main = do
  -- Check compiled version
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  when (sdlVersionAtLeast 3 3 0) $ sdlLog "Compiled with at least SDL 3.3.0"

  -- Get linked version
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  -- Initialize SDL
  initSuccess <- sdlInit [SDL_INIT_VIDEO, SDL_INIT_EVENTS]
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
    deltaTime <- readIORef deltaTimeRef -- Read delta time
    sdlLog $ printf "Key event received. Delta Time: %.3f ms" deltaTime
    return $ scancode == SDL_SCANCODE_Q
  SDLEventMouseButton mouseEvent -> do
    deltaTime <- readIORef deltaTimeRef -- Read delta time
    let button = sdlMouseButtonButton mouseEvent
    let x = sdlMouseButtonX mouseEvent
    let y = sdlMouseButtonY mouseEvent
    let clicks = sdlMouseButtonClicks mouseEvent
    let down = sdlMouseButtonDown mouseEvent
    let buttonName = case fromIntegral button of
          SDL_BUTTON_LEFT -> "Left"
          SDL_BUTTON_MIDDLE -> "Middle"
          SDL_BUTTON_RIGHT -> "Right"
          SDL_BUTTON_X1 -> "X1"
          SDL_BUTTON_X2 -> "X2"
          _ -> "Unknown"
    let action = if down then "pressed" else "released"
    sdlLog $
      printf
        "Mouse %s button %s at (%.1f, %.1f), clicks: %d. Delta Time: %.3f ms"
        buttonName
        action
        x
        y
        clicks
        deltaTime
    return False
  _ -> return False

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

-- Function to log the frame time in milliseconds with decimal precision
logFrameTime :: Double -> IO ()
logFrameTime frameTime = sdlLog $ printf "Frame time: %.4f ms" frameTime
