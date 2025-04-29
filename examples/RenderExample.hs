{-|
Example     : SDL.Render
Description : SDL Window, Event, and Basic Rendering Example
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3
|-}

module Main where

import SDL
import Control.Monad (unless, when, void)
import System.Exit (exitFailure, exitSuccess)
import Foreign.Ptr (nullPtr)
import Data.IORef
import Data.Word (Word32, Word64)
import Text.Printf (printf)
import Data.Maybe (isJust, fromJust, fromMaybe)

main :: IO ()
main = do
  -- Check compiled version
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  when (sdlVersionAtLeast 3 2 0) $ sdlLog "Compiled with at least SDL 3.2.0"

  -- Get linked version
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  -- Initialize SDL (Ensure Video is initialized for rendering)
  initSuccess <- sdlInit [InitVideo, InitEvents]
  unless initSuccess $ do
    sdlLog "Failed to initialize SDL!"
    exitFailure

  -- Check initialized subsystems
  initializedSystems <- sdlWasInit []
  sdlLog "Initialized subsystems:"
  mapM_ printSubsystem initializedSystems

  -- Create a window
  window <- sdlCreateWindow "SDL3 Haskell Render Example" 800 600 [sdlWindowResizable]
  case window of
    Nothing -> do
      sdlLog "Failed to create window!"
      sdlQuit
      exitFailure
    Just win -> do
      sdlLog "Window created successfully!"

      -- *** Create a Renderer ***
      -- Try creating a GPU accelerated renderer first, fall back to software
      renderer <- sdlCreateRenderer win Nothing -- Let SDL choose the best driver
      case renderer of
        Nothing -> do
            sdlLog "Failed to create default renderer!"
            err <- sdlGetError
            sdlLog $ "SDL Error: " ++ err
            sdlDestroyWindow win
            sdlQuit
            exitFailure
        Just ren -> do
            rendererName <- sdlGetRendererName ren
            sdlLog $ "Created renderer: " ++ fromMaybe "Unknown" rendererName -- Corrected fromMaybe usage below
            runApp win ren

      -- *** Cleanup moved inside runApp or called after it returns ***

  sdlLog "Shutting down SDL..."
  sdlQuit
  sdlLog "Application terminated successfully"
  exitSuccess

-- | Encapsulate the application logic with window and renderer
runApp :: SDLWindow -> SDLRenderer -> IO ()
runApp win renderer = do
    -- Start event loop with initial time
    startTime <- sdlGetPerformanceCounter
    freq <- sdlGetPerformanceFrequency
    deltaTimeRef <- newIORef 0.0
    rectPosRef <- newIORef (SDLFPoint 100 100) -- Initial position for rectangle
    eventLoop win renderer startTime freq deltaTimeRef rectPosRef

    -- Cleanup Renderer and Window
    sdlLog "Destroying renderer..."
    sdlDestroyRenderer renderer
    sdlLog "Renderer destroyed."
    sdlLog "Destroying window..."
    sdlDestroyWindow win
    sdlLog "Window destroyed."


-- | Main event loop that tracks FPS, processes events, and renders
eventLoop :: SDLWindow -> SDLRenderer -> Word64 -> Word64 -> IORef Double -> IORef SDLFPoint -> IO ()
eventLoop window renderer lastTime freq deltaTimeRef rectPosRef = do
  currentTime <- sdlGetPerformanceCounter
  let deltaTime = fromIntegral (currentTime - lastTime) / fromIntegral freq -- Delta time in seconds

  -- Store the new deltaTime (in milliseconds for logging consistency)
  writeIORef deltaTimeRef (deltaTime * 1000.0)

  -- Event handling
  sdlPumpEvents
  shouldQuitRef <- newIORef False
  -- Process all available events this frame
  processEvents shouldQuitRef rectPosRef deltaTimeRef

  shouldQuit <- readIORef shouldQuitRef
  unless shouldQuit $ do
    -- *** Rendering ***
    renderFrame renderer rectPosRef

    -- Continue loop
    eventLoop window renderer currentTime freq deltaTimeRef rectPosRef

-- | Process all pending events
processEvents :: IORef Bool -> IORef SDLFPoint -> IORef Double -> IO ()
processEvents shouldQuitRef rectPosRef deltaTimeRef = do
    maybeEvent <- sdlPollEvent
    case maybeEvent of
        Nothing -> return () -- No more events
        Just event -> do
            quit <- handleEvent event deltaTimeRef rectPosRef
            when quit $ writeIORef shouldQuitRef True
            processEvents shouldQuitRef rectPosRef deltaTimeRef -- Process next event

-- | Handle a single SDL event
handleEvent :: SDLEvent -> IORef Double -> IORef SDLFPoint -> IO Bool
handleEvent event deltaTimeRef rectPosRef = case event of
  SDLEventQuit _ -> do
    sdlLog "Quit event received."
    return True
  SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ scancode _ _ _ down _) | down -> do
    dtMs <- readIORef deltaTimeRef
    sdlLog $ printf "Key '%s' pressed. Delta Time: %.3f ms" (show scancode) dtMs
    -- Move rectangle based on arrow keys
    let moveSpeed = 400.0 -- Pixels per second
    let dtSec = dtMs / 1000.0
    let moveAmount = realToFrac (moveSpeed * dtSec)
    case scancode of
        SDL_SCANCODE_Q -> return True -- Quit on Q
        SDL_SCANCODE_UP -> modifyIORef' rectPosRef (\(SDLFPoint x y) -> SDLFPoint x (y - moveAmount)) >> return False
        SDL_SCANCODE_DOWN -> modifyIORef' rectPosRef (\(SDLFPoint x y) -> SDLFPoint x (y + moveAmount)) >> return False
        SDL_SCANCODE_LEFT -> modifyIORef' rectPosRef (\(SDLFPoint x y) -> SDLFPoint (x - moveAmount) y) >> return False
        SDL_SCANCODE_RIGHT -> modifyIORef' rectPosRef (\(SDLFPoint x y) -> SDLFPoint (x + moveAmount) y) >> return False
        _ -> return False
  _ -> return False

-- | Render a single frame
renderFrame :: SDLRenderer -> IORef SDLFPoint -> IO ()
renderFrame renderer rectPosRef = do
    -- 1. Set draw color to clear color (e.g., dark blue) and clear
    _ <- sdlSetRenderDrawColor renderer 32 32 64 255 -- Use return value to silence warning if needed
    clearSuccess <- sdlRenderClear renderer
    unless clearSuccess $ sdlLog "Warning: Failed to clear renderer"

    -- 2. Set draw color for rectangle (e.g., yellow)
    _ <- sdlSetRenderDrawColor renderer 255 255 0 255

    -- 3. Get current rectangle position
    (SDLFPoint rectX rectY) <- readIORef rectPosRef

    -- 4. Define rectangle geometry
    let rect = SDLFRect rectX rectY 50 50 -- x, y, width, height

    -- 5. Draw the filled rectangle
    fillRectSuccess <- sdlRenderFillRect renderer (Just rect)
    unless fillRectSuccess $ sdlLog "Warning: Failed to draw filled rect"

    -- 6. Present the rendered frame
    presentSuccess <- sdlRenderPresent renderer
    unless presentSuccess $ do
      err <- sdlGetError -- Check why presentation failed (e.g., device lost)
      sdlLog $ "Warning: Failed to present renderer: " ++ err
      -- Potentially handle device lost errors here if needed

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
