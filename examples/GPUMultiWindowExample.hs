{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-} -- For explicit type annotations if needed
{-# LANGUAGE LambdaCase #-}

{-|
Example     : GPUMultiWindowClear
Description : SDL Example: Clearing two windows with different colors using SDL_gpu
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Based on the SDL_gpu_examples/ClearScreenMultiWindow C example.
Demonstrates creating two windows, claiming both for the same GPU device,
and clearing each with a distinct color within a single command buffer submission.
|-}

module Main where

import SDL
import SDL.GPU          -- Import the GPU module specifically
import SDL.Video        -- Need SDLWindowFlags constructor
import GPUCommon        -- Import common init/quit logic

import Control.Monad (unless, when, void)
import Control.Exception (bracket) -- For managing the second window's lifecycle
import System.Exit (exitFailure, exitSuccess)
import Foreign.Ptr (nullPtr)
-- Foreign.C.Types (CUInt) -- Not needed in this version
import Data.IORef
import Data.Word (Word32, Word64)
-- Data.Bits ((.|.)) -- Not needed directly here
import Text.Printf (printf)
-- Data.Maybe (isJust, fromJust, fromMaybe) -- Not needed directly here

main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  -- Use withContext for initialization and cleanup of the *first* window and device
  maybeResult <- withContext "SDL3 Haskell GPU Multi-Window Clear (1)" [SDL_WINDOW_RESIZABLE] runAppGPU
  case maybeResult of
      -- Note: maybeResult being Nothing means commonInit failed.
      -- Errors *within* runAppGPU (like failing to create the 2nd window)
      -- won't make maybeResult Nothing, but runAppGPU might exit or log errors.
      Nothing -> do
          sdlLog "Application initialization failed (commonInit)."
          exitFailure
      Just _ -> do
          sdlLog "Application finished successfully."
          exitSuccess

-- | Application logic, receives the Context for the first window/device
runAppGPU :: Context -> IO ()
runAppGPU context@Context{..} = do -- Deconstruct context (device, window1)
    sdlLog "First window and GPU device initialized."

    -- Bracket pattern to manage the second window's lifecycle
    bracket createAndClaimSecondWindow releaseAndDestroySecondWindow $ \case
            Nothing -> do
                sdlLog "Failed to initialize the second window. Exiting."
            Just secondWindow -> do
                sdlLog "Second window created and claimed successfully."
                -- Start event loop with initial time, passing both windows
                startTime <- sdlGetPerformanceCounter
                freq <- sdlGetPerformanceFrequency
                deltaTimeRef <- newIORef 0.0
                eventLoopGPU context secondWindow startTime freq deltaTimeRef
                -- Cleanup for the second window is handled by the bracket's release action
                -- Cleanup for the first window/device is handled by withContext

  where
    -- Action to create and claim the second window
    createAndClaimSecondWindow :: IO (Maybe SDLWindow)
    createAndClaimSecondWindow = do
        maybeWin <- sdlCreateWindow "SDL3 Haskell GPU Multi-Window Clear (2)" 640 480 []
        case maybeWin of
            Nothing -> do
                err <- sdlGetError
                sdlLog $ "Failed to create second window: " ++ err
                return Nothing -- Return Nothing if creation fails
            Just win2 -> do
                claimed <- sdlClaimWindowForGPUDevice contextDevice win2
                if claimed
                then do
                    -- Claim succeeded: Return Just win2
                    sdlLog "Second window created and claimed successfully." -- Log success here
                    return (Just win2)
                else do
                    -- Claim failed: Log, cleanup, and return Nothing
                    err <- sdlGetError
                    sdlLog $ "Failed to claim second window for GPU device: " ++ err
                    sdlDestroyWindow win2 -- Clean up the window since claim failed
                    return Nothing      -- Return Nothing as the final result

    -- Action to release and destroy the second window
    releaseAndDestroySecondWindow :: Maybe SDLWindow -> IO ()
    releaseAndDestroySecondWindow Nothing = return () -- Nothing to clean up
    releaseAndDestroySecondWindow (Just win2) = do
        sdlLog "Releasing second window from GPU device..."
        sdlReleaseWindowFromGPUDevice contextDevice win2
        sdlLog "Destroying second window..."
        sdlDestroyWindow win2
        sdlLog "Second window destroyed."


-- | Main event loop, now manages two windows
eventLoopGPU :: Context -> SDLWindow -> Word64 -> Word64 -> IORef Double -> IO ()
eventLoopGPU context@Context{..} secondWindow lastTime freq deltaTimeRef = do
  currentTime <- sdlGetPerformanceCounter
  let deltaTime = fromIntegral (currentTime - lastTime) / fromIntegral freq

  writeIORef deltaTimeRef (deltaTime * 1000.0)

  sdlPumpEvents
  shouldQuitRef <- newIORef False
  processEventsGPU shouldQuitRef deltaTimeRef -- Event processing is generic

  shouldQuit <- readIORef shouldQuitRef
  unless shouldQuit $ do
    -- *** GPU Rendering for Both Windows ***
    renderFrameGPU context secondWindow -- Pass context (win1) and win2

    -- Continue loop
    eventLoopGPU context secondWindow currentTime freq deltaTimeRef

-- | Process all pending events (doesn't need modification for multiple windows)
processEventsGPU :: IORef Bool -> IORef Double -> IO ()
processEventsGPU shouldQuitRef deltaTimeRef = do
    maybeEvent <- sdlPollEvent
    case maybeEvent of
        Nothing -> return () -- No more events
        Just event -> do
            -- Check if the event indicates quit, regardless of which window has focus
            quit <- handleEventGPU event deltaTimeRef
            when quit $ writeIORef shouldQuitRef True
            processEventsGPU shouldQuitRef deltaTimeRef -- Process next event

-- | Handle a single SDL event (doesn't need modification for multiple windows)
handleEventGPU :: SDLEvent -> IORef Double -> IO Bool
handleEventGPU event deltaTimeRef = case event of
  SDLEventQuit _ -> do
    sdlLog "Quit event received."
    return True
  -- Handle key presses regardless of which window has focus
  SDLEventKeyboard (SDLKeyboardEvent _windowId _timestamp _which _state scancode _sym _mod _repeat down _) | down -> do
    dtMs <- readIORef deltaTimeRef
    sdlLog $ printf "Key '%s' pressed. Delta Time: %.3f ms" (show scancode) dtMs
    return $ scancode == SDL_SCANCODE_Q -- Quit on Q
  _ -> return False


-- | Render a frame, clearing both windows with different colors
renderFrameGPU :: Context -> SDLWindow -> IO ()
renderFrameGPU Context{..} secondWindow = do -- Deconstruct context (device, window1)
    -- 1. Acquire *one* Command Buffer for both windows
    maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
    case maybeCmdbuf of
        Nothing -> do
            err <- sdlGetError
            sdlLog $ "Error: Failed to acquire command buffer: " ++ err
        Just cmdbuf -> do
            -- 2. Render First Window
            renderSingleWindow cmdbuf contextWindow (SDLFColor 0.3 0.4 0.5 1.0) "first"

            -- 3. Render Second Window (using the same command buffer)
            renderSingleWindow cmdbuf secondWindow (SDLFColor 1.0 0.5 0.6 1.0) "second"

            -- 4. Submit Command Buffer (contains commands for both windows)
            submitted <- sdlSubmitGPUCommandBuffer cmdbuf
            unless submitted $ do
                 err <- sdlGetError
                 sdlLog $ "Error: Failed to submit command buffer: " ++ err

-- | Helper to render one window's clear pass
renderSingleWindow :: SDLGPUCommandBuffer -> SDLWindow -> SDLFColor -> String -> IO ()
renderSingleWindow cmdbuf window clearColor windowName = do
    -- Acquire Swapchain Texture for the specified window
    maybeSwapchain <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf window
    case maybeSwapchain of
        Nothing -> sdlLog $ "Warning: Failed to acquire swapchain texture for " ++ windowName ++ " window"
        Just (swapchainTexture, _w, _h) -> do
            -- Define Color Target Info for this window
            let colorTargetInfo = SDLGPUColorTargetInfo
                    { texture           = swapchainTexture
                    , mipLevel          = 0
                    , layerOrDepthPlane = 0
                    , clearColor        = clearColor -- Use the provided color
                    , loadOp            = SDL_GPU_LOADOP_CLEAR
                    , storeOp           = SDL_GPU_STOREOP_STORE
                    , resolveTexture    = Nothing
                    , resolveMipLevel   = 0
                    , resolveLayer      = 0
                    , targetCycle             = False
                    , targetCycleResolve      = False
                    }

            -- Begin Render Pass for this window
            maybeRenderPass <- sdlBeginGPURenderPass cmdbuf [colorTargetInfo] Nothing
            case maybeRenderPass of
                Nothing -> do
                     err <- sdlGetError
                     sdlLog $ "Error: Failed to begin render pass for " ++ windowName ++ " window: " ++ err
                Just renderPass -> do
                    -- Clear happens via LoadOp
                    sdlEndGPURenderPass renderPass -- End pass for this window
