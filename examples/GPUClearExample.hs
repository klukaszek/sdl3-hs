{-# LANGUAGE RecordWildCards #-}

-- |
-- Example     : SDL.GPUClear
-- Description : SDL Window, Event, and Basic GPU Rendering Example
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Based on the examples from: https://github.com/TheSpydog/SDL_gpu_examples/
-- |
module Main where

import Control.Monad (unless, when)
-- For shader format flags

import Data.IORef
import Data.Word (Word64)
import GPUCommon
import SDL
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  -- Use withContext for initialization and cleanup
  maybeResult <- withContext "SDL3 Haskell GPU Example" [SDL_WINDOW_RESIZABLE] runAppGPU
  case maybeResult of
    Nothing -> do
      sdlLog "Application initialization failed."
      exitFailure
    Just _ -> do
      sdlLog "Application finished successfully."
      exitSuccess

-- | Application logic, now receives the Context
runAppGPU :: Context -> IO ()
runAppGPU context@Context {} = do
  -- Deconstruct context
  -- Start event loop with initial time
  startTime <- sdlGetPerformanceCounter
  freq <- sdlGetPerformanceFrequency
  deltaTimeRef <- newIORef 0.0
  eventLoopGPU context startTime freq deltaTimeRef

-- Cleanup is handled by withContext

-- | Main event loop
eventLoopGPU :: Context -> Word64 -> Word64 -> IORef Double -> IO ()
eventLoopGPU context@Context {} lastTime freq deltaTimeRef = do
  currentTime <- sdlGetPerformanceCounter
  let deltaTime = fromIntegral (currentTime - lastTime) / fromIntegral freq

  writeIORef deltaTimeRef (deltaTime * 1000.0)

  sdlPumpEvents
  shouldQuitRef <- newIORef False
  processEventsGPU shouldQuitRef deltaTimeRef

  shouldQuit <- readIORef shouldQuitRef
  unless shouldQuit $ do
    -- \*** GPU Rendering ***
    renderFrameGPU context -- Pass context

    -- Continue loop
    eventLoopGPU context currentTime freq deltaTimeRef

-- | Process all pending events (simplified for GPU example)
processEventsGPU :: IORef Bool -> IORef Double -> IO ()
processEventsGPU shouldQuitRef deltaTimeRef = do
  maybeEvent <- sdlPollEvent
  case maybeEvent of
    Nothing -> return () -- No more events
    Just event -> do
      quit <- handleEventGPU event deltaTimeRef
      when quit $ writeIORef shouldQuitRef True
      processEventsGPU shouldQuitRef deltaTimeRef -- Process next event

-- | Handle a single SDL event (simplified for GPU example)
handleEventGPU :: SDLEvent -> IORef Double -> IO Bool
handleEventGPU event deltaTimeRef = case event of
  SDLEventQuit _ -> do
    sdlLog "Quit event received."
    return True
  SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ scancode _ _ _ down _) | down -> do
    dtMs <- readIORef deltaTimeRef
    sdlLog $ printf "Key '%s' pressed. Delta Time: %.3f ms" (show scancode) dtMs
    return $ scancode == SDL_SCANCODE_Q -- Quit on Q
  _ -> return False

renderFrameGPU :: Context -> IO ()
renderFrameGPU Context {..} = do
  -- Deconstruct context
  -- 1. Acquire Command Buffer
  maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
  case maybeCmdbuf of
    Nothing -> sdlLog "Error: Failed to acquire command buffer"
    Just cmdbuf -> do
      -- 2. Acquire Swapchain Texture
      maybeSwapchain <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
      case maybeSwapchain of
        Nothing -> sdlLog "Warning: Failed to acquire swapchain texture"
        Just (swapchainTexture, _w, _h) -> do
          -- 3. Define Color Target Info
          let clearColor = SDLFColor 0.3 0.4 0.5 1.0
          let colorTargetInfo =
                SDLGPUColorTargetInfo
                  { texture = swapchainTexture,
                    mipLevel = 0, -- Use base level
                    layerOrDepthPlane = 0, -- Use base layer
                    clearColor = clearColor,
                    loadOp = SDL_GPU_LOADOP_CLEAR, -- Clear the target
                    storeOp = SDL_GPU_STOREOP_STORE, -- Store the result
                    resolveTexture = Nothing, -- No MSAA resolve needed
                    resolveMipLevel = 0,
                    resolveLayer = 0,
                    targetCycle = False, -- Don't cycle (relevant for multi-buffer resources)
                    targetCycleResolve = False
                  }

          -- 4. Begin Render Pass
          maybeRenderPass <- sdlBeginGPURenderPass cmdbuf [colorTargetInfo] Nothing -- Only one color target, no depth/stencil
          case maybeRenderPass of
            Nothing -> sdlLog "Error: Failed to begin render pass"
            Just renderPass -> do
              -- \*** Insert Drawing Commands Here (in the future) ***
              -- For now, we just clear via the LoadOp.

              -- 5. End Render Pass
              sdlEndGPURenderPass renderPass

      -- 6. Submit Command Buffer (regardless of whether texture was acquired, buffer might have other commands)
      submitted <- sdlSubmitGPUCommandBuffer cmdbuf
      unless submitted $ do
        err <- sdlGetError
        sdlLog $ "Error: Failed to submit command buffer: " ++ err
