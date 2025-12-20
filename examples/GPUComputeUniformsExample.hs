{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Example     : GPUComputeUniforms (Gradient Texture)
-- Description : Generates an animated gradient texture using a compute shader with uniforms, then blits it to the screen.
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Based on the SDL_gpu_examples/GradientTexture C example.
-- This example demonstrates a more advanced use of compute shaders by:
-- - Defining a simple uniform structure (`GradientUniforms`) containing a 'time' variable.
-- - Creating a compute pipeline (`GradientTexture.comp`) that:
--     - Expects one uniform buffer (for `GradientUniforms`).
--     - Expects one read/write storage texture (to write the gradient into).
-- - Creating a 2D texture with usage flags allowing it to be written to by a compute shader
--   (`SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_WRITE`) and subsequently sampled/read from
--   (`SDL_GPU_TEXTUREUSAGE_SAMPLER`).
-- - In each frame:
--     - Updating the 'time' value in the `GradientUniforms`.
--     - Beginning a compute pass.
--     - Binding the target texture as a read/write storage texture.
--     - Binding the compute pipeline.
--     - Pushing the updated `GradientUniforms` to the compute shader using `SDL_PushGPUComputeUniformData`.
--     - Dispatching the compute shader to fill the texture based on the current time and invocation ID.
--     - Ending the compute pass.
--     - Blitting the generated (and now animated) gradient texture to the swapchain (window)
--       using `SDL_BlitGPUTexture` with a linear filter.
-- - This showcases a common pattern: generate dynamic content with a compute shader into an
--   offscreen texture, then use that texture in a graphics pass (here, a simple blit).
-- |
module Main where

-- Import common setup, GradientUniforms, helpers

import Control.Exception (bracket, finally)
import Control.Monad (unless, void, when)
-- For Storable class constraints
-- For time type

import Data.Bits ((.|.))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Foreign.C.Types (CFloat)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (..))
import GPUCommon
import SDL3
import System.Exit (exitFailure, exitSuccess)

-- AppResources for this example
data AppResources = AppResources
  { resGradientPipeline :: SDLGPUComputePipeline,
    resGradientTexture :: SDLGPUTexture
  }
  deriving (Show)

newtype GradientUniforms = GradientUniforms
  { time :: CFloat
  }
  deriving (Show, Eq)

instance Storable GradientUniforms where
  sizeOf _ = sizeOf (undefined :: CFloat)
  alignment _ = alignment (undefined :: CFloat)
  peek ptr = GradientUniforms <$> peek (castPtr ptr)
  poke ptr (GradientUniforms t) = poke (castPtr ptr) t

-- main
main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  maybeResult <- withContext "SDL3 Haskell GPU Gradient Texture" [] runAppGPU
  case maybeResult of
    Nothing -> do
      sdlLog "Application initialization failed."
      exitFailure
    Just _ -> do
      sdlLog "Application finished successfully."
      exitSuccess

-- runAppGPU
runAppGPU :: Context -> IO ()
runAppGPU context@Context {} = do
  sdlLog "Base context initialized."
  -- IORef for the 'time' uniform value
  timeUniformRef <- newIORef (GradientUniforms {time = 0.0 :: CFloat})

  bracket
    (createResources context)
    (releaseResources context)
    $ \case
      Nothing -> sdlLog "Failed to create resources. Exiting."
      Just resources -> do
        sdlLog "Resources created successfully. Entering event loop."
        eventLoopGPU context resources timeUniformRef

-- createResources
createResources :: Context -> IO (Maybe AppResources)
createResources Context {contextDevice = dev, contextWindow = win} = do
  sdlLog "--- Beginning Resource Creation ---"

  -- 1. Create Compute Pipeline for Gradient
  sdlLog "Creating gradient compute pipeline..."
  let computeCI =
        defaultComputePipelineCreateInfo
          { numReadWriteStorageTextures = 1, -- We write to one texture
            numUniformBuffers = 1, -- We use one uniform buffer for time
            threadCountX = 8,
            threadCountY = 8,
            threadCountZ = 1
          }
  maybeGradientPipeline <- createComputePipelineFromShader dev "GradientTexture.comp" computeCI

  case maybeGradientPipeline of
    Nothing -> sdlLog "!!! Failed to create gradient compute pipeline." >> return Nothing
    Just pipeline -> do
      sdlLog "Gradient compute pipeline created successfully."

      -- 2. Get window size for texture dimensions
      maybeDims <- sdlGetWindowSizeInPixels win
      case maybeDims of
        Nothing -> do
          sdlLog "!!! Failed to get window size for texture creation."
          sdlReleaseGPUComputePipeline dev pipeline -- Clean up already created pipeline
          return Nothing
        Just (w, h) -> do
          -- 3. Create Texture (target for compute, source for blit)
          swapchainFormat <- sdlGetGPUSwapchainTextureFormat dev win
          let textureCI =
                (defaultTextureCreateInfo w h)
                  { texInfoFormat = swapchainFormat,
                    texInfoUsage = SDL_GPU_TEXTUREUSAGE_SAMPLER .|. SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_WRITE
                  }
          maybeTexture <- sdlCreateGPUTexture dev textureCI

          case maybeTexture of
            Nothing -> do
              sdlLog "!!! Failed to create gradient render texture."
              sdlReleaseGPUComputePipeline dev pipeline
              return Nothing
            Just tex -> do
              sdlLog "Gradient render texture created successfully."
              return $
                Just
                  AppResources
                    { resGradientPipeline = pipeline,
                      resGradientTexture = tex
                    }

-- releaseResources
releaseResources :: Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources Context {contextDevice = dev} (Just AppResources {..}) = do
  sdlLog "--> Releasing AppResources..."
  sdlLog $ "  --> Releasing Gradient Compute Pipeline: " ++ show resGradientPipeline
  sdlReleaseGPUComputePipeline dev resGradientPipeline
  sdlLog $ "  --> Releasing Gradient Texture: " ++ show resGradientTexture
  sdlReleaseGPUTexture dev resGradientTexture
  sdlLog "<-- AppResources Released."

-- eventLoopGPU
eventLoopGPU :: Context -> AppResources -> IORef GradientUniforms -> IO ()
eventLoopGPU context resources timeUniformRef = do
  -- Update time uniform
  modifyIORef' timeUniformRef (\u -> u {time = time (u :: GradientUniforms) + 0.01})

  sdlPumpEvents
  shouldQuitRef <- newIORef False
  processEventsGPU shouldQuitRef

  shouldQuit <- readIORef shouldQuitRef
  unless shouldQuit $ do
    renderFrameGPU context resources timeUniformRef
    eventLoopGPU context resources timeUniformRef

processEventsGPU :: IORef Bool -> IO ()
processEventsGPU shouldQuitRef = do
  maybeEvent <- sdlPollEvent
  case maybeEvent of
    Nothing -> return ()
    Just event -> do
      quit <- handleEventGPU event
      when quit $ writeIORef shouldQuitRef True
      processEventsGPU shouldQuitRef

handleEventGPU :: SDLEvent -> IO Bool
handleEventGPU event = case event of
  SDLEventQuit _ -> sdlLog "Quit event received." >> return True
  SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ scancode _ _ _ down _) | down -> do
    case scancode of
      SDL_SCANCODE_Q -> return True
      _ -> return False
  _ -> return False

-- renderFrameGPU
renderFrameGPU :: Context -> AppResources -> IORef GradientUniforms -> IO ()
renderFrameGPU Context {..} AppResources {..} timeUniformRef = do
  maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
  case maybeCmdbuf of
    Nothing -> sdlLog "Error: Failed to acquire render command buffer."
    Just cmdbuf -> do
      maybeSwapchainResult <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
      case maybeSwapchainResult of
        Nothing -> do
          sdlLog "Error: Failed to acquire swapchain texture with dimensions."
          void (sdlSubmitGPUCommandBuffer cmdbuf `finally` return ())
        Just (swapchainTexture, w, h) -> do
          -- Begin Compute Pass to generate gradient
          maybeComputePass <-
            sdlBeginGPUComputePass
              cmdbuf
              [SDLGPUStorageTextureReadWriteBinding resGradientTexture 0 0 True] -- Cycle the texture
              [] -- No storage buffers
          case maybeComputePass of
            Nothing -> sdlLog "!!! Failed to begin compute pass for gradient."
            Just computePass -> do
              sdlBindGPUComputePipeline computePass resGradientPipeline
              gradientUniformsValue <- readIORef timeUniformRef
              sdlPushGPUComputeUniformData cmdbuf 0 gradientUniformsValue
              sdlDispatchGPUCompute computePass (w `div` 8) (h `div` 8) 1
              sdlEndGPUComputePass computePass

          -- Blit the generated gradient texture to the swapchain
          let srcRegion =
                SDLGPUBlitRegion
                  { gpuBlitRegTexture = resGradientTexture,
                    gpuBlitRegMipLevel = 0,
                    gpuBlitRegLayerOrDepthPlane = 0, -- It's a 2D texture
                    gpuBlitRegX = 0,
                    gpuBlitRegY = 0,
                    gpuBlitRegW = w, -- Blit entire texture
                    gpuBlitRegH = h
                  }
          let dstRegion =
                SDLGPUBlitRegion
                  { gpuBlitRegTexture = swapchainTexture,
                    gpuBlitRegMipLevel = 0,
                    gpuBlitRegLayerOrDepthPlane = 0,
                    gpuBlitRegX = 0,
                    gpuBlitRegY = 0,
                    gpuBlitRegW = w, -- Blit to entire swapchain
                    gpuBlitRegH = h
                  }
          let blitInfo =
                SDLGPUBlitInfo
                  { gpuBlitInfoSource = srcRegion,
                    gpuBlitInfoDestination = dstRegion,
                    gpuBlitInfoLoadOp = SDL_GPU_LOADOP_DONT_CARE, -- Destination content doesn't matter before blit
                    gpuBlitInfoClearColor = SDLFColor 0 0 0 0, -- Not used
                    gpuBlitInfoFlipMode = SDL_FLIP_NONE,
                    gpuBlitInfoFilter = SDL_GPU_FILTER_LINEAR, -- Linear filter as in C example
                    gpuBlitInfoCycle = False
                  }
          sdlBlitGPUTexture cmdbuf blitInfo

          -- Submit all commands
          submitted <- sdlSubmitGPUCommandBuffer cmdbuf
          unless submitted $ sdlLog "Error: Failed to submit render command buffer."
