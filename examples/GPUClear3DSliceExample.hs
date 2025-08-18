{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Example     : GPUClear3DSliceAndBlit
-- Description : Demonstrates using a 3D texture as a multi-layer render target and blitting its layers.
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Based on the SDL_gpu_examples/Clear3DSliceAndBlit C example.
-- Demonstrates:
-- - Creating a 3D texture (SDL_GPU_TEXTURETYPE_3D) with multiple layers (depth slices).
-- - Setting the 3D texture's usage flags to allow it to be both a color render target
--   (SDL_GPU_TEXTUREUSAGE_COLOR_TARGET) and a source for sampling/blitting
--   (SDL_GPU_TEXTUREUSAGE_SAMPLER).
-- - Iteratively rendering to each layer (depth slice) of the 3D texture:
--     - Beginning a render pass targeting a specific `layerOrDepthPlane` of the 3D texture.
--     - Using `SDL_GPU_LOADOP_CLEAR` to clear the targeted layer to a distinct color.
--     - Ending the render pass for each layer (no actual drawing commands within these passes, only clearing).
-- - Acquiring the swapchain (window) texture and its dimensions.
-- - Blitting each cleared layer from the 3D texture to a different quadrant of the
--   swapchain texture using SDL_BlitGPUTexture.
-- - Demonstrates the setup of SDLGPUBlitInfo for specifying source and destination
--   regions for blitting, including selecting the source layer from the 3D texture.
-- - Using SDL_GPU_FILTER_NEAREST for the blit operation.
-- |
module Main where

import Control.Exception (bracket, finally)
import Control.Monad (forM_, unless, void, when)
import Data.Bits ((.|.))
import Data.IORef
import Data.Word (Word32)
import GPUCommon
import SDL
import System.Exit (exitFailure, exitSuccess)

-- AppResources for this example
data AppResources = AppResources
  { resTexture3D :: SDLGPUTexture
  }
  deriving (Show)

-- main
main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  -- Window is resizable in C example, let's match
  maybeResult <- withContext "SDL3 Haskell GPU 3D Texture Example" [SDL_WINDOW_RESIZABLE] runAppGPU
  case maybeResult of
    Nothing -> do
      sdlLog "Application initialization failed (commonInit)."
      exitFailure
    Just _ -> do
      sdlLog "Application finished successfully."
      exitSuccess

-- runAppGPU
runAppGPU :: Context -> IO ()
runAppGPU context@Context {} = do
  sdlLog "Base context initialized."

  bracket
    (createResources context)
    (releaseResources context)
    $ \case
      Nothing -> sdlLog "Failed to create resources. Exiting."
      Just resources -> do
        sdlLog "Resources created successfully."
        eventLoopGPU context resources -- Simplified event loop

-- createResources: Creates the 3D Texture
createResources :: Context -> IO (Maybe AppResources)
createResources Context {contextDevice = dev, contextWindow = win} = do
  sdlLog "--- Beginning Resource Creation ---"

  swapchainFormat <- sdlGetGPUSwapchainTextureFormat dev win
  when (swapchainFormat == SDL_GPU_TEXTUREFORMAT_INVALID) $
    sdlLog "Warning: Could not get swapchain texture format, 3D texture might fail to be created or used as render target."

  let tex3DCI =
        SDLGPUTextureCreateInfo
          { texInfoType = SDL_GPU_TEXTURETYPE_3D,
            texInfoFormat = swapchainFormat, -- Use swapchain format for simplicity as render target
            texInfoWidth = 64,
            texInfoHeight = 64,
            texInfoLayerCountOrDepth = 4, -- Depth for 3D texture
            texInfoNumLevels = 1,
            texInfoUsage = SDL_GPU_TEXTUREUSAGE_COLOR_TARGET .|. SDL_GPU_TEXTUREUSAGE_SAMPLER,
            texInfoSampleCount = SDL_GPU_SAMPLECOUNT_1,
            texInfoProps = 0
          }

  maybeTex3D <- sdlCreateGPUTexture dev tex3DCI
  case maybeTex3D of
    Nothing -> do
      sdlLog "!!! Failed to create 3D Texture."
      sdlGetError >>= sdlLog . ("SDL Error: " ++)
      return Nothing
    Just tex3D -> do
      sdlLog $ "3D Texture created successfully: " ++ show tex3D
      return $ Just AppResources {resTexture3D = tex3D}

releaseResources :: Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources Context {contextDevice = dev} (Just AppResources {..}) = do
  sdlLog "--> Releasing AppResources..."
  sdlLog $ "  --> Releasing 3D Texture: " ++ show resTexture3D
  sdlReleaseGPUTexture dev resTexture3D
  sdlLog "<-- AppResources Released."

eventLoopGPU :: Context -> AppResources -> IO ()
eventLoopGPU context resources = do
  sdlPumpEvents
  shouldQuitRef <- newIORef False
  processEventsGPU shouldQuitRef

  shouldQuit <- readIORef shouldQuitRef
  unless shouldQuit $ do
    renderFrameGPU context resources
    eventLoopGPU context resources -- Recursive call

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
renderFrameGPU :: Context -> AppResources -> IO ()
renderFrameGPU Context {..} AppResources {..} = do
  maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
  case maybeCmdbuf of
    Nothing -> sdlLog "Error: Failed to acquire render command buffer."
    Just cmdbuf -> do
      maybeSwapchainResult <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
      case maybeSwapchainResult of
        Nothing -> do
          sdlLog "Error: Failed to acquire swapchain texture with dimensions."
          void (sdlSubmitGPUCommandBuffer cmdbuf `finally` return ())
        Just (swapchainTexture, w_uint32, h_uint32) -> do
          -- w, h are Word32
          let w = w_uint32 :: Word32 -- Keep as Word32 for calculations
          let h = h_uint32 :: Word32

          -- Render to each layer of the 3D texture
          let clearColors =
                [ SDLFColor 1 0 0 1, -- Red
                  SDLFColor 0 1 0 1, -- Green
                  SDLFColor 0 0 1 1, -- Blue
                  SDLFColor 1 0 1 1 -- Magenta
                ]
          forM_ (zip [0 .. 3] clearColors) $ \(layerIndex, clearCol) -> do
            let colorTargetInfo =
                  defaultColorTargetInfo
                    { texture = resTexture3D,
                      mipLevel = 0,
                      layerOrDepthPlane = layerIndex,
                      clearColor = clearCol,
                      loadOp = SDL_GPU_LOADOP_CLEAR,
                      storeOp = SDL_GPU_STOREOP_STORE,
                      targetCycle = layerIndex == 0
                    }
            bracket
              (sdlBeginGPURenderPass cmdbuf [colorTargetInfo] Nothing)
              cleanupMaybeRenderPass
              $ \case
                Nothing -> sdlLog $ "Error: Failed to begin render pass for layer " ++ show layerIndex
                Just _renderPass -> pure () -- No drawing, just clear via loadOp

          -- Blit each layer of the 3D texture to a quadrant of the swapchain
          forM_ [0 .. 3] $ \i_word32 -> do
            -- Use Word32 for layer index to match struct fields
            let i = i_word32 -- Keep for clarity, will be Word32
            let destX = if (i `mod` 2) == 0 then 0 else w `div` 2
            let destY = if i < 2 then 0 else h `div` 2
            let destW = w `div` 2
            let destH = h `div` 2

            let srcRegion =
                  SDLGPUBlitRegion
                    { gpuBlitRegTexture = resTexture3D,
                      gpuBlitRegMipLevel = 0,
                      gpuBlitRegLayerOrDepthPlane = i, -- Source layer (Word32)
                      gpuBlitRegX = 0,
                      gpuBlitRegY = 0, -- Z not used for layer in BlitRegion
                      gpuBlitRegW = 64, -- Source dimensions (Word32)
                      gpuBlitRegH = 64
                    }
            let dstRegion =
                  SDLGPUBlitRegion
                    { gpuBlitRegTexture = swapchainTexture,
                      gpuBlitRegMipLevel = 0,
                      gpuBlitRegLayerOrDepthPlane = 0, -- Dest layer for 2D swapchain (Word32)
                      gpuBlitRegX = destX, -- Word32
                      gpuBlitRegY = destY, -- Word32
                      gpuBlitRegW = destW, -- Destination dimensions (Word32)
                      gpuBlitRegH = destH
                    }

            let blitInfo =
                  SDLGPUBlitInfo
                    { gpuBlitInfoSource = srcRegion,
                      gpuBlitInfoDestination = dstRegion,
                      gpuBlitInfoLoadOp = SDL_GPU_LOADOP_LOAD,
                      gpuBlitInfoClearColor = SDLFColor 0 0 0 0, -- Not used with LOADOP_LOAD
                      gpuBlitInfoFlipMode = SDL_FLIP_NONE,
                      gpuBlitInfoFilter = SDL_GPU_FILTER_NEAREST,
                      gpuBlitInfoCycle = False
                    }
            sdlBlitGPUTexture cmdbuf blitInfo

          -- Submit all commands
          submitted <- sdlSubmitGPUCommandBuffer cmdbuf
          unless submitted $ sdlLog "Error: Failed to submit render command buffer."
