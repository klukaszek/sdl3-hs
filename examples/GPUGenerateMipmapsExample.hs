{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Example     : GPUGenerateMipmaps
-- Description : Demonstrates mipmap generation for GPU textures.
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Based on the SDL_gpu_examples/GenerateMipmaps C example.
-- This example demonstrates:
-- - Creating a texture with multiple mip levels
-- - Uploading image data to the base mip level
-- - Using SDL_GenerateMipmapsForGPUTexture to auto-generate lower mip levels
-- - Blitting a specific mip level to the swapchain
module Main where

import Control.Exception (bracket)
import Control.Monad (unless, void, when)
import Data.Bits ((.|.))
import Data.Maybe (isJust)
import Data.Word (Word32)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek)
import GPUCommon
import SDL3
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

-- | Application resources
data AppResources = AppResources
  { resMipmapTexture :: SDLGPUTexture
  }
  deriving (Show)

-- | main
main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion
  maybeResult <- withContext "SDL3 Haskell GPU Generate Mipmaps" [] runAppGPU
  case maybeResult of
    Nothing -> sdlLog "Application initialization failed." >> exitFailure
    Just _ -> sdlLog "Application finished successfully." >> exitSuccess

-- | runAppGPU
runAppGPU :: Context -> IO ()
runAppGPU context@Context {..} = do
  sdlLog "Base context initialized."
  bracket
    (createResources context)
    (releaseResources context)
    $ \case
      Nothing -> sdlLog "Failed to create resources. Exiting."
      Just resources -> do
        sdlLog "Resources created successfully."
        eventLoopGPU context resources

-- | createResources - Creates the mipmap texture and generates mipmaps
createResources :: Context -> IO (Maybe AppResources)
createResources Context {..} = do
  sdlLog "--- Beginning Resource Creation ---"

  -- Create texture with 3 mip levels (32x32 -> 16x16 -> 8x8)
  -- Note: Usage includes COLOR_TARGET for mipmap generation
  let texCI =
        SDLGPUTextureCreateInfo
          { texInfoType = SDL_GPU_TEXTURETYPE_2D,
            texInfoFormat = SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM,
            texInfoUsage = SDL_GPU_TEXTUREUSAGE_SAMPLER .|. SDL_GPU_TEXTUREUSAGE_COLOR_TARGET,
            texInfoWidth = 32,
            texInfoHeight = 32,
            texInfoLayerCountOrDepth = 1,
            texInfoNumLevels = 3, -- 3 mip levels
            texInfoSampleCount = SDL_GPU_SAMPLECOUNT_1,
            texInfoProps = 0
          }
  maybeMipmapTexture <- sdlCreateGPUTexture contextDevice texCI

  case maybeMipmapTexture of
    Nothing -> do
      sdlLog "!!! Failed to create mipmap texture."
      return Nothing
    Just mipmapTexture -> do
      sdlLog "Created mipmap texture (32x32, 3 mip levels)."

      -- Load source image (cube0.bmp)
      maybeSurf <- loadImage ("Content" </> "Images" </> "cube0.bmp")
      case maybeSurf of
        Nothing -> do
          sdlLog "!!! Failed to load cube0.bmp."
          sdlReleaseGPUTexture contextDevice mipmapTexture
          return Nothing
        Just surfPtr -> do
          sdlLog "Loaded cube0.bmp."

          let byteCount = 32 * 32 * 4 :: Word32

          -- Upload and generate mipmaps
          uploadSuccess <- bracket
            (createTransferBuffer contextDevice byteCount SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "MipmapTextureTransfer")
            (cleanupTransferBuffer contextDevice)
            $ \case
              Nothing -> return False
              Just tb -> do
                -- Map and copy pixel data
                mapOk <- bracket
                  (sdlMapGPUTransferBuffer contextDevice tb False)
                  (\mptr -> when (isJust mptr) $ sdlUnmapGPUTransferBuffer contextDevice tb)
                  $ \case
                    Nothing -> return False
                    Just ptr -> do
                      surfData <- peek surfPtr
                      copyBytes (castPtr ptr) (surfacePixels surfData) (fromIntegral byteCount)
                      return True

                if mapOk
                  then do
                    -- Upload to GPU and generate mipmaps
                    bracket
                      (sdlAcquireGPUCommandBuffer contextDevice)
                      cleanupCommandBuffer
                      $ \case
                        Nothing -> return False
                        Just cmd -> do
                          mcp <- sdlBeginGPUCopyPass cmd
                          case mcp of
                            Nothing -> return False
                            Just cp -> do
                              sdlUploadToGPUTexture
                                cp
                                (SDLGPUTextureTransferInfo tb 0 0 0)
                                (SDLGPUTextureRegion mipmapTexture 0 0 0 0 0 32 32 1)
                                False
                              sdlEndGPUCopyPass cp

                              -- Generate mipmaps from the base level
                              sdlGenerateMipmapsForGPUTexture cmd mipmapTexture

                              submitted <- sdlSubmitGPUCommandBuffer cmd
                              if submitted
                                then do
                                  void $ sdlWaitForGPUIdle contextDevice
                                  return True
                                else return False
                  else return False

          -- Clean up surface
          sdlDestroySurface surfPtr

          if uploadSuccess
            then do
              sdlLog "--- Resource Creation and Mipmap Generation Successful ---"
              return $ Just AppResources {resMipmapTexture = mipmapTexture}
            else do
              sdlLog "!!! Failed to upload texture or generate mipmaps."
              sdlReleaseGPUTexture contextDevice mipmapTexture
              return Nothing

-- | releaseResources
releaseResources :: Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources Context {..} (Just AppResources {..}) = do
  sdlLog "--> Releasing AppResources..."
  sdlReleaseGPUTexture contextDevice resMipmapTexture
  sdlLog "<-- AppResources Released."

-- | eventLoopGPU
eventLoopGPU :: Context -> AppResources -> IO ()
eventLoopGPU context resources = do
  sdlPumpEvents
  shouldQuit <- processEventsGPU
  unless shouldQuit $ do
    renderFrameGPU context resources
    eventLoopGPU context resources

-- | processEventsGPU - Handle input events
processEventsGPU :: IO Bool
processEventsGPU =
  sdlPollEvent >>= \case
    Nothing -> return False
    Just event -> case event of
      SDLEventQuit _ -> sdlLog "Quit." >> return True
      SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ sc _ _ _ down _)
        | down && sc == SDL_SCANCODE_Q -> return True
      _ -> processEventsGPU

-- | renderFrameGPU - Blit the smallest mip level (8x8) to the swapchain
renderFrameGPU :: Context -> AppResources -> IO ()
renderFrameGPU Context {..} AppResources {..} = do
  maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
  case maybeCmdbuf of
    Nothing -> sdlLog "Error: Failed to acquire render command buffer."
    Just cmdbuf -> do
      maybeSwapResult <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
      case maybeSwapResult of
        Nothing -> void $ sdlSubmitGPUCommandBuffer cmdbuf
        Just (swapchainTexture, w, h) -> do
          -- Blit the smallest mip level (level 2 = 8x8) to fill the window
          let blitInfo =
                SDLGPUBlitInfo
                  { gpuBlitInfoSource = SDLGPUBlitRegion resMipmapTexture 2 0 0 0 8 8, -- mip level 2
                    gpuBlitInfoDestination = SDLGPUBlitRegion swapchainTexture 0 0 0 0 (fromIntegral w) (fromIntegral h),
                    gpuBlitInfoLoadOp = SDL_GPU_LOADOP_DONT_CARE,
                    gpuBlitInfoClearColor = SDLFColor 0 0 0 1,
                    gpuBlitInfoFlipMode = SDL_FLIP_NONE,
                    gpuBlitInfoFilter = SDL_GPU_FILTER_NEAREST,
                    gpuBlitInfoCycle = False
                  }
          sdlBlitGPUTexture cmdbuf blitInfo

          void $ sdlSubmitGPUCommandBuffer cmdbuf
