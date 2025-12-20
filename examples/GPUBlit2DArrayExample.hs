{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Example     : GPUBlit2DArray
-- Description : Demonstrates blitting between 2D texture array layers.
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Based on the SDL_gpu_examples/Blit2DArray C example.
-- This example demonstrates:
-- - Creating 2D texture arrays with multiple layers
-- - Uploading different images to each layer
-- - Using SDL_BlitGPUTexture to blit between layers
-- - Displaying source and destination textures side by side
module Main where

import Control.Exception (bracket)
import Control.Monad (unless, void, when)
import Data.Bits ((.|.))
import Data.Maybe (isJust)
import Data.Word (Word32)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (peek)
import GPUCommon
import SDL3
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

-- | Application resources
data AppResources = AppResources
  { resSourceTexture :: SDLGPUTexture,
    resDestinationTexture :: SDLGPUTexture,
    resSrcWidth :: Word32,
    resSrcHeight :: Word32
  }
  deriving (Show)

-- | main
main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion
  maybeResult <- withContext "SDL3 Haskell GPU Blit 2D Array" [] runAppGPU
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

-- | createResources - Creates textures and blits between layers
createResources :: Context -> IO (Maybe AppResources)
createResources Context {..} = do
  sdlLog "--- Beginning Resource Creation ---"

  -- Load images
  maybeSurf1 <- loadImage ("Content" </> "Images" </> "ravioli.bmp")
  maybeSurf2 <- loadImage ("Content" </> "Images" </> "ravioli_inverted.bmp")

  case (maybeSurf1, maybeSurf2) of
    (Just surfPtr1, Just surfPtr2) -> do
      surfData1 <- peek surfPtr1
      surfData2 <- peek surfPtr2

      let srcWidth = fromIntegral $ surfaceW surfData1
          srcHeight = fromIntegral $ surfaceH surfData1
          imageSizeInBytes = srcWidth * srcHeight * 4

      sdlLog $ "Loaded ravioli textures (" ++ show srcWidth ++ "x" ++ show srcHeight ++ ")."

      -- Create source texture (2D array with 2 layers)
      let srcTexCI =
            SDLGPUTextureCreateInfo
              { texInfoType = SDL_GPU_TEXTURETYPE_2D_ARRAY,
                texInfoFormat = SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM,
                texInfoUsage = SDL_GPU_TEXTUREUSAGE_SAMPLER,
                texInfoWidth = srcWidth,
                texInfoHeight = srcHeight,
                texInfoLayerCountOrDepth = 2,
                texInfoNumLevels = 1,
                texInfoSampleCount = SDL_GPU_SAMPLECOUNT_1,
                texInfoProps = 0
              }
      maybeSrcTex <- sdlCreateGPUTexture contextDevice srcTexCI

      -- Create destination texture (2D array with 2 layers, half size)
      let dstTexCI =
            SDLGPUTextureCreateInfo
              { texInfoType = SDL_GPU_TEXTURETYPE_2D_ARRAY,
                texInfoFormat = SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM,
                texInfoUsage = SDL_GPU_TEXTUREUSAGE_SAMPLER .|. SDL_GPU_TEXTUREUSAGE_COLOR_TARGET,
                texInfoWidth = srcWidth `div` 2,
                texInfoHeight = srcHeight `div` 2,
                texInfoLayerCountOrDepth = 2,
                texInfoNumLevels = 1,
                texInfoSampleCount = SDL_GPU_SAMPLECOUNT_1,
                texInfoProps = 0
              }
      maybeDstTex <- sdlCreateGPUTexture contextDevice dstTexCI

      case (maybeSrcTex, maybeDstTex) of
        (Just srcTex, Just dstTex) -> do
          -- Upload both images to the source texture layers
          uploadSuccess <- bracket
            (createTransferBuffer contextDevice (imageSizeInBytes * 2) SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "Blit2DArrayTexTransfer")
            (cleanupTransferBuffer contextDevice)
            $ \case
              Nothing -> return False
              Just tb -> do
                mapOk <- bracket
                  (sdlMapGPUTransferBuffer contextDevice tb False)
                  (\mptr -> when (isJust mptr) $ sdlUnmapGPUTransferBuffer contextDevice tb)
                  $ \case
                    Nothing -> return False
                    Just ptr -> do
                      -- Copy first image
                      copyBytes (castPtr ptr) (surfacePixels surfData1) (fromIntegral imageSizeInBytes)
                      -- Copy second image after the first
                      copyBytes (castPtr $ plusPtr ptr (fromIntegral imageSizeInBytes)) (surfacePixels surfData2) (fromIntegral imageSizeInBytes)
                      return True

                if mapOk
                  then bracket
                    (sdlAcquireGPUCommandBuffer contextDevice)
                    cleanupCommandBuffer
                    $ \case
                      Nothing -> return False
                      Just cmd -> do
                        mcp <- sdlBeginGPUCopyPass cmd
                        case mcp of
                          Nothing -> return False
                          Just cp -> do
                            -- Upload layer 0 (ravioli)
                            sdlUploadToGPUTexture
                              cp
                              (SDLGPUTextureTransferInfo tb 0 0 0)
                              (SDLGPUTextureRegion srcTex 0 0 0 0 0 srcWidth srcHeight 1)
                              False
                            -- Upload layer 1 (ravioli_inverted)
                            sdlUploadToGPUTexture
                              cp
                              (SDLGPUTextureTransferInfo tb imageSizeInBytes 0 0)
                              (SDLGPUTextureRegion srcTex 0 1 0 0 0 srcWidth srcHeight 1)
                              False
                            sdlEndGPUCopyPass cp

                            -- Blit from source to destination (layer 0 -> layer 0)
                            let blitInfo0 =
                                  SDLGPUBlitInfo
                                    { gpuBlitInfoSource = SDLGPUBlitRegion srcTex 0 0 0 0 srcWidth srcHeight,
                                      gpuBlitInfoDestination = SDLGPUBlitRegion dstTex 0 0 0 0 (srcWidth `div` 2) (srcHeight `div` 2),
                                      gpuBlitInfoLoadOp = SDL_GPU_LOADOP_DONT_CARE,
                                      gpuBlitInfoClearColor = SDLFColor 0 0 0 1,
                                      gpuBlitInfoFlipMode = SDL_FLIP_NONE,
                                      gpuBlitInfoFilter = SDL_GPU_FILTER_LINEAR,
                                      gpuBlitInfoCycle = False
                                    }
                            sdlBlitGPUTexture cmd blitInfo0

                            -- Blit from source to destination (layer 1 -> layer 1)
                            let blitInfo1 =
                                  SDLGPUBlitInfo
                                    { gpuBlitInfoSource = SDLGPUBlitRegion srcTex 0 1 0 0 srcWidth srcHeight,
                                      gpuBlitInfoDestination = SDLGPUBlitRegion dstTex 0 1 0 0 (srcWidth `div` 2) (srcHeight `div` 2),
                                      gpuBlitInfoLoadOp = SDL_GPU_LOADOP_LOAD,
                                      gpuBlitInfoClearColor = SDLFColor 0 0 0 1,
                                      gpuBlitInfoFlipMode = SDL_FLIP_NONE,
                                      gpuBlitInfoFilter = SDL_GPU_FILTER_LINEAR,
                                      gpuBlitInfoCycle = False
                                    }
                            sdlBlitGPUTexture cmd blitInfo1

                            sdlSubmitGPUCommandBuffer cmd
                  else return False

          sdlDestroySurface surfPtr1
          sdlDestroySurface surfPtr2

          if uploadSuccess
            then do
              sdlLog "--- Resource Creation Successful ---"
              return $
                Just
                  AppResources
                    { resSourceTexture = srcTex,
                      resDestinationTexture = dstTex,
                      resSrcWidth = srcWidth,
                      resSrcHeight = srcHeight
                    }
            else do
              sdlLog "!!! Failed to upload textures."
              sdlReleaseGPUTexture contextDevice srcTex
              sdlReleaseGPUTexture contextDevice dstTex
              return Nothing
        _ -> do
          sdlLog "!!! Failed to create textures."
          sdlDestroySurface surfPtr1
          sdlDestroySurface surfPtr2
          return Nothing
    _ -> do
      sdlLog "!!! Failed to load images."
      return Nothing

-- | releaseResources
releaseResources :: Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources Context {..} (Just AppResources {..}) = do
  sdlLog "--> Releasing AppResources..."
  sdlReleaseGPUTexture contextDevice resSourceTexture
  sdlReleaseGPUTexture contextDevice resDestinationTexture
  sdlLog "<-- AppResources Released."

-- | eventLoopGPU
eventLoopGPU :: Context -> AppResources -> IO ()
eventLoopGPU context resources = do
  sdlPumpEvents
  shouldQuit <- processEventsGPU
  unless shouldQuit $ do
    renderFrameGPU context resources
    eventLoopGPU context resources

-- | processEventsGPU
processEventsGPU :: IO Bool
processEventsGPU =
  sdlPollEvent >>= \case
    Nothing -> return False
    Just event -> case event of
      SDLEventQuit _ -> sdlLog "Quit." >> return True
      SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ sc _ _ _ down _)
        | down && sc == SDL_SCANCODE_Q -> return True
      _ -> processEventsGPU

-- | renderFrameGPU - Displays source (left) and destination (right) textures
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
          -- Clear the screen
          let colorTargetInfo =
                defaultColorTargetInfo
                  { texture = swapchainTexture,
                    loadOp = SDL_GPU_LOADOP_CLEAR,
                    storeOp = SDL_GPU_STOREOP_STORE,
                    clearColor = SDLFColor 0 0 0 1
                  }
          maybeRp <- sdlBeginGPURenderPass cmdbuf [colorTargetInfo] Nothing
          case maybeRp of
            Nothing -> return ()
            Just rp -> sdlEndGPURenderPass rp

          let halfW = fromIntegral w `div` 2
              screenH = fromIntegral h

          -- Left side: blit source layer 0 (original)
          let blitSrc0 =
                SDLGPUBlitInfo
                  { gpuBlitInfoSource = SDLGPUBlitRegion resSourceTexture 0 0 0 0 resSrcWidth resSrcHeight,
                    gpuBlitInfoDestination = SDLGPUBlitRegion swapchainTexture 0 0 0 0 halfW screenH,
                    gpuBlitInfoLoadOp = SDL_GPU_LOADOP_LOAD,
                    gpuBlitInfoClearColor = SDLFColor 0 0 0 1,
                    gpuBlitInfoFlipMode = SDL_FLIP_NONE,
                    gpuBlitInfoFilter = SDL_GPU_FILTER_NEAREST,
                    gpuBlitInfoCycle = False
                  }
          sdlBlitGPUTexture cmdbuf blitSrc0

          -- Right side: blit destination layer 1 (downscaled inverted)
          let blitDst1 =
                SDLGPUBlitInfo
                  { gpuBlitInfoSource = SDLGPUBlitRegion resDestinationTexture 0 1 0 0 (resSrcWidth `div` 2) (resSrcHeight `div` 2),
                    gpuBlitInfoDestination = SDLGPUBlitRegion swapchainTexture 0 0 halfW 0 halfW screenH,
                    gpuBlitInfoLoadOp = SDL_GPU_LOADOP_LOAD,
                    gpuBlitInfoClearColor = SDLFColor 0 0 0 1,
                    gpuBlitInfoFlipMode = SDL_FLIP_NONE,
                    gpuBlitInfoFilter = SDL_GPU_FILTER_NEAREST,
                    gpuBlitInfoCycle = False
                  }
          sdlBlitGPUTexture cmdbuf blitDst1

          void $ sdlSubmitGPUCommandBuffer cmdbuf
