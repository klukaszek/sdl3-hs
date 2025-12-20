{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Example     : GPUBlitMirror
-- Description : Demonstrates texture blitting with flip modes (mirroring).
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Based on the SDL_gpu_examples/BlitMirror C example.
-- This example demonstrates:
-- - Loading a texture from an image file
-- - Using SDL_BlitGPUTexture with different flip modes
-- - Displaying 4 quadrants: normal, horizontal flip, vertical flip, both flips
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
  { resTexture :: SDLGPUTexture,
    resTexWidth :: Word32,
    resTexHeight :: Word32
  }
  deriving (Show)

-- | main
main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion
  maybeResult <- withContext "SDL3 Haskell GPU Blit Mirror" [] runAppGPU
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

-- | createResources - Creates and uploads the texture
createResources :: Context -> IO (Maybe AppResources)
createResources Context {..} = do
  sdlLog "--- Beginning Resource Creation ---"

  -- Load the ravioli image
  maybeSurf <- loadImage ("Content" </> "Images" </> "ravioli.bmp")
  case maybeSurf of
    Nothing -> do
      sdlLog "!!! Failed to load ravioli.bmp."
      return Nothing
    Just surfPtr -> do
      surfData <- peek surfPtr
      let texWidth = fromIntegral $ surfaceW surfData
          texHeight = fromIntegral $ surfaceH surfData
          byteCount = texWidth * texHeight * 4

      sdlLog $ "Loaded ravioli.bmp (" ++ show texWidth ++ "x" ++ show texHeight ++ ")."

      -- Create texture
      let texCI =
            SDLGPUTextureCreateInfo
              { texInfoType = SDL_GPU_TEXTURETYPE_2D,
                texInfoFormat = SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM,
                texInfoUsage = SDL_GPU_TEXTUREUSAGE_SAMPLER,
                texInfoWidth = texWidth,
                texInfoHeight = texHeight,
                texInfoLayerCountOrDepth = 1,
                texInfoNumLevels = 1,
                texInfoSampleCount = SDL_GPU_SAMPLECOUNT_1,
                texInfoProps = 0
              }
      maybeTex <- sdlCreateGPUTexture contextDevice texCI

      case maybeTex of
        Nothing -> do
          sdlLog "!!! Failed to create texture."
          sdlDestroySurface surfPtr
          return Nothing
        Just tex -> do
          -- Upload texture
          uploadSuccess <- bracket
            (createTransferBuffer contextDevice byteCount SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "BlitMirrorTexTransfer")
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
                      copyBytes (castPtr ptr) (surfacePixels surfData) (fromIntegral byteCount)
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
                            sdlUploadToGPUTexture
                              cp
                              (SDLGPUTextureTransferInfo tb 0 0 0)
                              (SDLGPUTextureRegion tex 0 0 0 0 0 texWidth texHeight 1)
                              False
                            sdlEndGPUCopyPass cp
                            sdlSubmitGPUCommandBuffer cmd
                  else return False

          sdlDestroySurface surfPtr

          if uploadSuccess
            then do
              sdlLog "--- Resource Creation Successful ---"
              return $ Just AppResources {resTexture = tex, resTexWidth = texWidth, resTexHeight = texHeight}
            else do
              sdlLog "!!! Failed to upload texture."
              sdlReleaseGPUTexture contextDevice tex
              return Nothing

-- | releaseResources
releaseResources :: Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources Context {..} (Just AppResources {..}) = do
  sdlLog "--> Releasing AppResources..."
  sdlReleaseGPUTexture contextDevice resTexture
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

-- | renderFrameGPU - Renders 4 quadrants with different flip modes
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
          let halfW = fromIntegral w `div` 2
              halfH = fromIntegral h `div` 2

          -- 1. Clear the screen first
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

          -- 2. Top-left: Normal
          let blitNormal =
                SDLGPUBlitInfo
                  { gpuBlitInfoSource = SDLGPUBlitRegion resTexture 0 0 0 0 resTexWidth resTexHeight,
                    gpuBlitInfoDestination = SDLGPUBlitRegion swapchainTexture 0 0 0 0 halfW halfH,
                    gpuBlitInfoLoadOp = SDL_GPU_LOADOP_DONT_CARE,
                    gpuBlitInfoClearColor = SDLFColor 0 0 0 1,
                    gpuBlitInfoFlipMode = SDL_FLIP_NONE,
                    gpuBlitInfoFilter = SDL_GPU_FILTER_NEAREST,
                    gpuBlitInfoCycle = False
                  }
          sdlBlitGPUTexture cmdbuf blitNormal

          -- 3. Top-right: Flipped Horizontally
          let blitHorizontal =
                SDLGPUBlitInfo
                  { gpuBlitInfoSource = SDLGPUBlitRegion resTexture 0 0 0 0 resTexWidth resTexHeight,
                    gpuBlitInfoDestination = SDLGPUBlitRegion swapchainTexture 0 0 halfW 0 halfW halfH,
                    gpuBlitInfoLoadOp = SDL_GPU_LOADOP_LOAD,
                    gpuBlitInfoClearColor = SDLFColor 0 0 0 1,
                    gpuBlitInfoFlipMode = SDL_FLIP_HORIZONTAL,
                    gpuBlitInfoFilter = SDL_GPU_FILTER_NEAREST,
                    gpuBlitInfoCycle = False
                  }
          sdlBlitGPUTexture cmdbuf blitHorizontal

          -- 4. Bottom-left: Flipped Vertically
          let blitVertical =
                SDLGPUBlitInfo
                  { gpuBlitInfoSource = SDLGPUBlitRegion resTexture 0 0 0 0 resTexWidth resTexHeight,
                    gpuBlitInfoDestination = SDLGPUBlitRegion swapchainTexture 0 0 0 halfH halfW halfH,
                    gpuBlitInfoLoadOp = SDL_GPU_LOADOP_LOAD,
                    gpuBlitInfoClearColor = SDLFColor 0 0 0 1,
                    gpuBlitInfoFlipMode = SDL_FLIP_VERTICAL,
                    gpuBlitInfoFilter = SDL_GPU_FILTER_NEAREST,
                    gpuBlitInfoCycle = False
                  }
          sdlBlitGPUTexture cmdbuf blitVertical

          -- 5. Bottom-right: Flipped Both
          let blitBoth =
                SDLGPUBlitInfo
                  { gpuBlitInfoSource = SDLGPUBlitRegion resTexture 0 0 0 0 resTexWidth resTexHeight,
                    gpuBlitInfoDestination = SDLGPUBlitRegion swapchainTexture 0 0 halfW halfH halfW halfH,
                    gpuBlitInfoLoadOp = SDL_GPU_LOADOP_LOAD,
                    gpuBlitInfoClearColor = SDLFColor 0 0 0 1,
                    gpuBlitInfoFlipMode = sdlFlipBoth,
                    gpuBlitInfoFilter = SDL_GPU_FILTER_NEAREST,
                    gpuBlitInfoCycle = False
                  }
          sdlBlitGPUTexture cmdbuf blitBoth

          void $ sdlSubmitGPUCommandBuffer cmdbuf

-- | Combined horizontal + vertical flip
sdlFlipBoth :: SDLFlipMode
sdlFlipBoth = toEnum (fromEnum SDL_FLIP_HORIZONTAL .|. fromEnum SDL_FLIP_VERTICAL)
