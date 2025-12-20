{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Example     : GPUTriangleMSAA
-- Description : Demonstrates multi-sample anti-aliasing (MSAA) with GPU rendering.
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Based on the SDL_gpu_examples/TriangleMSAA C example.
-- This example demonstrates:
-- - Creating pipelines with different MSAA sample counts (1, 2, 4, 8)
-- - Rendering to an MSAA render target
-- - Resolving MSAA textures for display
-- - Dynamically switching between sample counts
module Main where

import Control.Exception (bracket)
import Control.Monad (forM_, unless, void, when)
import Data.Bits ((.|.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (catMaybes, isJust)
import GPUCommon
import SDL3
import System.Exit (exitFailure, exitSuccess)

-- | MSAA resource for a specific sample count
data MSAAResource = MSAAResource
  { msaaPipeline :: SDLGPUGraphicsPipeline,
    msaaRenderTexture :: SDLGPUTexture,
    msaaSampleCount :: SDLGPUSampleCount
  }
  deriving (Show)

-- | Application resources
data AppResources = AppResources
  { resMSAAResources :: [MSAAResource], -- Up to 4 sample counts
    resResolveTexture :: SDLGPUTexture,
    resRTFormat :: SDLGPUTextureFormat
  }
  deriving (Show)

-- | main
main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion
  maybeResult <- withContext "SDL3 Haskell GPU Triangle MSAA" [] runAppGPU
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
        sdlLog "Press Left/Right to cycle between sample counts."
        currentIndexRef <- newIORef 0
        eventLoopGPU context resources currentIndexRef

-- | Sample count list to check
sampleCountsToTry :: [SDLGPUSampleCount]
sampleCountsToTry =
  [ SDL_GPU_SAMPLECOUNT_1,
    SDL_GPU_SAMPLECOUNT_2,
    SDL_GPU_SAMPLECOUNT_4,
    SDL_GPU_SAMPLECOUNT_8
  ]

-- | createResources
createResources :: Context -> IO (Maybe AppResources)
createResources Context {..} = do
  sdlLog "--- Beginning Resource Creation ---"

  -- Load shaders
  let shInfo = defaultShaderCreateInfo
  maybeVertShader <- loadShader contextDevice "RawTriangle.vert" SDL_GPU_SHADERSTAGE_VERTEX shInfo
  maybeFragShader <- loadShader contextDevice "SolidColor.frag" SDL_GPU_SHADERSTAGE_FRAGMENT shInfo

  case (maybeVertShader, maybeFragShader) of
    (Just vertS, Just fragS) -> do
      -- Get swapchain format
      rtFormat <- sdlGetGPUSwapchainTextureFormat contextDevice contextWindow

      -- Try to create pipelines and render textures for each supported sample count
      msaaResources <- catMaybes <$> mapM (tryCreateMSAAResource contextDevice vertS fragS rtFormat) sampleCountsToTry

      -- Release shaders
      sdlReleaseGPUShader contextDevice vertS
      sdlReleaseGPUShader contextDevice fragS

      if null msaaResources
        then do
          sdlLog "!!! No MSAA sample counts supported."
          return Nothing
        else do
          -- Create resolve texture
          let resolveTexCI =
                SDLGPUTextureCreateInfo
                  { texInfoType = SDL_GPU_TEXTURETYPE_2D,
                    texInfoFormat = rtFormat,
                    texInfoUsage = SDL_GPU_TEXTUREUSAGE_COLOR_TARGET .|. SDL_GPU_TEXTUREUSAGE_SAMPLER,
                    texInfoWidth = 640,
                    texInfoHeight = 480,
                    texInfoLayerCountOrDepth = 1,
                    texInfoNumLevels = 1,
                    texInfoSampleCount = SDL_GPU_SAMPLECOUNT_1,
                    texInfoProps = 0
                  }
          maybeResolveTex <- sdlCreateGPUTexture contextDevice resolveTexCI

          case maybeResolveTex of
            Nothing -> do
              sdlLog "!!! Failed to create resolve texture."
              forM_ msaaResources $ \r -> do
                sdlReleaseGPUGraphicsPipeline contextDevice (msaaPipeline r)
                sdlReleaseGPUTexture contextDevice (msaaRenderTexture r)
              return Nothing
            Just resolveTex -> do
              sdlLog $ "Created " ++ show (length msaaResources) ++ " MSAA configurations."
              forM_ msaaResources $ \r ->
                sdlLog $ "  Sample count: " ++ show (getSampleCountValue (msaaSampleCount r))
              sdlLog $ "Current sample count: " ++ show (getSampleCountValue (msaaSampleCount (head msaaResources)))
              return $
                Just
                  AppResources
                    { resMSAAResources = msaaResources,
                      resResolveTexture = resolveTex,
                      resRTFormat = rtFormat
                    }
    _ -> do
      sdlLog "!!! Failed to load shaders."
      return Nothing

-- | Try to create an MSAA resource for a given sample count
tryCreateMSAAResource ::
  SDLGPUDevice ->
  SDLGPUShader ->
  SDLGPUShader ->
  SDLGPUTextureFormat ->
  SDLGPUSampleCount ->
  IO (Maybe MSAAResource)
tryCreateMSAAResource device vertS fragS rtFormat sampleCount = do
  supported <- sdlGPUTextureSupportsSampleCount device rtFormat sampleCount
  if not supported
    then do
      sdlLog $ "Sample count " ++ show (getSampleCountValue sampleCount) ++ " not supported."
      return Nothing
    else do
      -- Create pipeline with this sample count
      let basePipelineCI = defaultGraphicsPipelineCreateInfo vertS fragS rtFormat
          pipelineCI =
            basePipelineCI
              { multisampleState = (multisampleState basePipelineCI) {sampleCount = sampleCount}
              }
      maybePipeline <- sdlCreateGPUGraphicsPipeline device pipelineCI

      case maybePipeline of
        Nothing -> do
          sdlLog $ "Failed to create pipeline for sample count " ++ show (getSampleCountValue sampleCount)
          return Nothing
        Just pipeline -> do
          -- Create render target texture
          let usage =
                if sampleCount == SDL_GPU_SAMPLECOUNT_1
                  then SDL_GPU_TEXTUREUSAGE_COLOR_TARGET .|. SDL_GPU_TEXTUREUSAGE_SAMPLER
                  else SDL_GPU_TEXTUREUSAGE_COLOR_TARGET
              texCI =
                SDLGPUTextureCreateInfo
                  { texInfoType = SDL_GPU_TEXTURETYPE_2D,
                    texInfoFormat = rtFormat,
                    texInfoUsage = usage,
                    texInfoWidth = 640,
                    texInfoHeight = 480,
                    texInfoLayerCountOrDepth = 1,
                    texInfoNumLevels = 1,
                    texInfoSampleCount = sampleCount,
                    texInfoProps = 0
                  }
          maybeTex <- sdlCreateGPUTexture device texCI

          case maybeTex of
            Nothing -> do
              sdlLog $ "Failed to create render texture for sample count " ++ show (getSampleCountValue sampleCount)
              sdlReleaseGPUGraphicsPipeline device pipeline
              return Nothing
            Just tex ->
              return $
                Just
                  MSAAResource
                    { msaaPipeline = pipeline,
                      msaaRenderTexture = tex,
                      msaaSampleCount = sampleCount
                    }

-- | Get numeric value of sample count for logging
getSampleCountValue :: SDLGPUSampleCount -> Int
getSampleCountValue sc
  | sc == SDL_GPU_SAMPLECOUNT_1 = 1
  | sc == SDL_GPU_SAMPLECOUNT_2 = 2
  | sc == SDL_GPU_SAMPLECOUNT_4 = 4
  | sc == SDL_GPU_SAMPLECOUNT_8 = 8
  | otherwise = 1

-- | releaseResources
releaseResources :: Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources Context {..} (Just AppResources {..}) = do
  sdlLog "--> Releasing AppResources..."
  forM_ resMSAAResources $ \r -> do
    sdlReleaseGPUGraphicsPipeline contextDevice (msaaPipeline r)
    sdlReleaseGPUTexture contextDevice (msaaRenderTexture r)
  sdlReleaseGPUTexture contextDevice resResolveTexture
  sdlLog "<-- AppResources Released."

-- | eventLoopGPU
eventLoopGPU :: Context -> AppResources -> IORef Int -> IO ()
eventLoopGPU context resources currentIndexRef = do
  sdlPumpEvents
  shouldQuit <- processEventsGPU resources currentIndexRef
  unless shouldQuit $ do
    renderFrameGPU context resources currentIndexRef
    eventLoopGPU context resources currentIndexRef

-- | processEventsGPU
processEventsGPU :: AppResources -> IORef Int -> IO Bool
processEventsGPU AppResources {..} currentIndexRef = go
  where
    numConfigs = length resMSAAResources
    go =
      sdlPollEvent >>= \case
        Nothing -> return False
        Just event -> do
          quit <- handleEvent event
          if quit then return True else go

    handleEvent (SDLEventQuit _) = sdlLog "Quit." >> return True
    handleEvent (SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ sc _ _ _ down _))
      | down = case sc of
          _ | sc == SDL_SCANCODE_Q -> return True
          _ | sc == SDL_SCANCODE_LEFT -> do
            idx <- readIORef currentIndexRef
            let newIdx = if idx <= 0 then numConfigs - 1 else idx - 1
            writeIORef currentIndexRef newIdx
            let newSampleCount = getSampleCountValue (msaaSampleCount (resMSAAResources !! newIdx))
            sdlLog $ "Current sample count: " ++ show newSampleCount
            return False
          _ | sc == SDL_SCANCODE_RIGHT -> do
            idx <- readIORef currentIndexRef
            let newIdx = (idx + 1) `mod` numConfigs
            writeIORef currentIndexRef newIdx
            let newSampleCount = getSampleCountValue (msaaSampleCount (resMSAAResources !! newIdx))
            sdlLog $ "Current sample count: " ++ show newSampleCount
            return False
          _ -> return False
      | otherwise = return False
    handleEvent _ = return False

-- | renderFrameGPU
renderFrameGPU :: Context -> AppResources -> IORef Int -> IO ()
renderFrameGPU Context {..} AppResources {..} currentIndexRef = do
  currentIdx <- readIORef currentIndexRef
  let currentResource = resMSAAResources !! currentIdx
      currentSampleCount = msaaSampleCount currentResource
      isMultiSample = currentSampleCount /= SDL_GPU_SAMPLECOUNT_1

  maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
  case maybeCmdbuf of
    Nothing -> sdlLog "Error: Failed to acquire render command buffer."
    Just cmdbuf -> do
      maybeSwapResult <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
      case maybeSwapResult of
        Nothing -> void $ sdlSubmitGPUCommandBuffer cmdbuf
        Just (swapchainTexture, w, h) -> do
          -- Render to MSAA target
          let colorTargetInfo =
                defaultColorTargetInfo
                  { texture = msaaRenderTexture currentResource,
                    loadOp = SDL_GPU_LOADOP_CLEAR,
                    storeOp = if isMultiSample then SDL_GPU_STOREOP_RESOLVE else SDL_GPU_STOREOP_STORE,
                    clearColor = SDLFColor 1 1 1 1, -- White background
                    resolveTexture = if isMultiSample then Just resResolveTexture else Nothing
                  }

          maybeRp <- sdlBeginGPURenderPass cmdbuf [colorTargetInfo] Nothing
          case maybeRp of
            Nothing -> return ()
            Just rp -> do
              sdlBindGPUGraphicsPipeline rp (msaaPipeline currentResource)
              sdlDrawGPUPrimitives rp 3 1 0 0
              sdlEndGPURenderPass rp

          -- Blit result to swapchain (from resolve texture if MSAA, otherwise from render texture)
          let blitSource = if isMultiSample then resResolveTexture else msaaRenderTexture currentResource
          let blitInfo =
                SDLGPUBlitInfo
                  { gpuBlitInfoSource = SDLGPUBlitRegion blitSource 0 0 160 0 320 240, -- Crop center
                    gpuBlitInfoDestination = SDLGPUBlitRegion swapchainTexture 0 0 0 0 (fromIntegral w) (fromIntegral h),
                    gpuBlitInfoLoadOp = SDL_GPU_LOADOP_DONT_CARE,
                    gpuBlitInfoClearColor = SDLFColor 0 0 0 1,
                    gpuBlitInfoFlipMode = SDL_FLIP_NONE,
                    gpuBlitInfoFilter = SDL_GPU_FILTER_LINEAR,
                    gpuBlitInfoCycle = False
                  }
          sdlBlitGPUTexture cmdbuf blitInfo

          void $ sdlSubmitGPUCommandBuffer cmdbuf
