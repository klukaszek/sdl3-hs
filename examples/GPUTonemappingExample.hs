{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Example     : GPUTonemappingExample
-- Description : Demonstrates HDR image loading, compute shader-based tonemapping, and color space conversion.
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Based on the SDL_gpu_examples/Tonemapping C example.
-- This example showcases a comprehensive GPU pipeline:
-- - Loading an HDR (High Dynamic Range) image file (e.g., .hdr) using a wrapper around stb_image's `stbi_loadf`.
-- - Creating multiple textures for different stages:
--     - `HDRTexture`: Stores the original HDR image data (e.g., R32G32B32A32_FLOAT format).
--     - `ToneMapTexture`: Intermediate texture to store the tonemapped HDR data (e.g., R16G16B16A16_FLOAT).
--     - `TransferTexture`: Intermediate texture for color space converted output, matching swapchain format.
-- - Uploading the loaded HDR pixel data to the `HDRTexture`.
-- - Creating several compute pipelines:
--     - A set of tonemapping operators (Reinhard, ACES, etc.), each implemented as a compute shader. These shaders
--       read from `HDRTexture` and write the tonemapped result to `ToneMapTexture`.
--     - `LinearToSRGBPipeline`: A compute shader to convert linear HDR data (from `ToneMapTexture`) to sRGB space,
--       writing to `TransferTexture`.
--     - `LinearToST2084Pipeline`: A compute shader to convert linear HDR data (from `ToneMapTexture`) to ST2084 (PQ)
--       HDR10 space, writing to `TransferTexture`.
-- - Managing user interaction to cycle through:
--     - Different SDL Swapchain Compositions (SDR, SDR_LINEAR, HDR_EXTENDED_LINEAR, HDR10_ST2084) using
--       `SDL_SetGPUSwapchainParameters` and checking support with `SDL_WindowSupportsGPUSwapchainComposition`.
--     - Different tonemapping operators.
-- - In each frame's render process:
--     1. **Tonemapping Pass:**
--         - A compute pass is initiated.
--         - The selected tonemapping compute pipeline is bound.
--         - `HDRTexture` is bound as a read-only storage texture input.
--         - `ToneMapTexture` is bound as a read/write storage texture output.
--         - The compute shader is dispatched.
--     2. **Color Space Conversion Pass (Conditional):**
--         - If the current swapchain composition is SDR or HDR10_ST2084, another compute pass is run.
--         - The appropriate conversion pipeline (`LinearToSRGBPipeline` or `LinearToST2084Pipeline`) is bound.
--         - `ToneMapTexture` is bound as a read-only storage texture input.
--         - `TransferTexture` is bound as a read/write storage texture output.
--         - The compute shader is dispatched.
--     3. **Blit to Swapchain:**
--         - The resulting texture (`ToneMapTexture` or `TransferTexture` if conversion occurred) is blitted to the
--           acquired swapchain texture using `SDL_BlitGPUTexture`.
-- - This example demonstrates a multi-stage image processing pipeline using compute shaders, handling
--   different color spaces, and adapting output based on swapchain capabilities.
-- |
module Main where

import Control.Exception (bracket, bracketOnError, finally)
import Control.Monad (forM, forM_, unless, void, when)
import Data.Bits ((.|.))
import Data.IORef
import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import Foreign.Ptr (Ptr, castPtr)
import GPUCommon
import SDL
import System.Exit (exitFailure, exitSuccess)

-- AppResources
data AppResources = AppResources
  { resHDRTexture :: SDLGPUTexture,
    resToneMapTexture :: SDLGPUTexture,
    resTransferTexture :: SDLGPUTexture,
    resTonemapOperators :: [SDLGPUComputePipeline], -- Store all loaded operators
    resLinearToSRGBPipeline :: SDLGPUComputePipeline,
    resLinearToST2084Pipeline :: SDLGPUComputePipeline,
    -- User interaction state
    currentSwapchainCompositionRef :: IORef SDLGPUSwapchainComposition,
    currentTonemapOperatorRef :: IORef SDLGPUComputePipeline,
    swapchainCompositionIndexRef :: IORef Int,
    tonemapOperatorIndexRef :: IORef Int
  }

-- Global constants for UI cycling (matching C example)
swapchainCompositions :: [SDLGPUSwapchainComposition]
swapchainCompositions =
  [ SDL_GPU_SWAPCHAINCOMPOSITION_SDR,
    SDL_GPU_SWAPCHAINCOMPOSITION_SDR_LINEAR,
    SDL_GPU_SWAPCHAINCOMPOSITION_HDR_EXTENDED_LINEAR,
    SDL_GPU_SWAPCHAINCOMPOSITION_HDR10_ST2084
  ]

swapchainCompositionNames :: [String]
swapchainCompositionNames =
  [ "SDR",
    "SDR_LINEAR",
    "HDR_EXTENDED_LINEAR",
    "HDR10_ST2084"
  ]

tonemapOperatorShaderFiles :: [String] -- Base filenames for compute shaders
tonemapOperatorShaderFiles =
  [ "ToneMapReinhard.comp",
    "ToneMapExtendedReinhardLuminance.comp",
    "ToneMapHable.comp",
    "ToneMapACES.comp"
  ]

tonemapOperatorDisplayNames :: [String]
tonemapOperatorDisplayNames = ["Reinhard", "ExtendedReinhardLuminance", "Hable", "ACES"]

-- main
main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  maybeResult <- withContext "SDL3 Haskell GPU Tonemapping" [] runAppGPU
  case maybeResult of
    Nothing -> sdlLog "Application initialization failed." >> exitFailure
    Just _ -> sdlLog "Application finished successfully." >> exitSuccess

-- runAppGPU
runAppGPU :: Context -> IO ()
runAppGPU context = do
  sdlLog "Base context initialized."
  bracket
    (createResources context)
    (releaseResources context)
    $ \case
      Nothing -> sdlLog "Failed to create resources. Exiting."
      Just resources -> do
        sdlLog "Resources created successfully."
        sdlLog "Press Left/Right to cycle swapchain composition"
        sdlLog "Press Up/Down to cycle tonemap operators"
        eventLoopGPU context resources

-- createResources
createResources :: Context -> IO (Maybe AppResources)
createResources context@Context {..} = do
  sdlLog "--- Beginning Resource Creation ---"
  let desiredChannels = 4

  -- Load HDR Image Data
  (wInt, hInt) <-
    bracketOnError
      (loadHDRImageFromFile "memorial.hdr" desiredChannels)
      ( \case
          Just (ptr, _, _, _) -> free (castPtr ptr)
          _ -> pure ()
      )
      ( \case
          Nothing -> sdlLog "Could not load HDR image data!" >> error "HDR Load Failed" -- Force exit
          Just (ptr, w, h, n) -> do
            sdlLog $ "HDR Image loaded: " ++ show w ++ "x" ++ show h ++ ", channels: " ++ show n
            -- Adjust window size based on loaded HDR image
            sdlSetWindowSize contextWindow w h
            return (w, h)
      )

  let w = fromIntegral wInt :: Int
  let h = fromIntegral hInt :: Int

  -- Create Textures
  let hdrFormat = SDL_GPU_TEXTUREFORMAT_R32G32B32A32_FLOAT
  let toneMapFormat = SDL_GPU_TEXTUREFORMAT_R16G16B16A16_FLOAT
  swapchainFmt <- sdlGetGPUSwapchainTextureFormat contextDevice contextWindow

  maybeHDRTexture <-
    sdlCreateGPUTexture
      contextDevice
      ( (defaultTextureCreateInfo w h)
          { texInfoFormat = hdrFormat,
            texInfoUsage = SDL_GPU_TEXTUREUSAGE_SAMPLER .|. SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_READ -- Read by tonemapper
          }
      )
  maybeToneMapTexture <-
    sdlCreateGPUTexture
      contextDevice
      ( (defaultTextureCreateInfo w h)
          { texInfoFormat = toneMapFormat,
            texInfoUsage = SDL_GPU_TEXTUREUSAGE_SAMPLER .|. SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_READ .|. SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_WRITE -- Read by post, written by tonemapper
          }
      )
  maybeTransferTexture <-
    sdlCreateGPUTexture
      contextDevice
      ( (defaultTextureCreateInfo w h)
          { texInfoFormat = swapchainFmt,
            texInfoUsage = SDL_GPU_TEXTUREUSAGE_SAMPLER .|. SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_WRITE -- Written by post, read by blit
          }
      )

  -- Create Compute Pipelines for Tonemapping
  let tonemapBaseCI =
        defaultComputePipelineCreateInfo
          { numReadOnlyStorageTextures = 1, -- Reads HDRTexture
            numReadWriteStorageTextures = 1, -- Writes ToneMapTexture
            threadCountX = 8,
            threadCountY = 8,
            threadCountZ = 1
          }
  -- Use forM to attempt creation of all tonemap operators
  tonemapOps <- forM tonemapOperatorShaderFiles $ \shaderFile ->
    createComputePipelineFromShader contextDevice shaderFile tonemapBaseCI

  -- Create Post-Processing Compute Pipelines
  let postProcessBaseCI =
        defaultComputePipelineCreateInfo
          { numReadOnlyStorageTextures = 1, -- Reads ToneMapTexture
            numReadWriteStorageTextures = 1, -- Writes TransferTexture
            threadCountX = 8,
            threadCountY = 8,
            threadCountZ = 1
          }
  maybeLinearToSRGB <- createComputePipelineFromShader contextDevice "LinearToSRGB.comp" postProcessBaseCI
  maybeLinearToST2084 <- createComputePipelineFromShader contextDevice "LinearToST2084.comp" postProcessBaseCI

  -- Upload HDR Image Data to HDRTexture
  -- (Re-load for upload, or pass ptr around; re-loading is simpler for bracket scope)
  uploadSuccess <-
    bracketOnError
      (loadHDRImageFromFile "memorial.hdr" 4)
      ( \case
          Just (ptr, _, _, _) -> free (castPtr ptr)
          _ -> pure ()
      )
      ( \case
          Nothing -> sdlLog "Failed to re-load HDR for upload." >> return False
          Just (pixelPtr, _, _, _) ->
            case maybeHDRTexture of
              Nothing -> sdlLog "HDR Texture not created, cannot upload." >> return False
              Just hdrTex -> do
                let dataSizeBytes = fromIntegral (wInt * hInt * desiredChannels * 4) -- 4 bytes per pixel
                bracket
                  (createTransferBuffer contextDevice dataSizeBytes SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "HDRUpload")
                  (cleanupTransferBuffer contextDevice)
                  $ \case
                    Nothing -> return False
                    Just transferBuf -> do
                      mapOk <- bracket
                        (sdlMapGPUTransferBuffer contextDevice transferBuf False)
                        (\mptr -> when (isJust mptr) $ sdlUnmapGPUTransferBuffer contextDevice transferBuf)
                        $ \case
                          Nothing -> return False
                          Just mappedPtr -> memcpy mappedPtr (castPtr pixelPtr) (fromIntegral dataSizeBytes) >> return True
                      if mapOk
                        then do
                          cmdBuf <- sdlAcquireGPUCommandBuffer contextDevice
                          case cmdBuf of
                            Nothing -> return False
                            Just cb -> do
                              mcp <- sdlBeginGPUCopyPass cb
                              case mcp of
                                Nothing -> return False
                                Just cp -> do
                                  let texSrc = SDLGPUTextureTransferInfo transferBuf 0 (fromIntegral wInt) (fromIntegral hInt)
                                  let texDst = defaultTextureRegion hdrTex wInt hInt
                                  sdlUploadToGPUTexture cp texSrc texDst False
                                  sdlEndGPUCopyPass cp
                                  sdlSubmitGPUCommandBuffer cb >>= \s -> if s then sdlWaitForGPUIdle contextDevice >> return True else return False
                        else return False
      )

  -- Check all creations
  if not uploadSuccess || any isNothing tonemapOps || isNothing maybeLinearToSRGB || isNothing maybeLinearToST2084 || isNothing maybeHDRTexture || isNothing maybeToneMapTexture || isNothing maybeTransferTexture
    then do
      sdlLog "!!! Failed to create one or more critical resources."
      -- Manual cleanup of successfully created resources before this point
      maybe (pure ()) (sdlReleaseGPUTexture contextDevice) maybeHDRTexture
      maybe (pure ()) (sdlReleaseGPUTexture contextDevice) maybeToneMapTexture
      maybe (pure ()) (sdlReleaseGPUTexture contextDevice) maybeTransferTexture
      forM_ (catMaybes tonemapOps) (sdlReleaseGPUComputePipeline contextDevice)
      maybe (pure ()) (sdlReleaseGPUComputePipeline contextDevice) maybeLinearToSRGB
      maybe (pure ()) (sdlReleaseGPUComputePipeline contextDevice) maybeLinearToST2084
      return Nothing
    else do
      let allTonemapOps = catMaybes tonemapOps
      -- Initialize IORefs for UI state
      swapCompIdxRef <- newIORef 0
      toneOpIdxRef <- newIORef 0
      currentSwapCompRef <- newIORef (head swapchainCompositions)
      currentToneOpRef <- newIORef (head allTonemapOps)

      return $
        Just
          AppResources
            { resHDRTexture = fromJust maybeHDRTexture,
              resToneMapTexture = fromJust maybeToneMapTexture,
              resTransferTexture = fromJust maybeTransferTexture,
              resTonemapOperators = allTonemapOps,
              resLinearToSRGBPipeline = fromJust maybeLinearToSRGB,
              resLinearToST2084Pipeline = fromJust maybeLinearToST2084,
              currentSwapchainCompositionRef = currentSwapCompRef,
              currentTonemapOperatorRef = currentToneOpRef,
              swapchainCompositionIndexRef = swapCompIdxRef,
              tonemapOperatorIndexRef = toneOpIdxRef
            }

-- releaseResources
releaseResources :: Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources context@Context {..} appResourcesMaybe =
  -- Use pattern match on Maybe
  case appResourcesMaybe of
    Just AppResources {..} -> do
      let dev = contextDevice -- For convenience
      sdlLog "--> Releasing AppResources..."

      sdlLog $ "  --> Releasing " ++ show (length resTonemapOperators) ++ " Tonemap Operator Compute Pipelines..."
      forM_ (zip [0 ..] resTonemapOperators) $ \(idx, pipeline) -> do
        -- It might be nice to associate names with these if you store them that way
        sdlLog $ "    Releasing Tonemap Operator Pipeline #" ++ show idx ++ ": " ++ show (tonemapOperatorDisplayNames !! idx) ++ " " ++ show pipeline
        sdlReleaseGPUComputePipeline dev pipeline

      sdlLog $ "  --> Releasing LinearToSRGB Compute Pipeline: " ++ show resLinearToSRGBPipeline
      sdlReleaseGPUComputePipeline dev resLinearToSRGBPipeline

      sdlLog $ "  --> Releasing LinearToST2084 Compute Pipeline: " ++ show resLinearToST2084Pipeline
      sdlReleaseGPUComputePipeline dev resLinearToST2084Pipeline

      sdlLog $ "  --> Releasing HDR Texture: " ++ show resHDRTexture
      sdlReleaseGPUTexture dev resHDRTexture

      sdlLog $ "  --> Releasing ToneMap Texture: " ++ show resToneMapTexture
      sdlReleaseGPUTexture dev resToneMapTexture

      sdlLog $ "  --> Releasing Transfer Texture: " ++ show resTransferTexture
      sdlReleaseGPUTexture dev resTransferTexture

      sdlLog "<-- AppResources Released."

-- eventLoopGPU
eventLoopGPU :: Context -> AppResources -> IO ()
eventLoopGPU context resources = do
  sdlPumpEvents
  shouldQuitRef <- newIORef False
  processEventsGPU context resources shouldQuitRef
  shouldQuit <- readIORef shouldQuitRef
  unless shouldQuit $ do
    renderFrameGPU context resources
    eventLoopGPU context resources

-- processEventsGPU
processEventsGPU :: Context -> AppResources -> IORef Bool -> IO ()
processEventsGPU context resources quitRef = do
  maybeEvent <- sdlPollEvent
  case maybeEvent of
    Nothing -> return ()
    Just event -> do
      quit <- handleEventGPU context resources event
      when quit $ writeIORef quitRef True
      processEventsGPU context resources quitRef

-- handleEventGPU for UI interaction
handleEventGPU :: Context -> AppResources -> SDLEvent -> IO Bool
handleEventGPU context@Context {..} AppResources {..} event = case event of
  SDLEventQuit _ -> sdlLog "Quit event received." >> return True
  SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ scancode _ _ _ down _) | down -> do
    case scancode of
      SDL_SCANCODE_Q -> return True
      SDL_SCANCODE_LEFT -> do
        idx <- atomicModifyIORef' swapchainCompositionIndexRef $ \i ->
          let newIdx = (i - 1 + length swapchainCompositions) `mod` length swapchainCompositions
           in (newIdx, newIdx)
        changeSwapchainComposition context (swapchainCompositions !! idx) (swapchainCompositionNames !! idx) currentSwapchainCompositionRef
        return False
      SDL_SCANCODE_RIGHT -> do
        idx <- atomicModifyIORef' swapchainCompositionIndexRef $ \i ->
          let newIdx = (i + 1) `mod` length swapchainCompositions
           in (newIdx, newIdx)
        changeSwapchainComposition context (swapchainCompositions !! idx) (swapchainCompositionNames !! idx) currentSwapchainCompositionRef
        return False
      SDL_SCANCODE_UP -> do
        idx <- atomicModifyIORef' tonemapOperatorIndexRef $ \i ->
          let newIdx = (i - 1 + length resTonemapOperators) `mod` length resTonemapOperators
           in (newIdx, newIdx)
        changeTonemapOperator (resTonemapOperators !! idx) (tonemapOperatorDisplayNames !! idx) currentTonemapOperatorRef
        return False
      SDL_SCANCODE_DOWN -> do
        idx <- atomicModifyIORef' tonemapOperatorIndexRef $ \i ->
          let newIdx = (i + 1) `mod` length resTonemapOperators
           in (newIdx, newIdx)
        changeTonemapOperator (resTonemapOperators !! idx) (tonemapOperatorDisplayNames !! idx) currentTonemapOperatorRef
        return False
      _ -> return False
  _ -> return False

-- UI Change Helpers
changeSwapchainComposition :: Context -> SDLGPUSwapchainComposition -> String -> IORef SDLGPUSwapchainComposition -> IO ()
changeSwapchainComposition Context {..} newComp compName compRef = do
  supported <- sdlWindowSupportsGPUSwapchainComposition contextDevice contextWindow newComp
  if supported
    then do
      sdlLog $ "Changing swapchain composition to " ++ compName
      success <- sdlSetGPUSwapchainParameters contextDevice contextWindow newComp SDL_GPU_PRESENTMODE_VSYNC
      when success $ writeIORef compRef newComp
      unless success $ sdlGetError >>= sdlLog . ("Failed to set swapchain params: " ++)
    else
      sdlLog $ "Swapchain composition " ++ compName ++ " unsupported."

changeTonemapOperator :: SDLGPUComputePipeline -> String -> IORef SDLGPUComputePipeline -> IO ()
changeTonemapOperator newOp opName opRef = do
  sdlLog $ "Changing tonemap operator to " ++ opName
  writeIORef opRef newOp

-- renderFrameGPU
renderFrameGPU :: Context -> AppResources -> IO ()
renderFrameGPU Context {..} AppResources {..} = do
  maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
  case maybeCmdbuf of
    Nothing -> sdlLog "Error: Failed to acquire render command buffer."
    Just cmdbuf -> do
      maybeSwapResult <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
      case maybeSwapResult of
        Nothing -> void (sdlSubmitGPUCommandBuffer cmdbuf `finally` pure ())
        Just (swapchainTexture, swapW, swapH) -> do
          let texW = swapW
          let texH = swapH
          -- 1. Tonemap Pass
          currentToneOp <- readIORef currentTonemapOperatorRef
          maybeTonePass <-
            sdlBeginGPUComputePass
              cmdbuf
              [SDLGPUStorageTextureReadWriteBinding resToneMapTexture 0 0 True] -- Write to ToneMapTexture, cycle
              [] -- No storage buffers
          case maybeTonePass of
            Nothing -> sdlLog "!!! Failed to begin Tonemap compute pass."
            Just tonePass -> do
              sdlBindGPUComputePipeline tonePass currentToneOp
              -- Bind HDRTexture as read-only for the tonemapper
              -- Assuming slot 0 for read-only storage textures in tonemapper shader
              sdlBindGPUComputeStorageTextures tonePass 0 [resHDRTexture]
              sdlDispatchGPUCompute tonePass (texW `div` 8) (texH `div` 8) 1
              sdlEndGPUComputePass tonePass

          -- 2. Transfer to Target Color Space if Necessary
          currentSwapComp <- readIORef currentSwapchainCompositionRef
          let blitSourceTexture =
                if currentSwapComp == SDL_GPU_SWAPCHAINCOMPOSITION_SDR
                  || currentSwapComp == SDL_GPU_SWAPCHAINCOMPOSITION_HDR10_ST2084
                  then do
                    -- This block needs to be IO (Maybe SDLGPUTexture)
                    mp <-
                      sdlBeginGPUComputePass
                        cmdbuf
                        [SDLGPUStorageTextureReadWriteBinding resTransferTexture 0 0 True]
                        []
                    case mp of
                      Nothing -> sdlLog "!!! Failed to begin Color Space transform pass." >> return resToneMapTexture -- Fallback
                      Just postPass -> do
                        let pipelineToUse =
                              if currentSwapComp == SDL_GPU_SWAPCHAINCOMPOSITION_SDR
                                then resLinearToSRGBPipeline
                                else resLinearToST2084Pipeline
                        sdlBindGPUComputePipeline postPass pipelineToUse
                        -- Bind ToneMapTexture as read-only for this pass
                        sdlBindGPUComputeStorageTextures postPass 0 [resToneMapTexture]
                        sdlDispatchGPUCompute postPass (texW `div` 8) (texH `div` 8) 1
                        sdlEndGPUComputePass postPass
                        return resTransferTexture -- Success, blit from TransferTexture
                  else return resToneMapTexture -- No transform needed, blit from ToneMapTexture
          finalBlitSource <- blitSourceTexture -- Execute the IO action

          -- 3. Blit to Swapchain
          let srcBlitRegion = defaultBlitRegion finalBlitSource (fromIntegral texW) (fromIntegral texH)
          let dstBlitRegion =
                (defaultBlitRegion swapchainTexture (fromIntegral swapW) (fromIntegral swapH))
                  { gpuBlitRegW = swapW,
                    gpuBlitRegH = swapH
                  }

          let blitInfo =
                SDLGPUBlitInfo
                  { gpuBlitInfoSource = srcBlitRegion,
                    gpuBlitInfoDestination = dstBlitRegion,
                    gpuBlitInfoLoadOp = SDL_GPU_LOADOP_DONT_CARE,
                    gpuBlitInfoClearColor = SDLFColor 0 0 0 0,
                    gpuBlitInfoFlipMode = SDL_FLIP_NONE,
                    gpuBlitInfoFilter = SDL_GPU_FILTER_NEAREST, -- C example uses NEAREST for final blit
                    gpuBlitInfoCycle = False
                  }
          sdlBlitGPUTexture cmdbuf blitInfo

          -- Submit
          submitted <- sdlSubmitGPUCommandBuffer cmdbuf
          unless submitted $ sdlGetError >>= sdlLog . ("Submit failed: " ++)
