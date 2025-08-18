{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Example     : GPUCustomSampling
-- Description : Demonstrates manual texture sampling in a fragment shader using a storage texture.
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Based on SDL_gpu_examples/CustomSampling.c example.
-- Demonstrates:
-- - Loading an image into a GPU texture with `GRAPHICS_STORAGE_READ` usage.
-- - A fragment shader (`CustomSampling.frag`) that performs custom texture filtering (e.g., nearest neighbor, bilinear)
--   by reading texels directly from a read-only storage texture (`texture2D` or `image2D` in GLSL, bound as storage).
-- - Passing a "sampler mode" uniform to the fragment shader to switch between different custom sampling techniques.
-- - No traditional `SDL_GPUSampler` object is used for the primary texture lookup in the shader.
-- |
module Main where

import Control.Exception (bracket, bracketOnError, finally)
import Control.Monad (unless, void, when)
import Data.IORef
import Data.Maybe (fromJust, isJust)
import Data.Word (Word16)
import Foreign.C.Types (CFloat)
import Foreign.Storable (peek, sizeOf)
import GPUCommon
import SDL
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

-- Vertex data (screen-filling quad)
vertexData :: [PositionTextureVertex]
vertexData =
  [ PositionTextureVertex (-1) 1 0 0 0, -- Top-left
    PositionTextureVertex 1 1 0 1 0, -- Top-right
    PositionTextureVertex 1 (-1) 0 1 1, -- Bottom-right
    PositionTextureVertex (-1) (-1) 0 0 1 -- Bottom-left
  ]

-- Index data
indexData :: [Word16]
indexData = [0, 1, 2, 0, 2, 3]

-- AppResources
data AppResources = AppResources
  { resPipeline :: SDLGPUGraphicsPipeline,
    resVertexBuffer :: SDLGPUBuffer,
    resIndexBuffer :: SDLGPUBuffer,
    resTexture :: SDLGPUTexture,
    -- No sampler object needed as we're doing custom sampling in shader
    resSamplerModeRef :: IORef Int -- To store 0 or 1 for sampler mode
  }

-- main
main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  maybeResult <- withContext "SDL3 Haskell GPU Custom Sampling" [] runAppGPU
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
        sdlLog "Press Left/Right to switch sampler modes"
        initialMode <- readIORef (resSamplerModeRef resources)
        sdlLog $ "Setting initial sampler mode to: " ++ show initialMode
        eventLoopGPU context resources

-- createResources
createResources :: Context -> IO (Maybe AppResources)
createResources Context {..} = do
  sdlLog "--- Beginning Resource Creation ---"

  -- 1. Load Shaders
  sdlLog "Loading graphics shaders..."
  -- Vertex shader: TexturedQuad.vert (no uniforms, no samplers)
  let vertShaderInfo = defaultShaderCreateInfo
  maybeVertShader <- loadShader contextDevice "TexturedQuad.vert" SDL_GPU_SHADERSTAGE_VERTEX vertShaderInfo

  -- Fragment shader: CustomSampling.frag
  -- Expects 1 read-only storage texture (slot 0), 1 uniform buffer (slot 0 for SamplerMode)
  let fragShaderInfo =
        defaultShaderCreateInfo
          { shaderNumStorageTextures = 1,
            shaderNumUniformBuffers = 1
          }
  maybeFragShader <- loadShader contextDevice "CustomSampling.frag" SDL_GPU_SHADERSTAGE_FRAGMENT fragShaderInfo

  case (maybeVertShader, maybeFragShader) of
    (Just vertShader, Just fragShader) -> do
      sdlLog "Shaders loaded successfully."

      -- 2. Load Image
      maybeSurf <-
        bracketOnError
          (loadImage ("Content" </> "Images" </> "ravioli.bmp"))
          (\mSurf -> case mSurf of Just surf -> sdlDestroySurface surf; _ -> pure ())
          pure -- Pass the Maybe Surface through
      case maybeSurf of
        Nothing -> do
          sdlLog "!!! Failed to load image."
          sdlReleaseGPUShader contextDevice vertShader
          sdlReleaseGPUShader contextDevice fragShader
          return Nothing
        Just surfacePtr ->
          -- Ensure surfacePtr is managed if subsequent steps fail
          bracketOnError (pure surfacePtr) (\_ -> sdlDestroySurface surfacePtr) $ \surf -> do
            surfaceData <- peek surf
            let imgW = surfaceW surfaceData
            let imgH = surfaceH surfaceData

            -- 3. Create Graphics Pipeline
            maybePipeline <- createDrawPipeline contextDevice contextWindow vertShader fragShader

            -- 4. Create Texture (Usage: GRAPHICS_STORAGE_READ)
            let textureCI =
                  (defaultTextureCreateInfo imgW imgH)
                    { texInfoUsage = SDL_GPU_TEXTUREUSAGE_GRAPHICS_STORAGE_READ
                    }
            maybeTexture <- sdlCreateGPUTexture contextDevice textureCI

            -- 5. Create Vertex and Index Buffers
            (_, _, vbSize) <- calculateBufferDataSize vertexData "Vertex"
            maybeVB <- createGPUBuffer contextDevice SDL_GPU_BUFFERUSAGE_VERTEX vbSize "QuadVB"
            (_, _, ibSize) <- calculateBufferDataSize indexData "Index"
            maybeIB <- createGPUBuffer contextDevice SDL_GPU_BUFFERUSAGE_INDEX ibSize "QuadIB"

            samplerModeRef_ <- newIORef (0 :: Int) -- Initialize SamplerMode
            let acquireUploadResources = do
                  mCmdBuf <- sdlAcquireGPUCommandBuffer contextDevice
                  mBufTransfer <- createTransferBuffer contextDevice (vbSize + ibSize) SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "VBIBTransfer"
                  mTexTransfer <- createTransferBuffer contextDevice (fromIntegral $ imgW * imgH * 4) SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "TexTransfer"
                  return (mCmdBuf, mBufTransfer, mTexTransfer)

            uploadSuccess <- bracket
              acquireUploadResources
              ( \(mCmd, mBufTrans, mTexTrans) -> do
                  -- cleanupCommandBuffer now needs to handle Maybe SDLGPUCommandBuffer
                  -- or you can case match here too
                  case mCmd of
                    Just cmd -> cleanupCommandBuffer (Just cmd) -- If cleanupCommandBuffer takes Maybe
                    Nothing -> pure ()
                  cleanupTransferBuffer contextDevice mBufTrans
                  cleanupTransferBuffer contextDevice mTexTrans
              )
              $ \(acquiredCmdBuf, acquiredBufTransfer, acquiredTexTransfer) ->
                case (acquiredCmdBuf, acquiredBufTransfer, acquiredTexTransfer, maybeTexture, maybeVB, maybeIB) of -- Added maybeVB, maybeIB checks
                  (Just cmdBuf, Just bufTransfer, Just texTransfer, Just texToUploadTo, Just vb, Just ib) -> do
                    -- Map and copy buffer data
                    -- Note: vbSize is the size of vertexData, not the offset for indexData
                    let vertexDataSizeBytes = vbSize -- From calculateBufferDataSize
                    let indexDataSizeBytes = ibSize -- From calculateBufferDataSize
                    -- The offset for indexData within bufTransfer should be vertexDataSizeBytes
                    mapOk1 <- mapAndCopyBufferData contextDevice bufTransfer vertexData indexData 0 vertexDataSizeBytes

                    unless mapOk1 $ sdlLog "Vertex/Index map or copy failed"

                    -- Map and copy texture data
                    mapOk2 <- if mapOk1 then mapAndCopyTextureData contextDevice texTransfer surf imgW imgH 4 else return False
                    unless mapOk2 $ sdlLog "Texture map or copy failed"

                    if mapOk1 && mapOk2
                      then do
                        mc <- sdlBeginGPUCopyPass cmdBuf
                        case mc of
                          Nothing -> sdlLog "Failed to begin copy pass for uploads" >> return False
                          Just cp -> do
                            -- Upload VB
                            let vbSrc = SDLGPUTransferBufferLocation bufTransfer 0
                            let vbDst = SDLGPUBufferRegion vb 0 vertexDataSizeBytes
                            sdlUploadToGPUBuffer cp vbSrc vbDst False
                            -- Upload IB
                            -- Offset for index data is after vertex data
                            let ibSrc = SDLGPUTransferBufferLocation bufTransfer vertexDataSizeBytes
                            let ibDst = SDLGPUBufferRegion ib 0 indexDataSizeBytes
                            sdlUploadToGPUBuffer cp ibSrc ibDst False
                            -- Upload Texture
                            let texSrcInfo = SDLGPUTextureTransferInfo texTransfer 0 (fromIntegral imgW) (fromIntegral imgH)
                            let texDstRegion = defaultTextureRegion texToUploadTo imgW imgH
                            sdlUploadToGPUTexture cp texSrcInfo texDstRegion False
                            sdlEndGPUCopyPass cp
                            sdlSubmitGPUCommandBuffer cmdBuf >>= \s ->
                              if s
                                then sdlWaitForGPUIdle contextDevice >> return True
                                else sdlGetError >>= \e -> sdlLog ("Upload Submit failed: " ++ e) >> return False
                      else
                        return False -- map or copy failed
                  _ -> sdlLog "Failed to acquire one or more resources (cmdBuf, transferBufs, texture, vb, ib) for upload" >> return False

            -- Release shaders now that pipeline is created
            sdlReleaseGPUShader contextDevice vertShader
            sdlReleaseGPUShader contextDevice fragShader

            if uploadSuccess && isJust maybePipeline && isJust maybeTexture && isJust maybeVB && isJust maybeIB
              then do
                sdlLog "--- Resource Creation Successful ---"
                return $
                  Just
                    AppResources
                      { resPipeline = fromJust maybePipeline,
                        resVertexBuffer = fromJust maybeVB,
                        resIndexBuffer = fromJust maybeIB,
                        resTexture = fromJust maybeTexture,
                        resSamplerModeRef = samplerModeRef_
                      }
              else do
                sdlLog "!!! Failed to create one or more critical resources or upload failed."
                -- Manually clean up what might have been created
                maybe (pure ()) (sdlReleaseGPUGraphicsPipeline contextDevice) maybePipeline
                maybe (pure ()) (sdlReleaseGPUTexture contextDevice) maybeTexture
                maybe (pure ()) (sdlReleaseGPUBuffer contextDevice) maybeVB
                maybe (pure ()) (sdlReleaseGPUBuffer contextDevice) maybeIB
                return Nothing
    _ -> do
      -- Shader loading failed
      sdlLog "!!! Failed to load shaders."
      maybe (return ()) (sdlReleaseGPUShader contextDevice) maybeVertShader
      maybe (return ()) (sdlReleaseGPUShader contextDevice) maybeFragShader
      return Nothing

-- createDrawPipeline (Standard textured quad pipeline)
createDrawPipeline :: SDLGPUDevice -> SDLWindow -> SDLGPUShader -> SDLGPUShader -> IO (Maybe SDLGPUGraphicsPipeline)
createDrawPipeline dev win vertShader fragShader = do
  swapchainFormat <- sdlGetGPUSwapchainTextureFormat dev win
  let vertexSize = sizeOf (undefined :: PositionTextureVertex)
  let texCoordOffset = sizeOf (undefined :: CFloat) * 3
  let vertexAttributes =
        [ SDLGPUVertexAttribute 0 0 SDL_GPU_VERTEXELEMENTFORMAT_FLOAT3 0,
          SDLGPUVertexAttribute 1 0 SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2 (fromIntegral texCoordOffset)
        ]
      vertexBufferDesc = [SDLGPUVertexBufferDescription 0 (fromIntegral vertexSize) SDL_GPU_VERTEXINPUTRATE_VERTEX 0]
      vertexInputState = SDLGPUVertexInputState vertexBufferDesc vertexAttributes
  let colorTargetDesc = defaultColorTargetDescription {targetFormat = swapchainFormat}
      targetInfo = SDLGPUGraphicsPipelineTargetInfo [colorTargetDesc] SDL_GPU_TEXTUREFORMAT_INVALID False
      pipelineCI =
        (defaultGraphicsPipelineCreateInfo vertShader fragShader swapchainFormat)
          { vertexInputState = vertexInputState,
            targetInfo = targetInfo
          }
  sdlCreateGPUGraphicsPipeline dev pipelineCI

-- releaseResources
releaseResources :: Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources Context {..} (Just AppResources {..}) = do
  sdlLog "--> Releasing AppResources..."
  sdlReleaseGPUGraphicsPipeline contextDevice resPipeline
  sdlReleaseGPUBuffer contextDevice resVertexBuffer
  sdlReleaseGPUBuffer contextDevice resIndexBuffer
  sdlReleaseGPUTexture contextDevice resTexture
  -- No sampler object to release
  sdlLog "<-- AppResources Released."

-- eventLoopGPU
eventLoopGPU :: Context -> AppResources -> IO ()
eventLoopGPU context resources = do
  sdlPumpEvents
  shouldQuitRef <- newIORef False
  processEventsGPU context resources shouldQuitRef -- Pass resources for UI updates
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
      quit <- handleEventGPU context resources event -- Pass context and resources
      when quit $ writeIORef quitRef True
      processEventsGPU context resources quitRef

-- handleEventGPU for UI interaction
handleEventGPU :: Context -> AppResources -> SDLEvent -> IO Bool
handleEventGPU _ AppResources {..} event = case event of
  SDLEventQuit _ -> sdlLog "Quit event received." >> return True
  SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ scancode _ _ _ down _) | down -> do
    case scancode of
      SDL_SCANCODE_Q -> return True
      SDL_SCANCODE_LEFT -> modifyIORef' resSamplerModeRef (\m -> if m == 0 then 1 else 0) >> readIORef resSamplerModeRef >>= \m -> sdlLog ("Setting sampler mode to: " ++ show m) >> return False
      SDL_SCANCODE_RIGHT -> modifyIORef' resSamplerModeRef (\m -> if m == 0 then 1 else 0) >> readIORef resSamplerModeRef >>= \m -> sdlLog ("Setting sampler mode to: " ++ show m) >> return False
      _ -> return False
  _ -> return False

-- renderFrameGPU
renderFrameGPU :: Context -> AppResources -> IO ()
renderFrameGPU Context {..} AppResources {..} = do
  maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
  case maybeCmdbuf of
    Nothing -> sdlLog "Error: Failed to acquire render command buffer."
    Just cmdbuf -> do
      maybeSwapchain <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
      case maybeSwapchain of
        Nothing -> void (sdlSubmitGPUCommandBuffer cmdbuf `finally` pure ())
        Just (swapchainTexture, _, _) -> do
          let clearColor = SDLFColor 0.0 0.0 0.0 1.0
          let colorTargetInfo =
                defaultColorTargetInfo
                  { texture = swapchainTexture,
                    loadOp = SDL_GPU_LOADOP_CLEAR,
                    storeOp = SDL_GPU_STOREOP_STORE,
                    clearColor = clearColor
                  }
          bracket
            (sdlBeginGPURenderPass cmdbuf [colorTargetInfo] Nothing)
            cleanupMaybeRenderPass
            $ \case
              Nothing -> sdlLog "Error: Failed to begin render pass."
              Just renderPass -> do
                sdlBindGPUGraphicsPipeline renderPass resPipeline
                let vbBinding = SDLGPUBufferBinding resVertexBuffer 0
                sdlBindGPUVertexBuffers renderPass 0 [vbBinding]
                let ibBinding = SDLGPUBufferBinding resIndexBuffer 0
                sdlBindGPUIndexBuffer renderPass ibBinding SDL_GPU_INDEXELEMENTSIZE_16BIT

                -- Bind texture as a read-only storage texture
                sdlBindGPUFragmentStorageTextures renderPass 0 [resTexture]

                -- Push SamplerMode uniform
                samplerMode <- readIORef resSamplerModeRef
                sdlPushGPUFragmentUniformData cmdbuf 0 samplerMode

                sdlDrawGPUIndexedPrimitives renderPass 6 1 0 0 0

          submitted <- sdlSubmitGPUCommandBuffer cmdbuf
          unless submitted $ sdlGetError >>= sdlLog . ("Submit failed: " ++)
