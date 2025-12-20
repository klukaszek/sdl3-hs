{-# LANGUAGE LambdaCase #-}
-- For defining lists directly in records
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Example     : GPUStencil
-- Description : SDL Example: Demonstrating basic stencil buffer usage
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Based on the SDL_gpu_examples/BasicStencil C example.
-- Demonstrates:
-- - Creating a dedicated depth/stencil texture.
-- - Using two pipelines with different stencil states:
--     - One pipeline ("masker") writes a reference value to the stencil buffer.
--     - Another pipeline ("maskee") only draws where the stencil buffer matches the reference value.
-- - Attaching a depth/stencil target to a render pass.
-- - Setting the stencil reference value during a render pass.
-- - Drawing different primitives from the same vertex buffer using offsets.
-- |
module Main where

-- Import common init/quit and shader loading

import Control.Exception (bracket, bracketOnError)
import Control.Monad (forM, unless, void, when)
import Data.Foldable (find, for_)
import Data.IORef
import Data.Maybe (isJust, isNothing)
import Data.Word (Word32, Word64)
import Foreign.C.Types (CFloat, CSize)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (..), sizeOf)
import GPUCommon
import SDL3
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

-- Define vertex data for both triangles
vertexData :: [PositionColorVertex]
vertexData =
  [ -- Masker Triangle (Yellow)
    PositionColorVertex (-0.5) (-0.5) 0 255 255 0 255,
    PositionColorVertex 0.5 (-0.5) 0 255 255 0 255,
    PositionColorVertex 0 0.5 0 255 255 0 255,
    -- Maskee Triangle (Colorful)
    PositionColorVertex (-1.0) (-1.0) 0 255 0 0 255, -- Red
    PositionColorVertex 1.0 (-1.0) 0 0 255 0 255, -- Green
    PositionColorVertex 0 1.0 0 0 0 255 255 -- Blue
  ]

-- 2. Define Managed Resources
data AppResources = AppResources
  { resMaskerPipeline :: SDLGPUGraphicsPipeline,
    resMaskeePipeline :: SDLGPUGraphicsPipeline,
    resVertexBuffer :: SDLGPUBuffer,
    resDepthStencilTexture :: SDLGPUTexture
  }

-- 3. Define Application State (Empty for this example)
data AppState = AppState -- No mutable state needed

main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  maybeResult <- withContext "SDL3 Haskell GPU Stencil" [] runAppGPU
  case maybeResult of
    Nothing -> sdlLog "Application initialization failed." >> exitFailure
    Just _ -> sdlLog "Application finished successfully." >> exitSuccess

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
        let state = AppState -- Create empty state
        startTime <- sdlGetPerformanceCounter
        freq <- sdlGetPerformanceFrequency
        deltaTimeRef <- newIORef 0.0
        eventLoopGPU context resources state startTime freq deltaTimeRef
  where
    -- Action to create all GPU resources
    createResources :: Context -> IO (Maybe AppResources)
    createResources Context {contextDevice = dev, contextWindow = win} = do
      sdlLog "--- Creating App Resources ---"
      -- Find suitable Depth/Stencil Format
      maybeDepthStencilFormat <- findDepthStencilFormat dev
      case maybeDepthStencilFormat of
        Nothing -> sdlLog "!!! No suitable Depth/Stencil format found!" >> return Nothing
        Just depthStencilFormat -> do
          sdlLog $ "Using Depth/Stencil Format: " ++ show depthStencilFormat

          -- Bracket ensures shaders are released even if subsequent steps fail
          bracketOnError (loadShaders dev) (releaseShaders dev) $ \case
            Nothing -> return Nothing -- Shader loading failed
            Just (vertShader, fragShader) -> do
              sdlLog "Shaders loaded successfully."

              -- Bracket ensures texture is released if pipelines/VB fail
              bracketOnError
                (createDepthStencilTexture dev win depthStencilFormat)
                (maybe (return ()) (sdlReleaseGPUTexture dev))
                $ \case
                  Nothing -> return Nothing -- DS Texture creation failed
                  Just dsTexture -> do
                    sdlLog "Depth/Stencil Texture created."

                    -- Bracket ensures pipelines are released if VB fails
                    bracketOnError
                      (createPipelines dev win vertShader fragShader depthStencilFormat)
                      (releasePipelines dev)
                      $ \case
                        Nothing -> return Nothing -- Pipeline creation failed
                        Just (maskerPipe, maskeePipe) -> do
                          sdlLog "Pipelines created successfully."

                          -- Bracket ensures VB is released on upload failure
                          bracketOnError
                            (createAndUploadVertexBuffer dev vertexData)
                            (maybe (return ()) (sdlReleaseGPUBuffer dev))
                            $ \case
                              Nothing -> return Nothing -- VB creation/upload failed
                              Just vertexBuffer -> do
                                sdlLog "Vertex Buffer created and data uploaded."
                                sdlLog "--- App Resources Creation Successful ---"
                                -- Shaders released by outer bracket
                                return $
                                  Just $
                                    AppResources
                                      { resMaskerPipeline = maskerPipe,
                                        resMaskeePipeline = maskeePipe,
                                        resVertexBuffer = vertexBuffer,
                                        resDepthStencilTexture = dsTexture
                                      }

    -- Helper: Load both shaders
    loadShaders dev = do
      mVert <- loadShader dev "PositionColor.vert" SDL_GPU_SHADERSTAGE_VERTEX defaultShaderCreateInfo
      mFrag <- loadShader dev "SolidColor.frag" SDL_GPU_SHADERSTAGE_FRAGMENT defaultShaderCreateInfo
      case (mVert, mFrag) of
        (Just vs, Just fs) -> return $ Just (vs, fs)
        _ -> sdlLog "!!! Failed to load shaders." >> return Nothing

    -- Helper: Release shaders
    releaseShaders dev maybePair = for_ maybePair $ \(vs, fs) -> do
      sdlLog "Releasing shaders..."
      sdlReleaseGPUShader dev vs
      sdlReleaseGPUShader dev fs

    -- Helper: Find supported Depth/Stencil format
    findDepthStencilFormat :: SDLGPUDevice -> IO (Maybe SDLGPUTextureFormat)
    findDepthStencilFormat dev = do
      let usage = SDL_GPU_TEXTUREUSAGE_DEPTH_STENCIL_TARGET
          texType = SDL_GPU_TEXTURETYPE_2D
          formatsToCheck =
            [ SDL_GPU_TEXTUREFORMAT_D24_UNORM_S8_UINT,
              SDL_GPU_TEXTUREFORMAT_D32_FLOAT_S8_UINT
            ]
      supported <- forM formatsToCheck $ \fmt ->
        sdlGPUTextureSupportsFormat dev fmt texType usage
      -- Find the first supported format
      return $ fst <$> find snd (zip formatsToCheck supported)

    -- Helper: Create Depth/Stencil Texture
    createDepthStencilTexture dev win fmt = do
      -- \*** USE THIS CORRECTED VERSION ***
      maybeSize <- sdlGetWindowSizeInPixels win -- Get the Maybe result
      case maybeSize of
        Nothing -> do
          -- Handle failure case
          err <- sdlGetError -- Get error since the function failed
          sdlLog $ "!!! Failed to get window size in pixels: " ++ err
          return Nothing
        Just (w, h) -> do
          -- Handle success case
          sdlLog $ printf "Window size in pixels: %d x %d" w h -- Optional log
          -- Proceed with texture creation using w and h
          let texCI =
                SDLGPUTextureCreateInfo
                  { texInfoType = SDL_GPU_TEXTURETYPE_2D,
                    texInfoWidth = fromIntegral w, -- Use retrieved width
                    texInfoHeight = fromIntegral h, -- Use retrieved height
                    texInfoLayerCountOrDepth = 1,
                    texInfoNumLevels = 1,
                    texInfoSampleCount = SDL_GPU_SAMPLECOUNT_1,
                    texInfoFormat = fmt,
                    texInfoUsage = SDL_GPU_TEXTUREUSAGE_DEPTH_STENCIL_TARGET,
                    texInfoProps = 0
                  }
          mTex <- sdlCreateGPUTexture dev texCI
          when (isNothing mTex) $ sdlGetError >>= \e -> sdlLog $ "!!! Failed to create Depth/Stencil Texture: " ++ e
          return mTex

    -- Helper: Create both pipelines
    createPipelines dev win vertShader fragShader depthStencilFormat = do
      swapchainFormat <- sdlGetGPUSwapchainTextureFormat dev win

      -- Vertex Input State (Consistent with previous examples)
      let vertexSize = fromIntegral $ sizeOf (undefined :: PositionColorVertex)
      let colorOffset = fromIntegral $ sizeOf (undefined :: CFloat) * 3
      let vertexAttributes =
            [ SDLGPUVertexAttribute 0 0 SDL_GPU_VERTEXELEMENTFORMAT_FLOAT3 0,
              SDLGPUVertexAttribute 1 0 SDL_GPU_VERTEXELEMENTFORMAT_UBYTE4_NORM colorOffset
            ]
          vertexBufferDesc =
            [ SDLGPUVertexBufferDescription 0 vertexSize SDL_GPU_VERTEXINPUTRATE_VERTEX 0
            ]
          vertexInputState = SDLGPUVertexInputState vertexBufferDesc vertexAttributes

      let colorTargetDesc = SDLGPUColorTargetDescription swapchainFormat defaultColorTargetBlendState
          targetInfo =
            SDLGPUGraphicsPipelineTargetInfo
              { colorTargets = [colorTargetDesc],
                hasDepthStencil = True,
                depthStencilFormat = depthStencilFormat
              }

          rasterizerState = defaultRasterizerState -- CullNone, Fill, CCW
          basePipelineCI =
            (defaultGraphicsPipelineCreateInfo vertShader fragShader swapchainFormat)
              { vertexInputState = vertexInputState,
                rasterizerState = rasterizerState,
                targetInfo = targetInfo,
                pipelineProps = 0
              }

      -- Masker Pipeline State
      let maskerStencilOpState =
            SDLGPUStencilOpState
              { stencilCompareOp = SDL_GPU_COMPAREOP_NEVER, -- Always "fail" compare
                stencilFailOp = SDL_GPU_STENCILOP_REPLACE, -- Replace stencil value on "failure"
                stencilPassOp = SDL_GPU_STENCILOP_KEEP,
                stencilDepthFailOp = SDL_GPU_STENCILOP_KEEP
              }
          maskerDepthStencilState =
            SDLGPUDepthStencilState
              { enableDepthTest = False,
                enableDepthWrite = False,
                depthStencilCompareOp = SDL_GPU_COMPAREOP_ALWAYS, -- Depth part unused
                enableStencilTest = True,
                frontStencilState = maskerStencilOpState,
                backStencilState = maskerStencilOpState, -- Same behavior for back faces
                stencilCompareMask = 0xFF,
                stencilWriteMask = 0xFF -- Write all bits
              }
          maskerCI = basePipelineCI {depthStencilState = maskerDepthStencilState}

      sdlLog "Creating Masker Pipeline..."
      mMaskerPipe <- sdlCreateGPUGraphicsPipeline dev maskerCI
      when (isNothing mMaskerPipe) $ sdlGetError >>= \e -> sdlLog $ "!!! Failed Masker Pipeline: " ++ e

      -- Maskee Pipeline State
      let maskeeStencilOpState =
            SDLGPUStencilOpState
              { stencilCompareOp = SDL_GPU_COMPAREOP_EQUAL, -- Pass only if stencil == reference
                stencilFailOp = SDL_GPU_STENCILOP_KEEP,
                stencilPassOp = SDL_GPU_STENCILOP_KEEP,
                stencilDepthFailOp = SDL_GPU_STENCILOP_KEEP
              }
          maskeeDepthStencilState =
            SDLGPUDepthStencilState
              { enableDepthTest = False,
                enableDepthWrite = False,
                depthStencilCompareOp = SDL_GPU_COMPAREOP_ALWAYS, -- Depth part unused
                enableStencilTest = True,
                frontStencilState = maskeeStencilOpState,
                backStencilState = maskeeStencilOpState {stencilCompareOp = SDL_GPU_COMPAREOP_NEVER}, -- Don't draw back faces (matches C)
                stencilCompareMask = 0xFF, -- Compare all bits
                stencilWriteMask = 0x00 -- Do NOT write to stencil buffer
              }
          maskeeCI = basePipelineCI {depthStencilState = maskeeDepthStencilState}

      sdlLog "Creating Maskee Pipeline..."
      mMaskeePipe <- sdlCreateGPUGraphicsPipeline dev maskeeCI
      when (isNothing mMaskeePipe) $ sdlGetError >>= \e -> sdlLog $ "!!! Failed Maskee Pipeline: " ++ e

      case (mMaskerPipe, mMaskeePipe) of
        (Just p1, Just p2) -> return $ Just (p1, p2)
        (p1, p2) -> do
          -- Cleanup partially created pipelines
          sdlLog "Pipeline creation failed, cleaning up..."
          for_ p1 (sdlReleaseGPUGraphicsPipeline dev)
          for_ p2 (sdlReleaseGPUGraphicsPipeline dev)
          return Nothing

    -- Helper: Release pipelines
    releasePipelines dev maybePair = for_ maybePair $ \(p1, p2) -> do
      sdlLog "Releasing pipelines..."
      sdlReleaseGPUGraphicsPipeline dev p1
      sdlReleaseGPUGraphicsPipeline dev p2

    -- Helper: Create and Upload Vertex Buffer (using helpers from previous example)
    createAndUploadVertexBuffer :: SDLGPUDevice -> [PositionColorVertex] -> IO (Maybe SDLGPUBuffer)
    createAndUploadVertexBuffer dev dataList = do
      sdlLog "--- Beginning Vertex Buffer Creation and Upload ---"
      if null dataList
        then sdlLog "Error: Vertex data list is empty." >> return Nothing
        else do
          (_, _, totalSizeW32) <- calculateVertexDataSize dataList
          bracketOnError
            (sdlCreateGPUBuffer dev (SDLGPUBufferCreateInfo SDL_GPU_BUFFERUSAGE_VERTEX totalSizeW32 0))
            (maybe (return ()) (sdlReleaseGPUBuffer dev))
            $ \case
              Nothing -> sdlLog "!!! Vertex Buffer creation failed." >> return Nothing
              Just vertexBuffer -> do
                uploadSuccess <- uploadViaTransferBuffer dev vertexBuffer dataList totalSizeW32
                if uploadSuccess
                  then do
                    sdlLog "--- Vertex Buffer Creation and Upload Successful ---"
                    return $ Just vertexBuffer
                  else do
                    sdlLog "!!! Vertex Buffer Upload Failed."
                    -- Cleanup handled by bracketOnError
                    return Nothing

    -- Action to release resources
    releaseResources :: Context -> Maybe AppResources -> IO ()
    releaseResources _ Nothing = return () -- Nothing created
    releaseResources Context {..} (Just AppResources {..}) = do
      sdlLog "--- Releasing App Resources ---"
      sdlLog $ "Releasing Masker Pipeline: " ++ show resMaskerPipeline
      sdlReleaseGPUGraphicsPipeline contextDevice resMaskerPipeline
      sdlLog $ "Releasing Maskee Pipeline: " ++ show resMaskeePipeline
      sdlReleaseGPUGraphicsPipeline contextDevice resMaskeePipeline
      sdlLog $ "Releasing Depth/Stencil Texture: " ++ show resDepthStencilTexture
      sdlReleaseGPUTexture contextDevice resDepthStencilTexture
      sdlLog $ "Releasing Vertex Buffer: " ++ show resVertexBuffer
      sdlReleaseGPUBuffer contextDevice resVertexBuffer
      sdlLog "--- App Resources Released ---"

-- | Helper to calculate sizes (copied from previous)
calculateVertexDataSize :: [PositionColorVertex] -> IO (Int, CSize, Word32)
calculateVertexDataSize dataList = do
  let vertexSize = sizeOf (head dataList)
  let numVertices = length dataList
  let totalBytes = numVertices * vertexSize
  let totalCSize = fromIntegral totalBytes :: CSize
  let totalSizeWord32 = fromIntegral totalBytes :: Word32
  sdlLog $
    printf
      "Vertex Info - SizeOf: %d, Count: %d, Total Bytes (CSize): %d, Total Bytes (Word32): %d"
      vertexSize
      numVertices
      (fromIntegral totalCSize :: Int)
      totalSizeWord32
  when (totalBytes == 0) $ sdlLog "!!! WARNING: Vertex data is empty!"
  return (vertexSize, totalCSize, totalSizeWord32)

-- | Reusable upload helper (copied from previous)
uploadViaTransferBuffer :: SDLGPUDevice -> SDLGPUBuffer -> [PositionColorVertex] -> Word32 -> IO Bool
uploadViaTransferBuffer dev vertexBuffer dataList transferSize = do
  bracket (createTransferBuffer dev transferSize SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "buffer") (cleanupTransferBuffer dev) $ \case
    Nothing -> return False
    Just transferBuffer -> do
      mapCopySuccess <- mapAndCopyData dev transferBuffer dataList
      if mapCopySuccess
        then uploadDataCommandBuffer dev vertexBuffer transferBuffer transferSize
        else return False

mapAndCopyData :: SDLGPUDevice -> SDLGPUTransferBuffer -> [PositionColorVertex] -> IO Bool
mapAndCopyData dev tb dataList = do
  bracket
    (sdlMapGPUTransferBuffer dev tb False)
    (\mPtr -> when (isJust mPtr) $ sdlUnmapGPUTransferBuffer dev tb)
    $ \case
      Nothing -> sdlLog "!!! Map failed" >> return False
      Just mappedPtr -> do
        sdlLog $ "Mapping successful. Ptr: " ++ show mappedPtr ++ ". Copying data..."
        pokeArray (castPtr mappedPtr) dataList
        sdlLog "Data copied."
        return True

uploadDataCommandBuffer :: SDLGPUDevice -> SDLGPUBuffer -> SDLGPUTransferBuffer -> Word32 -> IO Bool
uploadDataCommandBuffer dev vertexBuffer tb totalSizeW32 = do
  bracket (sdlAcquireGPUCommandBuffer dev) cleanupCommandBuffer $ \case
    Nothing -> sdlLog "!!! Acquire upload CB failed" >> return False
    Just cmdBuf -> do
      -- Inner bracket: Executes the copy pass, returns IO Bool indicating success/failure of recording
      copyCommandsOk <- bracket (sdlBeginGPUCopyPass cmdBuf) cleanupCopyPass $ \case
        Nothing -> do
          sdlLog "!!! Begin copy pass failed"
          return False -- Indicate recording failure
        Just copyPass -> do
          let srcLoc = SDLGPUTransferBufferLocation tb 0
              dstReg = SDLGPUBufferRegion vertexBuffer 0 totalSizeW32
          sdlLog $ "Recording upload (Size: " ++ show totalSizeW32 ++ ")"
          sdlUploadToGPUBuffer copyPass srcLoc dstReg False
          sdlLog "Upload recorded."
          return True -- Indicate recording success
          -- End of inner bracket (sdlEndGPUCopyPass is called by cleanupCopyPass)

      -- Now, check the result of the recording step (copyCommandsOk :: Bool)
      if copyCommandsOk
        then do
          sdlLog "Submitting upload command buffer..."
          submitted <- sdlSubmitGPUCommandBuffer cmdBuf
          unless submitted $ sdlGetError >>= \e -> sdlLog $ "!!! Upload submission failed: " ++ e
          return submitted -- Return submission success/failure
        else do
          sdlLog "Copy pass failed, skipping submission."
          return False -- Return overall failure

-- | Main event loop (Simple version for non-interactive example)
eventLoopGPU :: Context -> AppResources -> AppState -> Word64 -> Word64 -> IORef Double -> IO ()
eventLoopGPU context resources state _ _ _ = do
  shouldQuitRef <- newIORef False
  let loop = do
        sdlPumpEvents
        processEventsGPU context shouldQuitRef state -- Check for quit event
        shouldQuit <- readIORef shouldQuitRef
        unless shouldQuit $ do
          renderFrameGPU context resources state True -- Use non-blocking acquire
          loop

  loop -- Start the loop

-- | Process events (Just check for quit)
processEventsGPU :: Context -> IORef Bool -> AppState -> IO ()
processEventsGPU context shouldQuitRef state = do
  maybeEvent <- sdlPollEvent
  case maybeEvent of
    Nothing -> return () -- No more events
    Just event -> do
      quit <- handleEventGPU context event state
      when quit $ writeIORef shouldQuitRef True
      processEventsGPU context shouldQuitRef state -- Process next

-- | Handle a single event (Just Quit)
handleEventGPU :: Context -> SDLEvent -> AppState -> IO Bool
handleEventGPU _ event _ = case event of
  SDLEventQuit _ -> sdlLog "Quit event received." >> return True
  SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ SDL_SCANCODE_Q _ _ _ True _) -> return True -- Quit on Q down
  _ -> return False

-- | Render a frame
renderFrameGPU :: Context -> AppResources -> AppState -> Bool -> IO ()
renderFrameGPU Context {..} AppResources {..} _ useNonBlockingAcquire = do
  -- 1. Acquire Command Buffer
  maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
  case maybeCmdbuf of
    Nothing -> sdlGetError >>= \err -> sdlLog $ "Error: Failed to acquire render command buffer: " ++ err
    Just cmdbuf -> do
      -- 2. Acquire Swapchain Texture
      maybeSwapchain <-
        if useNonBlockingAcquire
          then sdlAcquireGPUSwapchainTexture cmdbuf contextWindow
          else sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
      case maybeSwapchain of
        Nothing -> do
          -- Avoid spamming logs in non-blocking mode if swapchain isn't ready
          unless useNonBlockingAcquire $ sdlLog "!!! Error: Failed to acquire swapchain texture (blocking)"
          -- Must submit *something* or we might deadlock waiting for the buffer
          void $ sdlSubmitGPUCommandBuffer cmdbuf
        Just (swapchainTexture, _, _) -> do
          -- Don't need width/height here
          -- Define Color Target
          let clearColor = SDLFColor 0.0 0.0 0.0 1.0
          let colorTargetInfo =
                SDLGPUColorTargetInfo
                  { texture = swapchainTexture,
                    mipLevel = 0,
                    layerOrDepthPlane = 0,
                    clearColor = clearColor,
                    loadOp = SDL_GPU_LOADOP_CLEAR,
                    storeOp = SDL_GPU_STOREOP_STORE,
                    resolveTexture = Nothing,
                    resolveMipLevel = 0,
                    resolveLayer = 0,
                    targetCycle = False,
                    targetCycleResolve = False
                  }

          -- Define Depth/Stencil Target
          let depthStencilTargetInfo =
                SDLGPUDepthStencilTargetInfo
                  { depthStencilTexture = resDepthStencilTexture,
                    depthStencilClearDepth = 1.0, -- Clear depth to farthest
                    depthStencilClearStencil = 0, -- Clear stencil to 0
                    depthStencilLoadOp = SDL_GPU_LOADOP_CLEAR,
                    depthStencilStoreOp = SDL_GPU_STOREOP_DONT_CARE, -- Don't need depth after frame
                    depthStencilStencilLoadOp = SDL_GPU_LOADOP_CLEAR,
                    depthStencilStencilStoreOp = SDL_GPU_STOREOP_DONT_CARE, -- Don't need stencil after frame
                    depthStencilCycle = True -- IMPORTANT: Cycle this texture for reuse next frame
                  }

          -- 3. Begin Render Pass (with both color and depth/stencil targets)
          bracket
            (sdlBeginGPURenderPass cmdbuf [colorTargetInfo] (Just depthStencilTargetInfo))
            (\mrp -> for_ mrp sdlEndGPURenderPass)
            $ \case
              Nothing -> sdlGetError >>= \err -> sdlLog $ "!!! Error: Failed to begin render pass: " ++ err
              Just renderPass -> do
                -- Bind the single vertex buffer (used by both draw calls)
                let vbBinding = SDLGPUBufferBinding resVertexBuffer 0
                sdlBindGPUVertexBuffers renderPass 0 [vbBinding]

                -- Draw Masker (Yellow Triangle, writes stencil=1)
                sdlSetGPUStencilReference renderPass 1 -- Set value to write/compare
                sdlBindGPUGraphicsPipeline renderPass resMaskerPipeline
                sdlDrawGPUPrimitives renderPass 3 1 0 0 -- Draw vertices 0, 1, 2

                -- Draw Maskee (Colorful Triangle, reads stencil==1)
                sdlSetGPUStencilReference renderPass 0 -- Set value to compare against (pipeline uses EQUAL against buffer value which is 1)
                sdlBindGPUGraphicsPipeline renderPass resMaskeePipeline
                sdlDrawGPUPrimitives renderPass 3 1 3 0 -- Draw vertices 3, 4, 5

          -- 4. Submit Command Buffer
          submitted <- sdlSubmitGPUCommandBuffer cmdbuf
          unless submitted $ sdlGetError >>= \err -> sdlLog $ "!!! Error: Failed to submit render command buffer: " ++ err
