{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE FlexibleInstances    #-} -- To define Storable for our custom type

{-|
Example     : GPUVertexBuffer
Description : SDL Example: Drawing a triangle using a vertex buffer
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Based on the SDL_gpu_examples/VertexBuffer C example.
Demonstrates:
- Defining a vertex structure (PositionColorVertex).
- Creating a GPU vertex buffer.
- Uploading vertex data using a transfer buffer.
- Defining vertex input state in the pipeline.
- Binding the vertex buffer and drawing.
|-}

module Main where

import SDL
import GPUCommon        -- Import common init/quit and shader loading

import Control.Monad (unless, when, void)
import Control.Exception (bracket, bracketOnError, onException, finally) -- For resource management
import System.Exit (exitFailure, exitSuccess)
import Foreign.Ptr (Ptr, nullPtr, castPtr, plusPtr)
import Foreign.Storable (Storable(..), peekByteOff, pokeByteOff, sizeOf, poke)
import Foreign.C.Types (CFloat, CSize) -- Import CSize
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (pokeArray)
import Data.IORef       -- Not needed for state toggles anymore
import Data.Word (Word32, Word64, Word8)
import Text.Printf (printf)
import Data.Maybe (isJust, fromJust, fromMaybe, isNothing)
import Data.Bits ((.|.)) -- For color component flags if needed

-- Define vertex data
vertexData :: [PositionColorVertex]
vertexData =
    [ PositionColorVertex (-1) (-1) 0   255   0   0 255 -- Red
    , PositionColorVertex   1  (-1) 0     0 255   0 255 -- Green
    , PositionColorVertex   0    1  0     0   0 255 255 -- Blue
    ]

-- 2. Define Managed Resources (Pipeline and Vertex Buffer)
data AppResources = AppResources
    { resPipeline    :: SDLGPUGraphicsPipeline
    , resVertexBuffer :: SDLGPUBuffer
    }

main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  -- Use withContext for base SDL/GPU/Window setup
  maybeResult <- withContext "SDL3 Haskell GPU Vertex Buffer" [sdlWindowResizable] runAppGPU
  case maybeResult of
      Nothing -> do
          sdlLog "Application initialization failed (commonInit)."
          exitFailure
      Just _ -> do
          sdlLog "Application finished successfully."
          exitSuccess

-- | Application logic, receives the base Context
runAppGPU :: Context -> IO ()
runAppGPU context@Context{..} = do
    sdlLog "Base context initialized."

    -- Bracket pattern to manage app resources (pipeline and vertex buffer)
    bracket (createResources context)
            (\mRes -> do -- Add logging to the release action
                sdlLog "--> Entering AppResources release bracket..."
                releaseResources context mRes
                sdlLog "<-- Exiting AppResources release bracket."
            ) $ \case
            Nothing -> sdlLog "Failed to create resources. Exiting."
            Just resources -> do
                sdlLog "Resources created successfully."
                -- Start event loop with initial time, passing context and resources
                startTime <- sdlGetPerformanceCounter
                freq <- sdlGetPerformanceFrequency
                deltaTimeRef <- newIORef 0.0 -- Keep for event timing display if desired
                eventLoopGPU context resources startTime freq deltaTimeRef
                -- Resource cleanup is handled by the bracket's release action
                -- Base context cleanup is handled by withContext

  where
    -- Action to create all GPU resources (pipeline, vertex buffer, upload data)
    createResources :: Context -> IO (Maybe AppResources)
    createResources ctx@Context{ contextDevice = dev, contextWindow = win } = do
        -- Load Shaders
        sdlLog "Loading shaders..."
        maybeVertShader <- loadShader dev "PositionColor.vert" SDL_GPU_SHADERSTAGE_VERTEX defaultShaderCreateInfo
        maybeFragShader <- loadShader dev "SolidColor.frag" SDL_GPU_SHADERSTAGE_FRAGMENT defaultShaderCreateInfo

        -- Create Pipeline (conditional on shaders loading)
        maybePipeline <- case (maybeVertShader, maybeFragShader) of
            (Just vertShader, Just fragShader) -> do
                sdlLog "Shaders loaded successfully. Creating pipeline..."
                pipeline <- createPipeline dev win vertShader fragShader
                -- Release shaders now that pipeline is created (or creation failed)
                sdlReleaseGPUShader dev vertShader
                sdlReleaseGPUShader dev fragShader
                sdlLog "Shaders released."
                return pipeline -- Maybe SDLGPUGraphicsPipeline
            _ -> do
                sdlLog "Failed to load one or both shaders."
                -- Clean up potentially loaded shader if one succeeded
                maybe (return ()) (sdlReleaseGPUShader dev) maybeVertShader
                maybe (return ()) (sdlReleaseGPUShader dev) maybeFragShader
                return Nothing

        -- Create Vertex Buffer and Upload Data (conditional on pipeline creation)
        case maybePipeline of
            Nothing -> do
                sdlLog "Pipeline creation failed. Cannot proceed."
                return Nothing
            Just pipeline -> do
                sdlLog "Pipeline created successfully. Creating vertex buffer..."
                -- Call the meticulous upload function
                maybeVertexBuffer <- createAndUploadVertexBuffer dev vertexData
                case maybeVertexBuffer of
                    Nothing -> do
                        sdlLog "Vertex buffer creation or upload failed (detailed function)."
                        sdlReleaseGPUGraphicsPipeline dev pipeline -- Clean up created pipeline
                        return Nothing
                    Just vertexBuffer -> do
                        sdlLog "Vertex buffer reported success from detailed function."
                        return $ Just (AppResources pipeline vertexBuffer)

    -- Helper to create the graphics pipeline
    createPipeline :: SDLGPUDevice -> SDLWindow -> SDLGPUShader -> SDLGPUShader -> IO (Maybe SDLGPUGraphicsPipeline)
    createPipeline dev win vertShader fragShader = do
        swapchainFormat <- sdlGetGPUSwapchainTextureFormat dev win

        -- Define Vertex Input State (matches PositionColorVertex)
        let vertexSize = sizeOf (undefined :: PositionColorVertex)
        let colorOffset = sizeOf (undefined :: CFloat) * 3
        sdlLog $ "Pipeline Vertex Input - Stride: " ++ show vertexSize ++ ", Color Offset: " ++ show colorOffset

        let vertexAttributes =
              [ SDLGPUVertexAttribute -- Position (float3)
                { attribLocation = 0 -- Corresponds to layout(location=0) in shader
                , attribSlot = 0 -- Uses vertex buffer bound to slot 0
                , attribFormat = SDL_GPU_VERTEXELEMENTFORMAT_FLOAT3
                , attribOffset = 0 -- Starts at the beginning of the struct
                }
              , SDLGPUVertexAttribute -- Color (ubyte4 normalized)
                { attribLocation = 1 -- Corresponds to layout(location=1) in shader
                , attribSlot = 0 -- Uses vertex buffer bound to slot 0
                , attribFormat = SDL_GPU_VERTEXELEMENTFORMAT_UBYTE4_NORM
                , attribOffset = fromIntegral colorOffset -- Offset after the 3 floats
                }
              ]
            vertexBufferDesc =
              [ SDLGPUVertexBufferDescription
                { descSlot = 0 -- Binding slot 0
                , descPitch = fromIntegral vertexSize -- Stride
                , descInputRate = SDL_GPU_VERTEXINPUTRATE_VERTEX
                , descInstanceStepRate = 0
                }
              ]
            vertexInputState = SDLGPUVertexInputState
                { inputVertexBuffers = vertexBufferDesc
                , inputVertexAttribs = vertexAttributes
                }

        -- Define remaining pipeline state using defaults
        let colorTargetDesc = SDLGPUColorTargetDescription
                { targetFormat = swapchainFormat
                , targetBlendState = defaultColorTargetBlendState
                }
            targetInfo = SDLGPUGraphicsPipelineTargetInfo
                { colorTargets = [colorTargetDesc]
                , depthStencilFormat = SDL_GPU_TEXTUREFORMAT_INVALID
                , hasDepthStencil = False
                }
            pipelineCI = SDLGPUGraphicsPipelineCreateInfo
                { vertexShader = vertShader
                , fragmentShader = fragShader -- Direct field
                , vertexInputState = vertexInputState -- Use specific layout
                , primitiveType = SDL_GPU_PRIMITIVETYPE_TRIANGLELIST
                , multisampleState = defaultMultiSampleState
                , rasterizerState = defaultRasterizerState
                , targetInfo = targetInfo
                , depthStencilState = defaultDepthStencilState
                , props = 0
                }

        -- Create the pipeline
        maybePipeline <- sdlCreateGPUGraphicsPipeline dev pipelineCI
        when (isNothing maybePipeline) $ do
            err <- sdlGetError
            sdlLog $ "!!! Failed to create graphics pipeline: " ++ err
        return maybePipeline

    -- Action to release resources
    releaseResources :: Context -> Maybe AppResources -> IO ()
    releaseResources _ Nothing = return () -- Nothing to clean up
    releaseResources Context{..} (Just AppResources{..}) = do
        sdlLog "--> Releasing AppResources..."
        sdlLog $ "  --> Releasing Pipeline: " ++ show resPipeline
        sdlReleaseGPUGraphicsPipeline contextDevice resPipeline
        sdlLog $ "  <-- Pipeline Released."
        sdlLog $ "  --> Releasing Vertex Buffer: " ++ show resVertexBuffer
        sdlReleaseGPUBuffer contextDevice resVertexBuffer
        sdlLog $ "  <-- Vertex Buffer Released."
        sdlLog "<-- AppResources Released."


-- | Helper to calculate sizes and log them
calculateVertexDataSize :: [PositionColorVertex] -> IO (Int, CSize, Word32)
calculateVertexDataSize dataList = do
    let vertexSize = sizeOf (head dataList) -- Size of one vertex
    let numVertices = length dataList
    let totalBytes = numVertices * vertexSize
    let totalCSize = fromIntegral totalBytes :: CSize
    let totalSizeWord32 = fromIntegral totalBytes :: Word32
    sdlLog $ "Vertex Info - SizeOf: " ++ show vertexSize ++
             ", Count: " ++ show numVertices ++
             ", Total Bytes (CSize): " ++ show totalCSize ++
             ", Total Bytes (Word32): " ++ show totalSizeWord32
    -- Verify assumption: CFloat is 4 bytes, Word8 is 1 byte. 3*4 + 4*1 = 16.
    when (vertexSize /= 16) $
        sdlLog "!!! WARNING: Calculated vertex size is not 16 bytes!"
    when (totalBytes == 0) $
        sdlLog "!!! WARNING: Vertex data is empty!"
    return (vertexSize, totalCSize, totalSizeWord32)

-- | Meticulously rewritten vertex buffer creation and upload function
createAndUploadVertexBuffer :: SDLGPUDevice -> [PositionColorVertex] -> IO (Maybe SDLGPUBuffer)
createAndUploadVertexBuffer dev vertexData = do
    sdlLog "--- Beginning Vertex Buffer Creation and Upload ---"
    if null vertexData then do
        sdlLog "Error: Vertex data list is empty. Cannot create buffer."
        return Nothing
    else do
        (vertexSize, totalCSize, totalSizeWord32) <- calculateVertexDataSize vertexData

        -- Use bracketOnError for the entire process involving the vertex buffer
        bracketOnError
            -- Resource acquisition: Create Vertex Buffer
            (do sdlLog $ "Creating Vertex Buffer (Size: " ++ show totalCSize ++ " bytes)..."
                let vbCreateInfo = SDLGPUBufferCreateInfo
                                     { bufferUsage = SDL_GPU_BUFFERUSAGE_VERTEX
                                     , bufferSize = fromIntegral totalCSize -- Use CSize
                                     , bufferProps = 0
                                     }
                maybeVB <- sdlCreateGPUBuffer dev vbCreateInfo
                case maybeVB of
                   Nothing -> do
                       err <- sdlGetError
                       sdlLog $ "!!! Failed to create vertex buffer: " ++ err
                       return Nothing -- Signal failure
                   Just vb -> do
                       sdlLog $ "Vertex Buffer created successfully: " ++ show vb
                       return (Just vb) -- Signal success with resource
            )
            -- Cleanup action (only if acquisition succeeded but subsequent steps failed)
            (\maybeVertexBuffer ->
                when (isJust maybeVertexBuffer) $ do
                    let vb = fromJust maybeVertexBuffer
                    sdlLog $ "Error occurred during upload, releasing vertex buffer: " ++ show vb
                    sdlReleaseGPUBuffer dev vb
            )
            -- Main action (runs only if vertex buffer acquisition succeeded)
            (\maybeVertexBuffer -> do
                 -- Check again, though bracketOnError should handle Nothing case
                 case maybeVertexBuffer of
                     Nothing -> return Nothing -- Should not happen if bracketOnError works correctly
                     Just vertexBuffer -> do
                         -- Proceed with transfer buffer, mapping, copy, upload
                         uploadSuccess <- uploadViaTransferBuffer dev vertexBuffer totalCSize totalSizeWord32
                         if uploadSuccess then do
                             sdlLog "--- Vertex Buffer Creation and Upload Successful ---"
                             return (Just vertexBuffer) -- Final success
                         else do
                             sdlLog "!!! Vertex Buffer Creation or Upload Failed Overall."
                             -- Cleanup of vertexBuffer is handled by bracketOnError
                             return Nothing -- Final failure
            )
  where
    -- Inner helper grouping transfer buffer logic
    uploadViaTransferBuffer :: SDLGPUDevice -> SDLGPUBuffer -> CSize -> Word32 -> IO Bool
    uploadViaTransferBuffer dev vb transferSizeCSize uploadSizeWord32 = do
        bracket (createTransferBuffer dev (fromIntegral transferSizeCSize))
                (cleanupTransferBuffer dev) $ \maybeTransferBuffer -> do
            case maybeTransferBuffer of
               Nothing -> return False -- Transfer buffer creation failed
               Just transferBuffer -> do
                   -- Map, Copy, Unmap
                   mapCopySuccess <- mapAndCopyData dev transferBuffer vertexData transferSizeCSize
                   unless mapCopySuccess $
                       sdlLog "Mapping or copying data to transfer buffer failed."

                   -- Upload via Command Buffer (only if map/copy succeeded)
                   if mapCopySuccess
                   then uploadDataCommandBuffer dev vb transferBuffer uploadSizeWord32
                   else return False

    -- Helper to create Transfer Buffer
    createTransferBuffer dev size = do
        sdlLog $ "Creating Transfer Buffer (Size: " ++ show size ++ " bytes)..."
        let tbCreateInfo = SDLGPUTransferBufferCreateInfo
                             { transferUsage = SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD
                             , transferSize = size
                             , transferProps = 0
                             }
        maybeTb <- sdlCreateGPUTransferBuffer dev tbCreateInfo
        when (isNothing maybeTb) $ do
            err <- sdlGetError
            sdlLog $ "!!! Failed to create transfer buffer: " ++ err
        return maybeTb

    -- Helper to clean up Transfer Buffer (for bracket)
    cleanupTransferBuffer dev maybeTb =
        when (isJust maybeTb) $ do
             let tb = fromJust maybeTb
             sdlLog $ "Releasing Transfer Buffer: " ++ show tb
             sdlReleaseGPUTransferBuffer dev tb

    -- Helper for Map, Copy, Unmap
    mapAndCopyData dev tb vData tSize = do
        sdlLog $ "Mapping Transfer Buffer: " ++ show tb
        -- Use bracket for map/unmap ensures unmap even on exception during poke
        bracket (sdlMapGPUTransferBuffer dev tb False)
                (\maybePtr -> when (isJust maybePtr) $ do -- Only unmap if map succeeded
                    sdlLog $ "Unmapping Transfer Buffer: " ++ show tb
                    sdlUnmapGPUTransferBuffer dev tb
                ) $ \maybeMappedPtr -> do
            case maybeMappedPtr of
                Nothing -> do
                    err <- sdlGetError
                    sdlLog $ "!!! Failed to map transfer buffer: " ++ err
                    return False -- Bracket cleanup (unmap) won't run, return failure
                Just mappedPtr -> do
                    sdlLog $ "Transfer Buffer mapped to " ++ show mappedPtr ++ ". Copying data..."
                    -- Use 'onException' during poke? Maybe overkill. PokeArray is usually safe.
                    pokeArray (castPtr mappedPtr) vData -- Copy the data
                    sdlLog "Data copied."
                    return True -- Success

    -- Helper for Command Buffer Upload Logic
    uploadDataCommandBuffer :: SDLGPUDevice -> SDLGPUBuffer -> SDLGPUTransferBuffer -> Word32 -> IO Bool
    uploadDataCommandBuffer dev vb tb sizeW32 = do
        bracket (sdlAcquireGPUCommandBuffer dev)
                cleanupCommandBuffer $ \maybeCmdBuf -> do
           case maybeCmdBuf of
               Nothing -> do
                   err <- sdlGetError
                   sdlLog $ "!!! Failed to acquire upload command buffer: " ++ err
                   return False -- Indicate failure
               Just cmdBuf -> do
                   sdlLog $ "Acquired Upload Command Buffer: " ++ show cmdBuf
                   -- Use bracket for copy pass begin/end
                   bracket (sdlBeginGPUCopyPass cmdBuf)
                           cleanupCopyPass $ \maybeCopyPass -> do
                       case maybeCopyPass of
                           Nothing -> do
                               err <- sdlGetError
                               sdlLog $ "!!! Failed to begin copy pass: " ++ err
                               return False -- Indicate copy pass failure
                           Just copyPass -> do
                               sdlLog $ "Begun Copy Pass: " ++ show copyPass
                               let srcLocation = SDLGPUTransferBufferLocation
                                                 { gpuTransferBufferLocBuffer = tb
                                                 , gpuTransferBufferLocOffset = 0 -- Word32
                                                 }
                               let dstRegion = SDLGPUBufferRegion
                                                 { gpuBufRegBuffer = vb
                                                 , gpuBufRegOffset = 0 -- Word32
                                                 , gpuBufRegSize = sizeW32 -- Use Word32
                                                 }
                               sdlLog $ "Recording upload from " ++ show tb ++ " offset 0 to " ++ show vb ++ " offset 0 size " ++ show sizeW32
                               -- Use 'onException' for the actual upload call? Might hide errors. Let's try without first.
                               sdlUploadToGPUBuffer copyPass srcLocation dstRegion False
                               sdlLog "Upload recorded."
                               return True -- Indicate copy pass section succeeded

                   >>= \copyPassOk -> -- Check result of copy pass bracket

                   if copyPassOk then do
                       sdlLog $ "Submitting Upload Command Buffer: " ++ show cmdBuf
                       submitted <- sdlSubmitGPUCommandBuffer cmdBuf
                       unless submitted $ do
                           err <- sdlGetError
                           sdlLog $ "!!! Failed to submit upload command buffer: " ++ err
                       return submitted
                   else do
                       sdlLog $ "Copy Pass failed, cancelling command buffer: " ++ show cmdBuf
                       void $ sdlCancelGPUCommandBuffer cmdBuf -- Cancel if copy pass failed
                       return False -- Upload ultimately failed

    -- Helper to cleanup Command Buffer (Cancel if not submitted)
    -- Note: This cleanup runs *after* the main block of the command buffer bracket finishes.
    -- The logic inside determines if submission or cancellation is appropriate.
    cleanupCommandBuffer maybeCmdBuf =
        case maybeCmdBuf of
            Nothing -> return ()
            Just cmdbuf -> sdlLog $ "Command Buffer bracket cleanup for: " ++ show cmdbuf
                           -- If submission failed inside, cancelling again is harmless
                           -- If copy pass failed, it was cancelled inside.
                           -- If submission succeeded, cancelling is harmless.

    -- Helper to cleanup Copy Pass
    cleanupCopyPass maybeCopyPass =
        when (isJust maybeCopyPass) $ do
            let cp = fromJust maybeCopyPass
            sdlLog $ "Ending Copy Pass: " ++ show cp
            sdlEndGPUCopyPass cp


-- | Main event loop
eventLoopGPU :: Context -> AppResources -> Word64 -> Word64 -> IORef Double -> IO ()
eventLoopGPU context resources lastTime freq deltaTimeRef = do
  currentTime <- sdlGetPerformanceCounter
  let deltaTime = fromIntegral (currentTime - lastTime) / fromIntegral freq
  writeIORef deltaTimeRef (deltaTime * 1000.0) -- Store dt in ms

  sdlPumpEvents
  shouldQuitRef <- newIORef False
  processEventsGPU shouldQuitRef deltaTimeRef -- No AppState needed

  shouldQuit <- readIORef shouldQuitRef
  unless shouldQuit $ do
    -- *** GPU Rendering ***
    -- Use non-blocking acquire as it worked previously for rendering
    renderFrameGPU context resources True -- Pass True to indicate using non-blocking acquire

    -- Continue loop
    eventLoopGPU context resources currentTime freq deltaTimeRef

-- | Process all pending events (simplified, no toggles)
processEventsGPU :: IORef Bool -> IORef Double -> IO ()
processEventsGPU shouldQuitRef deltaTimeRef = do
    maybeEvent <- sdlPollEvent
    case maybeEvent of
        Nothing -> return () -- No more events
        Just event -> do
            quit <- handleEventGPU event deltaTimeRef
            when quit $ writeIORef shouldQuitRef True
            processEventsGPU shouldQuitRef deltaTimeRef -- Process next event

-- | Handle a single SDL event (simplified, no toggles)
handleEventGPU :: SDLEvent -> IORef Double -> IO Bool
handleEventGPU event deltaTimeRef = case event of
  SDLEventQuit _ -> do
    sdlLog "Quit event received."
    return True
  SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ scancode _ _ _ down _) | down -> do
    dtMs <- readIORef deltaTimeRef
    sdlLog $ printf "Key '%s' pressed. Delta Time: %.3f ms" (show scancode) dtMs
    return $ scancode == SDL_SCANCODE_Q -- Quit on Q
  _ -> return False -- Other event, don't quit


-- | Render a frame using the vertex buffer
-- Added flag to use non-blocking swapchain acquire
renderFrameGPU :: Context -> AppResources -> Bool -> IO ()
renderFrameGPU Context{..} AppResources{..} useNonBlockingAcquire = do
    -- 1. Acquire Command Buffer
    sdlLog "Acquiring Command Buffer for Render..."
    maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
    case maybeCmdbuf of
        Nothing -> do
            err <- sdlGetError
            sdlLog $ "Error: Failed to acquire render command buffer: " ++ err
        Just cmdbuf -> do
            sdlLog $ "Acquired Render Command Buffer: " ++ show cmdbuf

            -- 2. Acquire Swapchain Texture
            maybeSwapchain <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow

            case maybeSwapchain of
                Nothing -> do
                    -- This is expected sometimes with non-blocking, log appropriately
                    when useNonBlockingAcquire $
                        sdlLog "Warning: Failed to acquire swapchain texture (non-blocking)"
                    -- If blocking call failed, it's more serious
                    unless useNonBlockingAcquire $
                        sdlLog "!!! Error: Failed to acquire swapchain texture (blocking)"
                    -- Always submit/cancel the command buffer, even if empty
                    void $ sdlSubmitGPUCommandBuffer cmdbuf -- Or cancel if preferred for empty
                Just (swapchainTexture, w, h) -> do
                    sdlLog $ "Acquired Swapchain Texture: " ++ show swapchainTexture ++ " Size: " ++ show w ++ "x" ++ show h
                    -- 3. Define Color Target Info (Clear to black)
                    let clearColor = SDLFColor 0.0 0.0 0.0 1.0
                    let colorTargetInfo = SDLGPUColorTargetInfo
                            { texture           = swapchainTexture
                            , mipLevel          = 0
                            , layerOrDepthPlane = 0
                            , clearColor        = clearColor
                            , loadOp            = SDL_GPU_LOADOP_CLEAR
                            , storeOp           = SDL_GPU_STOREOP_STORE
                            , resolveTexture    = Nothing
                            , resolveMipLevel   = 0
                            , resolveLayer      = 0
                            , targetCycle             = False
                            , targetCycleResolve      = False
                            }

                    -- 4. Begin Render Pass
                    sdlLog "Beginning Render Pass..."
                    bracket (sdlBeginGPURenderPass cmdbuf [colorTargetInfo] Nothing)
                            (\mrp -> when (isJust mrp) $ do
                                sdlLog "Ending Render Pass..."
                                sdlEndGPURenderPass (fromJust mrp)) $ \maybeRenderPass -> do
                        case maybeRenderPass of
                            Nothing -> do
                                err <- sdlGetError
                                sdlLog $ "!!! Error: Failed to begin render pass: " ++ err
                            Just renderPass -> do
                                sdlLog $ "Render Pass Begun: " ++ show renderPass
                                -- 5. Bind the graphics pipeline
                                sdlLog $ "Binding Pipeline: " ++ show resPipeline
                                sdlBindGPUGraphicsPipeline renderPass resPipeline

                                -- 6. Bind the vertex buffer
                                let vertexBufferBinding = SDLGPUBufferBinding resVertexBuffer 0
                                sdlLog $ "Binding Vertex Buffer: " ++ show resVertexBuffer ++ " with offset " ++ show (bindingOffset vertexBufferBinding)
                                sdlBindGPUVertexBuffers renderPass 0 [vertexBufferBinding] -- Slot 0, list

                                -- 7. Draw the triangle (3 vertices, 1 instance)
                                sdlLog "Drawing Primitives..."
                                sdlDrawGPUPrimitives renderPass 3 1 0 0
                                sdlLog "Primitives Drawn."
                                -- Bracket handles sdlEndGPURenderPass

                    -- 9. Submit Command Buffer (only if swapchain was acquired)
                    sdlLog $ "Submitting Render Command Buffer: " ++ show cmdbuf
                    submitted <- sdlSubmitGPUCommandBuffer cmdbuf
                    unless submitted $ do
                         err <- sdlGetError
                         sdlLog $ "!!! Error: Failed to submit render command buffer: " ++ err
