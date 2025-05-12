{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedLists      #-} -- For defining lists directly in records

{-|
Example     : GPUIndexOffsetDraw
Description : SDL Example: Indexed and offset drawing
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Based on the SDL_gpu_examples/IndexOffsetDraw C example.
Demonstrates:
- Creating and populating vertex and index buffers.
- Uploading data for both buffers using a single transfer buffer.
- Using SDL_DrawGPUIndexedPrimitives with vertex and index offsets.
- Using SDL_DrawGPUPrimitives with a vertex offset.
- Toggling between indexed and non-indexed drawing.
- Toggling the use of vertex/index offsets via keyboard input.
|-}

module Main where

import SDL
import qualified SDL.Properties as Props
import GPUCommon        -- Import common init/quit and shader loading

import Control.Monad (unless, when, void, forM_)
import Control.Exception (bracket, bracketOnError, finally)
import System.Exit (exitFailure, exitSuccess)
import Foreign.Ptr (Ptr, nullPtr, castPtr, plusPtr)
import Foreign.Storable (Storable(..), peekByteOff, pokeByteOff, sizeOf, poke)
import Foreign.C.Types (CFloat, CSize, CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (pokeArray)
import Data.Foldable (for_)
import Data.IORef
import Data.Word (Word32, Word64, Word16, Word8)
import Text.Printf (printf)
import Data.Maybe (isJust, fromJust, fromMaybe, isNothing)
import Data.Bits ((.|.))

-- Define vertex data for three triangles
vertexData :: [PositionColorVertex]
vertexData =
    [ -- Triangle 1 (RGB)
      PositionColorVertex (-1) (-1) 0   255   0   0 255 -- Red
    , PositionColorVertex   1  (-1) 0     0 255   0 255 -- Green
    , PositionColorVertex   0    1  0     0   0 255 255 -- Blue
      -- Triangle 2 (Orange/Green/Cyan)
    , PositionColorVertex (-1) (-1) 0   255 165   0 255 -- Orange
    , PositionColorVertex   1  (-1) 0     0 128   0 255 -- Dark Green
    , PositionColorVertex   0    1  0     0 255 255 255 -- Cyan
      -- Triangle 3 (White)
    , PositionColorVertex (-1) (-1) 0   255 255 255 255 -- White
    , PositionColorVertex   1  (-1) 0   255 255 255 255 -- White
    , PositionColorVertex   0    1  0   255 255 255 255 -- White
    ]

-- Define index data (enough for two triangles)
indexData :: [Word16]
indexData = [ 0, 1, 2, 3, 4, 5 ]

-- 2. Define Managed Resources
data AppResources = AppResources
    { resPipeline     :: SDLGPUGraphicsPipeline
    , resVertexBuffer :: SDLGPUBuffer
    , resIndexBuffer  :: SDLGPUBuffer
    }

-- 3. Define Application State (Toggles)
data AppState = AppState
    { appUseVertexOffset :: IORef Bool
    , appUseIndexOffset  :: IORef Bool
    , appUseIndexBuffer  :: IORef Bool
    }

main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  maybeResult <- withContext "SDL3 Haskell GPU Index/Offset Draw" [] runAppGPU
  case maybeResult of
      Nothing -> sdlLog "Application initialization failed." >> exitFailure
      Just _  -> sdlLog "Application finished successfully." >> exitSuccess

runAppGPU :: Context -> IO ()
runAppGPU context = do
    sdlLog "Base context initialized."
    bracket (createResources context)
            (releaseResources context) $ \case
        Nothing -> sdlLog "Failed to create resources. Exiting."
        Just resources -> do
            sdlLog "Resources created successfully."
            sdlLog "Press Left Arrow to toggle vertex offset (0 or 3)"
            sdlLog "Press Right Arrow to toggle index offset (0 or 3)"
            sdlLog "Press Up Arrow to toggle index buffer usage"

            -- Create mutable state refs, initialized to C example defaults
            state <- AppState <$> newIORef False <*> newIORef False <*> newIORef True

            startTime <- sdlGetPerformanceCounter
            freq <- sdlGetPerformanceFrequency
            deltaTimeRef <- newIORef 0.0
            eventLoopGPU context resources state startTime freq deltaTimeRef

  where
    -- Action to create all GPU resources
    createResources :: Context -> IO (Maybe AppResources)
    createResources ctx@Context{ contextDevice = dev, contextWindow = win } = do
        sdlLog "--- Creating App Resources ---"
        -- Bracket ensures shaders are released even if subsequent steps fail
        bracketOnError (loadShaders dev) (releaseShaders dev) $ \case
            Nothing -> return Nothing -- Shader loading failed
            Just (vertShader, fragShader) -> do
                sdlLog "Shaders loaded successfully."

                -- Bracket ensures pipeline released if buffer creation fails
                bracketOnError (createPipeline dev win vertShader fragShader)
                               (maybe (return ()) (sdlReleaseGPUGraphicsPipeline dev)) $ \case
                    Nothing -> return Nothing -- Pipeline creation failed
                    Just pipeline -> do
                        sdlLog "Pipeline created successfully."

                        -- Bracket ensures buffers released if upload fails
                        bracketOnError (createBuffers dev) (releaseBuffers dev) $ \case
                            Nothing -> return Nothing -- Buffer creation failed
                            Just (vertexBuffer, indexBuffer) -> do
                                sdlLog "Vertex and Index Buffers created."

                                -- Upload data to buffers
                                uploadOk <- uploadBufferData dev vertexBuffer indexBuffer vertexData indexData
                                if uploadOk then do
                                    sdlLog "Buffer data uploaded successfully."
                                    sdlLog "--- App Resources Creation Successful ---"
                                    -- Shaders released by outer bracket
                                    return $ Just $ AppResources
                                        { resPipeline = pipeline
                                        , resVertexBuffer = vertexBuffer
                                        , resIndexBuffer = indexBuffer
                                        }
                                else do
                                    sdlLog "!!! Buffer data upload failed."
                                    -- Buffers released by bracketOnError
                                    return Nothing

    -- Helper: Load both shaders
    loadShaders dev = do
        -- NOTE: Using "PositionColorInstanced.vert" like the C code,
        -- assuming it's compatible with our PositionColorVertex structure
        -- and the non-instanced pipeline setup.
        mVert <- loadShader dev "PositionColorInstanced.vert" SDL_GPU_SHADERSTAGE_VERTEX defaultShaderCreateInfo
        mFrag <- loadShader dev "SolidColor.frag" SDL_GPU_SHADERSTAGE_FRAGMENT defaultShaderCreateInfo
        case (mVert, mFrag) of
            (Just vs, Just fs) -> return $ Just (vs, fs)
            _                  -> sdlLog "!!! Failed to load shaders." >> return Nothing

    -- Helper: Release shaders
    releaseShaders dev maybePair = for_ maybePair $ \(vs, fs) -> do
        sdlLog "Releasing shaders..."
        sdlReleaseGPUShader dev vs
        sdlReleaseGPUShader dev fs

    -- Helper: Create the graphics pipeline
    createPipeline dev win vertShader fragShader = do
        swapchainFormat <- sdlGetGPUSwapchainTextureFormat dev win

        -- Vertex Input State (Matches PositionColorVertex)
        let vertexSize = fromIntegral $ sizeOf (undefined :: PositionColorVertex)
        let colorOffset = fromIntegral $ sizeOf (undefined :: CFloat) * 3
        let vertexAttributes =
              [ SDLGPUVertexAttribute -- Position
                  { attribLocation = 0
                  , attribSlot = 0
                  , attribFormat = SDL_GPU_VERTEXELEMENTFORMAT_FLOAT3
                  , attribOffset = 0
                  }
              , SDLGPUVertexAttribute -- Color
                  { attribLocation = 1
                  , attribSlot = 0
                  , attribFormat = SDL_GPU_VERTEXELEMENTFORMAT_UBYTE4_NORM
                  , attribOffset = colorOffset
                  }
              ]
            vertexBufferDesc =
              [ SDLGPUVertexBufferDescription -- Single buffer binding
                  { descSlot = 0
                  , descPitch = vertexSize
                  , descInputRate = SDL_GPU_VERTEXINPUTRATE_VERTEX
                  , descInstanceStepRate = 0 -- IMPORTANT: Not instanced
                  }
              ]
            vertexInputState = SDLGPUVertexInputState vertexBufferDesc vertexAttributes

        let colorTargetDesc = SDLGPUColorTargetDescription swapchainFormat defaultColorTargetBlendState
            targetInfo = SDLGPUGraphicsPipelineTargetInfo
                { colorTargets = [colorTargetDesc]
                , hasDepthStencil = False
                , depthStencilFormat = SDL_GPU_TEXTUREFORMAT_INVALID
                }

            pipelineCI = SDLGPUGraphicsPipelineCreateInfo
                { vertexShader = vertShader
                , fragmentShader = fragShader
                , vertexInputState = vertexInputState
                , primitiveType = SDL_GPU_PRIMITIVETYPE_TRIANGLELIST
                , rasterizerState = defaultRasterizerState
                , multisampleState = defaultMultiSampleState
                , targetInfo = targetInfo
                , depthStencilState = defaultDepthStencilState -- Depth/Stencil disabled
                , props = 0
                }

        mPipeline <- sdlCreateGPUGraphicsPipeline dev pipelineCI
        when (isNothing mPipeline) $ sdlGetError >>= \e -> sdlLog $ "!!! Failed Pipeline creation: " ++ e
        return mPipeline

    -- Helper: Create Vertex and Index Buffers
    createBuffers :: SDLGPUDevice -> IO (Maybe (SDLGPUBuffer, SDLGPUBuffer))
    createBuffers dev = do
        -- Vertex Buffer
        let vertexBufferSize = fromIntegral $ length vertexData * sizeOf (undefined :: PositionColorVertex)
        let vbCI = SDLGPUBufferCreateInfo SDL_GPU_BUFFERUSAGE_VERTEX vertexBufferSize 0
        mVB <- sdlCreateGPUBuffer dev vbCI
        when (isNothing mVB) $ sdlGetError >>= \e -> sdlLog $ "!!! Failed Vertex Buffer creation: " ++ e

        -- Index Buffer
        let indexBufferSize = fromIntegral $ length indexData * sizeOf (undefined :: Word16)
        let ibCI = SDLGPUBufferCreateInfo SDL_GPU_BUFFERUSAGE_INDEX indexBufferSize 0
        mIB <- sdlCreateGPUBuffer dev ibCI
        when (isNothing mIB) $ sdlGetError >>= \e -> sdlLog $ "!!! Failed Index Buffer creation: " ++ e

        case (mVB, mIB) of
            (Just vb, Just ib) -> return $ Just (vb, ib)
            (mvb, mib) -> do -- Cleanup partially created buffers
                sdlLog "Buffer creation failed, cleaning up..."
                for_ mvb (sdlReleaseGPUBuffer dev)
                for_ mib (sdlReleaseGPUBuffer dev)
                return Nothing

    -- Helper: Release Buffers
    releaseBuffers dev maybePair = for_ maybePair $ \(vb, ib) -> do
        sdlLog "Releasing vertex and index buffers..."
        sdlReleaseGPUBuffer dev vb
        sdlReleaseGPUBuffer dev ib

    -- Helper: Upload data to both buffers using one transfer buffer
    uploadBufferData :: SDLGPUDevice -> SDLGPUBuffer -> SDLGPUBuffer -> [PositionColorVertex] -> [Word16] -> IO Bool
    uploadBufferData dev vertexBuffer indexBuffer vertices indices = do
        sdlLog "--- Beginning Buffer Data Upload ---"
        let vertexSize = sizeOf (head vertices)
            numVertices = length vertices
            vertexDataBytes = numVertices * vertexSize
            vertexDataCSize = fromIntegral vertexDataBytes :: CSize
            vertexDataSizeW32 = fromIntegral vertexDataBytes :: Word32

        let indexSize = sizeOf (head indices)
            numIndices = length indices
            indexDataBytes = numIndices * indexSize
            indexDataCSize = fromIntegral indexDataBytes :: CSize
            indexDataSizeW32 = fromIntegral indexDataBytes :: Word32

        let totalTransferSize = vertexDataCSize + indexDataCSize
        sdlLog $ printf "Vertex Data: %d bytes (%d verts * %d bytes/vert)" vertexDataBytes numVertices vertexSize
        sdlLog $ printf "Index Data: %d bytes (%d indices * %d bytes/index)" indexDataBytes numIndices indexSize
        sdlLog $ printf "Total Transfer Size: %d bytes" (fromIntegral totalTransferSize :: Int)

        bracket (createTransferBuffer dev totalTransferSize SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "transfer")
                (cleanupTransferBuffer dev) $ \case
            Nothing -> return False -- Transfer buffer creation failed
            Just transferBuffer -> do
                -- Map, Poke, Unmap
                mapCopySuccess <- bracket (sdlMapGPUTransferBuffer dev transferBuffer False)
                                         (\mPtr -> when (isJust mPtr) $ sdlUnmapGPUTransferBuffer dev transferBuffer) $ \case
                    Nothing -> sdlLog "!!! Map Transfer Buffer failed" >> return False
                    Just mappedPtr -> do
                        sdlLog $ "Mapping successful. Ptr: " ++ show mappedPtr ++ ". Copying data..."
                        -- Poke vertex data at offset 0
                        pokeArray (castPtr mappedPtr) vertices
                        sdlLog "Vertex data poked."
                        -- Poke index data *after* vertex data
                        let indexOffsetPtr = mappedPtr `plusPtr` vertexDataBytes
                        pokeArray (castPtr indexOffsetPtr) indices
                        sdlLog "Index data poked."
                        return True

                -- If mapping/poking succeeded, proceed with upload command buffer
                if mapCopySuccess
                then uploadDataCommandBuffer dev vertexBuffer indexBuffer transferBuffer vertexDataSizeW32 indexDataSizeW32 vertexDataCSize
                else return False

    -- Helper: Submit copy commands
    uploadDataCommandBuffer :: SDLGPUDevice -> SDLGPUBuffer -> SDLGPUBuffer -> SDLGPUTransferBuffer -> Word32 -> Word32 -> CSize -> IO Bool
    uploadDataCommandBuffer dev vb ib tb vbSizeW32 ibSizeW32 vbOffsetCSize = do
        bracket (sdlAcquireGPUCommandBuffer dev) cleanupCommandBuffer $ \case
            Nothing -> sdlLog "!!! Acquire upload CB failed" >> return False
            Just cmdBuf -> do
                copyCommandsOk <- bracket (sdlBeginGPUCopyPass cmdBuf) cleanupCopyPass $ \case
                    Nothing -> sdlLog "!!! Begin copy pass failed" >> return False
                    Just copyPass -> do
                        -- Upload Vertex Data
                        let srcVertLoc = SDLGPUTransferBufferLocation tb 0
                            dstVertReg = SDLGPUBufferRegion vb 0 vbSizeW32
                        sdlLog $ "Recording vertex upload (Size: " ++ show vbSizeW32 ++ ")"
                        sdlUploadToGPUBuffer copyPass srcVertLoc dstVertReg False

                        -- Upload Index Data
                        let srcIndexLoc = SDLGPUTransferBufferLocation tb (fromIntegral vbOffsetCSize) -- Start after vertex data
                            dstIndexReg = SDLGPUBufferRegion ib 0 ibSizeW32
                        sdlLog $ "Recording index upload (Size: " ++ show ibSizeW32 ++ ")"
                        sdlUploadToGPUBuffer copyPass srcIndexLoc dstIndexReg False

                        sdlLog "Uploads recorded."
                        return True -- Recording success

                if copyCommandsOk then do
                    sdlLog "Submitting upload command buffer..."
                    submitted <- sdlSubmitGPUCommandBuffer cmdBuf
                    unless submitted $ sdlGetError >>= \e -> sdlLog $ "!!! Upload submission failed: " ++ e
                    return submitted
                else do
                    sdlLog "Copy pass failed, skipping submission."
                    return False

    -- Action to release resources
    releaseResources :: Context -> Maybe AppResources -> IO ()
    releaseResources _ Nothing = return () -- Nothing created
    releaseResources Context{..} (Just AppResources{..}) = do
        sdlLog "--- Releasing App Resources ---"
        sdlLog $ "Releasing Pipeline: " ++ show resPipeline
        sdlReleaseGPUGraphicsPipeline contextDevice resPipeline
        sdlLog $ "Releasing Vertex Buffer: " ++ show resVertexBuffer
        sdlReleaseGPUBuffer contextDevice resVertexBuffer
        sdlLog $ "Releasing Index Buffer: " ++ show resIndexBuffer
        sdlReleaseGPUBuffer contextDevice resIndexBuffer
        sdlLog "--- App Resources Released ---"

-- | Main event loop
eventLoopGPU :: Context -> AppResources -> AppState -> Word64 -> Word64 -> IORef Double -> IO ()
eventLoopGPU context resources state lastTime freq deltaTimeRef = do
  currentTime <- sdlGetPerformanceCounter
  let deltaTime = fromIntegral (currentTime - lastTime) / fromIntegral freq
  writeIORef deltaTimeRef (deltaTime * 1000.0) -- Store dt in ms
  -- dtMs <- readIORef deltaTimeRef
  -- sdlLog $ printf "DT: %.3fms" dtMs -- Optional: Log delta time

  sdlPumpEvents
  shouldQuitRef <- newIORef False
  -- Pass state to event processor so it can modify the IORefs
  processEventsGPU context shouldQuitRef state

  shouldQuit <- readIORef shouldQuitRef
  unless shouldQuit $ do
    -- Render frame using current state
    renderFrameGPU context resources state True -- Use non-blocking acquire

    -- Continue loop
    eventLoopGPU context resources state currentTime freq deltaTimeRef

-- | Process all pending events, modifying state based on input
processEventsGPU :: Context -> IORef Bool -> AppState -> IO ()
processEventsGPU context shouldQuitRef state = do
    maybeEvent <- sdlPollEvent
    case maybeEvent of
        Nothing -> return () -- No more events
        Just event -> do
            -- Handle event and check if it requests quitting
            quit <- handleEventGPU context event state
            when quit $ writeIORef shouldQuitRef True
            processEventsGPU context shouldQuitRef state -- Process next event

-- | Handle a single SDL event, modifying state IORefs
handleEventGPU :: Context -> SDLEvent -> AppState -> IO Bool
handleEventGPU context event AppState{..} = case event of
  SDLEventQuit _ -> sdlLog "Quit event received." >> return True
  SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ scancode _ _ _ down _) | down -> do
    case scancode of
      SDL_SCANCODE_Q -> return True -- Quit on Q
      SDL_SCANCODE_LEFT -> do
          modifyIORef' appUseVertexOffset not
          readIORef appUseVertexOffset >>= \v -> sdlLog $ "Using vertex offset: " ++ show v
          return False
      SDL_SCANCODE_RIGHT -> do
          modifyIORef' appUseIndexOffset not
          readIORef appUseIndexOffset >>= \v -> sdlLog $ "Using index offset: " ++ show v
          return False
      SDL_SCANCODE_UP -> do
          modifyIORef' appUseIndexBuffer not
          readIORef appUseIndexBuffer >>= \v -> sdlLog $ "Using index buffer: " ++ show v
          return False
      _ -> return False -- Other key press, don't quit
  _ -> return False -- Other event, don't quit

-- | Render a frame
renderFrameGPU :: Context -> AppResources -> AppState -> Bool -> IO ()
renderFrameGPU Context{..} AppResources{..} AppState{..} useNonBlockingAcquire = do
    -- Read current state flags
    useVertexOffset <- readIORef appUseVertexOffset
    useIndexOffset <- readIORef appUseIndexOffset
    useIndexBuffer <- readIORef appUseIndexBuffer

    -- Calculate offsets (0 or 3 vertices/indices)
    let vertexOffsetW32 :: Word32 = if useVertexOffset then 3 else 0
    let indexOffsetW32 :: Word32 = if useIndexOffset then 3 else 0

    -- 1. Acquire Command Buffer
    maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
    case maybeCmdbuf of
        Nothing -> sdlGetError >>= \err -> sdlLog $ "Error: Failed to acquire render command buffer: " ++ err
        Just cmdbuf -> do
            -- 2. Acquire Swapchain Texture
            maybeSwapchain <- if useNonBlockingAcquire
                              then sdlAcquireGPUSwapchainTexture cmdbuf contextWindow
                              else sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
            case maybeSwapchain of
                Nothing -> do
                    unless useNonBlockingAcquire $ sdlLog "!!! Error: Failed to acquire swapchain texture (blocking)"
                    void $ sdlSubmitGPUCommandBuffer cmdbuf -- Submit empty buffer
                Just (swapchainTexture, _, _) -> do
                    -- Define Color Target (Clear to black)
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

                    -- 3. Begin Render Pass
                    bracket (sdlBeginGPURenderPass cmdbuf [colorTargetInfo] Nothing) -- No depth/stencil
                            (\mrp -> for_ mrp sdlEndGPURenderPass) $ \case
                        Nothing -> sdlGetError >>= \err -> sdlLog $ "!!! Error: Failed to begin render pass: " ++ err
                        Just renderPass -> do
                            -- Bind Pipeline
                            sdlBindGPUGraphicsPipeline renderPass resPipeline

                            -- Bind Vertex Buffer (offset is always 0 here, draw call uses offset)
                            let vbBinding = SDLGPUBufferBinding resVertexBuffer 0
                            sdlBindGPUVertexBuffers renderPass 0 [vbBinding]

                            -- Choose Draw Call based on state
                            if useIndexBuffer then do
                                -- Bind Index Buffer (offset is always 0 here)
                                let ibBinding = SDLGPUBufferBinding resIndexBuffer 0
                                sdlBindGPUIndexBuffer renderPass ibBinding SDL_GPU_INDEXELEMENTSIZE_16BIT

                                -- Draw Indexed Primitives
                                -- Args: indexCount, instanceCount, firstIndex, vertexOffset, firstInstance
                                -- Draw 16 triangles (3 indices)
                                sdlDrawGPUIndexedPrimitives renderPass 3 16 indexOffsetW32 vertexOffsetW32 0
                            else do
                                -- Draw Non-Indexed Primitives
                                -- Args: vertexCount, instanceCount, firstVertex, firstInstance
                                -- Draw 16 triangles (3 vertices)
                                sdlDrawGPUPrimitives renderPass 3 16 vertexOffsetW32 0

                    -- 4. Submit Command Buffer
                    submitted <- sdlSubmitGPUCommandBuffer cmdbuf
                    unless submitted $ sdlGetError >>= \err -> sdlLog $ "!!! Error: Failed to submit render command buffer: " ++ err
