{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE LambdaCase           #-}

{-|
Example     : GPUDrawIndirect
Description : Utilizes indirect drawing commands read from a GPU buffer.
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Based on the SDL_gpu_examples/DrawIndirect.c example.
Demonstrates:
- Populating a GPU buffer (`DrawBuffer`) with `SDL_GPUIndexedIndirectDrawCommand` and `SDL_GPUIndirectDrawCommand` structures.
- Using `SDL_DrawGPUIndexedPrimitivesIndirect` to issue an indexed draw call whose parameters (numIndices, numInstances, etc.)
  are sourced from the `DrawBuffer`.
- Using `SDL_DrawGPUPrimitivesIndirect` to issue non-indexed draw calls with parameters also read from the `DrawBuffer`
  at a specified offset.
- This allows draw call parameters to be generated or modified on the GPU or uploaded once and reused,
  reducing CPU overhead for many similar draw calls.
|-}

module Main where

import SDL
import GPUCommon -- Import common setup, Storable indirect draw commands

import Control.Monad (unless, when, void, (>=>))
import Control.Exception (bracket, bracketOnError, finally)
import Foreign.Ptr (Ptr, nullPtr, castPtr, plusPtr)
import Foreign.Storable (sizeOf, poke, pokeByteOff)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Marshal.Utils (copyBytes, with)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word64, Word32, Word16)
import Data.Maybe (isJust, fromJust, isNothing, fromMaybe)
import Data.Bits ((.|.))
import System.Exit (exitFailure, exitSuccess)

-- Vertex Data (10 vertices with PositionColor)
vertexData :: [PositionColorVertex]
vertexData =
    [ PositionColorVertex (-1)   (-1) 0   255 0   0   255 -- Red
    , PositionColorVertex   1    (-1) 0   0   255 0   255 -- Green
    , PositionColorVertex   1      1  0   0   0   255 255 -- Blue
    , PositionColorVertex (-1)     1  0   255 255 255 255 -- White

    , PositionColorVertex   1    (-1) 0   0   255 0   255 -- Green (for non-indexed 1)
    , PositionColorVertex   0    (-1) 0   0   0   255 255 -- Blue
    , PositionColorVertex   0.5    1  0   255 0   0   255 -- Red

    , PositionColorVertex (-1)   (-1) 0   0   255 0   255 -- Green (for non-indexed 2)
    , PositionColorVertex   0    (-1) 0   0   0   255 255 -- Blue
    , PositionColorVertex (-0.5)   1  0   255 0   0   255 -- Red
    ]

-- Index Data (for the first quad)
indexData :: [Word16]
indexData = [0, 1, 2,  0, 2, 3]

-- Indirect Draw Commands
indexedDrawCmd :: SDLGPUIndexedIndirectDrawCommand
indexedDrawCmd = SDLGPUIndexedIndirectDrawCommand
    { gpuIdxIndirectDrawNumIndices    = 6 -- Draw 6 indices (2 triangles)
    , gpuIdxIndirectDrawNumInstances  = 1
    , gpuIdxIndirectDrawFirstIndex    = 0 -- Start at the beginning of the index buffer
    , gpuIdxIndirectDrawVertexOffset  = 0 -- No offset into the vertex buffer
    , gpuIdxIndirectDrawFirstInstance = 0
    }

drawCmd1 :: SDLGPUIndirectDrawCommand
drawCmd1 = SDLGPUIndirectDrawCommand
    { gpuIndirectDrawNumVertices   = 3 -- Draw 1 triangle
    , gpuIndirectDrawNumInstances  = 1
    , gpuIndirectDrawFirstVertex   = 4 -- Start at vertex index 4
    , gpuIndirectDrawFirstInstance = 0
    }

drawCmd2 :: SDLGPUIndirectDrawCommand
drawCmd2 = SDLGPUIndirectDrawCommand
    { gpuIndirectDrawNumVertices   = 3 -- Draw 1 triangle
    , gpuIndirectDrawNumInstances  = 1
    , gpuIndirectDrawFirstVertex   = 7 -- Start at vertex index 7
    , gpuIndirectDrawFirstInstance = 0
    }

-- AppResources
data AppResources = AppResources
    { resPipeline     :: SDLGPUGraphicsPipeline
    , resVertexBuffer :: SDLGPUBuffer
    , resIndexBuffer  :: SDLGPUBuffer
    , resDrawBuffer   :: SDLGPUBuffer -- For indirect draw commands
    } deriving Show

-- main
main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion
  maybeResult <- withContext "SDL3 Haskell GPU Indirect Draw" [] runAppGPU
  case maybeResult of
      Nothing -> sdlLog "Application initialization failed." >> exitFailure
      Just _  -> sdlLog "Application finished successfully." >> exitSuccess

-- runAppGPU
runAppGPU :: Context -> IO ()
runAppGPU context = do
    sdlLog "Base context initialized."
    bracket (createResources context)
            (releaseResources context)
            $ \case
            Nothing -> sdlLog "Failed to create resources. Exiting."
            Just resources -> do
                sdlLog "Resources created successfully."
                eventLoopGPU context resources

-- createResources
createResources :: Context -> IO (Maybe AppResources)
createResources Context{..} = do
    sdlLog "--- Beginning Resource Creation ---"

    -- 1. Load Shaders
    maybeVertShader <- loadShader contextDevice "PositionColor.vert" SDL_GPU_SHADERSTAGE_VERTEX defaultShaderCreateInfo
    maybeFragShader <- loadShader contextDevice "SolidColor.frag" SDL_GPU_SHADERSTAGE_FRAGMENT defaultShaderCreateInfo

    case (maybeVertShader, maybeFragShader) of
        (Just vertShader, Just fragShader) -> do
            sdlLog "Shaders loaded."
            -- 2. Create Graphics Pipeline
            maybePipeline <- createDrawPipeline contextDevice contextWindow vertShader fragShader
            sdlReleaseGPUShader contextDevice vertShader -- Release after pipeline creation
            sdlReleaseGPUShader contextDevice fragShader

            -- 3. Create Buffers
            (_, _, vbSizeW32) <- calculateBufferDataSize vertexData "Vertex"
            maybeVB <- createGPUBuffer contextDevice SDL_GPU_BUFFERUSAGE_VERTEX vbSizeW32 "VertexBuffer"

            (_, _, ibSizeW32) <- calculateBufferDataSize indexData "Index"
            maybeIB <- createGPUBuffer contextDevice SDL_GPU_BUFFERUSAGE_INDEX ibSizeW32 "IndexBuffer"

            let indexedCmdSize = fromIntegral (sizeOf indexedDrawCmd)
            let drawCmdSize    = fromIntegral (sizeOf drawCmd1)
            let drawBufferSize = indexedCmdSize + (2 * drawCmdSize)
            maybeDrawBuf <- createGPUBuffer contextDevice SDL_GPU_BUFFERUSAGE_INDIRECT drawBufferSize "DrawCmdBuffer"

            -- Upload Data
            let totalTransferSize = vbSizeW32 + ibSizeW32 + drawBufferSize
            uploadSuccess <- bracket (createTransferBuffer contextDevice totalTransferSize SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "CombinedTransfer")
                                     (cleanupTransferBuffer contextDevice)
                                     $ \maybeTransferBuf -> case maybeTransferBuf of
                Nothing -> sdlLog "Failed to create transfer buffer for combined data." >> return False
                Just transferBuf ->
                    bracket (sdlMapGPUTransferBuffer contextDevice transferBuf False)
                            (\mptr -> when (isJust mptr) $ sdlUnmapGPUTransferBuffer contextDevice transferBuf)
                            $ \maybeMappedPtr -> case maybeMappedPtr of
                        Nothing -> sdlLog "Failed to map combined transfer buffer." >> return False
                        Just mappedPtr -> do
                            -- Poke Vertex Data
                            pokeArray (castPtr mappedPtr) vertexData
                            let currentOffset1 = vbSizeW32

                            -- Poke Index Data
                            pokeArray (castPtr mappedPtr `plusPtr` fromIntegral currentOffset1) indexData
                            let currentOffset2 = currentOffset1 + ibSizeW32

                            -- Poke Indexed Draw Command
                            poke (castPtr mappedPtr `plusPtr` fromIntegral currentOffset2) indexedDrawCmd
                            let currentOffset3 = currentOffset2 + indexedCmdSize

                            -- Poke Non-Indexed Draw Command 1
                            poke (castPtr mappedPtr `plusPtr` fromIntegral currentOffset3) drawCmd1
                            let currentOffset4 = currentOffset3 + drawCmdSize

                            -- Poke Non-Indexed Draw Command 2
                            poke (castPtr mappedPtr `plusPtr` fromIntegral currentOffset4) drawCmd2
                            return True

            if not uploadSuccess then sdlLog "Mapping and poking data to transfer buffer failed." >> cleanupAndFail maybePipeline maybeVB maybeIB maybeDrawBuf contextDevice >> return Nothing
            else
                bracket (sdlAcquireGPUCommandBuffer contextDevice)
                        cleanupCommandBuffer
                        $ \mCmdBuf -> case (mCmdBuf, maybePipeline, maybeVB, maybeIB, maybeDrawBuf) of
                    (Just cmdBuf, Just pipeline, Just vb, Just ib, Just drawBuf) -> do
                        -- Assume transferBuf is accessible from the uploadSuccess's bracket scope (it is not here)
                        -- For simplicity, re-acquire/re-map or pass it. Let's assume re-acquire is fine for now.
                        -- This is not ideal, better to structure brackets.
                        -- For this example, let's assume a single transfer buffer for all uploads for simplicity of the example.
                        -- In a real app, you might use separate transfers or a more robust staging system.
                        bracket (createTransferBuffer contextDevice totalTransferSize SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "ReUpload") -- Re-create for simplicity
                                (cleanupTransferBuffer contextDevice) $ \mTransferBufForUpload ->
                            case mTransferBufForUpload of
                                Nothing -> return Nothing
                                Just transferBufForUpload -> do
                                    -- Re-map and re-poke (this is inefficient but simplifies bracket nesting for example)
                                    reMapOk <- bracket (sdlMapGPUTransferBuffer contextDevice transferBufForUpload False)
                                                       (\mptr -> when (isJust mptr) $ sdlUnmapGPUTransferBuffer contextDevice transferBufForUpload)
                                                       $ \mMapped -> case mMapped of
                                                           Nothing -> return False
                                                           Just mapped -> do
                                                               pokeArray (castPtr mapped) vertexData
                                                               pokeArray (castPtr mapped `plusPtr` fromIntegral vbSizeW32) indexData
                                                               poke (castPtr mapped `plusPtr` fromIntegral (vbSizeW32 + ibSizeW32)) indexedDrawCmd
                                                               poke (castPtr mapped `plusPtr` fromIntegral (vbSizeW32 + ibSizeW32 + indexedCmdSize)) drawCmd1
                                                               poke (castPtr mapped `plusPtr` fromIntegral (vbSizeW32 + ibSizeW32 + indexedCmdSize + drawCmdSize)) drawCmd2
                                                               return True
                                    unless reMapOk $ error "Re-map for upload failed"

                                    mcp <- sdlBeginGPUCopyPass cmdBuf
                                    case mcp of
                                        Nothing -> return Nothing
                                        Just cp -> do
                                            sdlUploadToGPUBuffer cp (SDLGPUTransferBufferLocation transferBufForUpload 0)        (SDLGPUBufferRegion vb 0 vbSizeW32) False
                                            sdlUploadToGPUBuffer cp (SDLGPUTransferBufferLocation transferBufForUpload vbSizeW32) (SDLGPUBufferRegion ib 0 ibSizeW32) False
                                            sdlUploadToGPUBuffer cp (SDLGPUTransferBufferLocation transferBufForUpload (vbSizeW32 + ibSizeW32)) (SDLGPUBufferRegion drawBuf 0 (fromIntegral drawBufferSize)) False
                                            sdlEndGPUCopyPass cp
                                            submitSuccess <- sdlSubmitGPUCommandBuffer cmdBuf
                                            when submitSuccess $ void $ sdlWaitForGPUIdle contextDevice
                                            if submitSuccess
                                            then do
                                                return $ Just AppResources { resPipeline     = pipeline
                                                                          , resVertexBuffer = vb
                                                                          , resIndexBuffer  = ib
                                                                          , resDrawBuffer   = drawBuf
                                                                          }                                            else return Nothing
                    _ -> sdlLog "Failed to create one or more resources for upload." >> return Nothing
        _ -> sdlLog "Failed to load shaders." >> return Nothing

cleanupAndFail mp mvb mib mdb dev = do
    maybe (pure()) (sdlReleaseGPUGraphicsPipeline dev) mp
    maybe (pure()) (sdlReleaseGPUBuffer dev) mvb
    maybe (pure()) (sdlReleaseGPUBuffer dev) mib
    maybe (pure()) (sdlReleaseGPUBuffer dev) mdb

-- createDrawPipeline
createDrawPipeline :: SDLGPUDevice -> SDLWindow -> SDLGPUShader -> SDLGPUShader -> IO (Maybe SDLGPUGraphicsPipeline)
createDrawPipeline dev win vertShader fragShader = do
    swapchainFormat <- sdlGetGPUSwapchainTextureFormat dev win
    let vertexSize = sizeOf (undefined :: PositionColorVertex)
    let colorOffset = sizeOf (undefined :: Float) * 3 -- Offset for color attribute
    let vertexAttributes =
          [ SDLGPUVertexAttribute 0 0 SDL_GPU_VERTEXELEMENTFORMAT_FLOAT3 0 -- Position
          , SDLGPUVertexAttribute 1 0 SDL_GPU_VERTEXELEMENTFORMAT_UBYTE4_NORM (fromIntegral colorOffset) -- Color
          ]
        vertexBufferDesc = [ SDLGPUVertexBufferDescription 0 (fromIntegral vertexSize) SDL_GPU_VERTEXINPUTRATE_VERTEX 0 ]
        vertexInputState = SDLGPUVertexInputState vertexBufferDesc vertexAttributes
    let colorTargetDesc = defaultColorTargetDescription { targetFormat = swapchainFormat }
        targetInfo = SDLGPUGraphicsPipelineTargetInfo [colorTargetDesc] SDL_GPU_TEXTUREFORMAT_INVALID False
        pipelineCI = (defaultGraphicsPipelineCreateInfo vertShader fragShader swapchainFormat)
            { vertexInputState = vertexInputState
            , targetInfo = targetInfo
            }
    sdlCreateGPUGraphicsPipeline dev pipelineCI

-- releaseResources
releaseResources :: Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources Context{..} (Just AppResources{..}) = do
    sdlLog "--> Releasing AppResources..."
    sdlReleaseGPUGraphicsPipeline contextDevice resPipeline
    sdlReleaseGPUBuffer contextDevice resVertexBuffer
    sdlReleaseGPUBuffer contextDevice resIndexBuffer
    sdlReleaseGPUBuffer contextDevice resDrawBuffer
    sdlLog "<-- AppResources Released."

-- eventLoopGPU, processEventsGPU, handleEventGPU (Simplified, no UI interaction beyond quit)
eventLoopGPU :: Context -> AppResources -> IO ()
eventLoopGPU context resources = do
  sdlPumpEvents
  shouldQuitRef <- newIORef False
  processEventsGPU shouldQuitRef
  shouldQuit <- readIORef shouldQuitRef
  unless shouldQuit $ renderFrameGPU context resources >> eventLoopGPU context resources

processEventsGPU :: IORef Bool -> IO ()
processEventsGPU sr = sdlPollEvent >>= maybe (pure()) (handleEventGPU >=> \q -> when q (writeIORef sr True) >> processEventsGPU sr)

handleEventGPU :: SDLEvent -> IO Bool
handleEventGPU (SDLEventQuit _) = sdlLog "Quit." >> return True
handleEventGPU (SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ sc _ _ _ d _)) | d && sc == SDL_SCANCODE_Q = return True
handleEventGPU _ = return False

-- renderFrameGPU
renderFrameGPU :: Context -> AppResources -> IO ()
renderFrameGPU Context{..} AppResources{..} = do
    maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
    case maybeCmdbuf of
        Nothing -> sdlLog "Error: Failed to acquire render command buffer."
        Just cmdbuf -> do
            maybeSwapchain <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
            case maybeSwapchain of
                Nothing -> void (sdlSubmitGPUCommandBuffer cmdbuf `finally` pure ())
                Just (swapchainTexture, _, _) -> do
                    let colorTargetInfo = defaultColorTargetInfo
                            { texture = swapchainTexture, loadOp = SDL_GPU_LOADOP_CLEAR
                            , storeOp = SDL_GPU_STOREOP_STORE, clearColor = SDLFColor 0 0 0 1
                            }
                    bracket (sdlBeginGPURenderPass cmdbuf [colorTargetInfo] Nothing)
                            cleanupMaybeRenderPass $ \case
                            Nothing -> sdlLog "Error: Failed to begin render pass."
                            Just renderPass -> do
                                sdlBindGPUGraphicsPipeline renderPass resPipeline
                                sdlBindGPUVertexBuffers renderPass 0 [SDLGPUBufferBinding resVertexBuffer 0]
                                sdlBindGPUIndexBuffer renderPass (SDLGPUBufferBinding resIndexBuffer 0) SDL_GPU_INDEXELEMENTSIZE_16BIT

                                -- Indirect Indexed Draw
                                sdlDrawGPUIndexedPrimitivesIndirect renderPass resDrawBuffer 0 1

                                -- Indirect Non-Indexed Draws
                                let indexedCmdSize = fromIntegral (sizeOf indexedDrawCmd) :: Word32
                                sdlDrawGPUPrimitivesIndirect renderPass resDrawBuffer indexedCmdSize 2

                    submitted <- sdlSubmitGPUCommandBuffer cmdbuf
                    unless submitted $ sdlGetError >>= sdlLog . ("Submit failed: " ++)
