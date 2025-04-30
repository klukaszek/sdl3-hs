{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeApplications     #-} -- For using sizeOf with type applications
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

import Control.Monad (unless, when, void, forM_)
import Control.Exception (bracket) -- For resource management
import System.Exit (exitFailure, exitSuccess)
import Foreign.Ptr (Ptr, nullPtr, castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Foreign.C.Types (CFloat, CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (pokeArray)
import Data.IORef       -- Not needed for state toggles anymore
import Data.Word (Word32, Word64, Word8)
import Text.Printf (printf)
import Data.Maybe (isJust, fromJust, fromMaybe, isNothing)
import Data.Bits ((.|.)) -- For color component flags if needed

-- 1. Define Vertex Structure and Storable Instance
data PositionColorVertex = PositionColorVertex
    { pcVertexX :: {-# UNPACK #-} !CFloat
    , pcVertexY :: {-# UNPACK #-} !CFloat
    , pcVertexZ :: {-# UNPACK #-} !CFloat
    , pcVertexR :: {-# UNPACK #-} !Word8
    , pcVertexG :: {-# UNPACK #-} !Word8
    , pcVertexB :: {-# UNPACK #-} !Word8
    , pcVertexA :: {-# UNPACK #-} !Word8
    } deriving (Show, Eq)

instance Storable PositionColorVertex where
    sizeOf ~_ = (3 * sizeOf (undefined :: CFloat)) + (4 * sizeOf (undefined :: Word8))
    alignment ~_ = alignment (undefined :: CFloat) -- Align based on the largest member (CFloat)

    peek ptr = do
        x <- peekByteOff ptr 0
        y <- peekByteOff ptr (sizeOf x)
        z <- peekByteOff ptr (sizeOf x + sizeOf y)
        r <- peekByteOff ptr (sizeOf x + sizeOf y + sizeOf z)
        g <- peekByteOff ptr (sizeOf x + sizeOf y + sizeOf z + sizeOf r)
        b <- peekByteOff ptr (sizeOf x + sizeOf y + sizeOf z + sizeOf r + sizeOf g)
        a <- peekByteOff ptr (sizeOf x + sizeOf y + sizeOf z + sizeOf r + sizeOf g + sizeOf b)
        return (PositionColorVertex x y z r g b a)

    poke ptr (PositionColorVertex x y z r g b a) = do
        let offR = sizeOf x + sizeOf y + sizeOf z
        let offG = offR + sizeOf r
        let offB = offG + sizeOf g
        let offA = offB + sizeOf b
        pokeByteOff ptr 0 x
        pokeByteOff ptr (sizeOf x) y
        pokeByteOff ptr (sizeOf x + sizeOf y) z
        pokeByteOff ptr offR r
        pokeByteOff ptr offG g
        pokeByteOff ptr offB b
        pokeByteOff ptr offA a

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

-- Reuse default states from previous example (ensure they are correct)
defaultShaderCreateInfo :: SDLGPUShaderCreateInfo
defaultShaderCreateInfo = SDLGPUShaderCreateInfo
    { shaderCode             = nullPtr, shaderCodeSize         = 0
    , shaderEntryPoint       = "", shaderFormat           = SDL_GPU_SHADERFORMAT_INVALID
    , shaderStage            = SDL_GPU_SHADERSTAGE_VERTEX, shaderNumSamplers        = 0
    , shaderNumStorageTextures = 0, shaderNumStorageBuffers  = 0
    , shaderNumUniformBuffers  = 0, shaderProps            = 0
    }

defaultColorTargetBlendState :: SDLGPUColorTargetBlendState
defaultColorTargetBlendState = SDLGPUColorTargetBlendState
    { writeMask = 0x0F, enableBlend = False, blendOp = SDL_GPU_BLENDOP_ADD
    , srcColorFactor = SDL_GPU_BLENDFACTOR_ONE, dstColorFactor = SDL_GPU_BLENDFACTOR_ZERO
    , alphaOp = SDL_GPU_BLENDOP_ADD, srcAlphaFactor = SDL_GPU_BLENDFACTOR_ONE
    , dstAlphaFactor = SDL_GPU_BLENDFACTOR_ZERO, enableColorWrite = True
    }

defaultStencilOpState :: SDLGPUStencilOpState
defaultStencilOpState = SDLGPUStencilOpState
    { stencilFailOp = SDL_GPU_STENCILOP_KEEP, stencilPassOp = SDL_GPU_STENCILOP_KEEP
    , stencilDepthFailOp = SDL_GPU_STENCILOP_KEEP, stencilCompareOp = SDL_GPU_COMPAREOP_ALWAYS
    }

defaultDepthStencilState :: SDLGPUDepthStencilState
defaultDepthStencilState = SDLGPUDepthStencilState
    { enableDepthTest = False, enableDepthWrite = False
    , depthStencilCompareOp = SDL_GPU_COMPAREOP_ALWAYS, enableStencilTest = False
    , backStencilState = defaultStencilOpState, frontStencilState = defaultStencilOpState
    , stencilCompareMask = 0xFF, stencilWriteMask = 0xFF
    }

defaultMultiSampleState :: SDLGPUMultisampleState
defaultMultiSampleState = SDLGPUMultisampleState
    { sampleCount = SDL_GPU_SAMPLECOUNT_1, sampleMask = 0
    , enableAlphaToCoverage = False, enableMask = False
    }

defaultRasterizerState :: SDLGPURasterizerState
defaultRasterizerState = SDLGPURasterizerState
    { fillMode = SDL_GPU_FILLMODE_FILL, cullMode = SDL_GPU_CULLMODE_NONE
    , frontFace = SDL_GPU_FRONTFACE_COUNTER_CLOCKWISE, enableDepthBias = False
    , depthBiasConstantFactor = 0.0, depthBiasClamp = 0.0
    , depthBiasSlopeFactor = 0.0, enableDepthClip = False
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
    bracket (createResources context) (releaseResources context) $ \case
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
                let vertexBufferSize = fromIntegral (length vertexData * sizeOf (head vertexData))
                maybeVertexBuffer <- createAndUploadVertexBuffer dev vertexBufferSize
                case maybeVertexBuffer of
                    Nothing -> do
                        sdlLog "Vertex buffer creation or upload failed."
                        sdlReleaseGPUGraphicsPipeline dev pipeline -- Clean up created pipeline
                        return Nothing
                    Just vertexBuffer -> do
                        sdlLog "Vertex buffer created and data uploaded successfully."
                        return $ Just (AppResources pipeline vertexBuffer)

    -- Helper to create the graphics pipeline
    createPipeline :: SDLGPUDevice -> SDLWindow -> SDLGPUShader -> SDLGPUShader -> IO (Maybe SDLGPUGraphicsPipeline)
    createPipeline dev win vertShader fragShader = do
        swapchainFormat <- sdlGetGPUSwapchainTextureFormat dev win

        -- Define Vertex Input State (matches PositionColorVertex)
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
                , attribOffset = fromIntegral $ sizeOf (undefined :: CFloat) * 3 -- Offset after the 3 floats
                }
              ]
            vertexBufferDesc =
              [ SDLGPUVertexBufferDescription
                { descSlot = 0 -- Binding slot 0
                , descPitch = fromIntegral $ sizeOf (undefined :: PositionColorVertex) -- Stride
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

    -- Helper to create vertex buffer and upload data
    createAndUploadVertexBuffer :: SDLGPUDevice -> CSize -> IO (Maybe SDLGPUBuffer)
    createAndUploadVertexBuffer dev bufferSize = do
        -- Create Vertex Buffer
        let vbCreateInfo = SDLGPUBufferCreateInfo
                             { bufferUsage = SDL_GPU_BUFFERUSAGE_VERTEX
                             , bufferSize = fromIntegral bufferSize -- Use fromIntegral
                             , bufferProps = 0
                             }
        maybeVertexBuffer <- sdlCreateGPUBuffer dev vbCreateInfo
        case maybeVertexBuffer of
            Nothing -> do
                err <- sdlGetError
                sdlLog $ "!!! Failed to create vertex buffer: " ++ err
                return Nothing
            Just vertexBuffer -> do
                -- Create Transfer Buffer
                let tbCreateInfo = SDLGPUTransferBufferCreateInfo
                                     { transferUsage = SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD
                                     , transferSize = fromIntegral bufferSize -- Use fromIntegral
                                     , transferProps = 0
                                     }
                bracket (sdlCreateGPUTransferBuffer dev tbCreateInfo) -- Manage transfer buffer lifetime
                        (maybe (return ()) (sdlReleaseGPUTransferBuffer dev)) $ \maybeTransferBuffer ->
                    case maybeTransferBuffer of
                        Nothing -> do
                            err <- sdlGetError
                            sdlLog $ "!!! Failed to create transfer buffer: " ++ err
                            sdlReleaseGPUBuffer dev vertexBuffer -- Clean up vertex buffer
                            return Nothing
                        Just transferBuffer -> do
                            -- Map, Write, Unmap (Use case for Maybe return)
                            maybeMappedPtr <- sdlMapGPUTransferBuffer dev transferBuffer False -- Assume IO (Maybe (Ptr ()))
                            case maybeMappedPtr of
                                Nothing -> do -- Check for Nothing to handle failure
                                    err <- sdlGetError
                                    sdlLog $ "!!! Failed to map transfer buffer: " ++ err
                                    sdlReleaseGPUBuffer dev vertexBuffer -- Clean up vertex buffer
                                    return Nothing
                                Just mappedPtr -> do -- Pattern match succeeds, mappedPtr is Ptr ()
                                    -- This block only runs if mapping succeeded.
                                    pokeArray (castPtr mappedPtr) vertexData -- castPtr works on Ptr ()
                                    sdlUnmapGPUTransferBuffer dev transferBuffer

                                    -- Upload via Command Buffer (continue inside this 'Just' block)
                                    maybeUploadSuccess <- bracket (sdlAcquireGPUCommandBuffer dev)
                                                                  (maybe (return ()) (void . sdlCancelGPUCommandBuffer)) $ \maybeCmdBuf -> -- Wrap in void
                                        case maybeCmdBuf of
                                            Nothing -> do
                                                err <- sdlGetError
                                                sdlLog $ "!!! Failed to acquire upload command buffer: " ++ err
                                                return (Just False) -- Indicate failure
                                            Just cmdBuf -> do
                                                -- Begin copy pass (Use case for Maybe return)
                                                maybeCopyPass <- sdlBeginGPUCopyPass cmdBuf -- Assume IO (Maybe SDLGPUCopyPass)
                                                case maybeCopyPass of
                                                    Nothing -> do -- Check for Nothing
                                                        err <- sdlGetError
                                                        sdlLog $ "!!! Failed to begin copy pass: " ++ err
                                                        return (Just False) -- Indicate failure
                                                    Just copyPass -> do -- Pattern match succeeds, copyPass is SDLGPUCopyPass
                                                        -- Copy pass succeeded, now use it
                                                        let srcLocation = SDLGPUTransferBufferLocation transferBuffer 0
                                                            dstRegion = SDLGPUBufferRegion vertexBuffer 0 (fromIntegral bufferSize) -- Use fromIntegral
                                                        -- These functions should expect SDLGPUCopyPass
                                                        sdlUploadToGPUBuffer copyPass srcLocation dstRegion False
                                                        sdlEndGPUCopyPass copyPass -- This too

                                                        -- Submit the command buffer
                                                        submitted <- sdlSubmitGPUCommandBuffer cmdBuf
                                                        unless submitted $ do
                                                            err <- sdlGetError
                                                            sdlLog $ "!!! Failed to submit upload command buffer: " ++ err
                                                        return (Just submitted) -- Return success/failure

                                    case maybeUploadSuccess of
                                        Just True -> return (Just vertexBuffer) -- Success!
                                        _         -> do -- Upload failed or command buffer error
                                             sdlReleaseGPUBuffer dev vertexBuffer -- Clean up vertex buffer
                                             return Nothing

    -- Action to release resources
    releaseResources :: Context -> Maybe AppResources -> IO ()
    releaseResources _ Nothing = return () -- Nothing to clean up
    releaseResources Context{..} (Just AppResources{..}) = do
        sdlLog "Releasing resources..."
        sdlReleaseGPUGraphicsPipeline contextDevice resPipeline
        sdlReleaseGPUBuffer contextDevice resVertexBuffer
        sdlLog "Resources released."


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
    renderFrameGPU context resources -- Pass context and resources

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
renderFrameGPU :: Context -> AppResources -> IO ()
renderFrameGPU Context{..} AppResources{..} = do
    -- 1. Acquire Command Buffer
    maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
    case maybeCmdbuf of
        Nothing -> do
            err <- sdlGetError
            sdlLog $ "Error: Failed to acquire command buffer: " ++ err
        Just cmdbuf -> do
            -- 2. Acquire Swapchain Texture
            maybeSwapchain <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
            case maybeSwapchain of
                Nothing -> sdlLog "Warning: Failed to acquire swapchain texture"
                Just (swapchainTexture, _w, _h) -> do
                    sdlLog $ "Acquired Swapchain Texture: " ++ show swapchainTexture ++ " Size: " ++ show _w ++ "x" ++ show _h
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
                    maybeRenderPass <- sdlBeginGPURenderPass cmdbuf [colorTargetInfo] Nothing
                    case maybeRenderPass of
                        Nothing -> do
                            err <- sdlGetError
                            sdlLog $ "Error: Failed to begin render pass: " ++ err
                        Just renderPass -> do
                            -- 5. Bind the graphics pipeline
                            sdlLog $ "Binding Pipeline: " ++ show resPipeline
                            sdlBindGPUGraphicsPipeline renderPass resPipeline

                            -- 6. Bind the vertex buffer (Use list directly)
                            let vertexBufferBinding = SDLGPUBufferBinding resVertexBuffer 0
                            sdlLog $ "Binding Vertex Buffer: " ++ show resVertexBuffer ++ " with offset " ++ show (bindingOffset vertexBufferBinding)
                            sdlBindGPUVertexBuffers renderPass 0 [vertexBufferBinding] -- Slot 0, list

                            -- 7. Draw the triangle (3 vertices, 1 instance)
                            sdlDrawGPUPrimitives renderPass 3 1 0 0

                            -- 8. End Render Pass
                            sdlEndGPURenderPass renderPass

                    -- 9. Submit Command Buffer (only if swapchain was acquired and render pass started)
                    submitted <- sdlSubmitGPUCommandBuffer cmdbuf
                    unless submitted $ do
                         err <- sdlGetError
                         sdlLog $ "Error: Failed to submit command buffer: " ++ err
