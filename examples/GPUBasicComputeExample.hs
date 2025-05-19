{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE LambdaCase           #-}

module Main where

import SDL
import GPUCommon

import Control.Monad (unless, when, void)
import Control.Exception (bracket, bracketOnError, finally)
import Foreign.Storable (sizeOf)
import Foreign.C.Types (CFloat)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.Maybe (isJust, fromJust, fromMaybe, isNothing)
import Data.Bits ((.|.))

import System.Exit (exitFailure, exitSuccess)

-- Vertex data (Triangle strip for a full-screen quad, UVs from 0,0 to 1,1)
-- C example uses 6 vertices to form two triangles.
vertexData :: [PositionTextureVertex]
vertexData =
    [ PositionTextureVertex (-1) (-1) 0   0 0 -- Bottom-left
    , PositionTextureVertex   1  (-1) 0   1 0 -- Bottom-right
    , PositionTextureVertex   1    1  0   1 1 -- Top-right

    , PositionTextureVertex (-1) (-1) 0   0 0 -- Bottom-left (repeat for second tri)
    , PositionTextureVertex   1    1  0   1 1 -- Top-right (repeat for second tri)
    , PositionTextureVertex (-1)   1  0   0 1 -- Top-left
    ]

-- AppResources
data AppResources = AppResources
    { resDrawPipeline  :: SDLGPUGraphicsPipeline
    , resTexture       :: SDLGPUTexture -- Filled by compute, sampled by graphics
    , resSampler       :: SDLGPUSampler
    , resVertexBuffer  :: SDLGPUBuffer
    } deriving Show

-- main
main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  maybeResult <- withContext "SDL3 Haskell GPU Compute Fill Texture" [] runAppGPU
  case maybeResult of
      Nothing -> do
          sdlLog "Application initialization failed."
          exitFailure
      Just _ -> do
          sdlLog "Application finished successfully."
          exitSuccess

-- runAppGPU
runAppGPU :: Context -> IO ()
runAppGPU context@Context{..} = do
    sdlLog "Base context initialized."
    bracket (createResources context)
            (releaseResources context)
            $ \case
            Nothing -> sdlLog "Failed to create resources. Exiting."
            Just _resources -> do -- Resources used directly in Draw now
                sdlLog "Resources created successfully. Entering event loop."
                eventLoopGPU context _resources -- Pass resources for Draw to access

-- createResources
createResources :: Context -> IO (Maybe AppResources)
createResources Context{ contextDevice = dev, contextWindow = win } = do
    sdlLog "--- Beginning Resource Creation ---"

    -- 1. Load Graphics Shaders
    sdlLog "Loading graphics shaders..."
    let gfxVertShaderInfo = defaultShaderCreateInfo -- No uniforms
    maybeGfxVertShader <- loadShader dev "TexturedQuad.vert" SDL_GPU_SHADERSTAGE_VERTEX gfxVertShaderInfo
    let gfxFragShaderInfo = defaultShaderCreateInfo { shaderNumSamplers = 1 } -- 1 sampler
    maybeGfxFragShader <- loadShader dev "TexturedQuad.frag" SDL_GPU_SHADERSTAGE_FRAGMENT gfxFragShaderInfo

    -- 2. Create Compute Pipeline
    sdlLog "Creating compute pipeline..."
    let computeCI = defaultComputePipelineCreateInfo
          { numReadWriteStorageTextures = 1 -- We write to one texture
          , threadCountX = 8
          , threadCountY = 8
          , threadCountZ = 1
          }
    maybeFillTexturePipeline <- createComputePipelineFromShader dev "FillTexture.comp" computeCI

    case (maybeGfxVertShader, maybeGfxFragShader, maybeFillTexturePipeline) of
        (Just gfxVert, Just gfxFrag, Just fillComp) -> do
            sdlLog "All shaders and compute pipeline loaded/created successfully."

            -- Get window size for texture dimensions
            maybeDims <- sdlGetWindowSizeInPixels win
            case maybeDims of
                Nothing -> sdlLog "Failed to get window size." >> return Nothing
                Just (wInt, hInt) -> do
                    let w = fromIntegral wInt :: Int
                    let h = fromIntegral hInt :: Int

                    -- Create the Graphics Pipeline
                    maybeDrawPipeline <- createDrawPipeline dev win gfxVert gfxFrag

                    -- Create Texture (target for compute, source for graphics)
                    let textureCI = (defaultTextureCreateInfo w h)
                          { texInfoUsage = SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_WRITE .|. SDL_GPU_TEXTUREUSAGE_SAMPLER }
                    maybeTexture <- sdlCreateGPUTexture dev textureCI

                    -- Create Sampler
                    let samplerCI = defaultSamplerCreateInfoRepeat
                    maybeSampler <- sdlCreateGPUSampler dev samplerCI

                    -- Create Vertex Buffer
                    (_, _, vertexDataSize) <- calculateBufferDataSize vertexData "Vertex"
                    maybeVertexBuffer <- createGPUBuffer dev SDL_GPU_BUFFERUSAGE_VERTEX vertexDataSize "ScreenQuadVB"

                    -- Bracket for resources that need compute pass
                    bracketOnError (sdlAcquireGPUCommandBuffer dev)
                                   cleanupCommandBuffer $ \maybeInitialCmdBuf ->
                      bracketOnError (createTransferBuffer dev vertexDataSize SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "VBTransfer")
                                     (cleanupTransferBuffer dev) $ \maybeVBTransfer ->

                        case (maybeDrawPipeline, maybeTexture, maybeSampler, maybeVertexBuffer, maybeInitialCmdBuf, maybeVBTransfer) of
                            (Just drawPipe, Just tex, Just samp, Just vb, Just initialCmdBuf, Just vbTransfer) -> do
                                sdlLog "Core resources created. Setting up initial data and compute pass..."

                                -- Upload Vertex Buffer Data
                                vbMapOk <- mapAndCopyBufferData dev vbTransfer vertexData [] 0 0
                                unless vbMapOk $ sdlLog "!!! VB map/copy failed."

                                if vbMapOk then do
                                    maybeCopyPass <- sdlBeginGPUCopyPass initialCmdBuf
                                    case maybeCopyPass of
                                        Nothing -> sdlLog "!!! Failed to begin copy pass for VB."
                                        Just cp -> do
                                            sdlLog "Copy pass begun for VB."
                                            let vbSrc = SDLGPUTransferBufferLocation vbTransfer 0
                                            let vbDst = SDLGPUBufferRegion vb 0 (fromIntegral vertexDataSize)
                                            sdlUploadToGPUBuffer cp vbSrc vbDst False
                                            sdlEndGPUCopyPass cp
                                            sdlLog "VB upload commands recorded and copy pass ended."

                                    -- Run Compute Shader to Fill Texture
                                    maybeComputePass <- sdlBeginGPUComputePass initialCmdBuf
                                                         [SDLGPUStorageTextureReadWriteBinding tex 0 0 False]
                                                         []
                                    case maybeComputePass of
                                        Nothing -> sdlLog "!!! Failed to begin compute pass."
                                        Just cp -> do
                                            sdlLog "Compute pass begun."
                                            sdlBindGPUComputePipeline cp fillComp
                                            sdlDispatchGPUCompute cp (fromIntegral w `div` 8) (fromIntegral h `div` 8) 1
                                            sdlEndGPUComputePass cp
                                            sdlLog "Compute dispatch commands recorded and compute pass ended."

                                    -- Submit initial command buffer
                                    submitted <- sdlSubmitGPUCommandBuffer initialCmdBuf
                                    unless submitted $ sdlLog "!!! Failed to submit initial command buffer."

                                    -- Wait for GPU to finish compute and uploads
                                    when submitted $ sdlWaitForGPUIdle dev >> sdlLog "Initial GPU work complete."

                                    -- Release transient resources
                                    sdlReleaseGPUComputePipeline dev fillComp
                                    -- Vertex transfer buffer released by its bracket

                                    if submitted then
                                        return $ Just AppResources { resDrawPipeline = drawPipe
                                                                   , resTexture = tex
                                                                   , resSampler = samp
                                                                   , resVertexBuffer = vb
                                                                 }
                                    else return Nothing
                                else return Nothing -- VB map failed
                            _ -> sdlLog "!!! Failed to create one or more core resources or command buffer." >> return Nothing

        _ -> do -- Shader or compute pipeline loading failed
            sdlLog "!!! Failed to load shaders or create compute pipeline."
            maybe (return ()) (sdlReleaseGPUShader dev) maybeGfxVertShader
            maybe (return ()) (sdlReleaseGPUShader dev) maybeGfxFragShader
            maybe (return ()) (sdlReleaseGPUComputePipeline dev) maybeFillTexturePipeline
            return Nothing

-- Helper to create the graphics pipeline for drawing the texture
createDrawPipeline :: SDLGPUDevice -> SDLWindow -> SDLGPUShader -> SDLGPUShader -> IO (Maybe SDLGPUGraphicsPipeline)
createDrawPipeline dev win vertShader fragShader = do
    swapchainFormat <- sdlGetGPUSwapchainTextureFormat dev win
    let vertexSize = sizeOf (undefined :: PositionTextureVertex)
    let texCoordOffset = sizeOf (undefined :: CFloat) * 3
    let vertexAttributes =
          [ SDLGPUVertexAttribute 0 0 SDL_GPU_VERTEXELEMENTFORMAT_FLOAT3 0
          , SDLGPUVertexAttribute 1 0 SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2 (fromIntegral texCoordOffset)
          ]
        vertexBufferDesc = [ SDLGPUVertexBufferDescription 0 (fromIntegral vertexSize) SDL_GPU_VERTEXINPUTRATE_VERTEX 0 ]
        vertexInputState = SDLGPUVertexInputState vertexBufferDesc vertexAttributes
    let colorTargetDesc = defaultColorTargetDescription { targetFormat = swapchainFormat } -- Default blend state (no blend)
        targetInfo = SDLGPUGraphicsPipelineTargetInfo [colorTargetDesc] SDL_GPU_TEXTUREFORMAT_INVALID False
        pipelineCI = (defaultGraphicsPipelineCreateInfo vertShader fragShader swapchainFormat)
            { vertexInputState = vertexInputState
            , targetInfo = targetInfo
            }
    sdlCreateGPUGraphicsPipeline dev pipelineCI

releaseResources :: Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources Context{..} (Just AppResources{..}) = do
    sdlLog "--> Releasing AppResources..."
    sdlReleaseGPUGraphicsPipeline contextDevice resDrawPipeline
    sdlReleaseGPUTexture contextDevice resTexture
    sdlReleaseGPUSampler contextDevice resSampler
    sdlReleaseGPUBuffer contextDevice resVertexBuffer
    sdlLog "<-- AppResources Released."

eventLoopGPU :: Context -> AppResources -> IO ()
eventLoopGPU context resources = do
  sdlPumpEvents
  shouldQuitRef <- newIORef False
  processEventsGPU shouldQuitRef
  shouldQuit <- readIORef shouldQuitRef
  unless shouldQuit $ do
    renderFrameGPU context resources
    eventLoopGPU context resources

processEventsGPU :: IORef Bool -> IO ()
processEventsGPU shouldQuitRef = do
    maybeEvent <- sdlPollEvent
    case maybeEvent of
        Nothing -> return ()
        Just event -> do
            quit <- handleEventGPU event
            when quit $ writeIORef shouldQuitRef True
            processEventsGPU shouldQuitRef

handleEventGPU :: SDLEvent -> IO Bool
handleEventGPU event = case event of
  SDLEventQuit _ -> sdlLog "Quit event received." >> return True
  SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ scancode _ _ _ down _) | down -> do
    case scancode of
        SDL_SCANCODE_Q -> return True
        _ -> return False
  _ -> return False

renderFrameGPU :: Context -> AppResources -> IO ()
renderFrameGPU Context{..} AppResources{..} = do
    maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
    case maybeCmdbuf of
        Nothing -> sdlLog "Error: Failed to acquire render command buffer."
        Just cmdbuf -> do
            maybeSwapchain <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
            case maybeSwapchain of
                Nothing -> do
                    sdlLog "Error: Failed to acquire swapchain texture."
                    void (sdlSubmitGPUCommandBuffer cmdbuf `finally` return ())
                Just (swapchainTexture, _, _) -> do
                    let clearColor = SDLFColor 0.0 0.0 0.0 1.0 -- Black
                    let colorTargetInfo = defaultColorTargetInfo
                            { texture = swapchainTexture
                            , loadOp = SDL_GPU_LOADOP_CLEAR
                            , storeOp = SDL_GPU_STOREOP_STORE
                            , clearColor = clearColor
                            , targetCycle = False
                            }
                    bracket (sdlBeginGPURenderPass cmdbuf [colorTargetInfo] Nothing)
                            cleanupMaybeRenderPass $ \case
                            Nothing -> sdlLog "Error: Failed to begin render pass."
                            Just renderPass -> do
                                sdlBindGPUGraphicsPipeline renderPass resDrawPipeline
                                let vbBinding = SDLGPUBufferBinding resVertexBuffer 0
                                sdlBindGPUVertexBuffers renderPass 0 [vbBinding]
                                let texSamplerBinding = SDLGPUTextureSamplerBinding resTexture resSampler
                                sdlBindGPUFragmentSamplers renderPass 0 [texSamplerBinding]
                                -- Draw 6 vertices, forming two triangles for the quad
                                sdlDrawGPUPrimitives renderPass 6 1 0 0

                    submitted <- sdlSubmitGPUCommandBuffer cmdbuf
                    unless submitted $ sdlLog "Error: Failed to submit render command buffer."

defaultSamplerCreateInfoRepeat :: SDLGPUSamplerCreateInfo
defaultSamplerCreateInfoRepeat = (defaultSamplerCreateInfo SDL_GPU_FILTER_NEAREST)
    { samplerAddressModeU = SDL_GPU_SAMPLERADDRESSMODE_REPEAT
    , samplerAddressModeV = SDL_GPU_SAMPLERADDRESSMODE_REPEAT
    }
