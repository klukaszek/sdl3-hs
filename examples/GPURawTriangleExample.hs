{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{-|
Example     : GPURawTriangle
Description : SDL Example: Drawing a single triangle with configurable pipeline state
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Based on the SDL_gpu_examples/RawTriangle C example.
Demonstrates:
- Loading vertex and fragment shaders.
- Creating graphics pipelines (fill and wireframe).
- Binding pipelines and drawing primitives.
- Using viewport and scissor rectangles.
- Handling keyboard input to toggle rendering states.
|-}

module Main where

import SDL
import qualified SDL.Properties as Props
import GPUCommon        -- Import common init/quit and shader loading

import Control.Monad (unless, when, void)
import Control.Exception (bracket, finally) -- For resource management
import System.Exit (exitFailure, exitSuccess)
import Foreign.Ptr (nullPtr)
import Foreign.C.Types (CFloat) -- For Viewport/Color depths/components
import Data.IORef       -- For mutable state (toggles)
import Data.Word (Word32, Word64)
import Text.Printf (printf)
import Data.Maybe (isJust, fromJust, fromMaybe, isNothing)

-- State Toggles (Mutable)
data AppState = AppState
    { appUseWireframe :: IORef Bool
    , appUseSmallViewport :: IORef Bool
    , appUseScissorRect :: IORef Bool
    }

-- Constant Rendering Parameters
smallViewport :: SDLGPUViewport
smallViewport = SDLGPUViewport
    { gpuViewportX = 160
    , gpuViewportY = 120
    , gpuViewportW = 320
    , gpuViewportH = 240
    , gpuViewportMinDepth = 0.1
    , gpuViewportMaxDepth = 1.0
    }

scissorRect :: SDLRect -- SDLRect uses CInt by default, but SetScissor takes SDLRect CFloat
scissorRect = SDLRect
    { rectX = 320 -- Use CFloat literals
    , rectY = 240
    , rectW = 320
    , rectH = 240
    }

-- Pipeline Resources (Managed)
data PipelineResources = PipelineResources
    { resFillPipeline :: SDLGPUGraphicsPipeline
    , resLinePipeline :: SDLGPUGraphicsPipeline
    }


defaultShaderCreateInfo :: SDLGPUShaderCreateInfo
defaultShaderCreateInfo = SDLGPUShaderCreateInfo
    { shaderCode             = nullPtr
    , shaderCodeSize         = 0
    , shaderEntryPoint       = "" -- loadShader will override
    , shaderFormat           = SDL_GPU_SHADERFORMAT_INVALID -- loadShader will override
    , shaderStage            = SDL_GPU_SHADERSTAGE_VERTEX -- loadShader will override
    , shaderNumSamplers        = 0 -- Sensible default
    , shaderNumStorageTextures = 0 -- Sensible default
    , shaderNumStorageBuffers  = 0 -- Sensible default
    , shaderNumUniformBuffers  = 0 -- Sensible default
    , shaderProps            =  0 -- Default properties
    }

defaultVertexInputState :: SDLGPUVertexInputState
defaultVertexInputState = SDLGPUVertexInputState
    { inputVertexBuffers = []
    , inputVertexAttribs = []
    }

-- | Default Blend State (Opaque)
defaultColorTargetBlendState :: SDLGPUColorTargetBlendState
defaultColorTargetBlendState = SDLGPUColorTargetBlendState
    { writeMask = 0x0F
    , enableBlend = False
    , blendOp = SDL_GPU_BLENDOP_ADD
    , srcColorFactor = SDL_GPU_BLENDFACTOR_ONE
    , dstColorFactor = SDL_GPU_BLENDFACTOR_ZERO
    , alphaOp = SDL_GPU_BLENDOP_ADD
    , srcAlphaFactor = SDL_GPU_BLENDFACTOR_ONE
    , dstAlphaFactor = SDL_GPU_BLENDFACTOR_ZERO
    , enableColorWrite = True
    }

defaultStencilOpState :: SDLGPUStencilOpState
defaultStencilOpState = SDLGPUStencilOpState
    { stencilFailOp       = SDL_GPU_STENCILOP_KEEP
    , stencilPassOp       = SDL_GPU_STENCILOP_KEEP
    , stencilDepthFailOp  = SDL_GPU_STENCILOP_KEEP
    , stencilCompareOp    = SDL_GPU_COMPAREOP_ALWAYS
    }

-- | Default Depth/Stencil State (Disabled) using your definitions
defaultDepthStencilState :: SDLGPUDepthStencilState
defaultDepthStencilState = SDLGPUDepthStencilState
    { enableDepthTest   = False -- Explicitly disable depth testing
    , enableDepthWrite  = False -- Don't write to depth buffer if testing is off
    , depthStencilCompareOp = SDL_GPU_COMPAREOP_ALWAYS -- Compare op for depth (doesn't matter when disabled)
    , enableStencilTest = False -- Explicitly disable stencil testing
    , backStencilState  = defaultStencilOpState -- Use the helper for back face
    , frontStencilState = defaultStencilOpState -- Use the helper for front face
    , stencilCompareMask = 0xFF -- Default read mask (Word8)
    , stencilWriteMask   = 0xFF -- Default write mask (Word8)
    }


-- | Default MultiSample State
defaultMultiSampleState :: SDLGPUMultisampleState
defaultMultiSampleState = SDLGPUMultisampleState
    { sampleCount = SDL_GPU_SAMPLECOUNT_1
    , sampleMask = 0 -- Default mask
    , enableAlphaToCoverage = False
    , enableMask = False
    }

-- | Default Rasterizer State
defaultRasterizerState :: SDLGPURasterizerState
defaultRasterizerState = SDLGPURasterizerState
    { fillMode = SDL_GPU_FILLMODE_FILL
    , cullMode = SDL_GPU_CULLMODE_NONE
    , frontFace = SDL_GPU_FRONTFACE_COUNTER_CLOCKWISE
    , enableDepthBias = False
    , depthBiasConstantFactor = 0.0
    , depthBiasClamp = 0.0
    , depthBiasSlopeFactor = 0.0
    , enableDepthClip = False
    }

main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  -- Use withContext for base SDL/GPU/Window setup
  maybeResult <- withContext "SDL3 Haskell GPU Raw Triangle" [sdlWindowResizable] runAppGPU
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

    -- Bracket pattern to manage pipeline resources
    bracket (createPipelines context) (releasePipelines context) $ \case
            Nothing -> sdlLog "Failed to create pipelines. Exiting."
            Just pipelines -> do
                sdlLog "Pipelines created successfully."
                sdlLog "Press Left to toggle wireframe mode"
                sdlLog "Press Down to toggle small viewport"
                sdlLog "Press Right to toggle scissor rect"

                -- Create mutable state refs
                state <- AppState <$> newIORef False <*> newIORef False <*> newIORef False

                -- Start event loop with initial time, passing context, pipelines, and state
                startTime <- sdlGetPerformanceCounter
                freq <- sdlGetPerformanceFrequency
                deltaTimeRef <- newIORef 0.0
                eventLoopGPU context pipelines state startTime freq deltaTimeRef
                -- Pipeline cleanup is handled by the bracket's release action
                -- Base context cleanup is handled by withContext

  where
    -- Action to create pipelines (and load shaders)
    createPipelines :: Context -> IO (Maybe PipelineResources)
    createPipelines ctx@Context{ contextDevice = dev, contextWindow = win } = do
        -- Load Shaders (using GPUCommon helper)
        maybeVertShader <- loadShader dev "RawTriangle.vert" SDL_GPU_SHADERSTAGE_VERTEX defaultShaderCreateInfo
        maybeFragShader <- loadShader dev "SolidColor.frag" SDL_GPU_SHADERSTAGE_FRAGMENT defaultShaderCreateInfo

        -- Ensure shaders loaded before proceeding
        case (maybeVertShader, maybeFragShader) of
            (Just vertShader, Just fragShader) -> do
                sdlLog "Shaders loaded successfully."
                -- Get swapchain format for pipeline
                swapchainFormat <- sdlGetGPUSwapchainTextureFormat dev win

                -- Define pipeline create info (using defaults)
                let colorTargetDesc = SDLGPUColorTargetDescription
                        { targetFormat = swapchainFormat
                        , targetBlendState = defaultColorTargetBlendState
                        }
                    targetInfo = SDLGPUGraphicsPipelineTargetInfo
                        { colorTargets = [colorTargetDesc]
                        , depthStencilFormat = SDL_GPU_TEXTUREFORMAT_INVALID
                        , hasDepthStencil = False
                        }
                    basePipelineCI = SDLGPUGraphicsPipelineCreateInfo
                        { vertexShader = vertShader
                        , vertexInputState = defaultVertexInputState
                        , fragmentShader = fragShader
                        , primitiveType = SDL_GPU_PRIMITIVETYPE_TRIANGLELIST
                        , multisampleState = defaultMultiSampleState
                        , rasterizerState = defaultRasterizerState
                        , targetInfo = targetInfo
                        , depthStencilState = defaultDepthStencilState
                        , props = 0
                        }

                -- Create Fill Pipeline & GET ERROR ON FAILURE
                sdlLog "Attempting to create Fill pipeline..."
                let fillRasterState = defaultRasterizerState { fillMode = SDL_GPU_FILLMODE_FILL }
                maybeFillPipeline <- sdlCreateGPUGraphicsPipeline dev (basePipelineCI { rasterizerState = fillRasterState })
                fillError <- if isNothing maybeFillPipeline
                             then Just <$> sdlGetError -- Get error if creation failed
                             else return Nothing      -- No error if succeeded

                -- Create Line Pipeline & GET ERROR ON FAILURE
                sdlLog "Attempting to create Line pipeline..."
                let lineRasterState = defaultRasterizerState { fillMode = SDL_GPU_FILLMODE_LINE }
                maybeLinePipeline <- sdlCreateGPUGraphicsPipeline dev (basePipelineCI { rasterizerState = lineRasterState })
                lineError <- if isNothing maybeLinePipeline
                             then Just <$> sdlGetError -- Get error if creation failed
                             else return Nothing      -- No error if succeeded

                -- Release shaders (no longer needed after pipeline creation)
                sdlReleaseGPUShader dev vertShader
                sdlReleaseGPUShader dev fragShader
                sdlLog "Shaders released."

                -- Check pipeline creation results and log specific errors
                case (maybeFillPipeline, maybeLinePipeline) of
                    (Just fillP, Just lineP) ->
                        return $ Just (PipelineResources fillP lineP)
                    _ -> do
                        sdlLog "!!! Failed to create one or both pipelines."
                        when (isJust fillError) $ sdlLog $ "  - Fill Pipeline Error: " ++ fromJust fillError
                        when (isJust lineError) $ sdlLog $ "  - Line Pipeline Error: " ++ fromJust lineError
                        -- Clean up potentially created pipeline if one succeeded and other failed
                        maybe (return ()) (sdlReleaseGPUGraphicsPipeline dev) maybeFillPipeline
                        maybe (return ()) (sdlReleaseGPUGraphicsPipeline dev) maybeLinePipeline
                        return Nothing
            _ -> do
                sdlLog "Failed to load one or both shaders."
                -- Clean up potentially loaded shader if one succeeded
                maybe (return ()) (sdlReleaseGPUShader dev) maybeVertShader
                maybe (return ()) (sdlReleaseGPUShader dev) maybeFragShader
                return Nothing

    -- Action to release pipelines
    releasePipelines :: Context -> Maybe PipelineResources -> IO ()
    releasePipelines _ Nothing = return () -- Nothing to clean up
    releasePipelines Context{..} (Just PipelineResources{..}) = do
        sdlLog "Releasing pipelines..."
        sdlReleaseGPUGraphicsPipeline contextDevice resFillPipeline
        sdlReleaseGPUGraphicsPipeline contextDevice resLinePipeline
        sdlLog "Pipelines released."


-- | Main event loop
eventLoopGPU :: Context -> PipelineResources -> AppState -> Word64 -> Word64 -> IORef Double -> IO ()
eventLoopGPU context pipelines state lastTime freq deltaTimeRef = do
  currentTime <- sdlGetPerformanceCounter
  let deltaTime = fromIntegral (currentTime - lastTime) / fromIntegral freq
  writeIORef deltaTimeRef (deltaTime * 1000.0) -- Store dt in ms
  dtMs <- readIORef deltaTimeRef
  sdlLog $ printf "DT: %.3fms" dtMs
   
  sdlPumpEvents
  shouldQuitRef <- newIORef False
  -- Pass state to event processor so it can modify the IORefs
  processEventsGPU shouldQuitRef deltaTimeRef state

  shouldQuit <- readIORef shouldQuitRef
  unless shouldQuit $ do
    -- *** GPU Rendering ***
    renderFrameGPU context pipelines state -- Pass context, pipelines, and state

    -- Continue loop
    eventLoopGPU context pipelines state currentTime freq deltaTimeRef

-- | Process all pending events, modifying state based on input
processEventsGPU :: IORef Bool -> IORef Double -> AppState -> IO ()
processEventsGPU shouldQuitRef deltaTimeRef state = do
    maybeEvent <- sdlPollEvent
    case maybeEvent of
        Nothing -> return () -- No more events
        Just event -> do
            -- Handle event and check if it requests quitting
            quit <- handleEventGPU event deltaTimeRef state
            when quit $ writeIORef shouldQuitRef True
            processEventsGPU shouldQuitRef deltaTimeRef state -- Process next event

-- | Handle a single SDL event, modifying state IORefs
handleEventGPU :: SDLEvent -> IORef Double -> AppState -> IO Bool
handleEventGPU event deltaTimeRef AppState{..} = case event of
  SDLEventQuit _ -> do
    sdlLog "Quit event received."
    return True
  SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ scancode _ _ _ down _) | down -> do
    dtMs <- readIORef deltaTimeRef
    sdlLog $ printf "Key '%s' pressed. Delta Time: %.3f ms" (show scancode) dtMs
    case scancode of
      SDL_SCANCODE_Q -> return True -- Quit on Q
      SDL_SCANCODE_LEFT -> do
          modifyIORef' appUseWireframe not -- Toggle wireframe state
          sdlLog . ("Wireframe mode: " ++) . show =<< readIORef appUseWireframe
          return False
      SDL_SCANCODE_DOWN -> do
          modifyIORef' appUseSmallViewport not -- Toggle viewport state
          sdlLog . ("Small viewport: " ++) . show =<< readIORef appUseSmallViewport
          return False
      SDL_SCANCODE_RIGHT -> do
          modifyIORef' appUseScissorRect not -- Toggle scissor state
          sdlLog . ("Scissor rect: " ++) . show =<< readIORef appUseScissorRect
          return False
      _ -> return False -- Other key press, don't quit
  _ -> return False -- Other event, don't quit


-- | Render a frame using the appropriate pipeline and state
renderFrameGPU :: Context -> PipelineResources -> AppState -> IO ()
renderFrameGPU Context{..} PipelineResources{..} AppState{..} = do
    -- Read current state flags
    useWireframe <- readIORef appUseWireframe
    useSmallVP <- readIORef appUseSmallViewport
    useScissor <- readIORef appUseScissorRect

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
                Nothing -> sdlLog "Warning: Failed to acquire swapchain texture" -- Don't submit if acquire fails
                Just (swapchainTexture, _w, _h) -> do
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
                            -- 5. Bind correct pipeline
                            let currentPipeline = if useWireframe then resLinePipeline else resFillPipeline
                            sdlBindGPUGraphicsPipeline renderPass currentPipeline

                            -- 6. Set viewport (conditionally)
                            when useSmallVP $
                                sdlSetGPUViewport renderPass smallViewport

                            -- 7. Set scissor rect (conditionally)
                            when useScissor $
                                sdlSetGPUScissor renderPass scissorRect

                            -- 8. Draw the triangle (3 vertices, 1 instance)
                            sdlDrawGPUPrimitives renderPass 3 1 0 0

                            -- 9. End Render Pass
                            sdlEndGPURenderPass renderPass

                    -- 10. Submit Command Buffer (only if swapchain was acquired and render pass started)
                    submitted <- sdlSubmitGPUCommandBuffer cmdbuf
                    unless submitted $ do
                         err <- sdlGetError
                         sdlLog $ "Error: Failed to submit command buffer: " ++ err


