{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Example     : GPUDepthSampler
-- Description : Demonstrates depth buffer sampling and post-processing.
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Based on the SDL_gpu_examples/DepthSampler C example.
-- This example demonstrates:
-- - Creating off-screen color and depth textures
-- - First pass: Render a 3D scene with depth testing
-- - Second pass: Sample color/depth to apply outline effect
-- - Animated camera orbiting around a cube
module Main where

import Control.Exception (bracket)
import Control.Monad (unless, void, when)
import Data.Bits ((.|.))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, poke, sizeOf)
import GPUCommon
import Linear hiding (transpose)
import qualified Linear
import SDL3 hiding (cos, pi, sin)
import System.Exit (exitFailure, exitSuccess)
import System.IO.Unsafe (unsafePerformIO)

-- | Application resources
data AppResources = AppResources
  { resScenePipeline :: SDLGPUGraphicsPipeline,
    resEffectPipeline :: SDLGPUGraphicsPipeline,
    resSceneVertexBuffer :: SDLGPUBuffer,
    resSceneIndexBuffer :: SDLGPUBuffer,
    resEffectVertexBuffer :: SDLGPUBuffer,
    resEffectIndexBuffer :: SDLGPUBuffer,
    resSceneColorTexture :: SDLGPUTexture,
    resSceneDepthTexture :: SDLGPUTexture,
    resEffectSampler :: SDLGPUSampler,
    resSceneWidth :: Int,
    resSceneHeight :: Int
  }
  deriving (Show)

-- | Vertex with position and color
data PositionColorVertex = PositionColorVertex
  { pcvX :: Float,
    pcvY :: Float,
    pcvZ :: Float,
    pcvR :: Word8,
    pcvG :: Word8,
    pcvB :: Word8,
    pcvA :: Word8
  }

-- | main
main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion
  maybeResult <- withContext "SDL3 Haskell GPU Depth Sampler" [] runAppGPU
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
        timeRef <- newIORef (0.0 :: Float)
        startTime <- sdlGetPerformanceCounter
        freq <- sdlGetPerformanceFrequency
        eventLoopGPU context resources timeRef startTime freq

-- | createResources
createResources :: Context -> IO (Maybe AppResources)
createResources Context {..} = do
  sdlLog "--- Beginning Resource Creation ---"

  -- Load shaders for scene pass
  let sceneVertInfo = defaultShaderCreateInfo {shaderNumUniformBuffers = 1}
  let sceneFragInfo = defaultShaderCreateInfo {shaderNumUniformBuffers = 1}

  maybeSceneVertShader <- loadShader contextDevice "PositionColorTransform.vert" SDL_GPU_SHADERSTAGE_VERTEX sceneVertInfo
  maybeSceneFragShader <- loadShader contextDevice "SolidColorDepth.frag" SDL_GPU_SHADERSTAGE_FRAGMENT sceneFragInfo

  -- Load shaders for effect pass
  let effectVertInfo = defaultShaderCreateInfo
  let effectFragInfo = defaultShaderCreateInfo {shaderNumSamplers = 2, shaderNumUniformBuffers = 1}

  maybeEffectVertShader <- loadShader contextDevice "TexturedQuad.vert" SDL_GPU_SHADERSTAGE_VERTEX effectVertInfo
  maybeEffectFragShader <- loadShader contextDevice "DepthOutline.frag" SDL_GPU_SHADERSTAGE_FRAGMENT effectFragInfo

  case (maybeSceneVertShader, maybeSceneFragShader, maybeEffectVertShader, maybeEffectFragShader) of
    (Just sceneVert, Just sceneFrag, Just effectVert, Just effectFrag) -> do
      swapchainFormat <- sdlGetGPUSwapchainTextureFormat contextDevice contextWindow

      -- Get window size for scene textures (1/4 scale for pixelated look)
      (w, h) <-
        sdlGetWindowSizeInPixels contextWindow >>= \case
          Just sz -> return sz
          Nothing -> return (800, 600)
      let sceneW = w `div` 4
          sceneH = h `div` 4

      -- Create scene pipeline (with depth testing)
      let sceneVertexBufferDesc =
            SDLGPUVertexBufferDescription
              { descSlot = 0,
                descPitch = 16, -- 3 floats + 4 bytes color = 16 bytes
                descInputRate = SDL_GPU_VERTEXINPUTRATE_VERTEX,
                descInstanceStepRate = 0
              }
      let scenePosAttr =
            SDLGPUVertexAttribute
              { attribLocation = 0,
                attribSlot = 0,
                attribFormat = SDL_GPU_VERTEXELEMENTFORMAT_FLOAT3,
                attribOffset = 0
              }
      let sceneColorAttr =
            SDLGPUVertexAttribute
              { attribLocation = 1,
                attribSlot = 0,
                attribFormat = SDL_GPU_VERTEXELEMENTFORMAT_UBYTE4_NORM,
                attribOffset = 12
              }
      let sceneVertexInputState =
            SDLGPUVertexInputState
              { inputVertexBuffers = [sceneVertexBufferDesc],
                inputVertexAttribs = [scenePosAttr, sceneColorAttr]
              }

      let sceneDepthStencilState =
            SDLGPUDepthStencilState
              { enableDepthTest = True,
                enableDepthWrite = True,
                enableStencilTest = False,
                depthStencilCompareOp = SDL_GPU_COMPAREOP_LESS,
                backStencilState = defaultStencilOpState,
                frontStencilState = defaultStencilOpState,
                stencilCompareMask = 0xFF,
                stencilWriteMask = 0xFF
              }

      let sceneColorTargetDesc =
            SDLGPUColorTargetDescription SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM defaultColorTargetBlendState
      let sceneTargetInfo =
            SDLGPUGraphicsPipelineTargetInfo [sceneColorTargetDesc] SDL_GPU_TEXTUREFORMAT_D16_UNORM True

      let sceneBasePipeline = defaultGraphicsPipelineCreateInfo sceneVert sceneFrag SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM
          scenePipelineCI =
            sceneBasePipeline
              { vertexInputState = sceneVertexInputState,
                depthStencilState = sceneDepthStencilState,
                targetInfo = sceneTargetInfo
              }

      maybeScenePipeline <- sdlCreateGPUGraphicsPipeline contextDevice scenePipelineCI

      -- Create effect pipeline (simple fullscreen quad)
      let effectVertexBufferDesc =
            SDLGPUVertexBufferDescription
              { descSlot = 0,
                descPitch = 20, -- 3 floats pos + 2 floats uv
                descInputRate = SDL_GPU_VERTEXINPUTRATE_VERTEX,
                descInstanceStepRate = 0
              }
      let effectPosAttr =
            SDLGPUVertexAttribute
              { attribLocation = 0,
                attribSlot = 0,
                attribFormat = SDL_GPU_VERTEXELEMENTFORMAT_FLOAT3,
                attribOffset = 0
              }
      let effectUVAttr =
            SDLGPUVertexAttribute
              { attribLocation = 1,
                attribSlot = 0,
                attribFormat = SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2,
                attribOffset = 12
              }
      let effectVertexInputState =
            SDLGPUVertexInputState
              { inputVertexBuffers = [effectVertexBufferDesc],
                inputVertexAttribs = [effectPosAttr, effectUVAttr]
              }

      let effectBlendState =
            defaultColorTargetBlendState
              { enableBlend = True,
                srcColorFactor = SDL_GPU_BLENDFACTOR_ONE,
                dstColorFactor = SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA,
                blendOp = SDL_GPU_BLENDOP_ADD,
                srcAlphaFactor = SDL_GPU_BLENDFACTOR_ONE,
                dstAlphaFactor = SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA,
                alphaOp = SDL_GPU_BLENDOP_ADD
              }
      let effectColorTargetDesc =
            SDLGPUColorTargetDescription swapchainFormat effectBlendState
      let effectTargetInfo =
            SDLGPUGraphicsPipelineTargetInfo [effectColorTargetDesc] SDL_GPU_TEXTUREFORMAT_INVALID False

      let effectBasePipeline = defaultGraphicsPipelineCreateInfo effectVert effectFrag swapchainFormat
          effectPipelineCI =
            effectBasePipeline
              { vertexInputState = effectVertexInputState,
                targetInfo = effectTargetInfo
              }

      maybeEffectPipeline <- sdlCreateGPUGraphicsPipeline contextDevice effectPipelineCI

      sdlReleaseGPUShader contextDevice sceneVert
      sdlReleaseGPUShader contextDevice sceneFrag
      sdlReleaseGPUShader contextDevice effectVert
      sdlReleaseGPUShader contextDevice effectFrag

      case (maybeScenePipeline, maybeEffectPipeline) of
        (Just scenePipeline, Just effectPipeline) -> do
          -- Create scene color texture
          maybeColorTex <-
            sdlCreateGPUTexture contextDevice $
              SDLGPUTextureCreateInfo
                { texInfoType = SDL_GPU_TEXTURETYPE_2D,
                  texInfoFormat = SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM,
                  texInfoUsage = SDL_GPU_TEXTUREUSAGE_SAMPLER .|. SDL_GPU_TEXTUREUSAGE_COLOR_TARGET,
                  texInfoWidth = fromIntegral sceneW,
                  texInfoHeight = fromIntegral sceneH,
                  texInfoLayerCountOrDepth = 1,
                  texInfoNumLevels = 1,
                  texInfoSampleCount = SDL_GPU_SAMPLECOUNT_1,
                  texInfoProps = 0
                }

          -- Create scene depth texture
          maybeDepthTex <-
            sdlCreateGPUTexture contextDevice $
              SDLGPUTextureCreateInfo
                { texInfoType = SDL_GPU_TEXTURETYPE_2D,
                  texInfoFormat = SDL_GPU_TEXTUREFORMAT_D16_UNORM,
                  texInfoUsage = SDL_GPU_TEXTUREUSAGE_SAMPLER .|. SDL_GPU_TEXTUREUSAGE_DEPTH_STENCIL_TARGET,
                  texInfoWidth = fromIntegral sceneW,
                  texInfoHeight = fromIntegral sceneH,
                  texInfoLayerCountOrDepth = 1,
                  texInfoNumLevels = 1,
                  texInfoSampleCount = SDL_GPU_SAMPLECOUNT_1,
                  texInfoProps = 0
                }

          -- Create sampler
          maybeSampler <-
            sdlCreateGPUSampler contextDevice $
              SDLGPUSamplerCreateInfo
                { samplerMinFilter = SDL_GPU_FILTER_NEAREST,
                  samplerMagFilter = SDL_GPU_FILTER_NEAREST,
                  samplerMipmapMode = SDL_GPU_SAMPLERMIPMAPMODE_NEAREST,
                  samplerAddressModeU = SDL_GPU_SAMPLERADDRESSMODE_REPEAT,
                  samplerAddressModeV = SDL_GPU_SAMPLERADDRESSMODE_REPEAT,
                  samplerAddressModeW = SDL_GPU_SAMPLERADDRESSMODE_REPEAT,
                  samplerMipLodBias = 0,
                  samplerMaxAnisotropy = 1,
                  samplerCompareOp = SDL_GPU_COMPAREOP_NEVER,
                  samplerMinLod = 0,
                  samplerMaxLod = 1000,
                  samplerEnableAnisotropy = False,
                  samplerEnableCompare = False,
                  samplerProps = 0
                }

          -- Create buffers
          maybeSceneVB <- sdlCreateGPUBuffer contextDevice $ SDLGPUBufferCreateInfo SDL_GPU_BUFFERUSAGE_VERTEX (24 * 16) 0
          maybeSceneIB <- sdlCreateGPUBuffer contextDevice $ SDLGPUBufferCreateInfo SDL_GPU_BUFFERUSAGE_INDEX (36 * 2) 0
          maybeEffectVB <- sdlCreateGPUBuffer contextDevice $ SDLGPUBufferCreateInfo SDL_GPU_BUFFERUSAGE_VERTEX (4 * 20) 0
          maybeEffectIB <- sdlCreateGPUBuffer contextDevice $ SDLGPUBufferCreateInfo SDL_GPU_BUFFERUSAGE_INDEX (6 * 2) 0

          case (maybeColorTex, maybeDepthTex, maybeSampler, maybeSceneVB, maybeSceneIB, maybeEffectVB, maybeEffectIB) of
            (Just colorTex, Just depthTex, Just sampler, Just sceneVB, Just sceneIB, Just effectVB, Just effectIB) -> do
              uploadSuccess <- uploadBufferData contextDevice sceneVB sceneIB effectVB effectIB

              if uploadSuccess
                then do
                  sdlLog "--- Resource Creation Successful ---"
                  return $
                    Just
                      AppResources
                        { resScenePipeline = scenePipeline,
                          resEffectPipeline = effectPipeline,
                          resSceneVertexBuffer = sceneVB,
                          resSceneIndexBuffer = sceneIB,
                          resEffectVertexBuffer = effectVB,
                          resEffectIndexBuffer = effectIB,
                          resSceneColorTexture = colorTex,
                          resSceneDepthTexture = depthTex,
                          resEffectSampler = sampler,
                          resSceneWidth = sceneW,
                          resSceneHeight = sceneH
                        }
                else do
                  sdlLog "!!! Failed to upload data."
                  return Nothing
            _ -> do
              sdlLog "!!! Failed to create GPU resources."
              return Nothing
        _ -> do
          sdlLog "!!! Failed to create pipelines."
          return Nothing
    _ -> do
      sdlLog "!!! Failed to load shaders."
      return Nothing

-- | Upload scene and effect buffer data
uploadBufferData :: SDLGPUDevice -> SDLGPUBuffer -> SDLGPUBuffer -> SDLGPUBuffer -> SDLGPUBuffer -> IO Bool
uploadBufferData device sceneVB sceneIB effectVB effectIB = do
  let sceneVertSize = 24 * 16 :: Word32
      sceneIdxSize = 36 * 2 :: Word32
      effectVertSize = 4 * 20 :: Word32
      effectIdxSize = 6 * 2 :: Word32
      totalSize = sceneVertSize + sceneIdxSize + effectVertSize + effectIdxSize

  maybeTB <- createTransferBuffer device totalSize SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "DepthSamplerTransfer"

  case maybeTB of
    Just tb -> bracket
      (sdlMapGPUTransferBuffer device tb False)
      (\mptr -> when (isJust mptr) $ sdlUnmapGPUTransferBuffer device tb)
      $ \case
        Nothing -> return False
        Just ptr -> do
          -- Scene cube vertices (24 vertices with color)
          let sceneVerts :: [Float]
              sceneVerts =
                concat
                  [ mkVertF (-10) (-10) (-10) 255 0 0,
                    mkVertF 10 (-10) (-10) 255 0 0,
                    mkVertF 10 10 (-10) 255 0 0,
                    mkVertF (-10) 10 (-10) 255 0 0,
                    mkVertF (-10) (-10) 10 255 255 0,
                    mkVertF 10 (-10) 10 255 255 0,
                    mkVertF 10 10 10 255 255 0,
                    mkVertF (-10) 10 10 255 255 0,
                    mkVertF (-10) (-10) (-10) 255 0 255,
                    mkVertF (-10) 10 (-10) 255 0 255,
                    mkVertF (-10) 10 10 255 0 255,
                    mkVertF (-10) (-10) 10 255 0 255,
                    mkVertF 10 (-10) (-10) 0 255 0,
                    mkVertF 10 10 (-10) 0 255 0,
                    mkVertF 10 10 10 0 255 0,
                    mkVertF 10 (-10) 10 0 255 0,
                    mkVertF (-10) (-10) (-10) 0 255 255,
                    mkVertF (-10) (-10) 10 0 255 255,
                    mkVertF 10 (-10) 10 0 255 255,
                    mkVertF 10 (-10) (-10) 0 255 255,
                    mkVertF (-10) 10 (-10) 0 0 255,
                    mkVertF (-10) 10 10 0 0 255,
                    mkVertF 10 10 10 0 0 255,
                    mkVertF 10 10 (-10) 0 0 255
                  ]
          pokeArray (castPtr ptr) sceneVerts

          -- Scene indices
          let sceneIndices :: [Word16]
              sceneIndices =
                [ 0,
                  1,
                  2,
                  0,
                  2,
                  3,
                  4,
                  5,
                  6,
                  4,
                  6,
                  7,
                  8,
                  9,
                  10,
                  8,
                  10,
                  11,
                  12,
                  13,
                  14,
                  12,
                  14,
                  15,
                  16,
                  17,
                  18,
                  16,
                  18,
                  19,
                  20,
                  21,
                  22,
                  20,
                  22,
                  23
                ]
          pokeArray (castPtr $ plusPtr ptr (fromIntegral sceneVertSize)) sceneIndices

          -- Effect quad vertices
          let effectVerts :: [Float]
              effectVerts =
                [ -1,
                  1,
                  0,
                  0,
                  0, -- top-left
                  1,
                  1,
                  0,
                  1,
                  0, -- top-right
                  1,
                  -1,
                  0,
                  1,
                  1, -- bottom-right
                  -1,
                  -1,
                  0,
                  0,
                  1 -- bottom-left
                ]
          pokeArray (castPtr $ plusPtr ptr (fromIntegral $ sceneVertSize + sceneIdxSize)) effectVerts

          -- Effect indices
          let effectIndices :: [Word16]
              effectIndices = [0, 1, 2, 0, 2, 3]
          pokeArray (castPtr $ plusPtr ptr (fromIntegral $ sceneVertSize + sceneIdxSize + effectVertSize)) effectIndices

          -- Upload
          bracket
            (sdlAcquireGPUCommandBuffer device)
            cleanupCommandBuffer
            $ \case
              Nothing -> return False
              Just cmd -> do
                mcp <- sdlBeginGPUCopyPass cmd
                case mcp of
                  Nothing -> return False
                  Just cp -> do
                    sdlUploadToGPUBuffer cp (SDLGPUTransferBufferLocation tb 0) (SDLGPUBufferRegion sceneVB 0 sceneVertSize) False
                    sdlUploadToGPUBuffer cp (SDLGPUTransferBufferLocation tb sceneVertSize) (SDLGPUBufferRegion sceneIB 0 sceneIdxSize) False
                    sdlUploadToGPUBuffer cp (SDLGPUTransferBufferLocation tb (sceneVertSize + sceneIdxSize)) (SDLGPUBufferRegion effectVB 0 effectVertSize) False
                    sdlUploadToGPUBuffer cp (SDLGPUTransferBufferLocation tb (sceneVertSize + sceneIdxSize + effectVertSize)) (SDLGPUBufferRegion effectIB 0 effectIdxSize) False
                    sdlEndGPUCopyPass cp
                    cleanupTransferBuffer device (Just tb)
                    sdlSubmitGPUCommandBuffer cmd
    Nothing -> return False
  where
    -- Make a vertex as [x, y, z, packed_color]
    mkVertF :: Float -> Float -> Float -> Word8 -> Word8 -> Word8 -> [Float]
    mkVertF x y z r g b =
      let color = fromIntegral r + fromIntegral g * 256 + fromIntegral b * 65536 + 255 * 16777216 :: Word32
          colorF = castWord32ToFloat color
       in [x, y, z, colorF]

    castWord32ToFloat :: Word32 -> Float
    castWord32ToFloat w =
      let ptr = castPtr (plusPtr nullPtr 0) :: Ptr Float
       in unsafePerformIO $ do
            alloca $ \p -> do
              poke (castPtr p) w
              peek p
      where
        nullPtr = castPtr (plusPtr nullPtr 0)

-- | releaseResources
releaseResources :: Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources Context {..} (Just AppResources {..}) = do
  sdlLog "--> Releasing AppResources..."
  sdlReleaseGPUGraphicsPipeline contextDevice resScenePipeline
  sdlReleaseGPUGraphicsPipeline contextDevice resEffectPipeline
  sdlReleaseGPUBuffer contextDevice resSceneVertexBuffer
  sdlReleaseGPUBuffer contextDevice resSceneIndexBuffer
  sdlReleaseGPUBuffer contextDevice resEffectVertexBuffer
  sdlReleaseGPUBuffer contextDevice resEffectIndexBuffer
  sdlReleaseGPUTexture contextDevice resSceneColorTexture
  sdlReleaseGPUTexture contextDevice resSceneDepthTexture
  sdlReleaseGPUSampler contextDevice resEffectSampler
  sdlLog "<-- AppResources Released."

-- | eventLoopGPU
eventLoopGPU :: Context -> AppResources -> IORef Float -> Word64 -> Word64 -> IO ()
eventLoopGPU context resources timeRef lastTime freq = do
  currentTime <- sdlGetPerformanceCounter
  let dt = fromIntegral (currentTime - lastTime) / fromIntegral freq :: Float
  modifyIORef' timeRef (+ dt)

  sdlPumpEvents
  shouldQuit <- processEventsGPU
  unless shouldQuit $ do
    renderFrameGPU context resources timeRef
    eventLoopGPU context resources timeRef currentTime freq

-- | processEventsGPU
processEventsGPU :: IO Bool
processEventsGPU = go
  where
    go =
      sdlPollEvent >>= \case
        Nothing -> return False
        Just event -> case event of
          SDLEventQuit _ -> sdlLog "Quit." >> return True
          SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ sc _ _ _ down _)
            | down && sc == SDL_SCANCODE_Q -> return True
            | otherwise -> go
          _ -> go

-- | renderFrameGPU
renderFrameGPU :: Context -> AppResources -> IORef Float -> IO ()
renderFrameGPU Context {..} AppResources {..} timeRef = do
  t <- readIORef timeRef

  maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
  case maybeCmdbuf of
    Nothing -> sdlLog "Error: Failed to acquire render command buffer."
    Just cmdbuf -> do
      maybeSwapResult <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
      case maybeSwapResult of
        Nothing -> void $ sdlSubmitGPUCommandBuffer cmdbuf
        Just (swapchainTexture, _, _) -> do
          -- Scene pass: render cube to off-screen textures
          let nearPlane = 20.0 :: Float
              farPlane = 60.0 :: Float
              fovRadians = 75.0 * Prelude.pi / 180.0 :: Float
              aspectRatio = fromIntegral resSceneWidth / fromIntegral resSceneHeight :: Float
              proj = perspective fovRadians aspectRatio nearPlane farPlane :: M44 Float
              camX = cos t * 30
              camZ = sin t * 30
              eye = V3 camX 30 camZ :: V3 Float
              center = V3 0 0 0 :: V3 Float
              up = V3 0 1 0 :: V3 Float
              view = lookAt eye center up :: M44 Float
              viewProjRaw = proj !*! view :: M44 Float
              viewProj = Linear.transpose viewProjRaw

          -- Push MVP uniform
          sdlPushGPUVertexUniformData cmdbuf 0 viewProj

          -- Push near/far planes for depth linearization
          let depthParams = V2 nearPlane farPlane :: V2 Float
          sdlPushGPUFragmentUniformData cmdbuf 0 depthParams

          -- Scene render pass
          let sceneColorTarget =
                defaultColorTargetInfo
                  { texture = resSceneColorTexture,
                    loadOp = SDL_GPU_LOADOP_CLEAR,
                    storeOp = SDL_GPU_STOREOP_STORE,
                    clearColor = SDLFColor 0 0 0 0
                  }
          let sceneDepthTarget =
                SDLGPUDepthStencilTargetInfo
                  { depthStencilTexture = resSceneDepthTexture,
                    depthStencilClearDepth = 1.0,
                    depthStencilLoadOp = SDL_GPU_LOADOP_CLEAR,
                    depthStencilStoreOp = SDL_GPU_STOREOP_STORE,
                    depthStencilStencilLoadOp = SDL_GPU_LOADOP_CLEAR,
                    depthStencilStencilStoreOp = SDL_GPU_STOREOP_STORE,
                    depthStencilCycle = True,
                    depthStencilClearStencil = 0,
                    depthMipLevel = 0,
                    depthLayer = 0
                  }

          maybeSceneRp <- sdlBeginGPURenderPass cmdbuf [sceneColorTarget] (Just sceneDepthTarget)
          case maybeSceneRp of
            Nothing -> return ()
            Just sceneRp -> do
              sdlBindGPUGraphicsPipeline sceneRp resScenePipeline
              sdlBindGPUVertexBuffers sceneRp 0 [SDLGPUBufferBinding resSceneVertexBuffer 0]
              sdlBindGPUIndexBuffer sceneRp (SDLGPUBufferBinding resSceneIndexBuffer 0) SDL_GPU_INDEXELEMENTSIZE_16BIT
              sdlDrawGPUIndexedPrimitives sceneRp 36 1 0 0 0
              sdlEndGPURenderPass sceneRp

          -- Effect pass: sample color/depth and draw outline effect
          let effectColorTarget =
                defaultColorTargetInfo
                  { texture = swapchainTexture,
                    loadOp = SDL_GPU_LOADOP_CLEAR,
                    storeOp = SDL_GPU_STOREOP_STORE,
                    clearColor = SDLFColor 0.2 0.5 0.4 1
                  }

          maybeEffectRp <- sdlBeginGPURenderPass cmdbuf [effectColorTarget] Nothing
          case maybeEffectRp of
            Nothing -> return ()
            Just effectRp -> do
              sdlBindGPUGraphicsPipeline effectRp resEffectPipeline
              sdlBindGPUVertexBuffers effectRp 0 [SDLGPUBufferBinding resEffectVertexBuffer 0]
              sdlBindGPUIndexBuffer effectRp (SDLGPUBufferBinding resEffectIndexBuffer 0) SDL_GPU_INDEXELEMENTSIZE_16BIT
              sdlBindGPUFragmentSamplers
                effectRp
                0
                [ SDLGPUTextureSamplerBinding resSceneColorTexture resEffectSampler,
                  SDLGPUTextureSamplerBinding resSceneDepthTexture resEffectSampler
                ]
              sdlDrawGPUIndexedPrimitives effectRp 6 1 0 0 0
              sdlEndGPURenderPass effectRp

          void $ sdlSubmitGPUCommandBuffer cmdbuf
