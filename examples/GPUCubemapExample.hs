{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Example     : GPUCubemap
-- Description : Demonstrates cubemap creation via render-to-texture.
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Based on the SDL_gpu_examples/Cubemap C example.
-- This example demonstrates:
-- - Creating a cubemap texture with render target usage
-- - Rendering to each cube face with a different clear color
-- - Rendering a skybox using the generated cubemap
-- - Using vertex/index buffers with shader uniforms
module Main where

import Control.Exception (bracket)
import Control.Monad (forM_, unless, void, when)
import Data.Bits ((.|.))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Word (Word16, Word32)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import GPUCommon
import Linear
import SDL3 hiding (lookAt, perspective, pi, transpose)
import System.Exit (exitFailure, exitSuccess)

-- | Application resources
data AppResources = AppResources
  { resPipeline :: SDLGPUGraphicsPipeline,
    resVertexBuffer :: SDLGPUBuffer,
    resIndexBuffer :: SDLGPUBuffer,
    resTexture :: SDLGPUTexture,
    resSampler :: SDLGPUSampler
  }
  deriving (Show)

-- | Clear colors for each cube face (R, G, B, Y, M, C)
clearColors :: [SDLFColor]
clearColors =
  [ SDLFColor 1 0 0 1, -- Red (+X)
    SDLFColor 0 1 0 1, -- Green (-X)
    SDLFColor 0 0 1 1, -- Blue (+Y)
    SDLFColor 1 1 0 1, -- Yellow (-Y)
    SDLFColor 1 0 1 1, -- Magenta (+Z)
    SDLFColor 0 1 1 1 -- Cyan (-Z)
  ]

-- | main
main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion
  maybeResult <- withContext "SDL3 Haskell GPU Cubemap" [] runAppGPU
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
        sdlLog "Press Left/Right to flip camera direction!"
        camZRef <- newIORef (4.0 :: Float)
        eventLoopGPU context resources camZRef

-- | createResources
createResources :: Context -> IO (Maybe AppResources)
createResources Context {..} = do
  sdlLog "--- Beginning Resource Creation ---"

  -- Load shaders
  let vertInfo = defaultShaderCreateInfo {shaderNumUniformBuffers = 1} -- MVP matrix
  let fragInfo = defaultShaderCreateInfo {shaderNumSamplers = 1} -- cubemap sampler
  maybeVertShader <- loadShader contextDevice "Skybox.vert" SDL_GPU_SHADERSTAGE_VERTEX vertInfo
  maybeFragShader <- loadShader contextDevice "Skybox.frag" SDL_GPU_SHADERSTAGE_FRAGMENT fragInfo

  case (maybeVertShader, maybeFragShader) of
    (Just vertS, Just fragS) -> do
      swapchainFormat <- sdlGetGPUSwapchainTextureFormat contextDevice contextWindow

      -- Create pipeline with position-only vertex layout
      let vertexBufferDesc =
            SDLGPUVertexBufferDescription
              { descSlot = 0,
                descPitch = 12, -- 3 floats * 4 bytes
                descInputRate = SDL_GPU_VERTEXINPUTRATE_VERTEX,
                descInstanceStepRate = 0
              }
      let positionAttr =
            SDLGPUVertexAttribute
              { attribLocation = 0,
                attribSlot = 0,
                attribFormat = SDL_GPU_VERTEXELEMENTFORMAT_FLOAT3,
                attribOffset = 0
              }
      let vertexInputState =
            SDLGPUVertexInputState
              { inputVertexBuffers = [vertexBufferDesc],
                inputVertexAttribs = [positionAttr]
              }

      let basePipelineCI = defaultGraphicsPipelineCreateInfo vertS fragS swapchainFormat
          pipelineCI = basePipelineCI {vertexInputState = vertexInputState}

      maybePipeline <- sdlCreateGPUGraphicsPipeline contextDevice pipelineCI

      sdlReleaseGPUShader contextDevice vertS
      sdlReleaseGPUShader contextDevice fragS

      case maybePipeline of
        Nothing -> do
          sdlLog "!!! Failed to create pipeline."
          return Nothing
        Just pipeline -> do
          -- Create vertex buffer (24 vertices for 6 cube faces)
          maybeVB <- sdlCreateGPUBuffer contextDevice $ SDLGPUBufferCreateInfo SDL_GPU_BUFFERUSAGE_VERTEX (24 * 12) 0
          -- Create index buffer (36 indices)
          maybeIB <- sdlCreateGPUBuffer contextDevice $ SDLGPUBufferCreateInfo SDL_GPU_BUFFERUSAGE_INDEX (36 * 2) 0

          -- Create cubemap texture (render target + sampler usage)
          let texCI =
                SDLGPUTextureCreateInfo
                  { texInfoType = SDL_GPU_TEXTURETYPE_CUBE,
                    texInfoFormat = SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM,
                    texInfoUsage = SDL_GPU_TEXTUREUSAGE_COLOR_TARGET .|. SDL_GPU_TEXTUREUSAGE_SAMPLER,
                    texInfoWidth = 64,
                    texInfoHeight = 64,
                    texInfoLayerCountOrDepth = 6,
                    texInfoNumLevels = 1,
                    texInfoSampleCount = SDL_GPU_SAMPLECOUNT_1,
                    texInfoProps = 0
                  }
          maybeTex <- sdlCreateGPUTexture contextDevice texCI

          -- Create sampler
          let samplerCI =
                SDLGPUSamplerCreateInfo
                  { samplerMinFilter = SDL_GPU_FILTER_NEAREST,
                    samplerMagFilter = SDL_GPU_FILTER_NEAREST,
                    samplerMipmapMode = SDL_GPU_SAMPLERMIPMAPMODE_NEAREST,
                    samplerAddressModeU = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
                    samplerAddressModeV = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
                    samplerAddressModeW = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
                    samplerMipLodBias = 0,
                    samplerMaxAnisotropy = 1,
                    samplerCompareOp = SDL_GPU_COMPAREOP_NEVER,
                    samplerMinLod = 0,
                    samplerMaxLod = 1000,
                    samplerEnableAnisotropy = False,
                    samplerEnableCompare = False,
                    samplerProps = 0
                  }
          maybeSampler <- sdlCreateGPUSampler contextDevice samplerCI

          case (maybeVB, maybeIB, maybeTex, maybeSampler) of
            (Just vb, Just ib, Just tex, Just sampler) -> do
              -- Upload vertex/index data and clear cube faces
              uploadSuccess <- uploadCubeData contextDevice vb ib tex

              if uploadSuccess
                then do
                  sdlLog "--- Resource Creation Successful ---"
                  return $
                    Just
                      AppResources
                        { resPipeline = pipeline,
                          resVertexBuffer = vb,
                          resIndexBuffer = ib,
                          resTexture = tex,
                          resSampler = sampler
                        }
                else do
                  sdlLog "!!! Failed to upload data."
                  return Nothing
            _ -> do
              sdlLog "!!! Failed to create GPU resources."
              sdlReleaseGPUGraphicsPipeline contextDevice pipeline
              return Nothing
    _ -> do
      sdlLog "!!! Failed to load shaders."
      return Nothing

-- | Upload cube vertices and indices, then clear each face with a color
uploadCubeData :: SDLGPUDevice -> SDLGPUBuffer -> SDLGPUBuffer -> SDLGPUTexture -> IO Bool
uploadCubeData device vb ib tex = do
  let vertexDataSize = 24 * 12 :: Word32
      indexDataSize = 36 * 2 :: Word32
      bufferUploadSize = vertexDataSize + indexDataSize

  -- Create transfer buffer for vertex/index data
  maybeBufferTB <- createTransferBuffer device bufferUploadSize SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "CubemapBufferTransfer"

  case maybeBufferTB of
    Just bufferTB -> do
      -- Map and fill buffer data
      bufferMapOK <- bracket
        (sdlMapGPUTransferBuffer device bufferTB False)
        (\mptr -> when (isJust mptr) $ sdlUnmapGPUTransferBuffer device bufferTB)
        $ \case
          Nothing -> return False
          Just ptr -> do
            let floatPtr = castPtr ptr :: Ptr Float
            -- Cube vertices (24 vertices, 6 faces, each with 4 corners)
            let cubeVerts :: [Float]
                cubeVerts =
                  [ -10,
                    -10,
                    -10,
                    10,
                    -10,
                    -10,
                    10,
                    10,
                    -10,
                    -10,
                    10,
                    -10, -- Front
                    -10,
                    -10,
                    10,
                    10,
                    -10,
                    10,
                    10,
                    10,
                    10,
                    -10,
                    10,
                    10, -- Back
                    -10,
                    -10,
                    -10,
                    -10,
                    10,
                    -10,
                    -10,
                    10,
                    10,
                    -10,
                    -10,
                    10, -- Left
                    10,
                    -10,
                    -10,
                    10,
                    10,
                    -10,
                    10,
                    10,
                    10,
                    10,
                    -10,
                    10, -- Right
                    -10,
                    -10,
                    -10,
                    -10,
                    -10,
                    10,
                    10,
                    -10,
                    10,
                    10,
                    -10,
                    -10, -- Bottom
                    -10,
                    10,
                    -10,
                    -10,
                    10,
                    10,
                    10,
                    10,
                    10,
                    10,
                    10,
                    -10 -- Top
                  ]
            pokeArray floatPtr cubeVerts

            let indexPtr = castPtr (plusPtr ptr (fromIntegral vertexDataSize)) :: Ptr Word16
            let cubeIndices :: [Word16]
                cubeIndices =
                  [ 0,
                    1,
                    2,
                    0,
                    2,
                    3, -- front
                    6,
                    5,
                    4,
                    7,
                    6,
                    4, -- back
                    8,
                    9,
                    10,
                    8,
                    10,
                    11, -- left
                    14,
                    13,
                    12,
                    15,
                    14,
                    12, -- right
                    16,
                    17,
                    18,
                    16,
                    18,
                    19, -- bottom
                    22,
                    21,
                    20,
                    23,
                    22,
                    20 -- top
                  ]
            pokeArray indexPtr cubeIndices
            return True

      if bufferMapOK
        then bracket
          (sdlAcquireGPUCommandBuffer device)
          cleanupCommandBuffer
          $ \case
            Nothing -> return False
            Just cmd -> do
              -- First, upload vertex/index data
              mcp <- sdlBeginGPUCopyPass cmd
              case mcp of
                Nothing -> return False
                Just cp -> do
                  sdlUploadToGPUBuffer
                    cp
                    (SDLGPUTransferBufferLocation bufferTB 0)
                    (SDLGPUBufferRegion vb 0 vertexDataSize)
                    False
                  sdlUploadToGPUBuffer
                    cp
                    (SDLGPUTransferBufferLocation bufferTB vertexDataSize)
                    (SDLGPUBufferRegion ib 0 indexDataSize)
                    False
                  sdlEndGPUCopyPass cp

                  cleanupTransferBuffer device (Just bufferTB)

                  -- Now clear each cube face with a different color (render-to-texture)
                  forM_ (zip [0 .. 5] clearColors) $ \(i, color) -> do
                    let colorTargetInfo =
                          SDLGPUColorTargetInfo
                            { texture = tex,
                              mipLevel = 0,
                              layerOrDepthPlane = i,
                              clearColor = color,
                              loadOp = SDL_GPU_LOADOP_CLEAR,
                              storeOp = SDL_GPU_STOREOP_STORE,
                              resolveTexture = Nothing,
                              resolveMipLevel = 0,
                              resolveLayer = 0,
                              targetCycle = False,
                              targetCycleResolve = False
                            }
                    maybeRp <- sdlBeginGPURenderPass cmd [colorTargetInfo] Nothing
                    case maybeRp of
                      Nothing -> sdlLog $ "Failed to begin render pass for face " ++ show i
                      Just rp -> sdlEndGPURenderPass rp

                  sdlSubmitGPUCommandBuffer cmd
        else return False
    Nothing -> return False

-- | releaseResources
releaseResources :: Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources Context {..} (Just AppResources {..}) = do
  sdlLog "--> Releasing AppResources..."
  sdlReleaseGPUGraphicsPipeline contextDevice resPipeline
  sdlReleaseGPUBuffer contextDevice resVertexBuffer
  sdlReleaseGPUBuffer contextDevice resIndexBuffer
  sdlReleaseGPUTexture contextDevice resTexture
  sdlReleaseGPUSampler contextDevice resSampler
  sdlLog "<-- AppResources Released."

-- | eventLoopGPU
eventLoopGPU :: Context -> AppResources -> IORef Float -> IO ()
eventLoopGPU context resources camZRef = do
  sdlPumpEvents
  shouldQuit <- processEventsGPU camZRef
  unless shouldQuit $ do
    renderFrameGPU context resources camZRef
    eventLoopGPU context resources camZRef

-- | processEventsGPU
processEventsGPU :: IORef Float -> IO Bool
processEventsGPU camZRef = go
  where
    go =
      sdlPollEvent >>= \case
        Nothing -> return False
        Just event -> case event of
          SDLEventQuit _ -> sdlLog "Quit." >> return True
          SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ sc _ _ _ down _)
            | down -> case sc of
                _ | sc == SDL_SCANCODE_Q -> return True
                _ | sc == SDL_SCANCODE_LEFT || sc == SDL_SCANCODE_RIGHT -> do
                  modifyIORef' camZRef negate
                  return False
                _ -> go
            | otherwise -> go
          _ -> go

-- | renderFrameGPU
renderFrameGPU :: Context -> AppResources -> IORef Float -> IO ()
renderFrameGPU Context {..} AppResources {..} camZRef = do
  camZ <- readIORef camZRef

  maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
  case maybeCmdbuf of
    Nothing -> sdlLog "Error: Failed to acquire render command buffer."
    Just cmdbuf -> do
      maybeSwapResult <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
      case maybeSwapResult of
        Nothing -> void $ sdlSubmitGPUCommandBuffer cmdbuf
        Just (swapchainTexture, _, _) -> do
          -- Create MVP matrix using Linear library
          let fovRadians = 75 * Prelude.pi / 180 :: Float
              aspectRatio = 640.0 / 480.0 :: Float
              nearPlane = 0.01 :: Float
              farPlane = 100.0 :: Float
              proj = perspective fovRadians aspectRatio nearPlane farPlane :: M44 Float
              eye = V3 0 0 camZ :: V3 Float
              center = V3 0 0 0 :: V3 Float
              up = V3 0 1 0 :: V3 Float
              view = lookAt eye center up :: M44 Float
              viewProjRaw = proj !*! view :: M44 Float
              viewProj = transpose viewProjRaw

          -- Push uniform data
          sdlPushGPUVertexUniformData cmdbuf 0 viewProj

          let colorTargetInfo =
                defaultColorTargetInfo
                  { texture = swapchainTexture,
                    loadOp = SDL_GPU_LOADOP_CLEAR,
                    storeOp = SDL_GPU_STOREOP_STORE,
                    clearColor = SDLFColor 0 0 0 1
                  }

          maybeRp <- sdlBeginGPURenderPass cmdbuf [colorTargetInfo] Nothing
          case maybeRp of
            Nothing -> return ()
            Just rp -> do
              sdlBindGPUGraphicsPipeline rp resPipeline
              sdlBindGPUVertexBuffers rp 0 [SDLGPUBufferBinding resVertexBuffer 0]
              sdlBindGPUIndexBuffer rp (SDLGPUBufferBinding resIndexBuffer 0) SDL_GPU_INDEXELEMENTSIZE_16BIT
              sdlBindGPUFragmentSamplers rp 0 [SDLGPUTextureSamplerBinding resTexture resSampler]
              sdlDrawGPUIndexedPrimitives rp 36 1 0 0 0
              sdlEndGPURenderPass rp

          void $ sdlSubmitGPUCommandBuffer cmdbuf
