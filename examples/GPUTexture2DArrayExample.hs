{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Example     : GPUTexture2DArray
-- Description : Demonstrates rendering with 2D texture arrays.
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Based on the SDL_gpu_examples/Texture2DArray C example.
-- This example demonstrates:
-- - Creating a 2D texture array with multiple layers
-- - Uploading images to each layer
-- - Rendering a quad that samples from the texture array
-- - Using the TexturedQuadArray fragment shader
module Main where

import Control.Exception (bracket)
import Control.Monad (unless, void, when)
import Data.Bits ((.|.))
import Data.Maybe (isJust)
import Data.Word (Word16, Word32)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek)
import GPUCommon
import SDL3
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

-- | Application resources
data AppResources = AppResources
  { resPipeline :: SDLGPUGraphicsPipeline,
    resVertexBuffer :: SDLGPUBuffer,
    resIndexBuffer :: SDLGPUBuffer,
    resTexture :: SDLGPUTexture,
    resSampler :: SDLGPUSampler
  }
  deriving (Show)

-- | main
main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion
  maybeResult <- withContext "SDL3 Haskell GPU Texture2DArray" [] runAppGPU
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
        eventLoopGPU context resources

-- | createResources
createResources :: Context -> IO (Maybe AppResources)
createResources Context {..} = do
  sdlLog "--- Beginning Resource Creation ---"

  -- Load shaders
  let vertInfo = defaultShaderCreateInfo
  let fragInfo = defaultShaderCreateInfo {shaderNumSamplers = 1}

  maybeVertShader <- loadShader contextDevice "TexturedQuad.vert" SDL_GPU_SHADERSTAGE_VERTEX vertInfo
  maybeFragShader <- loadShader contextDevice "TexturedQuadArray.frag" SDL_GPU_SHADERSTAGE_FRAGMENT fragInfo

  case (maybeVertShader, maybeFragShader) of
    (Just vertS, Just fragS) -> do
      swapchainFormat <- sdlGetGPUSwapchainTextureFormat contextDevice contextWindow

      -- Create pipeline with position+texcoord vertex layout
      let vertexBufferDesc =
            SDLGPUVertexBufferDescription
              { descSlot = 0,
                descPitch = 20, -- 3 floats pos + 2 floats uv = 20 bytes
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
      let texcoordAttr =
            SDLGPUVertexAttribute
              { attribLocation = 1,
                attribSlot = 0,
                attribFormat = SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2,
                attribOffset = 12 -- 3 floats * 4 bytes
              }
      let vertexInputState =
            SDLGPUVertexInputState
              { inputVertexBuffers = [vertexBufferDesc],
                inputVertexAttribs = [positionAttr, texcoordAttr]
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
          -- Create vertex buffer (4 vertices for quad)
          maybeVB <- sdlCreateGPUBuffer contextDevice $ SDLGPUBufferCreateInfo SDL_GPU_BUFFERUSAGE_VERTEX (4 * 20) 0
          -- Create index buffer (6 indices)
          maybeIB <- sdlCreateGPUBuffer contextDevice $ SDLGPUBufferCreateInfo SDL_GPU_BUFFERUSAGE_INDEX (6 * 2) 0

          -- Load images
          maybeSurf1 <- loadImage ("Content" </> "Images" </> "ravioli.bmp")
          maybeSurf2 <- loadImage ("Content" </> "Images" </> "ravioli_inverted.bmp")

          case (maybeSurf1, maybeSurf2) of
            (Just surfPtr1, Just surfPtr2) -> do
              surf1 <- peek surfPtr1
              surf2 <- peek surfPtr2
              let w = surfaceW surf1
                  h = surfaceH surf1

              -- Create 2D array texture
              let texCI =
                    SDLGPUTextureCreateInfo
                      { texInfoType = SDL_GPU_TEXTURETYPE_2D_ARRAY,
                        texInfoFormat = SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM,
                        texInfoUsage = SDL_GPU_TEXTUREUSAGE_SAMPLER,
                        texInfoWidth = fromIntegral w,
                        texInfoHeight = fromIntegral h,
                        texInfoLayerCountOrDepth = 2,
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
                  uploadSuccess <- uploadData contextDevice vb ib tex surfPtr1 surfPtr2 w h
                  sdlDestroySurface surfPtr1
                  sdlDestroySurface surfPtr2

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
                  sdlDestroySurface surfPtr1
                  sdlDestroySurface surfPtr2
                  sdlReleaseGPUGraphicsPipeline contextDevice pipeline
                  return Nothing
            _ -> do
              sdlLog "!!! Failed to load images."
              sdlReleaseGPUGraphicsPipeline contextDevice pipeline
              return Nothing
    _ -> do
      sdlLog "!!! Failed to load shaders."
      return Nothing

-- | Upload vertex, index, and texture data
uploadData :: SDLGPUDevice -> SDLGPUBuffer -> SDLGPUBuffer -> SDLGPUTexture -> Ptr SDLSurface -> Ptr SDLSurface -> Int -> Int -> IO Bool
uploadData device vb ib tex surfPtr1 surfPtr2 w h = do
  let vertexDataSize = 4 * 20 :: Word32 -- 4 vertices * 20 bytes
      indexDataSize = 6 * 2 :: Word32 -- 6 indices * 2 bytes
      bufferUploadSize = vertexDataSize + indexDataSize
      imageSizeBytes = fromIntegral (w * h * 4) :: Word32
      textureUploadSize = imageSizeBytes * 2

  maybeBufferTB <- createTransferBuffer device bufferUploadSize SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "BufferTransfer"
  maybeTextureTB <- createTransferBuffer device textureUploadSize SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "TextureTransfer"

  case (maybeBufferTB, maybeTextureTB) of
    (Just bufferTB, Just textureTB) -> do
      -- Map and fill buffer data
      bufferMapOK <- bracket
        (sdlMapGPUTransferBuffer device bufferTB False)
        (\mptr -> when (isJust mptr) $ sdlUnmapGPUTransferBuffer device bufferTB)
        $ \case
          Nothing -> return False
          Just ptr -> do
            let floatPtr = castPtr ptr :: Ptr Float
            -- Fullscreen quad vertices: pos (x,y,z) + uv (u,v)
            let quadVerts :: [Float]
                quadVerts =
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
            pokeArray floatPtr quadVerts

            let indexPtr = castPtr (plusPtr ptr (fromIntegral vertexDataSize)) :: Ptr Word16
            let quadIndices :: [Word16]
                quadIndices = [0, 1, 2, 0, 2, 3]
            pokeArray indexPtr quadIndices
            return True

      -- Map and fill texture data
      textureMapOK <-
        if bufferMapOK
          then bracket
            (sdlMapGPUTransferBuffer device textureTB False)
            (\mptr -> when (isJust mptr) $ sdlUnmapGPUTransferBuffer device textureTB)
            $ \case
              Nothing -> return False
              Just ptr -> do
                surf1 <- peek surfPtr1
                surf2 <- peek surfPtr2
                copyBytes (castPtr ptr) (surfacePixels surf1) (fromIntegral imageSizeBytes)
                copyBytes (castPtr $ plusPtr ptr (fromIntegral imageSizeBytes)) (surfacePixels surf2) (fromIntegral imageSizeBytes)
                return True
          else return False

      if textureMapOK
        then bracket
          (sdlAcquireGPUCommandBuffer device)
          cleanupCommandBuffer
          $ \case
            Nothing -> return False
            Just cmd -> do
              mcp <- sdlBeginGPUCopyPass cmd
              case mcp of
                Nothing -> return False
                Just cp -> do
                  -- Upload vertex buffer
                  sdlUploadToGPUBuffer
                    cp
                    (SDLGPUTransferBufferLocation bufferTB 0)
                    (SDLGPUBufferRegion vb 0 vertexDataSize)
                    False
                  -- Upload index buffer
                  sdlUploadToGPUBuffer
                    cp
                    (SDLGPUTransferBufferLocation bufferTB vertexDataSize)
                    (SDLGPUBufferRegion ib 0 indexDataSize)
                    False
                  -- Upload texture layer 0
                  sdlUploadToGPUTexture
                    cp
                    (SDLGPUTextureTransferInfo textureTB 0 0 0)
                    (SDLGPUTextureRegion tex 0 0 0 0 0 (fromIntegral w) (fromIntegral h) 1)
                    False
                  -- Upload texture layer 1
                  sdlUploadToGPUTexture
                    cp
                    (SDLGPUTextureTransferInfo textureTB imageSizeBytes 0 0)
                    (SDLGPUTextureRegion tex 0 1 0 0 0 (fromIntegral w) (fromIntegral h) 1)
                    False
                  sdlEndGPUCopyPass cp

                  cleanupTransferBuffer device (Just bufferTB)
                  cleanupTransferBuffer device (Just textureTB)
                  sdlSubmitGPUCommandBuffer cmd
        else return False
    _ -> return False

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
eventLoopGPU :: Context -> AppResources -> IO ()
eventLoopGPU context resources = do
  sdlPumpEvents
  shouldQuit <- processEventsGPU
  unless shouldQuit $ do
    renderFrameGPU context resources
    eventLoopGPU context resources

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
renderFrameGPU :: Context -> AppResources -> IO ()
renderFrameGPU Context {..} AppResources {..} = do
  maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
  case maybeCmdbuf of
    Nothing -> sdlLog "Error: Failed to acquire render command buffer."
    Just cmdbuf -> do
      maybeSwapResult <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
      case maybeSwapResult of
        Nothing -> void $ sdlSubmitGPUCommandBuffer cmdbuf
        Just (swapchainTexture, _, _) -> do
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
              sdlDrawGPUIndexedPrimitives rp 6 1 0 0 0
              sdlEndGPURenderPass rp

          void $ sdlSubmitGPUCommandBuffer cmdbuf
