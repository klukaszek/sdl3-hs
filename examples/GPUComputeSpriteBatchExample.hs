{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Example     : GPUComputeSpriteBatch
-- Description : Demonstrates a compute-to-graphics workflow.
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Ported from SDL_gpu_examples/ComputeSpriteBatch.c
module Main where

import Control.Exception (bracket)
import Control.Monad (forM, forM_, unless, void, when)
import Data.Bits ((.|.))
import Data.IORef (IORef, newIORef, readIORef)
import Data.Word (Word32)
import Foreign.C.Types (CFloat)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable (..))
import GPUCommon
import Linear (M44, V4 (..), ortho, transpose)
import SDL3
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

-- | Constants
spriteCount :: Word32
spriteCount = 8192

-- | Vertex Type
data PositionTextureColorVertex = PositionTextureColorVertex
  { ptcvX, ptcvY, ptcvZ, ptcvW :: !CFloat,
    ptcvU, ptcvV, ptcvPaddingA, ptcvPaddingB :: !CFloat,
    ptcvR, ptcvG, ptcvB, ptcvA :: !CFloat
  }
  deriving (Show, Eq)

instance Storable PositionTextureColorVertex where
  sizeOf _ = 48 -- 12 * 4
  alignment _ = 4
  peek ptr =
    PositionTextureColorVertex
      <$> peekByteOff ptr 0
      <*> peekByteOff ptr 4
      <*> peekByteOff ptr 8
      <*> peekByteOff ptr 12
      <*> peekByteOff ptr 16
      <*> peekByteOff ptr 20
      <*> peekByteOff ptr 24
      <*> peekByteOff ptr 28
      <*> peekByteOff ptr 32
      <*> peekByteOff ptr 36
      <*> peekByteOff ptr 40
      <*> peekByteOff ptr 44
  poke ptr PositionTextureColorVertex {..} = do
    pokeByteOff ptr 0 ptcvX
    pokeByteOff ptr 4 ptcvY
    pokeByteOff ptr 8 ptcvZ
    pokeByteOff ptr 12 ptcvW
    pokeByteOff ptr 16 ptcvU
    pokeByteOff ptr 20 ptcvV
    pokeByteOff ptr 24 ptcvPaddingA
    pokeByteOff ptr 28 ptcvPaddingB
    pokeByteOff ptr 32 ptcvR
    pokeByteOff ptr 36 ptcvG
    pokeByteOff ptr 40 ptcvB
    pokeByteOff ptr 44 ptcvA

-- | Instance Type
data ComputeSpriteInstance = ComputeSpriteInstance
  { csiX, csiY, csiZ :: !CFloat,
    csiRotation :: !CFloat,
    csiW, csiH, csiPaddingA, csiPaddingB :: !CFloat,
    csiTexU, csiTexV, csiTexW, csiTexH :: !CFloat,
    csiR, csiG, csiB, csiA :: !CFloat
  }
  deriving (Show, Eq)

instance Storable ComputeSpriteInstance where
  sizeOf _ = 64 -- 16 * 4
  alignment _ = 4
  peek ptr =
    ComputeSpriteInstance
      <$> peekByteOff ptr 0
      <*> peekByteOff ptr 4
      <*> peekByteOff ptr 8
      <*> peekByteOff ptr 12
      <*> peekByteOff ptr 16
      <*> peekByteOff ptr 20
      <*> peekByteOff ptr 24
      <*> peekByteOff ptr 28
      <*> peekByteOff ptr 32
      <*> peekByteOff ptr 36
      <*> peekByteOff ptr 40
      <*> peekByteOff ptr 44
      <*> peekByteOff ptr 48
      <*> peekByteOff ptr 52
      <*> peekByteOff ptr 56
      <*> peekByteOff ptr 60
  poke ptr ComputeSpriteInstance {..} = do
    pokeByteOff ptr 0 csiX
    pokeByteOff ptr 4 csiY
    pokeByteOff ptr 8 csiZ
    pokeByteOff ptr 12 csiRotation
    pokeByteOff ptr 16 csiW
    pokeByteOff ptr 20 csiH
    pokeByteOff ptr 24 csiPaddingA
    pokeByteOff ptr 28 csiPaddingB
    pokeByteOff ptr 32 csiTexU
    pokeByteOff ptr 36 csiTexV
    pokeByteOff ptr 40 csiTexW
    pokeByteOff ptr 44 csiTexH
    pokeByteOff ptr 48 csiR
    pokeByteOff ptr 52 csiG
    pokeByteOff ptr 56 csiB
    pokeByteOff ptr 60 csiA

-- | Resources
data AppResources = AppResources
  { resComputePipeline :: SDLGPUComputePipeline,
    resRenderPipeline :: SDLGPUGraphicsPipeline,
    resSampler :: SDLGPUSampler,
    resTexture :: SDLGPUTexture,
    resSpriteComputeTransferBuffer :: SDLGPUTransferBuffer,
    resSpriteComputeBuffer :: SDLGPUBuffer,
    resSpriteVertexBuffer :: SDLGPUBuffer,
    resSpriteIndexBuffer :: SDLGPUBuffer
  }

-- | main
main :: IO ()
main = do
  maybeResult <- withContext "SDL3 Haskell GPU ComputeSpriteBatch" [] runAppGPU
  case maybeResult of
    Nothing -> exitFailure
    Just _ -> exitSuccess

-- | runAppGPU
runAppGPU :: Context -> IO ()
runAppGPU context@Context {..} = do
  sdlLog "Base context initialized."
  bracket
    (createResources context)
    (releaseResources context)
    $ \case
      Nothing -> sdlLog "Failed to create resources."
      Just res -> do
        sdlLog "Resources created successfully. Press Q to quit."
        eventLoopGPU context res

-- | createResources
createResources :: Context -> IO (Maybe AppResources)
createResources Context {..} = do
  -- Shaders
  vertShader <- loadShader contextDevice "TexturedQuadColorWithMatrix.vert" SDL_GPU_SHADERSTAGE_VERTEX (defaultShaderCreateInfo {shaderNumUniformBuffers = 1})
  fragShader <- loadShader contextDevice "TexturedQuadColor.frag" SDL_GPU_SHADERSTAGE_FRAGMENT (defaultShaderCreateInfo {shaderNumSamplers = 1})

  case (vertShader, fragShader) of
    (Just vs, Just fs) -> do
      swapchainFormat <- sdlGetGPUSwapchainTextureFormat contextDevice contextWindow

      let renderPipelineCI =
            (defaultGraphicsPipelineCreateInfo vs fs swapchainFormat)
              { vertexInputState =
                  SDLGPUVertexInputState
                    { inputVertexBuffers = [SDLGPUVertexBufferDescription 0 (fromIntegral $ sizeOf (undefined :: PositionTextureColorVertex)) SDL_GPU_VERTEXINPUTRATE_VERTEX 0],
                      inputVertexAttribs =
                        [ SDLGPUVertexAttribute 0 0 SDL_GPU_VERTEXELEMENTFORMAT_FLOAT4 0,
                          SDLGPUVertexAttribute 1 0 SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2 16,
                          SDLGPUVertexAttribute 2 0 SDL_GPU_VERTEXELEMENTFORMAT_FLOAT4 32
                        ]
                    },
                targetInfo =
                  (defaultGraphicsPipelineTargetInfo swapchainFormat)
                    { colorTargets =
                        [ (defaultColorTargetDescription {targetFormat = swapchainFormat})
                            { targetBlendState =
                                defaultColorTargetBlendState
                                  { enableBlend = True,
                                    blendOp = SDL_GPU_BLENDOP_ADD,
                                    alphaOp = SDL_GPU_BLENDOP_ADD,
                                    srcColorFactor = SDL_GPU_BLENDFACTOR_SRC_ALPHA,
                                    dstColorFactor = SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA,
                                    srcAlphaFactor = SDL_GPU_BLENDFACTOR_SRC_ALPHA,
                                    dstAlphaFactor = SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA
                                  }
                            }
                        ]
                    }
              }

      maybeRenderPipeline <- sdlCreateGPUGraphicsPipeline contextDevice renderPipelineCI

      let computePipelineCI =
            defaultComputePipelineCreateInfo
              { numReadOnlyStorageBuffers = 1,
                numReadWriteStorageBuffers = 1,
                threadCountX = 64
              }
      maybeComputePipeline <- createComputePipelineFromShader contextDevice "SpriteBatch.comp" computePipelineCI

      maybeAtlasSurface <- loadImage ("Content" </> "Images" </> "ravioli_atlas.bmp")

      case (maybeRenderPipeline, maybeComputePipeline, maybeAtlasSurface) of
        (Just rp, Just cp, Just surfPtr) -> do
          surf <- peek surfPtr
          let w = fromIntegral $ surfaceW surf
              h = fromIntegral $ surfaceH surf

          let texCI = (defaultTextureCreateInfo w h) {texInfoUsage = SDL_GPU_TEXTUREUSAGE_SAMPLER}
          maybeTex <- sdlCreateGPUTexture contextDevice texCI

          let samplerCI =
                (defaultSamplerCreateInfo SDL_GPU_FILTER_NEAREST)
                  { samplerAddressModeU = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
                    samplerAddressModeV = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
                    samplerAddressModeW = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
                    samplerMipmapMode = SDL_GPU_SAMPLERMIPMAPMODE_NEAREST
                  }
          maybeSamp <- sdlCreateGPUSampler contextDevice samplerCI

          let instSize = fromIntegral $ sizeOf (undefined :: ComputeSpriteInstance)
              vertSize = fromIntegral $ sizeOf (undefined :: PositionTextureColorVertex)

          maybeComputeTB <- createTransferBuffer contextDevice (spriteCount * instSize) SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "SpriteComputeTransfer"
          maybeComputeBuf <- createGPUBuffer contextDevice SDL_GPU_BUFFERUSAGE_COMPUTE_STORAGE_READ (spriteCount * instSize) "SpriteComputeBuffer"
          maybeVertexBuf <- createGPUBuffer contextDevice (SDL_GPU_BUFFERUSAGE_COMPUTE_STORAGE_WRITE .|. SDL_GPU_BUFFERUSAGE_VERTEX) (spriteCount * 4 * vertSize) "SpriteVertexBuffer"
          maybeIndexBuf <- createGPUBuffer contextDevice SDL_GPU_BUFFERUSAGE_INDEX (spriteCount * 6 * 4) "SpriteIndexBuffer"

          case (maybeTex, maybeSamp, maybeComputeTB, maybeComputeBuf, maybeVertexBuf, maybeIndexBuf) of
            (Just tex, Just samp, Just ctb, Just cb, Just vb, Just ib) -> do
              let indices = concat [[j, j + 1, j + 2, j + 3, j + 2, j + 1] | j <- [0, 4 .. spriteCount * 4 - 4]]

              maybeIndexTB <- createTransferBuffer contextDevice (fromIntegral $ length indices * 4) SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "IndexTransfer"
              maybeAtlasTB <- createTransferBuffer contextDevice (fromIntegral $ w * h * 4) SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "AtlasTransfer"

              case (maybeIndexTB, maybeAtlasTB) of
                (Just itb, Just atb) -> do
                  mIndexPtr <- sdlMapGPUTransferBuffer contextDevice itb False
                  case mIndexPtr of
                    Just ptr -> pokeArray (castPtr ptr) (map fromIntegral indices :: [Word32]) >> sdlUnmapGPUTransferBuffer contextDevice itb
                    Nothing -> return ()

                  mAtlasPtr <- sdlMapGPUTransferBuffer contextDevice atb False
                  case mAtlasPtr of
                    Just ptr -> copyBytes ptr (castPtr $ surfacePixels surf) (fromIntegral $ w * h * 4) >> sdlUnmapGPUTransferBuffer contextDevice atb
                    Nothing -> return ()

                  maybeCmd <- sdlAcquireGPUCommandBuffer contextDevice
                  case maybeCmd of
                    Just cmd -> do
                      maybeCP <- sdlBeginGPUCopyPass cmd
                      case maybeCP of
                        Just cp_pass -> do
                          let texTrans = SDLGPUTextureTransferInfo atb 0 0 0
                          let texRegion = SDLGPUTextureRegion tex 0 0 0 0 0 (fromIntegral w) (fromIntegral h) 1
                          sdlUploadToGPUTexture cp_pass texTrans texRegion False

                          let bufLoc = SDLGPUTransferBufferLocation itb 0
                          let bufRegion = SDLGPUBufferRegion ib 0 (fromIntegral $ length indices * 4)
                          sdlUploadToGPUBuffer cp_pass bufLoc bufRegion False

                          sdlEndGPUCopyPass cp_pass
                        Nothing -> return ()
                      void $ sdlSubmitGPUCommandBuffer cmd
                    Nothing -> return ()

                  sdlReleaseGPUTransferBuffer contextDevice itb
                  sdlReleaseGPUTransferBuffer contextDevice atb
                  sdlDestroySurface surfPtr
                  sdlReleaseGPUShader contextDevice vs
                  sdlReleaseGPUShader contextDevice fs

                  return $ Just $ AppResources cp rp samp tex ctb cb vb ib
                _ -> return Nothing
            _ -> return Nothing
        _ -> return Nothing
    _ -> return Nothing

-- | releaseResources
releaseResources :: Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources Context {..} (Just AppResources {..}) = do
  sdlReleaseGPUComputePipeline contextDevice resComputePipeline
  sdlReleaseGPUGraphicsPipeline contextDevice resRenderPipeline
  sdlReleaseGPUSampler contextDevice resSampler
  sdlReleaseGPUTexture contextDevice resTexture
  sdlReleaseGPUTransferBuffer contextDevice resSpriteComputeTransferBuffer
  sdlReleaseGPUBuffer contextDevice resSpriteComputeBuffer
  sdlReleaseGPUBuffer contextDevice resSpriteVertexBuffer
  sdlReleaseGPUBuffer contextDevice resSpriteIndexBuffer

-- | atlas coords
uCoords, vCoords :: [CFloat]
uCoords = [0.0, 0.5, 0.0, 0.5]
vCoords = [0.0, 0.0, 0.5, 0.5]

-- | renderFrameGPU
renderFrameGPU :: Context -> AppResources -> IO ()
renderFrameGPU Context {..} AppResources {..} = do
  maybeCmd <- sdlAcquireGPUCommandBuffer contextDevice
  case maybeCmd of
    Nothing -> return ()
    Just cmd -> do
      maybeSwap <- sdlWaitAndAcquireGPUSwapchainTexture cmd contextWindow
      case maybeSwap of
        Nothing -> void $ sdlSubmitGPUCommandBuffer cmd
        Just (swapTex, _, _) -> do
          let instSize = fromIntegral $ sizeOf (undefined :: ComputeSpriteInstance)
          mPtr <- sdlMapGPUTransferBuffer contextDevice resSpriteComputeTransferBuffer True
          case mPtr of
            Just ptr -> do
              instances <- forM [0 .. spriteCount - 1] $ \_ -> do
                ravioli <- rand 4
                x <- rand 640
                y <- rand 480
                rot <- randf
                let u = uCoords !! fromIntegral ravioli
                    v = vCoords !! fromIntegral ravioli
                return $
                  ComputeSpriteInstance
                    { csiX = fromIntegral x,
                      csiY = fromIntegral y,
                      csiZ = 0,
                      csiRotation = rot * 3.14159 * 2,
                      csiW = 32,
                      csiH = 32,
                      csiPaddingA = 0,
                      csiPaddingB = 0,
                      csiTexU = u,
                      csiTexV = v,
                      csiTexW = 0.5,
                      csiTexH = 0.5,
                      csiR = 1,
                      csiG = 1,
                      csiB = 1,
                      csiA = 1
                    }
              pokeArray (castPtr ptr) instances
              sdlUnmapGPUTransferBuffer contextDevice resSpriteComputeTransferBuffer
            Nothing -> return ()

          maybeCP <- sdlBeginGPUCopyPass cmd
          case maybeCP of
            Just cp -> do
              let loc = SDLGPUTransferBufferLocation resSpriteComputeTransferBuffer 0
              let region = SDLGPUBufferRegion resSpriteComputeBuffer 0 (spriteCount * instSize)
              sdlUploadToGPUBuffer cp loc region True
              sdlEndGPUCopyPass cp
            Nothing -> return ()

          let rwBinding = SDLGPUStorageBufferReadWriteBinding resSpriteVertexBuffer True
          maybeCompP <- sdlBeginGPUComputePass cmd [] [rwBinding]
          case maybeCompP of
            Just compP -> do
              sdlBindGPUComputePipeline compP resComputePipeline
              sdlBindGPUComputeStorageBuffers compP 0 [resSpriteComputeBuffer]
              sdlDispatchGPUCompute compP (spriteCount `div` 64) 1 1
              sdlEndGPUComputePass compP
            Nothing -> return ()

          let colorTarget =
                defaultColorTargetInfo
                  { texture = swapTex,
                    loadOp = SDL_GPU_LOADOP_CLEAR,
                    storeOp = SDL_GPU_STOREOP_STORE,
                    clearColor = SDLFColor 0 0 0 1
                  }
          let projection = ortho 0 640 480 0 0 (-1) :: M44 CFloat
          sdlPushGPUVertexUniformData cmd 0 (transpose projection)

          maybeRP <- sdlBeginGPURenderPass cmd [colorTarget] Nothing
          case maybeRP of
            Just rp -> do
              sdlBindGPUGraphicsPipeline rp resRenderPipeline
              sdlBindGPUVertexBuffers rp 0 [SDLGPUBufferBinding resSpriteVertexBuffer 0]
              sdlBindGPUIndexBuffer rp (SDLGPUBufferBinding resSpriteIndexBuffer 0) SDL_GPU_INDEXELEMENTSIZE_32BIT
              sdlBindGPUFragmentSamplers rp 0 [SDLGPUTextureSamplerBinding resTexture resSampler]

              sdlDrawGPUIndexedPrimitives rp (spriteCount * 6) 1 0 0 0
              sdlEndGPURenderPass rp
            Nothing -> return ()

          void $ sdlSubmitGPUCommandBuffer cmd

-- | Event Loop
eventLoopGPU :: Context -> AppResources -> IO ()
eventLoopGPU context res = do
  sdlPumpEvents
  shouldQuit <- processEventsGPU
  unless shouldQuit $ do
    renderFrameGPU context res
    eventLoopGPU context res

processEventsGPU :: IO Bool
processEventsGPU =
  sdlPollEvent >>= \case
    Nothing -> return False
    Just event -> case event of
      SDLEventQuit _ -> return True
      SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ sc _ _ _ down _) ->
        if down && sc == SDL_SCANCODE_Q then return True else processEventsGPU
      _ -> processEventsGPU
