{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE LambdaCase           #-}

{-|
Example     : GPUCopyConsistency
Description : Demonstrates switching vertex and texture data for drawing via GPU-to-GPU copies.
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Based on the SDL_gpu_examples/CopyConsistency C example.
This example shows:
- Creating multiple source vertex buffers (LeftVertexBuffer, RightVertexBuffer) and source textures (LeftTexture, RightTexture).
- Creating a common "active" VertexBuffer and Texture that the graphics pipeline will use for drawing.
- In the render loop, before drawing each half of the screen:
    - Using SDL_CopyGPUBufferToBuffer to copy data from either LeftVertexBuffer or RightVertexBuffer
      into the common VertexBuffer.
    - Using SDL_CopyGPUTextureToTexture to copy data from either LeftTexture or RightTexture
      into the common Texture.
- Drawing a textured quad using the (now updated) common VertexBuffer and Texture.
- This technique allows for dynamic resource switching on the GPU without re-binding different
  objects to the pipeline in the render pass.
|-}

module Main where

import SDL3
import GPUCommon

import Control.Monad (unless, when, void, (>=>))
import Control.Exception (bracket, bracketOnError, finally)
import Foreign.Ptr (Ptr, nullPtr, castPtr, plusPtr)
import Foreign.Storable (peek, sizeOf, poke, pokeByteOff)
import Foreign.C.Types (CFloat, CSize, CUInt)
import Foreign.Marshal.Array (pokeArray, withArray)
import Foreign.Marshal.Utils (copyBytes, with)
import Data.IORef (IORef, newIORef, readIORef, writeIORef) -- Only for shouldQuitRef
import Data.Word (Word64, Word32, Word16)
import Data.Maybe (isJust, fromJust, isNothing, fromMaybe, catMaybes)
import Data.Bits ((.|.))
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

-- Vertex Data for Left Quad (maps to ravioli.bmp)
-- Coords map from -1 to 0 on X
leftVertexData :: [PositionTextureVertex]
leftVertexData =
    [ PositionTextureVertex (-1.0)   1.0  0   0 0 -- Top-left
    , PositionTextureVertex   0.0    1.0  0   1 0 -- Top-right (of left quad)
    , PositionTextureVertex   0.0  (-1.0) 0   1 1 -- Bottom-right (of left quad)
    , PositionTextureVertex (-1.0) (-1.0) 0   0 1 -- Bottom-left
    ]

-- Vertex Data for Right Quad (maps to ravioli_inverted.bmp)
-- Coords map from 0 to 1 on X
rightVertexData :: [PositionTextureVertex]
rightVertexData =
    [ PositionTextureVertex   0.0    1.0  0   0 0 -- Top-left (of right quad)
    , PositionTextureVertex   1.0    1.0  0   1 0 -- Top-right
    , PositionTextureVertex   1.0  (-1.0) 0   1 1 -- Bottom-right
    , PositionTextureVertex   0.0  (-1.0) 0   0 1 -- Bottom-left (of right quad)
    ]

-- Index Data (same for both quads)
indexData :: [Word16]
indexData = [0, 1, 2, 0, 2, 3]

-- AppResources
data AppResources = AppResources
    { resPipeline         :: SDLGPUGraphicsPipeline
    , resVertexBuffer     :: SDLGPUBuffer -- Common VB for drawing
    , resLeftVertexBuffer :: SDLGPUBuffer -- Source VB for left side
    , resRightVertexBuffer:: SDLGPUBuffer -- Source VB for right side
    , resIndexBuffer      :: SDLGPUBuffer
    , resTexture          :: SDLGPUTexture -- Common Texture for drawing
    , resLeftTexture      :: SDLGPUTexture -- Source Texture for left side
    , resRightTexture     :: SDLGPUTexture -- Source Texture for right side
    , resSampler          :: SDLGPUSampler
    } deriving Show

-- main
main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion
  maybeResult <- withContext "SDL3 Haskell GPU Copy Consistency" [] runAppGPU
  case maybeResult of
      Nothing -> sdlLog "Application initialization failed." >> exitFailure
      Just _  -> sdlLog "Application finished successfully." >> exitSuccess

-- runAppGPU
runAppGPU :: Context -> IO ()
runAppGPU context = do -- context is in scope here
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
createResources context@Context{..} = do
    sdlLog "--- Beginning Resource Creation ---"

    -- 1. Load Shaders (TexturedQuad.vert, TexturedQuad.frag)
    let vertInfo = defaultShaderCreateInfo { shaderNumSamplers = 0 } -- No samplers in vert shader
    let fragInfo = defaultShaderCreateInfo { shaderNumSamplers = 1 } -- One sampler in frag
    maybeVertShader <- loadShader contextDevice "TexturedQuad.vert" SDL_GPU_SHADERSTAGE_VERTEX vertInfo
    maybeFragShader <- loadShader contextDevice "TexturedQuad.frag" SDL_GPU_SHADERSTAGE_FRAGMENT fragInfo

    -- 2. Load Images
    maybeSurfL <- loadImage ("Content" </> "Images" </> "ravioli.bmp")
    maybeSurfR <- loadImage ("Content" </> "Images" </> "ravioli_inverted.bmp")

    -- 3. Create Graphics Pipeline
    case (maybeVertShader, maybeFragShader, maybeSurfL, maybeSurfR) of
        (Just vertS, Just fragS, Just surfL, Just surfR) ->
            bracketOnError (pure ((,) surfL surfR))
                           (\(sL,sR) -> sdlDestroySurface sL >> sdlDestroySurface sR) $ \(sL, sR) -> do

                surfLData <- peek sL
                surfRData <- peek sR
                -- Demo assumes images are same size, C uses assert
                let imgW = surfaceW surfLData
                let imgH = surfaceH surfLData

                maybePipeline <- createDrawPipeline contextDevice contextWindow vertS fragS
                sdlReleaseGPUShader contextDevice vertS
                sdlReleaseGPUShader contextDevice fragS

                -- 4. Create Textures
                let texCI = (defaultTextureCreateInfo imgW imgH) { texInfoUsage = SDL_GPU_TEXTUREUSAGE_SAMPLER }
                maybeTexL <- sdlCreateGPUTexture contextDevice texCI
                maybeTexR <- sdlCreateGPUTexture contextDevice texCI
                maybeTexActive <- sdlCreateGPUTexture contextDevice texCI -- Common active texture

                -- 5. Create Sampler
                let samplerCI = defaultSamplerCreateInfo SDL_GPU_FILTER_NEAREST
                maybeSampler <- sdlCreateGPUSampler contextDevice samplerCI

                -- 6. Create Buffers
                (_, lvbSizeC, lvbSizeW32) <- calculateBufferDataSize leftVertexData "LeftVB"
                maybeLVB <- createGPUBuffer contextDevice SDL_GPU_BUFFERUSAGE_VERTEX lvbSizeW32 "LeftVertexBuffer"
                (_, rvbSizeC, rvbSizeW32) <- calculateBufferDataSize rightVertexData "RightVB"
                maybeRVB <- createGPUBuffer contextDevice SDL_GPU_BUFFERUSAGE_VERTEX rvbSizeW32 "RightVertexBuffer"
                -- Active VB size matches source VBs
                maybeActiveVB <- createGPUBuffer contextDevice SDL_GPU_BUFFERUSAGE_VERTEX lvbSizeW32 "ActiveVertexBuffer"

                (_, ibSizeC, ibSizeW32) <- calculateBufferDataSize indexData "Index"
                maybeIB <- createGPUBuffer contextDevice SDL_GPU_BUFFERUSAGE_INDEX ibSizeW32 "IndexBuffer"

                -- Upload initial data
                uploadSuccess <- case (maybeTexL, maybeTexR, maybeLVB, maybeRVB, maybeIB) of
                    (Just texL, Just texR, Just lvb, Just rvb, Just ib) -> do
                        let totalVertexDataSize = lvbSizeW32 + rvbSizeW32
                        let totalTransferBufSize = totalVertexDataSize + ibSizeW32
                        let texLDataSize = fromIntegral (imgW * imgH * 4) :: Word32 -- Assuming 4bpp
                        let texRDataSize = texLDataSize
                        let totalTextureTransferSize = texLDataSize + texRDataSize

                        bracket ((,) <$> createTransferBuffer contextDevice totalTransferBufSize SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "BufferTransfer"
                                     <*> createTransferBuffer contextDevice totalTextureTransferSize SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "TextureTransfer")
                                (\(mBuf, mTex) -> cleanupTransferBuffer contextDevice mBuf >> cleanupTransferBuffer contextDevice mTex)
                                $ \(mBufTrans, mTexTrans) ->
                            bracket (sdlAcquireGPUCommandBuffer contextDevice)
                                    cleanupCommandBuffer
                                    $ \mCmd ->
                                case (mBufTrans, mTexTrans, mCmd) of
                                    (Just bufTrans, Just texTrans, Just cmd) -> do
                                        -- Map and poke buffer data
                                        mapOkBuf <- bracket (sdlMapGPUTransferBuffer contextDevice bufTrans False)
                                                            (\p -> when (isJust p) $ sdlUnmapGPUTransferBuffer contextDevice bufTrans)
                                                            $ \case
                                                                Nothing -> return False
                                                                Just ptr -> do
                                                                    pokeArray (castPtr ptr) leftVertexData
                                                                    pokeArray (castPtr ptr `plusPtr` fromIntegral lvbSizeW32) rightVertexData
                                                                    pokeArray (castPtr ptr `plusPtr` fromIntegral totalVertexDataSize) indexData
                                                                    return True
                                        -- Map and poke texture data
                                        mapOkTex <- bracket (sdlMapGPUTransferBuffer contextDevice texTrans False)
                                                            (\p -> when (isJust p) $ sdlUnmapGPUTransferBuffer contextDevice texTrans)
                                                            $ \case
                                                                Nothing -> return False
                                                                Just ptr -> do
                                                                    copyBytes ptr (surfacePixels surfLData) (fromIntegral texLDataSize)
                                                                    copyBytes (ptr `plusPtr` fromIntegral texLDataSize) (surfacePixels surfRData) (fromIntegral texRDataSize)
                                                                    return True

                                        if mapOkBuf && mapOkTex then do
                                            mcp <- sdlBeginGPUCopyPass cmd
                                            case mcp of
                                                Nothing -> return False
                                                Just cp -> do
                                                    sdlUploadToGPUBuffer cp (SDLGPUTransferBufferLocation bufTrans 0) (SDLGPUBufferRegion lvb 0 lvbSizeW32) False
                                                    sdlUploadToGPUBuffer cp (SDLGPUTransferBufferLocation bufTrans lvbSizeW32) (SDLGPUBufferRegion rvb 0 rvbSizeW32) False
                                                    sdlUploadToGPUBuffer cp (SDLGPUTransferBufferLocation bufTrans totalVertexDataSize) (SDLGPUBufferRegion ib 0 ibSizeW32) False
                                                    sdlUploadToGPUTexture cp (SDLGPUTextureTransferInfo texTrans 0 (fromIntegral imgW) (fromIntegral imgH)) (defaultTextureRegion texL imgW imgH) False
                                                    sdlUploadToGPUTexture cp (SDLGPUTextureTransferInfo texTrans texLDataSize (fromIntegral imgW) (fromIntegral imgH)) (defaultTextureRegion texR imgW imgH) False
                                                    sdlEndGPUCopyPass cp
                                                    sdlSubmitGPUCommandBuffer cmd >>= \s -> if s then sdlWaitForGPUIdle contextDevice >> return True else return False
                                        else return False
                                    _ -> return False
                    _ -> return False -- One of the initial texture/buffer creations failed

                if uploadSuccess && isJust maybePipeline && isJust maybeTexActive && isJust maybeSampler && isJust maybeActiveVB && isJust maybeLVB && isJust maybeRVB && isJust maybeIB && isJust maybeTexL && isJust maybeTexR
                then do
                    sdlLog "--- Resource Creation and Initial Upload Successful ---"
                    return $ Just AppResources
                        { resPipeline          = fromJust maybePipeline
                        , resVertexBuffer      = fromJust maybeActiveVB
                        , resLeftVertexBuffer  = fromJust maybeLVB
                        , resRightVertexBuffer = fromJust maybeRVB
                        , resIndexBuffer       = fromJust maybeIB
                        , resTexture           = fromJust maybeTexActive
                        , resLeftTexture       = fromJust maybeTexL
                        , resRightTexture      = fromJust maybeTexR
                        , resSampler           = fromJust maybeSampler
                        }
                else do
                    sdlLog "!!! Failed to create one or more resources or upload failed."
                    -- Manual cleanup
                    maybe (pure()) (sdlReleaseGPUGraphicsPipeline contextDevice) maybePipeline
                    maybe (pure()) (sdlReleaseGPUTexture contextDevice) maybeTexL
                    maybe (pure()) (sdlReleaseGPUTexture contextDevice) maybeTexR
                    maybe (pure()) (sdlReleaseGPUTexture contextDevice) maybeTexActive
                    maybe (pure()) (sdlReleaseGPUSampler contextDevice) maybeSampler
                    maybe (pure()) (sdlReleaseGPUBuffer contextDevice) maybeLVB
                    maybe (pure()) (sdlReleaseGPUBuffer contextDevice) maybeRVB
                    maybe (pure()) (sdlReleaseGPUBuffer contextDevice) maybeActiveVB
                    maybe (pure()) (sdlReleaseGPUBuffer contextDevice) maybeIB
                    return Nothing
        _ -> sdlLog "Failed to load shaders or images." >> return Nothing


-- createDrawPipeline (Standard textured quad pipeline with alpha blend)
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

    -- Alpha Blend State from C example
    let blendState = defaultColorTargetBlendState
          { enableBlend = True
          , alphaOp = SDL_GPU_BLENDOP_ADD, blendOp = SDL_GPU_BLENDOP_ADD
          , srcColorFactor = SDL_GPU_BLENDFACTOR_SRC_ALPHA, srcAlphaFactor = SDL_GPU_BLENDFACTOR_SRC_ALPHA
          , dstColorFactor = SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA, dstAlphaFactor = SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA
          }
    let colorTargetDesc = defaultColorTargetDescription { targetFormat = swapchainFormat, targetBlendState = blendState }
        targetInfo = SDLGPUGraphicsPipelineTargetInfo [colorTargetDesc] SDL_GPU_TEXTUREFORMAT_INVALID False
        pipelineCI = defaultGraphicsPipelineCreateInfo
            { vertexShader = vertShader
            , fragmentShader = fragShader
            , vertexInputState = vertexInputState
            , targetInfo = targetInfo
            }
    sdlCreateGPUGraphicsPipeline dev pipelineCI

releaseResources :: Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources Context{..} (Just AppResources{..}) = do
    sdlLog "--> Releasing AppResources..."
    sdlReleaseGPUGraphicsPipeline contextDevice resPipeline
    sdlReleaseGPUBuffer contextDevice resVertexBuffer
    sdlReleaseGPUBuffer contextDevice resLeftVertexBuffer
    sdlReleaseGPUBuffer contextDevice resRightVertexBuffer
    sdlReleaseGPUBuffer contextDevice resIndexBuffer
    sdlReleaseGPUTexture contextDevice resTexture
    sdlReleaseGPUTexture contextDevice resLeftTexture
    sdlReleaseGPUTexture contextDevice resRightTexture
    sdlReleaseGPUSampler contextDevice resSampler
    sdlLog "<-- AppResources Released."

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
            maybeSwapResult <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
            case maybeSwapResult of
                Nothing -> void (sdlSubmitGPUCommandBuffer cmdbuf `finally` pure ())
                Just (swapchainTexture, _, _) -> do
                    let colorTargetInfoClear = defaultColorTargetInfo
                            { texture = swapchainTexture, loadOp = SDL_GPU_LOADOP_CLEAR
                            , storeOp = SDL_GPU_STOREOP_STORE, clearColor = SDLFColor 0 0 0 1
                            }
                    let colorTargetInfoLoad = colorTargetInfoClear { loadOp = SDL_GPU_LOADOP_LOAD }
                    let lvbSize = fromIntegral (sizeOf (head leftVertexData) * length leftVertexData) :: Word32

                    -- Common bindings for render pass
                    let bindCommonResources renderPass = do
                            sdlBindGPUGraphicsPipeline renderPass resPipeline
                            sdlBindGPUVertexBuffers renderPass 0 [SDLGPUBufferBinding resVertexBuffer 0]
                            sdlBindGPUIndexBuffer renderPass (SDLGPUBufferBinding resIndexBuffer 0) SDL_GPU_INDEXELEMENTSIZE_16BIT
                            sdlBindGPUFragmentSamplers renderPass 0 [SDLGPUTextureSamplerBinding resTexture resSampler]

                    -- Copy left-side resources & Draw
                    mCopyPassL <- sdlBeginGPUCopyPass cmdbuf
                    case mCopyPassL of
                        Nothing -> sdlLog "Failed to begin copy pass for left side."
                        Just cpL -> do
                            sdlCopyGPUBufferToBuffer cpL (SDLGPUBufferLocation resLeftVertexBuffer 0) (SDLGPUBufferLocation resVertexBuffer 0) lvbSize False
                            sdlCopyGPUTextureToTexture cpL (SDLGPUTextureLocation resLeftTexture 0 0 0 0 0) (SDLGPUTextureLocation resTexture 0 0 0 0 0) 16 16 1 False
                            sdlEndGPUCopyPass cpL

                    mRenderPassL <- sdlBeginGPURenderPass cmdbuf [colorTargetInfoClear] Nothing
                    case mRenderPassL of
                        Nothing -> sdlLog "Failed to begin render pass for left side."
                        Just rpL -> bindCommonResources rpL >> sdlDrawGPUIndexedPrimitives rpL 6 1 0 0 0 >> sdlEndGPURenderPass rpL

                    -- Copy right-side resources & Draw
                    mCopyPassR <- sdlBeginGPUCopyPass cmdbuf
                    case mCopyPassR of
                        Nothing -> sdlLog "Failed to begin copy pass for right side."
                        Just cpR -> do
                            sdlCopyGPUBufferToBuffer cpR (SDLGPUBufferLocation resRightVertexBuffer 0) (SDLGPUBufferLocation resVertexBuffer 0) lvbSize False -- Assuming RVB is same size
                            sdlCopyGPUTextureToTexture cpR (SDLGPUTextureLocation resRightTexture 0 0 0 0 0) (SDLGPUTextureLocation resTexture 0 0 0 0 0) 16 16 1 False
                            sdlEndGPUCopyPass cpR

                    mRenderPassR <- sdlBeginGPURenderPass cmdbuf [colorTargetInfoLoad] Nothing -- Load existing content
                    case mRenderPassR of
                        Nothing -> sdlLog "Failed to begin render pass for right side."
                        Just rpR -> bindCommonResources rpR >> sdlDrawGPUIndexedPrimitives rpR 6 1 0 0 0 >> sdlEndGPURenderPass rpR

                    submitted <- sdlSubmitGPUCommandBuffer cmdbuf
                    unless submitted $ sdlGetError >>= sdlLog . ("Submit failed: " ++)
