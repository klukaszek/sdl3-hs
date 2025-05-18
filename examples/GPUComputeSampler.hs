{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE LambdaCase           #-}

{-|
Example     : GPUComputeSampler
Description : Samples a source texture in a compute shader using various sampler states and blits the result.
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Based on SDL_gpu_examples/ComputeSampler example.
Demonstrates:
- Loading a source texture and creating a separate writeable texture.
- Using a compute shader to process the source texture (applying different sampler effects)
  and write to the destination texture.
- Binding the source texture and a user-selectable sampler to the compute shader.
- Pushing a uniform (texcoord multiplier) to the compute shader.
- Blitting the compute shader's output texture to the screen.
- User can cycle through sampler states (Point, Linear, Anisotropic, Wrap/Clamp) via arrow keys.
|-}

module Main where

import SDL
import GPUCommon

import Control.Monad (unless, when, void, forM, forM_)
import Control.Exception (bracket, bracketOnError, finally, try, SomeException)
import Foreign.Storable (peek)
import Foreign.C.Types (CFloat)
import Data.IORef
import Data.Word (Word32)
import Data.Maybe (isJust, fromJust, isNothing, catMaybes)
import Data.Bits ((.|.))
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import Text.Printf (printf) -- For sampler names output

samplerCount :: Int
samplerCount = 6

samplerNames :: [String]
samplerNames =
    [ "PointClamp"    , "PointWrap"
    , "LinearClamp"   , "LinearWrap"
    , "AnisotropicClamp", "AnisotropicWrap"
    ]

samplerCreateInfos :: [SDLGPUSamplerCreateInfo]
samplerCreateInfos =
    [ (defaultSamplerCreateInfo SDL_GPU_FILTER_NEAREST)
        { samplerAddressModeU = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE
        , samplerAddressModeV = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE
        }
    , (defaultSamplerCreateInfo SDL_GPU_FILTER_NEAREST)
        { samplerAddressModeU = SDL_GPU_SAMPLERADDRESSMODE_REPEAT
        , samplerAddressModeV = SDL_GPU_SAMPLERADDRESSMODE_REPEAT
        }
    , (defaultSamplerCreateInfo SDL_GPU_FILTER_LINEAR)
        { samplerAddressModeU = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE
        , samplerAddressModeV = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE
        }
    , (defaultSamplerCreateInfo SDL_GPU_FILTER_LINEAR)
        { samplerAddressModeU = SDL_GPU_SAMPLERADDRESSMODE_REPEAT
        , samplerAddressModeV = SDL_GPU_SAMPLERADDRESSMODE_REPEAT
        }
    , (defaultSamplerCreateInfo SDL_GPU_FILTER_LINEAR) -- Anisotropic
        { samplerAddressModeU = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE
        , samplerAddressModeV = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE
        , samplerEnableAnisotropy = True
        , samplerMaxAnisotropy = 4.0
        }
    , (defaultSamplerCreateInfo SDL_GPU_FILTER_LINEAR) -- Anisotropic
        { samplerAddressModeU = SDL_GPU_SAMPLERADDRESSMODE_REPEAT
        , samplerAddressModeV = SDL_GPU_SAMPLERADDRESSMODE_REPEAT
        , samplerEnableAnisotropy = True
        , samplerMaxAnisotropy = 4.0
        }
    ]


-- AppResources
data AppResources = AppResources
    { resComputePipeline :: SDLGPUComputePipeline
    , resSourceTexture   :: SDLGPUTexture -- Loaded from image, read by compute
    , resWriteTexture    :: SDLGPUTexture -- Written by compute, read by blit
    , resSamplers        :: [SDLGPUSampler] -- Array of samplers for compute
    , currentSamplerIndexRef :: IORef Int  -- For UI
    }

-- main
main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion
  maybeResult <- withContext "SDL3 Haskell GPU Compute Textured Quad" [] runAppGPU
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
                sdlLog "Press Left/Right to switch sampler states for compute shader"
                initialSamplerIdx <- readIORef (currentSamplerIndexRef resources)
                sdlLog $ "Initial sampler for compute: " ++ (samplerNames !! initialSamplerIdx)
                eventLoopGPU context resources

-- createResources
createResources :: Context -> IO (Maybe AppResources)
createResources context@Context{..} = do
    sdlLog "--- Beginning Resource Creation ---"

    -- 1. Load Source Image ("ravioli.bmp")
    maybeSurf <- bracketOnError (loadImage ("Content" </> "Images" </> "ravioli.bmp"))
        (\mSurf -> case mSurf of Just surf -> sdlDestroySurface surf; _ -> pure ())
        pure

    case maybeSurf of
        Nothing -> sdlLog "!!! Failed to load source image." >> return Nothing
        Just surfacePtr ->
            bracketOnError (pure surfacePtr) (\_ -> sdlDestroySurface surfacePtr) $ \surf -> do
                surfaceData <- peek surf
                let imgW = fromIntegral (surfaceW surfaceData) :: Int
                let imgH = fromIntegral (surfaceH surfaceData) :: Int

                -- 2. Create Source Texture (for compute shader to read)
                let srcTextureCI = (defaultTextureCreateInfo imgW imgH)
                      { texInfoUsage = SDL_GPU_TEXTUREUSAGE_SAMPLER -- Compute will sample this
                      }
                maybeSrcTex <- sdlCreateGPUTexture contextDevice srcTextureCI
                forM_ maybeSrcTex $ \tex -> sdlSetGPUTextureName contextDevice tex "Ravioli Texture (Source)"


                -- 3. Create Write Texture (for compute shader to write to, fixed size 640x480)
                let writeTexW = 640
                let writeTexH = 480
                let writeTextureCI = (defaultTextureCreateInfo writeTexW writeTexH)
                      { texInfoUsage = SDL_GPU_TEXTUREUSAGE_SAMPLER .|. SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_WRITE
                      }
                maybeWriteTex <- sdlCreateGPUTexture contextDevice writeTextureCI

                -- 4. Create Compute Pipeline ("TexturedQuad.comp")
                let computeCI = defaultComputePipelineCreateInfo
                      { numSamplers = 1                 -- Compute shader uses 1 sampler
                      , numReadWriteStorageTextures = 1 -- Writes to WriteTexture
                      , numUniformBuffers = 1           -- For texcoordMultiplier
                      , threadCountX = 8, threadCountY = 8, threadCountZ = 1
                      }
                maybeComputePipe <- createComputePipelineFromShader contextDevice "TexturedQuad.comp" computeCI

                -- 5. Create Samplers
                samplersList <- forM samplerCreateInfos (sdlCreateGPUSampler contextDevice)
                let maybeSamplers = sequence samplersList -- Convert [Maybe a] to Maybe [a]

                -- Upload Source Texture Data
                uploadSuccess <- case maybeSrcTex of
                    Nothing -> return False
                    Just srcTexToUploadTo ->
                        bracket (createTransferBuffer contextDevice (fromIntegral $ imgW * imgH * 4) SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "SrcTexTransfer")
                                (cleanupTransferBuffer contextDevice)
                                $ \mTexTransfer -> case mTexTransfer of
                                    Nothing -> return False
                                    Just texTransfer -> do
                                        mapOk <- mapAndCopyTextureData contextDevice texTransfer surf imgW imgH 4
                                        if mapOk then
                                            bracket (sdlAcquireGPUCommandBuffer contextDevice)
                                                    cleanupCommandBuffer
                                                    $ \mCmd -> case mCmd of
                                                        Nothing -> return False
                                                        Just cmd -> do
                                                            mcp <- sdlBeginGPUCopyPass cmd
                                                            case mcp of
                                                                Nothing -> return False
                                                                Just cp -> do
                                                                    let texSrcDesc = SDLGPUTextureTransferInfo texTransfer 0 (fromIntegral imgW) (fromIntegral imgH)
                                                                    let texDstDesc = defaultTextureRegion srcTexToUploadTo imgW imgH
                                                                    sdlUploadToGPUTexture cp texSrcDesc texDstDesc False
                                                                    sdlEndGPUCopyPass cp
                                                                    sdlSubmitGPUCommandBuffer cmd >>= \s -> if s then sdlWaitForGPUIdle contextDevice >> return True else return False
                                        else return False


                if uploadSuccess && isJust maybeSrcTex && isJust maybeWriteTex && isJust maybeComputePipe && isJust maybeSamplers
                then do
                    samplerIdxRef <- newIORef 0
                    sdlLog "--- All core resources created and source texture uploaded ---"
                    return $ Just AppResources
                        { resComputePipeline = fromJust maybeComputePipe
                        , resSourceTexture   = fromJust maybeSrcTex
                        , resWriteTexture    = fromJust maybeWriteTex
                        , resSamplers        = fromJust maybeSamplers
                        , currentSamplerIndexRef = samplerIdxRef
                        }
                else do
                    sdlLog "!!! Failed to create one or more resources or upload source texture."
                    maybe (pure()) (sdlReleaseGPUTexture contextDevice) maybeSrcTex
                    maybe (pure()) (sdlReleaseGPUTexture contextDevice) maybeWriteTex
                    maybe (pure()) (sdlReleaseGPUComputePipeline contextDevice) maybeComputePipe
                    maybe (pure()) (mapM_ (sdlReleaseGPUSampler contextDevice)) (sequence samplersList)
                    return Nothing

-- releaseResources
releaseResources :: Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources Context{..} (Just AppResources{..}) = do
    sdlLog "--> Releasing AppResources..."
    sdlReleaseGPUComputePipeline contextDevice resComputePipeline
    sdlReleaseGPUTexture contextDevice resSourceTexture
    sdlReleaseGPUTexture contextDevice resWriteTexture
    forM_ resSamplers (sdlReleaseGPUSampler contextDevice)
    sdlLog "<-- AppResources Released."

-- eventLoopGPU
eventLoopGPU :: Context -> AppResources -> IO ()
eventLoopGPU context resources@AppResources{..} = do
  sdlPumpEvents
  shouldQuitRef <- newIORef False
  processEventsGPU context resources shouldQuitRef

  shouldQuit <- readIORef shouldQuitRef
  unless shouldQuit $ do
    renderFrameGPU context resources
    eventLoopGPU context resources

-- processEventsGPU
processEventsGPU :: Context -> AppResources -> IORef Bool -> IO ()
processEventsGPU context resources quitRef = do
    maybeEvent <- sdlPollEvent
    case maybeEvent of
        Nothing -> return ()
        Just event -> do
            quit <- handleEventGPU context resources event
            when quit $ writeIORef quitRef True
            processEventsGPU context resources quitRef

-- handleEventGPU for UI interaction (sampler switching)
handleEventGPU :: Context -> AppResources -> SDLEvent -> IO Bool
handleEventGPU _ AppResources{..} event = case event of
  SDLEventQuit _ -> sdlLog "Quit event received." >> return True
  SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ scancode _ _ _ down _) | down -> do
    case scancode of
        SDL_SCANCODE_Q -> return True
        SDL_SCANCODE_LEFT -> do
            idx <- atomicModifyIORef' currentSamplerIndexRef $ \i ->
                let newIdx = (i - 1 + samplerCount) `mod` samplerCount
                in (newIdx, newIdx)
            sdlLog $ "Compute Sampler: " ++ (samplerNames !! idx)
            return False
        SDL_SCANCODE_RIGHT -> do
            idx <- atomicModifyIORef' currentSamplerIndexRef $ \i ->
                let newIdx = (i + 1) `mod` samplerCount
                in (newIdx, newIdx)
            sdlLog $ "Compute Sampler: " ++ (samplerNames !! idx)
            return False
        _ -> return False
  _ -> return False

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
                Just (swapchainTexture, swapW, swapH) -> do

                    -- Texture dimensions for dispatch (WriteTexture is 640x480)
                    let dispatchW = 640 :: Word32
                    let dispatchH = 480 :: Word32

                    -- 1. Compute Pass to process/write to resWriteTexture
                    currentIndex <- readIORef currentSamplerIndexRef
                    let currentSamplerForCompute = resSamplers !! currentIndex

                    maybeComputePass <- sdlBeginGPUComputePass cmdbuf
                                           [SDLGPUStorageTextureReadWriteBinding resWriteTexture 0 0 True] -- Write to WriteTexture, cycle
                                           [] -- No storage buffers
                    case maybeComputePass of
                        Nothing -> sdlLog "!!! Failed to begin Compute pass."
                        Just computePass -> do
                            sdlBindGPUComputePipeline computePass resComputePipeline
                            -- Bind source texture and current sampler for compute shader to read
                            let computeSamplerBinding = SDLGPUTextureSamplerBinding resSourceTexture currentSamplerForCompute
                            sdlBindGPUComputeSamplers computePass 0 [computeSamplerBinding]
                            -- Push texcoordMultiplier uniform
                            let texcoordMultiplier = 0.25 :: CFloat
                            sdlPushGPUComputeUniformData cmdbuf 0 texcoordMultiplier
                            sdlDispatchGPUCompute computePass (dispatchW `div` 8) (dispatchH `div` 8) 1
                            sdlEndGPUComputePass computePass

                    -- 2. Blit resWriteTexture to Swapchain
                    let srcBlitRegion = (defaultBlitRegion resWriteTexture (fromIntegral dispatchW) (fromIntegral dispatchH))
                    let dstBlitRegion = (defaultBlitRegion swapchainTexture (fromIntegral swapW) (fromIntegral swapH))
                          { gpuBlitRegW = swapW, gpuBlitRegH = swapH }

                    let blitInfo = SDLGPUBlitInfo
                            { gpuBlitInfoSource      = srcBlitRegion
                            , gpuBlitInfoDestination = dstBlitRegion
                            , gpuBlitInfoLoadOp      = SDL_GPU_LOADOP_DONT_CARE
                            , gpuBlitInfoClearColor  = SDLFColor 0 0 0 0
                            , gpuBlitInfoFlipMode    = SDL_FLIP_NONE
                            , gpuBlitInfoFilter      = SDL_GPU_FILTER_NEAREST
                            , gpuBlitInfoCycle       = False
                            }
                    sdlBlitGPUTexture cmdbuf blitInfo

                    -- Submit
                    submitted <- sdlSubmitGPUCommandBuffer cmdbuf
                    unless submitted $ sdlGetError >>= sdlLog . ("Submit failed: " ++)
