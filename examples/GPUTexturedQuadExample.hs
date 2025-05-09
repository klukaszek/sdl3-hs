{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE FlexibleInstances    #-}

{-|
Example     : GPUTexturedQuad
Description : Loading a texture onto a quad using SDL GPU API.
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Based on the SDL_gpu_examples/TexturedQuad C example.
Demonstrates:
- Defining a vertex structure with texture coordinates (PositionTextureVertex).
- Loading a texture from a BMP file using SDL_Surface.
- Creating multiple GPU samplers with different filter/address modes.
- Creating vertex and index buffers for a quad.
- Uploading texture and buffer data.
- Binding texture and a selected sampler in the fragment shader.
- Switching between samplers using arrow keys.
|-}

module Main where

import SDL
import GPUCommon
import Paths_sdl3 (getDataFileName)
import System.FilePath ((</>))

import Control.Monad (unless, when, void, zipWithM_)
import Control.Exception (bracket, bracketOnError, onException, finally, try, SomeException)
import System.Exit (exitFailure, exitSuccess)
import Foreign.Ptr (Ptr, nullPtr, castPtr, plusPtr)
import Foreign.Storable (Storable(..), peek, peekByteOff, pokeByteOff, sizeOf, poke) -- Need peek
import Foreign.C.Types (CFloat, CInt, CSize, CUChar)
import Foreign.Marshal.Alloc (alloca, free, mallocBytes)
import Foreign.Marshal.Array (pokeArray, withArray)
import Foreign.Marshal.Utils (with)
import Data.IORef
import Data.Word (Word64, Word32, Word16, Word8)
import Text.Printf (printf)
import Data.Maybe (isJust, fromJust, fromMaybe, isNothing)
import Data.Bits ((.|.))
import Data.List (genericLength)
import System.IO (hFlush, stdout)

-- Constants
samplerCount :: Int
samplerCount = 6

samplerNames :: [String]
samplerNames =
    [ "PointClamp"
    , "PointWrap"
    , "LinearClamp"
    , "LinearWrap"
    , "AnisotropicClamp"
    , "AnisotropicWrap"
    ]

-- Vertex data
vertexData :: [PositionTextureVertex]
vertexData =
    [ PositionTextureVertex (-1)   1  0   0 0 -- Top-left
    , PositionTextureVertex   1    1  0   4 0 -- Top-right (adjust tex coords)
    , PositionTextureVertex   1  (-1) 0   4 4 -- Bottom-right
    , PositionTextureVertex (-1) (-1) 0   0 4 -- Bottom-left
    ]

-- Index data
indexData :: [Word16]
indexData = [0, 1, 2, 0, 2, 3]

-- AppResources
data AppResources = AppResources
    { resPipeline    :: SDLGPUGraphicsPipeline
    , resVertexBuffer :: SDLGPUBuffer
    , resIndexBuffer :: SDLGPUBuffer
    , resTexture     :: SDLGPUTexture
    , resSamplers    :: [SDLGPUSampler]
    } deriving Show

-- main
main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  maybeResult <- withContext "SDL3 Haskell GPU Sampler States" [SDL_WINDOW_RESIZABLE] runAppGPU
  case maybeResult of
      Nothing -> do
          sdlLog "Application initialization failed (commonInit)."
          exitFailure
      Just _ -> do
          sdlLog "Application finished successfully."
          exitSuccess

-- runAppGPU
runAppGPU :: Context -> IO ()
runAppGPU context@Context{..} = do
    sdlLog "Base context initialized."
    currentSamplerIndexRef <- newIORef 0

    bracket (createResources context)
            (releaseResources context)
            $ \case
            Nothing -> sdlLog "Failed to create resources. Exiting."
            Just resources -> do
                sdlLog "Resources created successfully."
                sdlLog "Press Left/Right to switch between sampler states"
                sdlLog $ "Setting initial sampler state to: " ++ samplerNames !! 0
                startTime <- sdlGetPerformanceCounter
                freq <- sdlGetPerformanceFrequency
                deltaTimeRef <- newIORef 0.0
                eventLoopGPU context resources startTime freq deltaTimeRef currentSamplerIndexRef

-- createResources -- CORRECTED VERSION WITH SEPARATE hFlush CALLS
createResources :: Context -> IO (Maybe AppResources)
createResources Context{ contextDevice = dev, contextWindow = win } = do
    sdlLog "--- Beginning Resource Creation ---"

    -- 1. Load Shaders
    sdlLog "Loading shaders..."
    maybeVertShader <- loadShader dev "TexturedQuad.vert" SDL_GPU_SHADERSTAGE_VERTEX defaultShaderCreateInfo { shaderNumSamplers = 0 }
    maybeFragShader <- loadShader dev "TexturedQuad.frag" SDL_GPU_SHADERSTAGE_FRAGMENT defaultShaderCreateInfo { shaderNumSamplers = 1 }

    case (maybeVertShader, maybeFragShader) of
        (Just vertShader, Just fragShader) -> do
            sdlLog "Shaders loaded successfully."

            -- 2. Load Image using the helper
            let imagePath = "Content" </> "Images" </> "ravioli.bmp" -- Path relative to data dir
            bracketOnError
                -- Acquire: Load and convert image using helper
                (loadImage imagePath)
                -- Cleanup for bracketOnError (if acquire fails or createGpuObjects fails)
                (\maybeSurfPtr -> do
                    when (isJust maybeSurfPtr) $ do
                        sdlLog $ "Error during resource creation. Cleaning up loaded/converted surface: " ++ show (fromJust maybeSurfPtr)
                        sdlDestroySurface (fromJust maybeSurfPtr)
                    -- Also clean up shaders if image loading failed *after* shaders loaded
                    sdlLog "Cleaning up shaders due to error after shader load."
                    sdlReleaseGPUShader dev vertShader
                    sdlReleaseGPUShader dev fragShader
                )
                -- Main resource creation block (uses the surface pointer from loadImage)
                (\maybeSurfacePtr -> do
                    case maybeSurfacePtr of
                        Nothing -> do
                            sdlLog "Failed to load or convert image. Resource creation aborted."
                            -- Shaders released by bracketOnError's cleanup if necessary.
                            return Nothing
                        Just surfacePtr -> do
                            sdlLog $ "Image loaded and converted successfully: " ++ show surfacePtr
                            -- This bracket now manages the FINAL surface pointer's lifetime
                            -- AFTER successful loading/conversion and DURING GPU object creation.
                            bracket (return surfacePtr)
                                    (\surfManagedPtr -> do
                                        sdlLog $ "Destroying final surface after GPU object creation/upload: " ++ show surfManagedPtr
                                        sdlDestroySurface surfManagedPtr
                                    )
                                    $ \surfManagedPtr -> do
                                        -- Create GPU objects using the final surface pointer
                                        maybeResources <- createGpuObjects dev win vertShader fragShader surfManagedPtr
                                        -- Release shaders *after* GPU objects are potentially created
                                        sdlLog "Releasing shaders..."
                                        sdlReleaseGPUShader dev vertShader
                                        sdlReleaseGPUShader dev fragShader
                                        sdlLog "Shaders released."
                                        return maybeResources
                )
        _ -> do
            sdlLog "!!! Failed to load one or both shaders. Resource creation aborted."
            -- Clean up any shaders that might have loaded
            maybe (return ()) (sdlReleaseGPUShader dev) maybeVertShader
            maybe (return ()) (sdlReleaseGPUShader dev) maybeFragShader
            return Nothing

-- | Helper function containing the core GPU object creation logic
-- Accepts Ptr SDLSurface
createGpuObjects :: SDLGPUDevice -> SDLWindow -> SDLGPUShader -> SDLGPUShader -> Ptr SDLSurface -> IO (Maybe AppResources)
createGpuObjects dev win vertShader fragShader surfacePtr = do
    sdlLog "Creating GPU objects (pipeline, samplers, buffers, texture)..."

    -- *** PEEK and PRINT Surface Info ***
    sdlLog $ "--- Surface Info for Ptr: " ++ show surfacePtr ++ " ---"
    if surfacePtr == nullPtr then do
        sdlLog "!!! Surface Pointer is NULL!"
        return Nothing -- Cannot proceed
    else do
        surfaceData <- peek surfacePtr :: IO SDLSurface
        let texWidthCInt  = surfaceW surfaceData
        let texHeightCInt = surfaceH surfaceData
        let pitchCInt = surfacePitch surfaceData
        let pixelsPtr = surfacePixels surfaceData
        let surfaceFlagsEnum = surfaceFlags surfaceData -- Use the field name from your SDLSurface definition
        let surfaceFormatEnum = surfaceFormat surfaceData -- Use the field name

        formatName <- case surfaceFormatEnum of
                         -- Handle known invalid/unknown formats if necessary
                         SDL_PIXELFORMAT_UNKNOWN -> return "Unknown/Invalid Format"
                         fmt -> sdlGetPixelFormatName fmt -- Assuming this function exists and takes your SDLPixelFormat type

        sdlLog $ printf "  Dimensions: %d x %d" (fromIntegral texWidthCInt :: Int) (fromIntegral texHeightCInt :: Int)
        sdlLog $ printf "  Pitch: %d bytes" (fromIntegral pitchCInt :: Int)
        sdlLog $ "  Pixel Format Enum: " ++ show surfaceFormatEnum
        sdlLog $ "  Pixel Format Name: " ++ formatName
        sdlLog $ printf "  Flags: %s" (show surfaceFlagsEnum)
        sdlLog $ "  Pixel Data Ptr:" ++ show pixelsPtr
        sdlLog "--- End Surface Info ---"

        when (pixelsPtr == nullPtr) $
            sdlLog "!!! WARNING: Surface pixel data pointer is NULL!"

        -- texWidth and texHeight are already Int here, which is fine
        let texWidth = fromIntegral texWidthCInt :: Int
        let texHeight = fromIntegral texHeightCInt :: Int

        -- Calculate data sizes
        (_, vertexDataSizeC, vertexDataSizeW32) <- calculateBufferDataSize vertexData "Vertex"
        (_, indexDataSizeC, indexDataSizeW32) <- calculateBufferDataSize indexData "Index"

        -- Assuming RGBA format (4 bytes per pixel) for now
        let bytesPerPixel = 4 -- Assumption
        let textureDataSizeC = fromIntegral (texWidth * texHeight * bytesPerPixel) :: CSize
        sdlLog $ printf "Texture Info - Width: %d, Height: %d, Assumed BPP: %d, Total Bytes: %d" texWidth texHeight bytesPerPixel (fromIntegral textureDataSizeC :: Int)

        -- Sampler Create Infos
        let samplerCIs =
              [ SDLGPUSamplerCreateInfo SDL_GPU_FILTER_NEAREST SDL_GPU_FILTER_NEAREST SDL_GPU_SAMPLERMIPMAPMODE_NEAREST SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE 0.0 1.0 SDL_GPU_COMPAREOP_NEVER 0.0 0.0 False False 0
              , SDLGPUSamplerCreateInfo SDL_GPU_FILTER_NEAREST SDL_GPU_FILTER_NEAREST SDL_GPU_SAMPLERMIPMAPMODE_NEAREST SDL_GPU_SAMPLERADDRESSMODE_REPEAT SDL_GPU_SAMPLERADDRESSMODE_REPEAT SDL_GPU_SAMPLERADDRESSMODE_REPEAT 0.0 1.0 SDL_GPU_COMPAREOP_NEVER 0.0 0.0 False False 0
              , SDLGPUSamplerCreateInfo SDL_GPU_FILTER_LINEAR SDL_GPU_FILTER_LINEAR SDL_GPU_SAMPLERMIPMAPMODE_LINEAR SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE 0.0 1.0 SDL_GPU_COMPAREOP_NEVER 0.0 (-1.0) False False 0
              , SDLGPUSamplerCreateInfo SDL_GPU_FILTER_LINEAR SDL_GPU_FILTER_LINEAR SDL_GPU_SAMPLERMIPMAPMODE_LINEAR SDL_GPU_SAMPLERADDRESSMODE_REPEAT SDL_GPU_SAMPLERADDRESSMODE_REPEAT SDL_GPU_SAMPLERADDRESSMODE_REPEAT 0.0 1.0 SDL_GPU_COMPAREOP_NEVER 0.0 (-1.0) False False 0
              , SDLGPUSamplerCreateInfo SDL_GPU_FILTER_LINEAR SDL_GPU_FILTER_LINEAR SDL_GPU_SAMPLERMIPMAPMODE_LINEAR SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE 0.0 4.0 SDL_GPU_COMPAREOP_NEVER 0.0 (-1.0) True False 0
              , SDLGPUSamplerCreateInfo SDL_GPU_FILTER_LINEAR SDL_GPU_FILTER_LINEAR SDL_GPU_SAMPLERMIPMAPMODE_LINEAR SDL_GPU_SAMPLERADDRESSMODE_REPEAT SDL_GPU_SAMPLERADDRESSMODE_REPEAT SDL_GPU_SAMPLERADDRESSMODE_REPEAT 0.0 4.0 SDL_GPU_COMPAREOP_NEVER 0.0 (-1.0) True False 0
              ]

        -- Bracket resource creation (logic remains the same)
        bracketOnError (createPipeline dev win vertShader fragShader)
                       (cleanupMaybe "Pipeline" (sdlReleaseGPUGraphicsPipeline dev)) $ \maybePipeline ->
          bracketOnError (createSamplers dev samplerCIs)
                         (cleanupList "Sampler" (sdlReleaseGPUSampler dev)) $ \maybeSamplers ->
          bracketOnError (createGPUBuffer dev SDL_GPU_BUFFERUSAGE_VERTEX vertexDataSizeC "VertexBuffer")
                         (cleanupMaybe "VertexBuffer" (sdlReleaseGPUBuffer dev)) $ \maybeVertexBuffer ->
          bracketOnError (createGPUBuffer dev SDL_GPU_BUFFERUSAGE_INDEX indexDataSizeC "IndexBuffer")
                         (cleanupMaybe "IndexBuffer" (sdlReleaseGPUBuffer dev)) $ \maybeIndexBuffer ->
          bracketOnError (createGPUTexture dev texWidth texHeight)
                         (cleanupMaybe "Texture" (sdlReleaseGPUTexture dev)) $ \maybeTexture ->

            case (maybePipeline, maybeSamplers, maybeVertexBuffer, maybeIndexBuffer, maybeTexture) of
              (Just pipeline, Just samplers, Just vertexBuffer, Just indexBuffer, Just texture) -> do
                  sdlLog "Core GPU objects created. Proceeding with data upload..."

                  -- Pass surfacePtr to uploadAllData
                  uploadOk <- uploadAllData dev surfacePtr texWidth texHeight bytesPerPixel
                                            vertexBuffer indexBuffer texture
                                            vertexDataSizeC indexDataSizeC textureDataSizeC
                  if uploadOk
                  then do
                      sdlLog "--- Resource Creation Successful ---"
                      sdlSetGPUBufferName dev vertexBuffer "Ravioli Vertex Buffer 🥣"
                      sdlSetGPUBufferName dev indexBuffer "Ravioli Index Buffer"
                      sdlSetGPUTextureName dev texture "Ravioli Texture 🖼️"

                      return $ Just AppResources { resPipeline = pipeline
                                                , resVertexBuffer = vertexBuffer
                                                , resIndexBuffer = indexBuffer
                                                , resTexture = texture
                                                , resSamplers = samplers
                                                }
                  else do
                      sdlLog "!!! Data upload failed. Resource creation aborted."
                      return Nothing -- Cleanup handled by brackets
              _ -> do
                  sdlLog "!!! Failed to create one or more core GPU objects. Resource creation aborted."
                  return Nothing -- Cleanup handled by brackets
  where
      -- Cleanup helpers remain the same
      cleanupMaybe name release = mapM_ (\res -> sdlLog ("Error occurred, releasing partially created " ++ name ++ ": " ++ show res) >> release res)
      cleanupList name release = mapM_ (\ress -> sdlLog ("Error occurred, releasing partially created " ++ name ++ " list (" ++ show (length ress) ++ " items)") >> mapM_ release ress)

-- createSamplers
createSamplers :: SDLGPUDevice -> [SDLGPUSamplerCreateInfo] -> IO (Maybe [SDLGPUSampler])
createSamplers dev cis = do
    sdlLog $ "Creating " ++ show (length cis) ++ " samplers..."
    results <- mapM (try . sdlCreateGPUSampler dev) cis :: IO [Either SomeException (Maybe SDLGPUSampler)]
    let maybeSamplers = sequence $ map processResult results
    case maybeSamplers of
        Nothing -> do
             sdlLog "!!! Failed to create one or more samplers."
             mapM_ (cleanupMaybeSampler dev) results
             return Nothing
        Just samplers -> do
             sdlLog "Samplers created successfully."
             return $ Just samplers
  where
    processResult (Left e) = Nothing
    processResult (Right Nothing) = Nothing
    processResult (Right (Just s)) = Just s
    cleanupMaybeSampler dev (Right (Just s)) = sdlReleaseGPUSampler dev s
    cleanupMaybeSampler _ _ = return ()

-- createPipeline
createPipeline :: SDLGPUDevice -> SDLWindow -> SDLGPUShader -> SDLGPUShader -> IO (Maybe SDLGPUGraphicsPipeline)
createPipeline dev win vertShader fragShader = do
    sdlLog "Creating Graphics Pipeline..."
    swapchainFormat <- sdlGetGPUSwapchainTextureFormat dev win
    let vertexSize = sizeOf (undefined :: PositionTextureVertex)
    let texCoordOffset = sizeOf (undefined :: CFloat) * 3
    sdlLog $ "Pipeline Vertex Input - Stride: " ++ show vertexSize ++ ", TexCoord Offset: " ++ show texCoordOffset
    let vertexAttributes =
          [
            SDLGPUVertexAttribute 0 0 SDL_GPU_VERTEXELEMENTFORMAT_FLOAT3 0
          , SDLGPUVertexAttribute 1 0 SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2 (fromIntegral texCoordOffset)
          ]
        vertexBufferDesc = [ SDLGPUVertexBufferDescription 0 (fromIntegral vertexSize) SDL_GPU_VERTEXINPUTRATE_VERTEX 0 ]
        vertexInputState = SDLGPUVertexInputState vertexBufferDesc vertexAttributes
    let colorTargetDesc = SDLGPUColorTargetDescription swapchainFormat defaultColorTargetBlendState
        targetInfo = SDLGPUGraphicsPipelineTargetInfo [colorTargetDesc] SDL_GPU_TEXTUREFORMAT_INVALID False
        pipelineCI = SDLGPUGraphicsPipelineCreateInfo
            { vertexShader = vertShader, fragmentShader = fragShader
            , vertexInputState = vertexInputState
            , primitiveType = SDL_GPU_PRIMITIVETYPE_TRIANGLELIST
            , rasterizerState = defaultRasterizerState
            , multisampleState = defaultMultiSampleState
            , depthStencilState = defaultDepthStencilState
            , targetInfo = targetInfo
            , props = 0
            }
    maybePipeline <- sdlCreateGPUGraphicsPipeline dev pipelineCI
    when (isNothing maybePipeline) $ sdlGetError >>= sdlLog . ("!!! Failed to create graphics pipeline: " ++)
    return maybePipeline

-- | Helper function to upload vertex, index, and texture data
-- Accepts Ptr SDLSurface
uploadAllData :: SDLGPUDevice -> Ptr SDLSurface -> Int -> Int -> Int -> SDLGPUBuffer -> SDLGPUBuffer -> SDLGPUTexture -> CSize -> CSize -> CSize -> IO Bool
uploadAllData dev surfacePtr texWidth texHeight bytesPerPixel vertexBuf indexBuf texture vertexSizeC indexSizeC textureSizeC = do
    sdlLog "Starting data upload process..."

    let bufferDataTotalSizeC = vertexSizeC + indexSizeC
    let vertexOffsetC = 0
    let indexOffsetC = vertexSizeC

    bracket (createTransferBuffer dev bufferDataTotalSizeC "Buffer")
            (cleanupTransferBuffer dev) $ \maybeBufTransfer ->
      bracket (createTransferBuffer dev textureSizeC "Texture")
              (cleanupTransferBuffer dev) $ \maybeTexTransfer ->
      bracket (sdlAcquireGPUCommandBuffer dev)
              cleanupCommandBuffer $ \maybeCmdBuf ->

        case (maybeBufTransfer, maybeTexTransfer, maybeCmdBuf) of
          (Just bufTransfer, Just texTransfer, Just cmdBuf) -> do
              sdlLog "All necessary transfer buffers and command buffer acquired." >> hFlush stdout

              bufMapOk <- mapAndCopyBufferData dev bufTransfer vertexData indexData vertexOffsetC indexOffsetC
              unless bufMapOk $ sdlLog "!!! Buffer data map/copy failed." >> hFlush stdout

              texMapOk <- if bufMapOk
                          then mapAndCopyTextureData dev texTransfer surfacePtr texWidth texHeight bytesPerPixel -- Uses converted surface
                          else return False
              unless texMapOk $ sdlLog "!!! Texture data map/copy failed." >> hFlush stdout

              if bufMapOk && texMapOk then do
                  recordOk <- recordUploadCommands dev cmdBuf bufTransfer texTransfer
                                                 vertexBuf indexBuf texture
                                                 vertexOffsetC indexOffsetC
                                                 texWidth texHeight
                  if recordOk then do
                      sdlLog "Submitting upload commands..." >> hFlush stdout
                      submitted <- sdlSubmitGPUCommandBuffer cmdBuf

                      if submitted then do
                          sdlLog "Upload command buffer submitted successfully." >> hFlush stdout
                          sdlLog "Waiting for GPU device idle after upload submission..." >> hFlush stdout
                          sdlWaitForGPUIdle dev `finally` (sdlLog "GPU device idle wait finished (or errored)." >> hFlush stdout)
                          sdlLog "GPU device idle." >> hFlush stdout
                          return True -- Indicate upload success
                      else do
                          errMsg <- sdlGetError
                          sdlLog ("!!! Failed to submit upload command buffer: " ++ errMsg) >> hFlush stdout
                          return False
                  else do
                      sdlLog "!!! Failed to record upload commands. Upload cancelled." >> hFlush stdout
                      return False
              else do
                  sdlLog "!!! Mapping/copying failed. Upload cancelled." >> hFlush stdout
                  return False
          _ -> do
              sdlLog "!!! Failed to acquire transfer buffers or command buffer for upload." >> hFlush stdout
              return False

recordUploadCommands :: SDLGPUDevice -> SDLGPUCommandBuffer -> SDLGPUTransferBuffer -> SDLGPUTransferBuffer -> SDLGPUBuffer -> SDLGPUBuffer -> SDLGPUTexture -> CSize -> CSize -> Int -> Int -> IO Bool
recordUploadCommands dev cmdBuf bufTransfer texTransfer vertexBuf indexBuf texture vOffsetC iOffsetC texWidth texHeight = do
    sdlLog "Beginning Copy Pass for uploads..."
    bracket (sdlBeginGPUCopyPass cmdBuf)
            (\mcp -> when (isJust mcp) $ sdlEndGPUCopyPass (fromJust mcp)) $
            \case
            Nothing -> sdlLog "!!! Failed to begin copy pass." >> return False
            Just copyPass -> do
                sdlLog "Copy Pass begun. Recording uploads..."
                (_, _, vertexSizeW32) <- calculateBufferDataSize vertexData "Vertex"
                (_, _, indexSizeW32) <- calculateBufferDataSize indexData "Index"

                let vbSrc = SDLGPUTransferBufferLocation bufTransfer (fromIntegral vOffsetC)
                let vbDst = SDLGPUBufferRegion vertexBuf 0 vertexSizeW32
                sdlLog $ "Recording Vertex Buffer Upload: " ++ show vbSrc ++ " -> " ++ show vbDst
                sdlUploadToGPUBuffer copyPass vbSrc vbDst False

                let ibSrc = SDLGPUTransferBufferLocation bufTransfer (fromIntegral iOffsetC)
                let ibDst = SDLGPUBufferRegion indexBuf 0 indexSizeW32
                sdlLog $ "Recording Index Buffer Upload: " ++ show ibSrc ++ " -> " ++ show ibDst
                sdlUploadToGPUBuffer copyPass ibSrc ibDst False

                let texSrc = SDLGPUTextureTransferInfo
                                { gpuTexTransferBuffer = texTransfer
                                , gpuTexTransferOffset = 0
                                , gpuTexTransferPixelsPerRow = 0
                                , gpuTexTransferRowsPerLayer = 0
                                }
                let texDst = SDLGPUTextureRegion texture 0 0 0 0 0 (fromIntegral texWidth) (fromIntegral texHeight) 1
                sdlLog $ "Recording Texture Upload (Simplified Src): " ++ show texSrc ++ " -> " ++ show texDst
                sdlUploadToGPUTexture copyPass texSrc texDst False

                sdlLog "Upload commands recorded."
                return True

releaseResources :: Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources Context{..} (Just AppResources{..}) = do
    sdlLog "--> Releasing AppResources..."
    sdlLog $ "  --> Releasing Pipeline: " ++ show resPipeline
    sdlReleaseGPUGraphicsPipeline contextDevice resPipeline
    sdlLog "  <-- Pipeline Released."
    sdlLog $ "  --> Releasing Vertex Buffer: " ++ show resVertexBuffer
    sdlReleaseGPUBuffer contextDevice resVertexBuffer
    sdlLog "  <-- Vertex Buffer Released."
    sdlLog $ "  --> Releasing Index Buffer: " ++ show resIndexBuffer
    sdlReleaseGPUBuffer contextDevice resIndexBuffer
    sdlLog "  <-- Index Buffer Released."
    sdlLog $ "  --> Releasing Texture: " ++ show resTexture
    sdlReleaseGPUTexture contextDevice resTexture
    sdlLog "  <-- Texture Released."
    sdlLog $ "  --> Releasing " ++ show (length resSamplers) ++ " Samplers..."
    mapM_ (sdlReleaseGPUSampler contextDevice) resSamplers
    sdlLog "  <-- Samplers Released."
    sdlLog "<-- AppResources Released."

eventLoopGPU :: Context -> AppResources -> Word64 -> Word64 -> IORef Double -> IORef Int -> IO ()
eventLoopGPU context resources lastTime freq deltaTimeRef samplerIndexRef = do
  currentTime <- sdlGetPerformanceCounter
  let deltaTime = fromIntegral (currentTime - lastTime) / fromIntegral freq
  writeIORef deltaTimeRef (deltaTime * 1000.0)
  sdlLog $ printf "%.4f" deltaTime

  sdlPumpEvents
  shouldQuitRef <- newIORef False
  processEventsGPU shouldQuitRef samplerIndexRef
  sdlLog "Processed GPU events"

  shouldQuit <- readIORef shouldQuitRef
  unless shouldQuit $ do
    renderFrameGPU context resources samplerIndexRef
    sdlLog "Render frame succeeded"
    eventLoopGPU context resources currentTime freq deltaTimeRef samplerIndexRef

processEventsGPU :: IORef Bool -> IORef Int -> IO ()
processEventsGPU shouldQuitRef samplerIndexRef = do
    maybeEvent <- sdlPollEvent
    case maybeEvent of
        Nothing -> return ()
        Just event -> do
            quit <- handleEventGPU event samplerIndexRef
            when quit $ writeIORef shouldQuitRef True
            processEventsGPU shouldQuitRef samplerIndexRef

handleEventGPU :: SDLEvent -> IORef Int -> IO Bool
handleEventGPU event samplerIndexRef = case event of
  SDLEventQuit _ -> sdlLog "Quit event received." >> return True
  SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ scancode _ _ _ down _) | down -> do
    case scancode of
        SDL_SCANCODE_LEFT -> do
            currentIndex <- atomicModifyIORef' samplerIndexRef $ \i ->
                let newIndex = (i - 1 + samplerCount) `mod` samplerCount
                in (newIndex, newIndex)
            sdlLog $ "Setting sampler state to: " ++ samplerNames !! currentIndex
            return False
        SDL_SCANCODE_RIGHT -> do
            currentIndex <- atomicModifyIORef' samplerIndexRef $ \i ->
                let newIndex = (i + 1) `mod` samplerCount
                in (newIndex, newIndex)
            sdlLog $ "Setting sampler state to: " ++ samplerNames !! currentIndex
            return False
        SDL_SCANCODE_Q -> return True
        _ -> return False
  _ -> return False


renderFrameGPU :: Context -> AppResources -> IORef Int -> IO ()
renderFrameGPU Context{..} AppResources{..} samplerIndexRef = do
    maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
    case maybeCmdbuf of
        Nothing -> sdlLog "Error: Failed to acquire render command buffer."
        Just cmdbuf -> do
            maybeSwapchain <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
            case maybeSwapchain of
                Nothing -> do
                    sdlLog "Error: Failed to acquire swapchain texture."
                    void $ sdlSubmitGPUCommandBuffer cmdbuf `finally` return ()
                Just (swapchainTexture, w, h) -> do
                    let clearColor = SDLFColor 0.0 0.0 0.0 1.0
                    let colorTargetInfo = defaultColorTargetInfo
                            { texture = swapchainTexture
                            , clearColor = clearColor
                            , loadOp = SDL_GPU_LOADOP_CLEAR
                            , storeOp = SDL_GPU_STOREOP_STORE
                            }
                    bracket (sdlBeginGPURenderPass cmdbuf [colorTargetInfo] Nothing)
                            cleanupMaybeRenderPass $ \case
                            Nothing -> sdlLog "Error: Failed to begin render pass."
                            Just renderPass -> do
                                sdlBindGPUGraphicsPipeline renderPass resPipeline
                                let vbBinding = SDLGPUBufferBinding resVertexBuffer 0
                                sdlBindGPUVertexBuffers renderPass 0 [vbBinding]
                                let ibBinding = SDLGPUBufferBinding resIndexBuffer 0
                                sdlBindGPUIndexBuffer renderPass ibBinding SDL_GPU_INDEXELEMENTSIZE_16BIT
                                currentSamplerIndex <- readIORef samplerIndexRef 
                                let currentSampler = resSamplers !! currentSamplerIndex
                                let texSamplerBinding = SDLGPUTextureSamplerBinding resTexture currentSampler
                                sdlLog $ "Binding Texture: " ++ show resTexture ++ " with Sampler: " ++ samplerNames !! currentSamplerIndex ++ " (" ++ show currentSampler ++ ")"
                                sdlBindGPUFragmentSamplers renderPass 0 [texSamplerBinding]
                                sdlDrawGPUIndexedPrimitives renderPass 6 1 0 0 0
                    submitted <- sdlSubmitGPUCommandBuffer cmdbuf
                    unless submitted $ sdlLog "Error: Failed to submit render command buffer."
