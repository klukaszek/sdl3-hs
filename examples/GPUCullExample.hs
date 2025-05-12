{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Example     : GPUCullMode
-- Description : SDL Example: Demonstrating cull modes and vertex winding order
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Based on the SDL_gpu_examples/CullMode C example.
-- Demonstrates:
-- - Creating multiple pipelines with different rasterizer states (cull mode, front face).
-- - Using two vertex buffers with different winding orders (CW vs CCW).
-- - Drawing primitives to different viewports within the same render pass.
-- - Handling keyboard input to cycle through pipeline states.
-- |
module Main where

-- Import common init/quit and shader loading

-- Add forM/forM_
import Control.Exception (bracket, bracketOnError, finally, onException)
import Control.Monad (forM, forM_, unless, void, when)
-- Add CInt

import Data.Bits ((.|.))
import Data.Foldable (for_)
import Data.IORef
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Word (Word32, Word64, Word8)
import Foreign.C.Types (CFloat, CInt, CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..), peekByteOff, poke, pokeByteOff, sizeOf)
import GPUCommon
import SDL
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

-- Define vertex data (CW and CCW)
vertexDataCW :: [PositionColorVertex]
vertexDataCW =
  -- Standard winding order (matches previous example)
  [ PositionColorVertex (-1) (-1) 0 255 0 0 255, -- Red    (Bottom-Left)
    PositionColorVertex 1 (-1) 0 0 255 0 255, -- Green  (Bottom-Right)
    PositionColorVertex 0 1 0 0 0 255 255 -- Blue   (Top-Center)
  ]

vertexDataCCW :: [PositionColorVertex]
vertexDataCCW =
  -- Reversed winding order
  [ PositionColorVertex 0 1 0 255 0 0 255, -- Blue   (Top-Center)
    PositionColorVertex 1 (-1) 0 0 255 0 255, -- Green  (Bottom-Right)
    PositionColorVertex (-1) (-1) 0 0 0 255 255 -- Red    (Bottom-Left)
  ]

-- Mode names for display
modeNames :: [String]
modeNames =
  [ "CW_CullNone",
    "CW_CullFront",
    "CW_CullBack",
    "CCW_CullNone",
    "CCW_CullFront",
    "CCW_CullBack"
  ]

-- 2. Define Managed Resources
data AppResources = AppResources
  { resPipelines :: [SDLGPUGraphicsPipeline], -- List of 6 pipelines
    resVertexBufferCW :: SDLGPUBuffer,
    resVertexBufferCCW :: SDLGPUBuffer
  }

-- 3. Define Application State
newtype AppState = AppState
  { appCurrentMode :: IORef Int -- Index into modeNames/resPipelines
  }

main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  -- Use withContext for base SDL/GPU/Window setup
  maybeResult <- withContext "SDL3 Haskell GPU Cull Mode" [SDL_WINDOW_RESIZABLE] runAppGPU
  case maybeResult of
    Nothing -> do
      sdlLog "Application initialization failed (commonInit)."
      exitFailure
    Just _ -> do
      sdlLog "Application finished successfully."
      exitSuccess

-- | Application logic, receives the base Context
runAppGPU :: Context -> IO ()
runAppGPU context@Context {..} = do
  sdlLog "Base context initialized."

  -- Bracket pattern to manage app resources
  bracket
    (createResources context)
    ( \mRes -> do
        sdlLog "--> Entering AppResources release bracket..."
        releaseResources context mRes
        sdlLog "<-- Exiting AppResources release bracket."
    )
    $ \case
      Nothing -> sdlLog "Failed to create resources. Exiting."
      Just resources -> do
        sdlLog "Resources created successfully."
        sdlLog "Press Left/Right to switch between modes"
        sdlLog $ "Initial Mode: " ++ head modeNames

        -- Create mutable state refs
        state <- AppState <$> newIORef 0 -- Start at mode 0

        -- Start event loop
        startTime <- sdlGetPerformanceCounter
        freq <- sdlGetPerformanceFrequency
        deltaTimeRef <- newIORef 0.0
        eventLoopGPU context resources state startTime freq deltaTimeRef
  where
    -- Action to create all GPU resources (pipelines, vertex buffers, upload data)
    createResources :: Context -> IO (Maybe AppResources)
    createResources ctx@Context {contextDevice = dev, contextWindow = win} = do
      -- Load Shaders
      sdlLog "Loading shaders..."
      -- Use bracket for shaders too
      bracketOnError
        (loadShader dev "PositionColor.vert" SDL_GPU_SHADERSTAGE_VERTEX defaultShaderCreateInfo)
        (maybe (return ()) (sdlReleaseGPUShader dev))
        $ \maybeVertShader ->
          bracketOnError
            (loadShader dev "SolidColor.frag" SDL_GPU_SHADERSTAGE_FRAGMENT defaultShaderCreateInfo)
            (maybe (return ()) (sdlReleaseGPUShader dev))
            $ \maybeFragShader -> do
              case (maybeVertShader, maybeFragShader) of
                (Just vertShader, Just fragShader) -> do
                  sdlLog "Shaders loaded successfully."

                  -- Create Vertex Buffers and Upload Data
                  maybeVertexBuffers <- createAndUploadVertexBuffers dev vertexDataCW vertexDataCCW
                  case maybeVertexBuffers of
                    Nothing -> do
                      sdlLog "Failed to create/upload vertex buffers."
                      -- Shaders released by bracketOnError
                      return Nothing
                    Just (vbCW, vbCCW) -> do
                      sdlLog "Vertex buffers created and data uploaded."

                      -- Create Pipelines
                      maybePipelines <- createPipelines dev win vertShader fragShader
                      case maybePipelines of
                        Nothing -> do
                          sdlLog "Failed to create pipelines."
                          -- Release the vertex buffers we just created
                          sdlReleaseGPUBuffer dev vbCW
                          sdlReleaseGPUBuffer dev vbCCW
                          -- Shaders released by bracketOnError
                          return Nothing
                        Just pipelines -> do
                          sdlLog "Pipelines created successfully."
                          -- Shaders will be released by bracketOnError now
                          return $ Just (AppResources pipelines vbCW vbCCW)
                _ -> do
                  sdlLog "Failed to load one or both shaders."
                  -- Shaders released by bracketOnError
                  return Nothing

    -- Helper to create the 6 pipelines
    createPipelines :: SDLGPUDevice -> SDLWindow -> SDLGPUShader -> SDLGPUShader -> IO (Maybe [SDLGPUGraphicsPipeline])
    createPipelines dev win vertShader fragShader = do
      swapchainFormat <- sdlGetGPUSwapchainTextureFormat dev win

      -- Define Vertex Input State (Same as previous example)
      let vertexSize = sizeOf (undefined :: PositionColorVertex)
      let colorOffset = sizeOf (undefined :: CFloat) * 3
      let vertexAttributes =
            [ SDLGPUVertexAttribute 0 0 SDL_GPU_VERTEXELEMENTFORMAT_FLOAT3 0,
              SDLGPUVertexAttribute 1 0 SDL_GPU_VERTEXELEMENTFORMAT_UBYTE4_NORM (fromIntegral colorOffset)
            ]
          vertexBufferDesc =
            [ SDLGPUVertexBufferDescription 0 (fromIntegral vertexSize) SDL_GPU_VERTEXINPUTRATE_VERTEX 0
            ]
          vertexInputState = SDLGPUVertexInputState vertexBufferDesc vertexAttributes

      let colorTargetDesc = SDLGPUColorTargetDescription swapchainFormat defaultColorTargetBlendState
          targetInfo = SDLGPUGraphicsPipelineTargetInfo [colorTargetDesc] SDL_GPU_TEXTUREFORMAT_INVALID False
          basePipelineCI =
            SDLGPUGraphicsPipelineCreateInfo
              { vertexShader = vertShader,
                fragmentShader = fragShader,
                vertexInputState = vertexInputState,
                primitiveType = SDL_GPU_PRIMITIVETYPE_TRIANGLELIST,
                multisampleState = defaultMultiSampleState,
                rasterizerState = defaultRasterizerState, -- Will be modified
                targetInfo = targetInfo,
                depthStencilState = defaultDepthStencilState,
                props = 0
              }

      -- Loop to create 6 pipelines
      results <- forM [0 .. 5] $ \i -> do
        let cullMode = toEnum (i `mod` 3) :: SDLGPUCullMode -- 0=None, 1=Front, 2=Back
            frontFace = if i < 3 then SDL_GPU_FRONTFACE_COUNTER_CLOCKWISE else SDL_GPU_FRONTFACE_CLOCKWISE
            modeName = modeNames !! i

        sdlLog $ "Creating Pipeline " ++ show i ++ " (" ++ modeName ++ "): Cull=" ++ show cullMode ++ ", Front=" ++ show frontFace
        let currentRasterizerState = defaultRasterizerState {cullMode = cullMode, frontFace = frontFace}
        let currentCI = basePipelineCI {rasterizerState = currentRasterizerState}

        maybePipeline <- sdlCreateGPUGraphicsPipeline dev currentCI
        when (isNothing maybePipeline) $ do
          err <- sdlGetError
          sdlLog $ "!!! Failed to create pipeline " ++ show i ++ ": " ++ err
        return maybePipeline

      -- Check if all pipelines were created
      if all isJust results
        then
          return $ Just (map fromJust results)
        else do
          sdlLog "!!! Cleaning up partially created pipelines due to failure."
          -- Clean up any pipelines that *were* successfully created
          forM_ results $ \maybeP -> maybe (return ()) (sdlReleaseGPUGraphicsPipeline dev) maybeP
          return Nothing

    -- Modified upload function for two buffers
    createAndUploadVertexBuffers :: SDLGPUDevice -> [PositionColorVertex] -> [PositionColorVertex] -> IO (Maybe (SDLGPUBuffer, SDLGPUBuffer))
    createAndUploadVertexBuffers dev dataCW dataCCW = do
      sdlLog "--- Beginning Vertex Buffer Creation and Upload (CW & CCW) ---"
      if null dataCW || null dataCCW
        then do
          sdlLog "Error: Vertex data list(s) are empty."
          return Nothing
        else do
          (vSizeCW, totalCSizeCW, totalSizeW32CW) <- calculateVertexDataSize dataCW
          (vSizeCCW, totalCSizeCCW, totalSizeW32CCW) <- calculateVertexDataSize dataCCW

          let totalTransferSize = totalSizeW32CW + totalSizeW32CCW -- Combine sizes for one transfer buffer
          sdlLog $ "Total Transfer Buffer Size: " ++ show totalTransferSize

          -- Create buffers first (using bracketOnError for complex cleanup)
          bracketOnError
            (createBufferPair dev (fromIntegral totalCSizeCW) (fromIntegral totalCSizeCCW))
            (cleanupBufferPair dev)
            $ \maybeBuffers -> do
              case maybeBuffers of
                Nothing -> return Nothing -- Buffer creation failed
                Just (vbCW, vbCCW) -> do
                  -- Proceed with transfer and upload
                  uploadSuccess <- uploadBothViaTransferBuffer dev vbCW vbCCW dataCW dataCCW totalTransferSize totalCSizeCW totalSizeW32CW totalSizeW32CCW
                  if uploadSuccess
                    then do
                      sdlLog "--- Vertex Buffers Creation and Upload Successful ---"
                      return $ Just (vbCW, vbCCW)
                    else do
                      sdlLog "!!! Vertex Buffer Upload Failed Overall."
                      -- Cleanup handled by bracketOnError
                      return Nothing
      where
        createBufferPair dev sizeCW sizeCCW = do
          sdlLog "Creating CW Vertex Buffer..."
          mVB_CW <- sdlCreateGPUBuffer dev (SDLGPUBufferCreateInfo SDL_GPU_BUFFERUSAGE_VERTEX sizeCW 0)
          case mVB_CW of
            Nothing -> sdlLog "!!! Failed to create CW VB" >> return Nothing
            Just vbCW -> do
              sdlLog "Creating CCW Vertex Buffer..."
              mVB_CCW <- sdlCreateGPUBuffer dev (SDLGPUBufferCreateInfo SDL_GPU_BUFFERUSAGE_VERTEX sizeCCW 0)
              case mVB_CCW of
                Nothing -> sdlLog "!!! Failed to create CCW VB" >> sdlReleaseGPUBuffer dev vbCW >> return Nothing
                Just vbCCW -> return $ Just (vbCW, vbCCW)

        cleanupBufferPair dev maybePair =
          when (isJust maybePair) $ do
            let (vbCW, vbCCW) = fromJust maybePair
            sdlLog "Error occurred, releasing vertex buffer pair..."
            sdlReleaseGPUBuffer dev vbCW
            sdlReleaseGPUBuffer dev vbCCW

        -- Reuses helpers from previous example's meticulous version, adapted slightly
        uploadBothViaTransferBuffer dev vbCW vbCCW dataCW dataCCW transferSize totalSizeCW_CSize totalSizeCW_W32 totalSizeCCW_W32 = do
          bracket
            (createTransferBuffer dev transferSize SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "buffer")
            (cleanupTransferBuffer dev)
            $ \case
              Nothing -> return False
              Just transferBuffer -> do
                mapCopySuccess <- mapAndCopyBothData dev transferBuffer dataCW dataCCW totalSizeCW_CSize
                if mapCopySuccess
                  then uploadBothDataCommandBuffer dev vbCW vbCCW transferBuffer totalSizeCW_W32 totalSizeCCW_W32
                  else return False

        -- Combined map and copy for both datasets
        mapAndCopyBothData dev tb dCW dCCW offsetBytesCW_CSize = do
          bracket
            (sdlMapGPUTransferBuffer dev tb False)
            (\mPtr -> when (isJust mPtr) $ sdlUnmapGPUTransferBuffer dev tb)
            $ \maybeMappedPtr -> do
              case maybeMappedPtr of
                Nothing -> sdlLog "!!! Map failed" >> return False
                Just mappedPtr -> do
                  let basePtr = castPtr mappedPtr :: Ptr PositionColorVertex -- Cast for type safety with pokeArray
                  sdlLog $ "Mapping successful. Base Ptr: " ++ show basePtr ++ ". Copying CW data..."
                  pokeArray basePtr dCW -- Write the first array at the start

                  -- Calculate the offset pointer for the second array
                  -- We need the offset in bytes (CSize)
                  let offsetBytes = fromIntegral offsetBytesCW_CSize :: Int
                  -- Use plusPtr for byte-offset pointer arithmetic
                  let offsetPtr = plusPtr basePtr offsetBytes

                  sdlLog $ "Copying CCW data at offset " ++ show offsetBytes ++ " (Ptr: " ++ show offsetPtr ++ ")"
                  pokeArray offsetPtr dCCW -- Write the second array at the calculated offset
                  sdlLog "Both datasets copied."
                  return True

        -- Combined upload command for both datasets
        uploadBothDataCommandBuffer dev vbCW vbCCW tb sizeCW sizeCCW = do
          bracket (sdlAcquireGPUCommandBuffer dev) cleanupCommandBuffer $ \maybeCmdBuf -> do
            case maybeCmdBuf of
              Nothing -> sdlLog "!!! Acquire upload CB failed" >> return False
              Just cmdBuf ->
                bracket (sdlBeginGPUCopyPass cmdBuf) cleanupCopyPass $ \maybeCopyPass ->
                  do
                    case maybeCopyPass of
                      Nothing -> sdlLog "!!! Begin copy pass failed" >> return False
                      Just copyPass -> do
                        -- Upload CW
                        let srcLocCW = SDLGPUTransferBufferLocation tb 0
                            dstRegCW = SDLGPUBufferRegion vbCW 0 sizeCW
                        sdlLog $ "Recording upload CW (Size: " ++ show sizeCW ++ ")"
                        sdlUploadToGPUBuffer copyPass srcLocCW dstRegCW False
                        -- Upload CCW
                        let offset = fromIntegral sizeCW -- Offset in transfer buffer
                        let srcLocCCW = SDLGPUTransferBufferLocation tb offset
                            dstRegCCW = SDLGPUBufferRegion vbCCW 0 sizeCCW
                        sdlLog $ "Recording upload CCW (Offset: " ++ show offset ++ ", Size: " ++ show sizeCCW ++ ")"
                        sdlUploadToGPUBuffer copyPass srcLocCCW dstRegCCW False
                        sdlLog "Uploads recorded."
                        return True -- Copy pass commands recorded ok
                    >>= \copyCommandsOk ->
                      if copyCommandsOk
                        then do
                          sdlLog "Submitting combined upload..."
                          submitted <- sdlSubmitGPUCommandBuffer cmdBuf
                          unless submitted $ sdlLog "!!! Combined upload submission failed"
                          return submitted
                        else do
                          sdlLog "Copy pass failed, cancelling."
                          void $ sdlCancelGPUCommandBuffer cmdBuf
                          return False

    -- Action to release resources
    releaseResources :: Context -> Maybe AppResources -> IO ()
    releaseResources _ Nothing = return () -- Nothing to clean up
    releaseResources Context {..} (Just AppResources {..}) = do
      sdlLog "--> Releasing AppResources..."
      sdlLog "  --> Releasing Pipelines..."
      forM_ (zip [0 ..] resPipelines) $ \(i, p) -> do
        sdlLog $ "    Releasing Pipeline " ++ show i ++ ": " ++ show p
        sdlReleaseGPUGraphicsPipeline contextDevice p
      sdlLog "  <-- Pipelines Released."
      sdlLog $ "  --> Releasing Vertex Buffer CW: " ++ show resVertexBufferCW
      sdlReleaseGPUBuffer contextDevice resVertexBufferCW
      sdlLog $ "  <-- Vertex Buffer CW Released."
      sdlLog $ "  --> Releasing Vertex Buffer CCW: " ++ show resVertexBufferCCW
      sdlReleaseGPUBuffer contextDevice resVertexBufferCCW
      sdlLog $ "  <-- Vertex Buffer CCW Released."
      sdlLog "<-- AppResources Released."

-- | Helper to calculate sizes and log them (same as before)
calculateVertexDataSize :: [PositionColorVertex] -> IO (Int, CSize, Word32)
calculateVertexDataSize dataList = do
  let vertexSize = sizeOf (head dataList) -- Size of one vertex
  let numVertices = length dataList
  let totalBytes = numVertices * vertexSize
  let totalCSize = fromIntegral totalBytes :: CSize
  let totalSizeWord32 = fromIntegral totalBytes :: Word32
  sdlLog $
    "Vertex Info - SizeOf: "
      ++ show vertexSize
      ++ ", Count: "
      ++ show numVertices
      ++ ", Total Bytes (CSize): "
      ++ show totalCSize
      ++ ", Total Bytes (Word32): "
      ++ show totalSizeWord32
  when (vertexSize /= 16) $ sdlLog "!!! WARNING: Calculated vertex size is not 16 bytes!"
  when (totalBytes == 0) $ sdlLog "!!! WARNING: Vertex data is empty!"
  return (vertexSize, totalCSize, totalSizeWord32)

-- | Main event loop
eventLoopGPU :: Context -> AppResources -> AppState -> Word64 -> Word64 -> IORef Double -> IO ()
eventLoopGPU context resources state lastTime freq deltaTimeRef = do
  currentTime <- sdlGetPerformanceCounter
  let deltaTime = fromIntegral (currentTime - lastTime) / fromIntegral freq
  writeIORef deltaTimeRef (deltaTime * 1000.0) -- Store dt in ms
  sdlPumpEvents
  shouldQuitRef <- newIORef False
  processEventsGPU context shouldQuitRef deltaTimeRef state

  shouldQuit <- readIORef shouldQuitRef
  unless shouldQuit $ do
    -- Use non-blocking acquire as it's safer based on previous findings
    renderFrameGPU context resources state True

    eventLoopGPU context resources state currentTime freq deltaTimeRef

-- | Process all pending events, modifying state based on input
processEventsGPU :: Context -> IORef Bool -> IORef Double -> AppState -> IO ()
processEventsGPU context shouldQuitRef deltaTimeRef state = do
  maybeEvent <- sdlPollEvent
  case maybeEvent of
    Nothing -> return () -- No more events
    Just event -> do
      quit <- handleEventGPU context event deltaTimeRef state -- Pass context
      when quit $ writeIORef shouldQuitRef True
      processEventsGPU context shouldQuitRef deltaTimeRef state -- Process next event

-- | Handle a single SDL event, modifying state IORefs and window title
handleEventGPU :: Context -> SDLEvent -> IORef Double -> AppState -> IO Bool
handleEventGPU context event deltaTimeRef state@AppState {..} = case event of
  SDLEventQuit _ -> sdlLog "Quit event received." >> return True
  SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ scancode _ _ _ down _) | down -> do
    let numModes = length modeNames
    case scancode of
      SDL_SCANCODE_Q -> return True
      SDL_SCANCODE_LEFT -> do
        -- Modify the state and get the new index back
        newModeIndex <- atomicModifyIORef' appCurrentMode $ \m ->
          let newIndex = (m - 1 + numModes) `mod` numModes
           in (newIndex, newIndex) -- Return (new state, result which is the new index)
          -- Print the mode name using the new index
        sdlLog $ "Switched to Mode: " ++ (modeNames !! newModeIndex)
        return False
      SDL_SCANCODE_RIGHT -> do
        -- Modify the state and get the new index back
        newModeIndex <- atomicModifyIORef' appCurrentMode $ \m ->
          let newIndex = (m + 1) `mod` numModes
           in (newIndex, newIndex) -- Return (new state, result which is the new index)
          -- Print the mode name using the new index
        sdlLog $ "Switched to Mode: " ++ (modeNames !! newModeIndex)
        return False
      _ -> return False
  _ -> return False

-- | Render a frame using the appropriate pipeline and state
renderFrameGPU :: Context -> AppResources -> AppState -> Bool -> IO ()
renderFrameGPU Context {..} AppResources {..} AppState {..} useNonBlockingAcquire = do
  currentMode <- readIORef appCurrentMode
  let currentPipeline = resPipelines !! currentMode

  -- 1. Acquire Command Buffer
  maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
  case maybeCmdbuf of
    Nothing -> sdlGetError >>= \err -> sdlLog $ "Error: Failed to acquire render command buffer: " ++ err
    Just cmdbuf -> do
      -- 2. Acquire Swapchain Texture
      maybeSwapchain <-
        if useNonBlockingAcquire
          then sdlAcquireGPUSwapchainTexture cmdbuf contextWindow
          else sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
      case maybeSwapchain of
        Nothing -> do
          when useNonBlockingAcquire $ return () -- sdlLog "Trace: Swapchain not ready (non-blocking)"
          unless useNonBlockingAcquire $ sdlLog "!!! Error: Failed to acquire swapchain texture (blocking)"
          void $ sdlSubmitGPUCommandBuffer cmdbuf -- Submit empty buffer on failure to prevent deadlock
        Just (swapchainTexture, winWidth, winHeight) -> do
          let clearColor = SDLFColor 0.0 0.0 0.0 1.0
          let colorTargetInfo = SDLGPUColorTargetInfo swapchainTexture 0 0 clearColor SDL_GPU_LOADOP_CLEAR SDL_GPU_STOREOP_STORE Nothing 0 0 False False

          -- Define viewports for left and right halves
          let halfWidth = fromIntegral winWidth / 2.0 :: Float
              vpHeight = fromIntegral winHeight :: Float
              vpLeft = SDLGPUViewport 0 0 halfWidth vpHeight 0.0 1.0
              vpRight = SDLGPUViewport halfWidth 0 halfWidth vpHeight 0.0 1.0

          -- 3. Begin Render Pass
          bracket
            (sdlBeginGPURenderPass cmdbuf [colorTargetInfo] Nothing)
            (\mrp -> for_ mrp sdlEndGPURenderPass)
            $ \case
              Nothing -> sdlGetError >>= \err -> sdlLog $ "!!! Error: Failed to begin render pass: " ++ err
              Just renderPass -> do
                -- Bind the CURRENT pipeline (selected by state)
                sdlBindGPUGraphicsPipeline renderPass currentPipeline

                -- Draw LEFT half (CW vertices)
                sdlSetGPUViewport renderPass vpLeft
                let vbBindingCW = SDLGPUBufferBinding resVertexBufferCW 0
                sdlBindGPUVertexBuffers renderPass 0 [vbBindingCW]
                sdlDrawGPUPrimitives renderPass 3 1 0 0

                -- Draw RIGHT half (CCW vertices)
                sdlSetGPUViewport renderPass vpRight
                let vbBindingCCW = SDLGPUBufferBinding resVertexBufferCCW 0
                sdlBindGPUVertexBuffers renderPass 0 [vbBindingCCW]
                sdlDrawGPUPrimitives renderPass 3 1 0 0

          -- 4. Submit Command Buffer
          submitted <- sdlSubmitGPUCommandBuffer cmdbuf
          unless submitted $ sdlGetError >>= \err -> sdlLog $ "!!! Error: Failed to submit render command buffer: " ++ err
