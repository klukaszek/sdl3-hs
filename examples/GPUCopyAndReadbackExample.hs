{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- \|
-- Example     : GPUCopyAndReadback
-- Description : Tests GPU texture/buffer copy, blit, upload, download, and data integrity.
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Based on SDL_gpu_examples/CopyAndReadback.c example.
-- Demonstrates:
-- - Uploading image and buffer data to the GPU.
-- - GPU-to-GPU copies: `SDL_CopyGPUTextureToTexture`, `SDL_CopyGPUBufferToBuffer`.
-- - GPU blit: `SDL_BlitGPUTexture` (including scaling).
-- - GPU-to-CPU downloads: `SDL_DownloadFromGPUTexture`, `SDL_DownloadFromGPUBuffer`.
-- - Synchronization with `SDL_GPUFence` for readback.
-- - Verification of downloaded data against original data.
-- - Displays original, copied, and blitted textures.
-- \|

import Control.Exception (bracket, bracketOnError, finally)
import Control.Monad (unless, void, when, (>=>))
import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.IORef
import Data.Maybe (isJust)
import Data.Word (Word32)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (peek)
import GPUCommon
import SDL3
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

-- AppResources
data AppResources = AppResources
  { resOriginalTexture :: SDLGPUTexture,
    resTextureCopy :: SDLGPUTexture,
    resTextureSmall :: SDLGPUTexture,
    resOriginalBuffer :: SDLGPUBuffer,
    resBufferCopy :: SDLGPUBuffer,
    -- Store original image and buffer data for comparison
    originalImageDataBytes :: BS.ByteString,
    originalBufferData :: [Word32],
    textureWidth :: Int,
    textureHeight :: Int
  }

-- Test buffer data
bufferData :: [Word32]
bufferData = [2, 4, 8, 16, 32, 64, 128] -- 7 elements = 28 bytes

-- main
main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion
  maybeResult <- withContext "SDL3 Haskell GPU Copy & Readback" [] runAppGPU
  case maybeResult of
    Nothing -> sdlLog "Application initialization failed." >> exitFailure
    Just _ -> sdlLog "Application finished successfully." >> exitSuccess

-- runAppGPU
runAppGPU :: Context -> IO ()
runAppGPU context = do
  sdlLog "Base context initialized."
  bracket
    (createAndTestResources context)
    (releaseResources context)
    $ \case
      Nothing -> sdlLog "Failed to create or test resources. Exiting."
      Just resources -> do
        sdlLog "Resources created, tested, and operations successful. Entering display loop."
        eventLoopGPU context resources

-- createAndTestResources: Combines Init() and the initial test logic from C
createAndTestResources :: Context -> IO (Maybe AppResources)
createAndTestResources Context {..} = do
  sdlLog "--- Beginning Resource Creation and Initial Tests ---"

  -- Load the source image (bracket ensures surface is freed if loadImage fails or subsequent ops fail)
  bracketOnError
    (loadImage ("Content" </> "Images" </> "ravioli.bmp"))
    (\case Just surf -> sdlDestroySurface surf; _ -> pure ())
    $ \case
      Nothing -> sdlLog "!!! Failed to load source image." >> return Nothing
      Just surfacePtr ->
        -- This inner bracket is to ensure the loaded surfacePtr is freed
        -- after we're done with its pixel data, even if later steps fail.
        bracketOnError (pure surfacePtr) (\_ -> sdlLog "Releasing loaded surface from inner bracket" >> sdlDestroySurface surfacePtr) $ \surf -> do
          surfaceData <- peek surf
          let imgW = surfaceW surfaceData
          let imgH = surfaceH surfaceData
          let imgPitch = surfacePitch surfaceData
          let imgPixelsPtr = surfacePixels surfaceData
          let imgDataSize = imgH * imgPitch
          originalBS <- BSU.unsafePackCStringLen (castPtr imgPixelsPtr, imgDataSize)

          -- Create Textures & Buffers
          let commonTexCI = (defaultTextureCreateInfo imgW imgH) {texInfoUsage = SDL_GPU_TEXTUREUSAGE_SAMPLER}
          maybeOrigTex <- sdlCreateGPUTexture contextDevice commonTexCI
          maybeTexCopy <- sdlCreateGPUTexture contextDevice commonTexCI
          maybeTexSmall <-
            sdlCreateGPUTexture
              contextDevice
              ( (defaultTextureCreateInfo (imgW `div` 2) (imgH `div` 2))
                  { texInfoUsage = SDL_GPU_TEXTUREUSAGE_SAMPLER .|. SDL_GPU_TEXTUREUSAGE_COLOR_TARGET
                  }
              )

          (_, _, bufferDataSizeW32) <- calculateBufferDataSize bufferData "OriginalBufferData"
          let bufferUsage = SDL_GPU_BUFFERUSAGE_GRAPHICS_STORAGE_READ
          maybeOrigBuf <- createGPUBuffer contextDevice bufferUsage bufferDataSizeW32 "OriginalBuffer"
          maybeBufCopy <- createGPUBuffer contextDevice bufferUsage bufferDataSizeW32 "BufferCopy"

          let totalUploadSize = fromIntegral imgDataSize + bufferDataSizeW32
          let totalDownloadSize = totalUploadSize
          maybeUploadTransfer <- createTransferBuffer contextDevice totalUploadSize SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "UploadCombined"
          maybeDownloadTransfer <- createTransferBuffer contextDevice totalDownloadSize SDL_GPU_TRANSFERBUFFERUSAGE_DOWNLOAD "DownloadCombined"

          -- Use a nested structure to handle Maybe resource creation and operations
          -- This ensures cleanup happens correctly for parts that were successfully created.
          case (maybeOrigTex, maybeTexCopy, maybeTexSmall, maybeOrigBuf, maybeBufCopy, maybeUploadTransfer, maybeDownloadTransfer) of
            (Just oTex, Just tCopy, Just tSmall, Just oBuf, Just bCopy, Just upTrans, Just downTrans) -> do
              mapAndPokeOk <- bracket
                (sdlMapGPUTransferBuffer contextDevice upTrans False)
                (\mptr -> when (isJust mptr) $ sdlUnmapGPUTransferBuffer contextDevice upTrans)
                $ \case
                  Nothing -> sdlLog "Failed to map upload transfer buffer" >> return False
                  Just mappedUpPtr -> do
                    copyBytes mappedUpPtr imgPixelsPtr imgDataSize
                    pokeArray (castPtr mappedUpPtr `plusPtr` imgDataSize) bufferData
                    return True

              if not mapAndPokeOk
                then do
                  sdlLog "Map and poke to upload buffer failed."
                  -- Release resources created so far in this 'Just' block before returning Nothing
                  sdlReleaseGPUTexture contextDevice oTex
                  sdlReleaseGPUTexture contextDevice tCopy
                  sdlReleaseGPUTexture contextDevice tSmall
                  sdlReleaseGPUBuffer contextDevice oBuf
                  sdlReleaseGPUBuffer contextDevice bCopy
                  sdlReleaseGPUTransferBuffer contextDevice upTrans
                  sdlReleaseGPUTransferBuffer contextDevice downTrans
                  return Nothing
                else do
                  mCmdBuf <- sdlAcquireGPUCommandBuffer contextDevice
                  case mCmdBuf of
                    Nothing -> sdlLog "Failed to acquire command buffer" >> return Nothing -- Further cleanup needed if this was the only failure
                    Just cmdBuf -> do
                      -- Perform GPU operations, assuming success until a step fails
                      -- First Copy Pass
                      validAfterCopyPass1 <- do
                        -- Introduce a sub-do block to capture the Bool
                        mCopyPass1 <- sdlBeginGPUCopyPass cmdBuf
                        case mCopyPass1 of
                          Nothing -> sdlLog "Failed to begin first copy pass" >> return False
                          Just cp1 -> do
                            let upTexSrcInfo = SDLGPUTextureTransferInfo upTrans 0 (fromIntegral imgW) (fromIntegral imgH)
                            let upTexDstRegion = defaultTextureRegion oTex imgW imgH
                            sdlUploadToGPUTexture cp1 upTexSrcInfo upTexDstRegion False
                            let locO = SDLGPUTextureLocation oTex 0 0 0 0 0
                            let locC = SDLGPUTextureLocation tCopy 0 0 0 0 0
                            sdlCopyGPUTextureToTexture cp1 locO locC (fromIntegral imgW) (fromIntegral imgH) 1 False
                            let upBufSrcLoc = SDLGPUTransferBufferLocation upTrans (fromIntegral imgDataSize)
                            let upBufDstReg = SDLGPUBufferRegion oBuf 0 bufferDataSizeW32
                            sdlUploadToGPUBuffer cp1 upBufSrcLoc upBufDstReg False
                            let locOBuf = SDLGPUBufferLocation oBuf 0
                            let locCBuf = SDLGPUBufferLocation bCopy 0
                            sdlCopyGPUBufferToBuffer cp1 locOBuf locCBuf bufferDataSizeW32 False
                            sdlEndGPUCopyPass cp1
                            return True -- Indicate success of this block
                      if not validAfterCopyPass1
                        then
                          return Nothing -- Propagate failure for the whole createAndTestResources
                        else do
                          -- Blit Operation (Only if copyPass1 was successful)
                          let blitSrcReg = defaultBlitRegion oTex imgW imgH
                          let blitDstReg = defaultBlitRegion tSmall (imgW `div` 2) (imgH `div` 2)
                          let blitInfo = SDLGPUBlitInfo blitSrcReg blitDstReg SDL_GPU_LOADOP_DONT_CARE (SDLFColor 0 0 0 0) SDL_FLIP_NONE SDL_GPU_FILTER_LINEAR False
                          sdlBlitGPUTexture cmdBuf blitInfo

                          -- Second Copy Pass (Downloads)
                          mCopyPass2 <- sdlBeginGPUCopyPass cmdBuf
                          case mCopyPass2 of
                            Nothing -> sdlLog "Failed to begin second copy pass" >> return Nothing
                            Just cp2 -> do
                              let downTexSrcReg = defaultTextureRegion tCopy imgW imgH
                              let downTexDstInfo = SDLGPUTextureTransferInfo downTrans 0 (fromIntegral imgW) (fromIntegral imgH)
                              sdlDownloadFromGPUTexture cp2 downTexSrcReg downTexDstInfo
                              let downBufSrcReg = SDLGPUBufferRegion bCopy 0 bufferDataSizeW32
                              let downBufDstLoc = SDLGPUTransferBufferLocation downTrans (fromIntegral imgDataSize)
                              sdlDownloadFromGPUBuffer cp2 downBufSrcReg downBufDstLoc
                              sdlEndGPUCopyPass cp2

                              mFence <- sdlSubmitGPUCommandBufferAndAcquireFence cmdBuf
                              case mFence of
                                Nothing -> sdlLog "Failed to acquire fence" >> return Nothing
                                Just fence -> do
                                  waitOk <- sdlWaitForGPUFences contextDevice True [fence]
                                  sdlReleaseGPUFence contextDevice fence

                                  if waitOk
                                    then bracket
                                      (sdlMapGPUTransferBuffer contextDevice downTrans False)
                                      (\mptr -> when (isJust mptr) $ sdlUnmapGPUTransferBuffer contextDevice downTrans)
                                      $ \case
                                        Nothing -> sdlLog "Failed to map download buffer" >> return Nothing
                                        Just mappedDownPtr -> do
                                          downloadedTexBS <- BSU.unsafePackCStringLen (castPtr mappedDownPtr, imgDataSize)
                                          let texMatch = downloadedTexBS == originalBS
                                          if texMatch
                                            then sdlLog "SUCCESS! Texture data matches."
                                            else sdlLog "FAILURE! Texture data MISMATCH."

                                          let downloadedBufPtr = castPtr mappedDownPtr `plusPtr` imgDataSize
                                          downloadedBufData <- peekArray (length bufferData) downloadedBufPtr :: IO [Word32]
                                          let bufMatch = downloadedBufData == bufferData
                                          if bufMatch
                                            then sdlLog "SUCCESS! Buffer data matches."
                                            else sdlLog $ "FAILURE! Buffer data MISMATCH. Expected: " ++ show bufferData ++ " Got: " ++ show downloadedBufData

                                          if texMatch && bufMatch
                                            then
                                              let appRes = AppResources oTex tCopy tSmall oBuf bCopy originalBS bufferData imgW imgH
                                               in return $ Just appRes
                                            else return Nothing -- Comparison failed
                                    else sdlLog "Wait for fence failed." >> return Nothing
            _ -> do
              -- Case for one of the initial Just oTex ... downTrans being Nothing
              sdlLog "!!! Failed to create one or more initial GPU resources."
              -- More robust cleanup would release any non-Nothing values from the tuple
              maybe (pure ()) (sdlReleaseGPUTexture contextDevice) maybeOrigTex
              maybe (pure ()) (sdlReleaseGPUTexture contextDevice) maybeTexCopy
              maybe (pure ()) (sdlReleaseGPUTexture contextDevice) maybeTexSmall
              maybe (pure ()) (sdlReleaseGPUBuffer contextDevice) maybeOrigBuf
              maybe (pure ()) (sdlReleaseGPUBuffer contextDevice) maybeBufCopy
              maybe (pure ()) (sdlReleaseGPUTransferBuffer contextDevice) maybeUploadTransfer
              maybe (pure ()) (sdlReleaseGPUTransferBuffer contextDevice) maybeDownloadTransfer
              return Nothing

-- releaseResources
releaseResources :: Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources Context {..} (Just AppResources {..}) = do
  sdlLog "--> Releasing AppResources..."
  sdlReleaseGPUTexture contextDevice resOriginalTexture
  sdlReleaseGPUTexture contextDevice resTextureCopy
  sdlReleaseGPUTexture contextDevice resTextureSmall
  sdlReleaseGPUBuffer contextDevice resOriginalBuffer
  sdlReleaseGPUBuffer contextDevice resBufferCopy
  sdlLog "<-- AppResources Released."

-- eventLoopGPU
eventLoopGPU :: Context -> AppResources -> IO ()
eventLoopGPU context resources = do
  sdlPumpEvents
  shouldQuitRef <- newIORef False
  processEventsGPU shouldQuitRef
  shouldQuit <- readIORef shouldQuitRef
  unless shouldQuit $ renderFrameGPU context resources >> eventLoopGPU context resources

-- processEventsGPU & handleEventGPU (Simplified to just quit)
processEventsGPU :: IORef Bool -> IO ()
processEventsGPU sr = sdlPollEvent >>= maybe (pure ()) (handleEventGPU >=> \q -> when q (writeIORef sr True) >> processEventsGPU sr)

handleEventGPU :: SDLEvent -> IO Bool
handleEventGPU (SDLEventQuit _) = sdlLog "Quit." >> return True
handleEventGPU (SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ sc _ _ _ d _)) | d && sc == SDL_SCANCODE_Q = return True
handleEventGPU _ = return False

-- renderFrameGPU: Blits the processed textures to the screen
renderFrameGPU :: Context -> AppResources -> IO ()
renderFrameGPU Context {..} AppResources {..} = do
  maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
  case maybeCmdbuf of
    Nothing -> sdlLog "Error: Failed to acquire render command buffer for drawing."
    Just cmdbuf -> do
      -- Use the helper function for dimensions
      maybeSwapResult <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
      case maybeSwapResult of
        Nothing -> void (sdlSubmitGPUCommandBuffer cmdbuf `finally` pure ())
        Just (swapchainTexture, swapW, swapH) -> do
          -- swapW, swapH are Word32

          -- Clear swapchain
          let colorTargetInfo =
                defaultColorTargetInfo -- Use default
                  { texture = swapchainTexture, -- Update texture
                    loadOp = SDL_GPU_LOADOP_CLEAR,
                    storeOp = SDL_GPU_STOREOP_STORE,
                    clearColor = SDLFColor 0 0 0 1
                    -- targetCycle = False (already default)
                  }
          mClearPass <- sdlBeginGPURenderPass cmdbuf [colorTargetInfo] Nothing
          case mClearPass of
            Nothing -> sdlLog "Failed to begin clear pass"
            Just clearPass -> sdlEndGPURenderPass clearPass

          -- Blit OriginalTexture to top-left quadrant
          let blitInfoOrig =
                SDLGPUBlitInfo
                  (defaultBlitRegion resOriginalTexture textureWidth textureHeight)
                  ((defaultBlitRegion swapchainTexture (fromIntegral swapW) (fromIntegral swapH)) {gpuBlitRegW = swapW `div` 2, gpuBlitRegH = swapH `div` 2})
                  SDL_GPU_LOADOP_LOAD
                  (SDLFColor 0 0 0 0)
                  SDL_FLIP_NONE
                  SDL_GPU_FILTER_NEAREST
                  False
          sdlBlitGPUTexture cmdbuf blitInfoOrig

          -- Blit TextureCopy to top-right quadrant
          let blitInfoCopy =
                SDLGPUBlitInfo
                  (defaultBlitRegion resTextureCopy textureWidth textureHeight)
                  ((defaultBlitRegion swapchainTexture (fromIntegral swapW) (fromIntegral swapH)) {gpuBlitRegX = swapW `div` 2, gpuBlitRegW = swapW `div` 2, gpuBlitRegH = swapH `div` 2})
                  SDL_GPU_LOADOP_LOAD
                  (SDLFColor 0 0 0 0)
                  SDL_FLIP_NONE
                  SDL_GPU_FILTER_NEAREST
                  False
          sdlBlitGPUTexture cmdbuf blitInfoCopy

          -- Blit TextureSmall to bottom-center-ish quadrant
          let blitInfoSmall =
                SDLGPUBlitInfo
                  (defaultBlitRegion resTextureSmall (textureWidth `div` 2) (textureHeight `div` 2))
                  ( (defaultBlitRegion swapchainTexture (fromIntegral swapW) (fromIntegral swapH))
                      { gpuBlitRegX = swapW `div` 4,
                        gpuBlitRegY = swapH `div` 2,
                        gpuBlitRegW = swapW `div` 2,
                        gpuBlitRegH = swapH `div` 2
                      }
                  )
                  SDL_GPU_LOADOP_LOAD
                  (SDLFColor 0 0 0 0)
                  SDL_FLIP_NONE
                  SDL_GPU_FILTER_NEAREST
                  False
          sdlBlitGPUTexture cmdbuf blitInfoSmall

          submitted <- sdlSubmitGPUCommandBuffer cmdbuf
          unless submitted $ sdlGetError >>= sdlLog . ("Draw Submit failed: " ++)
