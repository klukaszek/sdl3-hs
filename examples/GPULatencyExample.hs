{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Example     : GPULatency
-- Description : Frame latency testing using cursor tracking and texture blitting.
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Based on the SDL_gpu_examples/Latency C example.
-- This example demonstrates:
-- - Using SDL_SetGPUAllowedFramesInFlight to control frame pacing
-- - Using SDL_BlitGPUTexture to draw sprites on the swapchain
-- - Tracking mouse cursor position for latency visualization
-- - Testing input-to-display latency by moving cursor and drawing under it
--
-- Color legend for latency:
-- - Gray:   -1 frames lag
-- - White:   0 frames lag
-- - Green:   1 frame lag
-- - Yellow:  2 frames lag
-- - Red:     3 frames lag
-- - Cyan:    4 frames lag
-- - Purple:  5 frames lag
-- - Blue:    6 frames lag
-- |
module Main where

import Control.Exception (bracket)
import Control.Monad (unless, void, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Word (Word32)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import GPUCommon
import SDL3
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

-- | Application state
data AppState = AppState
  { lagX :: !Int, -- X position for cursor warping
    captureCursor :: !Bool, -- Whether to capture and warp cursor
    allowedFramesInFlight :: !Int, -- GPU frame pipelining (1-3)
    isFullscreen :: !Bool -- Fullscreen mode toggle
  }
  deriving (Show)

-- | Application resources
data AppResources = AppResources
  { resLagTexture :: SDLGPUTexture
  }
  deriving (Show)

-- | Initial application state
initialState :: AppState
initialState =
  AppState
    { lagX = 1,
      captureCursor = False,
      allowedFramesInFlight = 2,
      isFullscreen = False
    }

-- | main
main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion
  maybeResult <- withContext "SDL3 Haskell GPU Latency Test" [] runAppGPU
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
        printInstructions
        stateRef <- newIORef initialState
        -- Set initial frames in flight
        void $ sdlSetGPUAllowedFramesInFlight contextDevice 2
        eventLoopGPU context resources stateRef

-- | Print latency test instructions
printInstructions :: IO ()
printInstructions = do
  sdlLog "=== Latency Test Instructions ==="
  sdlLog "Press Left/Right to toggle capturing the mouse cursor."
  sdlLog "Press Down to change the number of allowed frames in flight."
  sdlLog "Press Up to toggle fullscreen mode."
  sdlLog "When the mouse cursor is captured the color directly above the cursor's point shows the lag."
  sdlLog "Negative lag can occur when cursor is below tear line with tearing enabled."
  sdlLog "Color Legend:"
  sdlLog "  Gray:   -1 frames lag"
  sdlLog "  White:   0 frames lag"
  sdlLog "  Green:   1 frame lag"
  sdlLog "  Yellow:  2 frames lag"
  sdlLog "  Red:     3 frames lag"
  sdlLog "  Cyan:    4 frames lag"
  sdlLog "  Purple:  5 frames lag"
  sdlLog "  Blue:    6 frames lag"

-- | createResources - Creates the latency test texture
createResources :: Context -> IO (Maybe AppResources)
createResources Context {..} = do
  sdlLog "--- Beginning Resource Creation ---"

  -- Create the latency indicator texture (8x32 pixels)
  let texCI =
        SDLGPUTextureCreateInfo
          { texInfoType = SDL_GPU_TEXTURETYPE_2D,
            texInfoFormat = SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM,
            texInfoUsage = SDL_GPU_TEXTUREUSAGE_SAMPLER,
            texInfoWidth = 8,
            texInfoHeight = 32,
            texInfoLayerCountOrDepth = 1,
            texInfoNumLevels = 1,
            texInfoSampleCount = SDL_GPU_SAMPLECOUNT_1,
            texInfoProps = 0
          }
  maybeLagTexture <- sdlCreateGPUTexture contextDevice texCI

  case maybeLagTexture of
    Nothing -> do
      sdlLog "!!! Failed to create lag texture."
      return Nothing
    Just lagTexture -> do
      sdlLog "Created lag texture."

      -- Load the latency.bmp image
      maybeSurf <- loadImage ("Content" </> "Images" </> "latency.bmp")
      case maybeSurf of
        Nothing -> do
          sdlLog "!!! Failed to load latency.bmp."
          sdlReleaseGPUTexture contextDevice lagTexture
          return Nothing
        Just surfPtr -> do
          sdlLog "Loaded latency.bmp."

          -- Upload texture data
          let byteCount = 8 * 32 * 4 :: Word32

          uploadSuccess <- bracket
            (createTransferBuffer contextDevice byteCount SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "LagTextureTransfer")
            (cleanupTransferBuffer contextDevice)
            $ \case
              Nothing -> return False
              Just tb -> do
                -- Map and copy pixel data
                mapOk <- bracket
                  (sdlMapGPUTransferBuffer contextDevice tb False)
                  (\mptr -> when (isJust mptr) $ sdlUnmapGPUTransferBuffer contextDevice tb)
                  $ \case
                    Nothing -> return False
                    Just ptr -> do
                      surfData <- peek surfPtr
                      copyBytes (castPtr ptr) (surfacePixels surfData) (fromIntegral byteCount)
                      return True

                if mapOk
                  then do
                    -- Upload to GPU
                    bracket
                      (sdlAcquireGPUCommandBuffer contextDevice)
                      cleanupCommandBuffer
                      $ \case
                        Nothing -> return False
                        Just cmd -> do
                          mcp <- sdlBeginGPUCopyPass cmd
                          case mcp of
                            Nothing -> return False
                            Just cp -> do
                              sdlUploadToGPUTexture
                                cp
                                (SDLGPUTextureTransferInfo tb 0 0 0)
                                (SDLGPUTextureRegion lagTexture 0 0 0 0 0 8 32 1)
                                False
                              sdlEndGPUCopyPass cp
                              submitted <- sdlSubmitGPUCommandBuffer cmd
                              if submitted
                                then do
                                  void $ sdlWaitForGPUIdle contextDevice
                                  return True
                                else return False
                  else return False

          -- Clean up surface
          sdlDestroySurface surfPtr

          if uploadSuccess
            then do
              sdlLog "--- Resource Creation and Upload Successful ---"
              return $ Just AppResources {resLagTexture = lagTexture}
            else do
              sdlLog "!!! Failed to upload lag texture."
              sdlReleaseGPUTexture contextDevice lagTexture
              return Nothing

-- | releaseResources
releaseResources :: Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources Context {..} (Just AppResources {..}) = do
  sdlLog "--> Releasing AppResources..."
  sdlReleaseGPUTexture contextDevice resLagTexture
  sdlLog "<-- AppResources Released."

-- | eventLoopGPU
eventLoopGPU :: Context -> AppResources -> IORef AppState -> IO ()
eventLoopGPU context resources stateRef = do
  sdlPumpEvents
  shouldQuit <- processEventsGPU context stateRef
  unless shouldQuit $ do
    renderFrameGPU context resources stateRef
    eventLoopGPU context resources stateRef

-- | processEventsGPU - Handle input events
processEventsGPU :: Context -> IORef AppState -> IO Bool
processEventsGPU Context {..} stateRef = go
  where
    go =
      sdlPollEvent >>= \case
        Nothing -> return False
        Just event -> do
          quit <- handleEvent event
          if quit then return True else go

    handleEvent (SDLEventQuit _) = sdlLog "Quit." >> return True
    handleEvent (SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ sc _ _ _ down _))
      | down = case sc of
          _ | sc == SDL_SCANCODE_Q -> return True
          _ | sc == SDL_SCANCODE_LEFT || sc == SDL_SCANCODE_RIGHT -> do
            modifyIORef' stateRef $ \s -> s {captureCursor = not (captureCursor s)}
            state <- readIORef stateRef
            sdlLog $ "Cursor capture: " ++ show (captureCursor state)
            return False
          _ | sc == SDL_SCANCODE_DOWN -> do
            state <- readIORef stateRef
            let newFrames = ((allowedFramesInFlight state) `mod` 3) + 1
            writeIORef stateRef $ state {allowedFramesInFlight = newFrames}
            void $ sdlSetGPUAllowedFramesInFlight contextDevice (fromIntegral newFrames)
            sdlLog $ "Allowed frames in flight: " ++ show newFrames
            return False
          _ | sc == SDL_SCANCODE_UP -> do
            state <- readIORef stateRef
            let newFullscreen = not (isFullscreen state)
            writeIORef stateRef $ state {isFullscreen = newFullscreen}
            void $ sdlSetWindowFullscreen contextWindow newFullscreen
            sdlLog $ "Fullscreen: " ++ show newFullscreen
            return False
          _ -> return False
      | otherwise = return False
    handleEvent _ = return False

-- | renderFrameGPU
renderFrameGPU :: Context -> AppResources -> IORef AppState -> IO ()
renderFrameGPU Context {..} AppResources {..} stateRef = do
  maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
  case maybeCmdbuf of
    Nothing -> sdlLog "Error: Failed to acquire render command buffer."
    Just cmdbuf -> do
      maybeSwapResult <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
      case maybeSwapResult of
        Nothing -> void $ sdlSubmitGPUCommandBuffer cmdbuf
        Just (swapchainTexture, w, h) -> do
          -- Get current mouse position (using global state for low latency)
          (_, (globalX, globalY)) <- sdlGetGlobalMouseState
          (winX, winY) <- sdlGetWindowPosition contextWindow

          let cursorXBase = globalX - fromIntegral winX
          let cursorY = globalY - fromIntegral winY

          state <- readIORef stateRef

          -- Handle cursor capture and warping
          cursorX <-
            if captureCursor state
              then do
                let newLagX = lagX state
                sdlWarpMouseInWindow (Just contextWindow) (fromIntegral newLagX) cursorY
                -- Update lagX for next frame
                let nextLagX = if newLagX >= fromIntegral w - 8 then 1 else newLagX + 1
                writeIORef stateRef $ state {lagX = nextLagX}
                return (fromIntegral newLagX)
              else return cursorXBase

          -- Draw sprite under cursor if within valid bounds
          if cursorX >= 1 && cursorX <= fromIntegral w - 8 && cursorY >= 5 && cursorY <= fromIntegral h - 27
            then do
              -- Use SDL_BlitGPUTexture to draw the latency indicator
              let blitInfo =
                    SDLGPUBlitInfo
                      { gpuBlitInfoSource = SDLGPUBlitRegion resLagTexture 0 0 0 0 8 32,
                        gpuBlitInfoDestination = SDLGPUBlitRegion swapchainTexture 0 0 (Prelude.floor cursorX - 1) (Prelude.floor cursorY - 5) 8 32,
                        gpuBlitInfoLoadOp = SDL_GPU_LOADOP_CLEAR,
                        gpuBlitInfoClearColor = SDLFColor 0 0 0 1,
                        gpuBlitInfoFlipMode = SDL_FLIP_NONE,
                        gpuBlitInfoFilter = SDL_GPU_FILTER_NEAREST,
                        gpuBlitInfoCycle = False
                      }
              sdlBlitGPUTexture cmdbuf blitInfo
            else do
              -- Just clear the screen if cursor is out of bounds
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
                Just rp -> sdlEndGPURenderPass rp

          void $ sdlSubmitGPUCommandBuffer cmdbuf
