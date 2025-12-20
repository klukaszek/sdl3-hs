{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Example     : GPUWindowResize
-- Description : Demonstrates window resizing with GPU rendering.
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Based on the SDL_gpu_examples/WindowResize C example.
-- This example demonstrates:
-- - Dynamically resizing the window using keyboard input
-- - Maintaining correct GPU swapchain behavior during resize
-- - Using predefined common resolutions
-- - Using SDL_SyncWindow to synchronize window state
module Main where

import Control.Exception (bracket)
import Control.Monad (unless, void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isNothing)
import Foreign.Storable (sizeOf)
import GPUCommon
import SDL3
import System.Exit (exitFailure, exitSuccess)

-- | Predefined resolutions
data Resolution = Resolution !Int !Int deriving (Show)

resolutions :: [Resolution]
resolutions =
  [ Resolution 640 480,
    Resolution 1280 720,
    Resolution 1024 1024,
    Resolution 1600 900,
    Resolution 1920 1080,
    Resolution 3200 1800,
    Resolution 3840 2160
  ]

resolutionCount :: Int
resolutionCount = length resolutions

-- | Application resources
data AppResources = AppResources
  { resPipeline :: SDLGPUGraphicsPipeline
  }
  deriving (Show)

-- | main
main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion
  maybeResult <- withContext "SDL3 Haskell GPU Window Resize" [SDL_WINDOW_RESIZABLE] runAppGPU
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
        sdlLog "Press Left/Right to resize the window!"
        resIndexRef <- newIORef 0
        eventLoopGPU context resources resIndexRef

-- | createResources - Creates the graphics pipeline
createResources :: Context -> IO (Maybe AppResources)
createResources Context {..} = do
  sdlLog "--- Beginning Resource Creation ---"

  -- Load shaders (RawTriangle.vert, SolidColor.frag)
  let vertInfo = defaultShaderCreateInfo
  let fragInfo = defaultShaderCreateInfo
  maybeVertShader <- loadShader contextDevice "RawTriangle.vert" SDL_GPU_SHADERSTAGE_VERTEX vertInfo
  maybeFragShader <- loadShader contextDevice "SolidColor.frag" SDL_GPU_SHADERSTAGE_FRAGMENT fragInfo

  case (maybeVertShader, maybeFragShader) of
    (Just vertS, Just fragS) -> do
      -- Get swapchain format and create pipeline
      swapchainFormat <- sdlGetGPUSwapchainTextureFormat contextDevice contextWindow

      let pipelineCI = defaultGraphicsPipelineCreateInfo vertS fragS swapchainFormat

      maybePipeline <- sdlCreateGPUGraphicsPipeline contextDevice pipelineCI

      -- Release shaders after pipeline creation
      sdlReleaseGPUShader contextDevice vertS
      sdlReleaseGPUShader contextDevice fragS

      case maybePipeline of
        Just pipeline -> do
          sdlLog "--- Resource Creation Successful ---"
          return $ Just AppResources {resPipeline = pipeline}
        Nothing -> do
          sdlLog "!!! Failed to create pipeline."
          return Nothing
    _ -> do
      sdlLog "!!! Failed to load shaders."
      return Nothing

-- | releaseResources
releaseResources :: Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources Context {..} (Just AppResources {..}) = do
  sdlLog "--> Releasing AppResources..."
  sdlReleaseGPUGraphicsPipeline contextDevice resPipeline
  sdlLog "<-- AppResources Released."

-- | eventLoopGPU
eventLoopGPU :: Context -> AppResources -> IORef Int -> IO ()
eventLoopGPU context resources resIndexRef = do
  sdlPumpEvents
  shouldQuit <- processEventsGPU context resIndexRef
  unless shouldQuit $ do
    renderFrameGPU context resources
    eventLoopGPU context resources resIndexRef

-- | processEventsGPU - Handle input events
processEventsGPU :: Context -> IORef Int -> IO Bool
processEventsGPU Context {..} resIndexRef = go
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
          _ | sc == SDL_SCANCODE_RIGHT -> do
            resIndex <- readIORef resIndexRef
            let newIndex = (resIndex + 1) `mod` resolutionCount
            writeIORef resIndexRef newIndex
            changeResolution newIndex
            return False
          _ | sc == SDL_SCANCODE_LEFT -> do
            resIndex <- readIORef resIndexRef
            let newIndex = if resIndex <= 0 then resolutionCount - 1 else resIndex - 1
            writeIORef resIndexRef newIndex
            changeResolution newIndex
            return False
          _ -> return False
      | otherwise = return False
    handleEvent _ = return False

    changeResolution idx = do
      let Resolution w h = resolutions !! idx
      sdlLog $ "Setting resolution to: " ++ show w ++ "x" ++ show h
      void $ sdlSetWindowSize contextWindow w h
      void $ sdlSetWindowPosition contextWindow sdlWindowPosCentered sdlWindowPosCentered
      sdlSyncWindow contextWindow

-- | sdlWindowPosCentered - Constant for centered window position
sdlWindowPosCentered :: Int
sdlWindowPosCentered = 0x2FFF0000 -- SDL_WINDOWPOS_CENTERED

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
              sdlDrawGPUPrimitives rp 3 1 0 0
              sdlEndGPURenderPass rp

          void $ sdlSubmitGPUCommandBuffer cmdbuf
