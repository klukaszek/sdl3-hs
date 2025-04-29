{-|
Example     : SDL.GPU
Description : SDL Window, Event, and Basic GPU Rendering Example
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3
|-}

module Main where

import SDL
import SDL.GPU -- Import the GPU module specifically
import SDL.Video -- Need SDLWindowFlags constructor
import Control.Monad (unless, when, void)
import System.Exit (exitFailure, exitSuccess)
import Foreign.Ptr (nullPtr)
import Foreign.C.Types (CUInt) -- For shader format flags
import Data.IORef
import Data.Word (Word32, Word64)
import Data.Bits ((.|.)) -- For combining flags
import Text.Printf (printf)
import Data.Maybe (isJust, fromJust, fromMaybe)

main :: IO ()
main = do
  -- Check compiled version
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  when (sdlVersionAtLeast 3 2 0) $ sdlLog "Compiled with at least SDL 3.2.0"

  -- Get linked version
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  -- Initialize SDL (Video + Events needed)
  initSuccess <- sdlInit [InitVideo, InitEvents]
  unless initSuccess $ do
    sdlLog "Failed to initialize SDL!"
    exitFailure

  -- Check initialized subsystems
  initializedSystems <- sdlWasInit []
  sdlLog "Initialized subsystems:"
  mapM_ printSubsystem initializedSystems

  -- *** Create GPU Device ***
  -- Specify desired shader formats (adjust as needed for your target platforms)
  let desiredFormats = SDL_GPU_SHADERFORMAT_SPIRV .|. SDL_GPU_SHADERFORMAT_DXIL .|. SDL_GPU_SHADERFORMAT_MSL
  -- Set debugMode to True for more validation/info during development
  let debugMode = False -- Set to True for development debugging
  maybeDevice <- sdlCreateGPUDevice desiredFormats debugMode Nothing -- Let SDL pick driver
  case maybeDevice of
    Nothing -> do
        sdlLog "Failed to create GPU Device!"
        err <- sdlGetError
        sdlLog $ "SDL Error: " ++ err
        sdlQuit
        exitFailure
    Just device -> do
        deviceName <- sdlGetGPUDeviceDriver device >>= return . fromMaybe "Unknown Driver"
        sdlLog $ "GPU Device created successfully with driver: " ++ deviceName

        -- *** Create Window ***
        -- Note: For some backends (like Vulkan/Metal on macOS), you might need specific window flags
        -- e.g., [sdlWindowResizable, sdlWindowVulkan] or [sdlWindowResizable, sdlWindowMetal]
        -- For this basic example, Resizable should be okay for most.
        let windowFlags = [sdlWindowResizable]
        maybeWindow <- sdlCreateWindow "SDL3 Haskell GPU Example" 640 480 windowFlags
        case maybeWindow of
          Nothing -> do
            sdlLog "Failed to create window!"
            err <- sdlGetError
            sdlLog $ "SDL Error: " ++ err
            sdlDestroyGPUDevice device -- Clean up device
            sdlQuit
            exitFailure
          Just window -> do
            sdlLog "Window created successfully!"

            -- *** Claim Window for GPU Device ***
            claimed <- sdlClaimWindowForGPUDevice device window
            unless claimed $ do
                sdlLog "Failed to claim window for GPU device!"
                err <- sdlGetError
                sdlLog $ "SDL Error: " ++ err
                sdlDestroyWindow window   -- Clean up window first
                sdlDestroyGPUDevice device -- Then device
                sdlQuit
                exitFailure

            sdlLog "Window claimed for GPU device."
            -- Proceed with the application loop
            runAppGPU device window

            -- *** Cleanup (after runAppGPU finishes) ***
            sdlLog "Releasing window from GPU device..."
            sdlReleaseWindowFromGPUDevice device window
            sdlLog "Destroying window..."
            sdlDestroyWindow window
            sdlLog "Window destroyed."

        sdlLog "Destroying GPU device..."
        sdlDestroyGPUDevice device
        sdlLog "GPU device destroyed."

  sdlLog "Shutting down SDL..."
  sdlQuit
  sdlLog "Application terminated successfully"
  exitSuccess

-- | Encapsulate the application logic with device and window
runAppGPU :: SDLGPUDevice -> SDLWindow -> IO ()
runAppGPU device window = do
    -- Start event loop with initial time
    startTime <- sdlGetPerformanceCounter
    freq <- sdlGetPerformanceFrequency
    deltaTimeRef <- newIORef 0.0
    eventLoopGPU device window startTime freq deltaTimeRef
    -- Cleanup inside main now

-- | Main event loop that tracks FPS, processes events, and renders using GPU API
eventLoopGPU :: SDLGPUDevice -> SDLWindow -> Word64 -> Word64 -> IORef Double -> IO ()
eventLoopGPU device window lastTime freq deltaTimeRef = do
  currentTime <- sdlGetPerformanceCounter
  let deltaTime = fromIntegral (currentTime - lastTime) / fromIntegral freq -- Delta time in seconds

  -- Store the new deltaTime (in milliseconds for logging consistency)
  writeIORef deltaTimeRef (deltaTime * 1000.0)

  -- Event handling
  sdlPumpEvents
  shouldQuitRef <- newIORef False
  processEventsGPU shouldQuitRef deltaTimeRef -- Simplified event processing

  shouldQuit <- readIORef shouldQuitRef
  unless shouldQuit $ do
    -- *** GPU Rendering ***
    renderFrameGPU device window

    -- Continue loop
    eventLoopGPU device window currentTime freq deltaTimeRef

-- | Process all pending events (simplified for GPU example)
processEventsGPU :: IORef Bool -> IORef Double -> IO ()
processEventsGPU shouldQuitRef deltaTimeRef = do
    maybeEvent <- sdlPollEvent
    case maybeEvent of
        Nothing -> return () -- No more events
        Just event -> do
            quit <- handleEventGPU event deltaTimeRef
            when quit $ writeIORef shouldQuitRef True
            processEventsGPU shouldQuitRef deltaTimeRef -- Process next event

-- | Handle a single SDL event (simplified for GPU example)
handleEventGPU :: SDLEvent -> IORef Double -> IO Bool
handleEventGPU event deltaTimeRef = case event of
  SDLEventQuit _ -> do
    sdlLog "Quit event received."
    return True
  SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ scancode _ _ _ down _) | down -> do
    dtMs <- readIORef deltaTimeRef
    sdlLog $ printf "Key '%s' pressed. Delta Time: %.3f ms" (show scancode) dtMs
    return $ scancode == SDL_SCANCODE_Q -- Quit on Q
  _ -> return False

-- | Render a single frame using the GPU API
renderFrameGPU :: SDLGPUDevice -> SDLWindow -> IO ()
renderFrameGPU device window = do
    -- 1. Acquire Command Buffer
    maybeCmdbuf <- sdlAcquireGPUCommandBuffer device
    case maybeCmdbuf of
        Nothing -> sdlLog "Error: Failed to acquire command buffer"
        Just cmdbuf -> do
            -- 2. Acquire Swapchain Texture (wait if necessary)
            -- Note: Width/Height returned aren't used in this simple clear example
            maybeSwapchain <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf window
            case maybeSwapchain of
                Nothing -> sdlLog "Warning: Failed to acquire swapchain texture (maybe window not visible?)"
                Just (swapchainTexture, _w, _h) -> do

                    -- 3. Define Color Target Info for clearing
                    let clearColor = SDLFColor 0.3 0.4 0.5 1.0 -- Dark cyan background
                    let colorTargetInfo = SDLGPUColorTargetInfo
                            { gpuColorTargetInfoTexture           = swapchainTexture
                            , gpuColorTargetInfoMipLevel          = 0 -- Use base level
                            , gpuColorTargetInfoLayerOrDepthPlane = 0 -- Use base layer
                            , gpuColorTargetInfoClearColor        = clearColor
                            , gpuColorTargetInfoLoadOp            = SDL_GPU_LOADOP_CLEAR -- Clear the target
                            , gpuColorTargetInfoStoreOp           = SDL_GPU_STOREOP_STORE -- Store the result
                            , gpuColorTargetInfoResolveTexture    = Nothing -- No MSAA resolve needed
                            , gpuColorTargetInfoResolveMipLevel   = 0
                            , gpuColorTargetInfoResolveLayer      = 0
                            , gpuColorTargetInfoCycle             = False -- Don't cycle (relevant for multi-buffer resources)
                            , gpuColorTargetInfoCycleResolve      = False
                            }

                    -- 4. Begin Render Pass
                    maybeRenderPass <- sdlBeginGPURenderPass cmdbuf [colorTargetInfo] Nothing -- Only one color target, no depth/stencil
                    case maybeRenderPass of
                        Nothing -> sdlLog "Error: Failed to begin render pass"
                        Just renderPass -> do
                            -- *** Insert Drawing Commands Here (in the future) ***
                            -- For now, we just clear via the LoadOp.

                            -- 5. End Render Pass
                            sdlEndGPURenderPass renderPass

            -- 6. Submit Command Buffer (regardless of whether texture was acquired, buffer might have other commands)
            submitted <- sdlSubmitGPUCommandBuffer cmdbuf
            unless submitted $ do
                 err <- sdlGetError
                 sdlLog $ "Error: Failed to submit command buffer: " ++ err

-- | Helper function to print subsystem names (same as before)
printSubsystem :: InitFlag -> IO ()
printSubsystem flag = sdlLog $ "  - " ++ case flag of
  InitAudio    -> "Audio"
  InitVideo    -> "Video"
  InitJoystick -> "Joystick"
  InitHaptic   -> "Haptic"
  InitGamepad  -> "Gamepad"
  InitEvents   -> "Events"
  InitSensor   -> "Sensor"
  InitCamera   -> "Camera"
  _            -> "Unknown subsystem"
