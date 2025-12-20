-- \|
-- Example     : WAV Example
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Streams audio on a loop until you quit with 'q'.
-- Events print delta time to show that the audio does not block.
--
-- Tested on Linux with Pulse AND Alsa.
-- Tested on Windows.
--
-- For more details, refer to the official SDL3 documentation:
-- https://wiki.libsdl.org/SDL3/CategoryAudio

import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign
import SDL3 hiding (round, sin)
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

-- | Main function to stream WAV audio with an event loop
main :: IO ()
main = do
  -- Initialize SDL with audio and events
  initSuccess <- sdlInit [SDL_INIT_AUDIO, SDL_INIT_VIDEO, SDL_INIT_EVENTS]
  unless initSuccess $ do
    sdlLog "Failed to initialize SDL!"
    exitFailure

  -- Log initialized subsystems
  initializedSystems <- sdlWasInit []
  sdlLog "Initialized subsystems:"
  mapM_ printSubsystem initializedSystems

  -- Load the WAV file
  maybeWav <- sdlLoadWAV "examples/Content/Audio/sound.wav"
  case maybeWav of
    Nothing -> do
      sdlLog "Failed to load WAV file!"
      sdlQuit
      exitFailure
    Just (spec, wavData) -> do
      sdlLog $ "Loaded WAV spec: " ++ show spec
      let wavDataLen = BS.length wavData
      sdlLog $ "WAV data length: " ++ show wavDataLen

      -- Open the audio stream
      maybeStream <- sdlOpenAudioDeviceStream SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK (Just spec) Nothing
      case maybeStream of
        Nothing -> do
          err <- sdlGetError
          sdlLog $ "Failed to open audio stream. SDL Error: " ++ err
          sdlQuit
          exitFailure
        Just stream -> do
          success <- sdlResumeAudioStreamDevice stream
          if not success
            then do
              sdlLog "Failed to resume audio stream!"
              sdlDestroyAudioStream stream
              sdlQuit
              exitFailure
            else do
              -- Initial feed of WAV data
              successPut <- sdlPutAudioStreamData stream wavData
              if not successPut
                then do
                  sdlLog "Failed to put initial audio data into stream!"
                  sdlDestroyAudioStream stream
                  sdlQuit
                  exitFailure
                else do
                  window <- sdlCreateWindow "SDL3 Haskell Audio Queue Loop" 640 480 [SDL_WINDOW_RESIZABLE]
                  case window of
                    Nothing -> do
                      sdlLog "Failed to create window!"
                      sdlQuit
                      exitFailure
                    Just win -> do
                      sdlLog "Window created successfully!"

                      -- Start event loop with initial time
                      startTime <- sdlGetPerformanceCounter
                      freq <- sdlGetPerformanceFrequency
                      deltaTimeRef <- newIORef 0.0
                      eventLoop stream wavData wavDataLen startTime freq deltaTimeRef

                      -- Cleanup
                      sdlDestroyWindow win
                      sdlLog "Window destroyed."

          -- Cleanup
          sdlDestroyAudioStream stream
          sdlLog "Audio stream destroyed."

  sdlLog "Shutting down SDL..."
  sdlQuit
  sdlLog "Application terminated successfully"
  exitSuccess

-- | Event loop to handle audio streaming and key interrupts
eventLoop :: SDLAudioStream -> BS.ByteString -> Int -> Word64 -> Word64 -> IORef Double -> IO ()
eventLoop stream wavData wavDataLen lastTime freq deltaTimeRef = do
  -- Update timing
  currentTime <- sdlGetPerformanceCounter
  let deltaTime = fromIntegral (currentTime - lastTime) / fromIntegral freq * 1000.0
  writeIORef deltaTimeRef deltaTime

  -- Check queued audio and feed more if needed
  queued <- sdlGetAudioStreamQueued stream
  shouldQuitAudio <- case queued of
    Nothing -> do
      sdlLog "Failed to get queued amount"
      return False
    Just q -> do
      -- sdlLog $ printf "Bytes queued: %d" q
      when (q < wavDataLen) $ do
        successPut <- sdlPutAudioStreamData stream wavData
        if not successPut
          then sdlLog "Failed to put additional audio data into stream"
          else sdlLog "Added more data to stream"
      return False

  -- Process events
  sdlPumpEvents
  maybeEvent <- sdlPollEvent
  shouldQuitEvent <- case maybeEvent of
    Nothing -> return False
    Just event -> handleEvent event deltaTimeRef

  unless (shouldQuitAudio || shouldQuitEvent) $
    eventLoop stream wavData wavDataLen currentTime freq deltaTimeRef

-- | Handle SDL events
handleEvent :: SDLEvent -> IORef Double -> IO Bool
handleEvent event deltaTimeRef = case event of
  SDLEventQuit _ -> do
    sdlLog "Quit event received."
    return True
  SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ scancode _ _ _ down _) | down -> do
    deltaTime <- readIORef deltaTimeRef
    sdlLog $ printf "Key event received for scancode: %u, Delta Time: %.3f ms" scancode deltaTime
    return $ scancode == SDL_SCANCODE_Q
  _ -> return False

-- Helper function to print subsystem names
printSubsystem :: SDLInitFlags -> IO ()
printSubsystem flag =
  sdlLog $
    "  - " ++ case flag of
      SDL_INIT_AUDIO -> "Audio"
      SDL_INIT_VIDEO -> "Video"
      SDL_INIT_JOYSTICK -> "Joystick"
      SDL_INIT_HAPTIC -> "Haptic"
      SDL_INIT_GAMEPAD -> "Gamepad"
      SDL_INIT_EVENTS -> "Events"
      SDL_INIT_SENSOR -> "Sensor"
      SDL_INIT_CAMERA -> "Camera"
      _ -> "Unknown subsystem"
