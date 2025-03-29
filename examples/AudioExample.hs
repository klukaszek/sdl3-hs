{-|
Example     : SDL.Audio
Description : SDL Audio Playback Example
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3
|-}

module Main where

import SDL
import SDL.Audio
import Control.Monad (unless, when, void)
import System.Exit (exitFailure, exitSuccess)
import Foreign (with, toBool)
import Foreign.C.String (peekCString)
import Foreign.Ptr (nullPtr, castPtr, Ptr)
import Foreign.Marshal.Alloc (free, alloca)
import Foreign.Marshal.Array (withArray, peekArray)
import Data.IORef
import Data.Word (Word8, Word16, Word32, Word64)
import Text.Printf (printf)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, finally, catch, SomeException)

main :: IO ()
main = do
  -- Check compiled version
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  when (sdlVersionAtLeast 3 0 0) $ sdlLog "Compiled with at least SDL 3.0.0"

  -- Get linked version
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  -- Initialize SDL
  initSuccess <- sdlInit [InitVideo, InitEvents, InitAudio]
  unless initSuccess $ do
    sdlLog "Failed to initialize SDL!"
    exitFailure

  -- Check initialized subsystems
  initializedSystems <- sdlWasInit []
  sdlLog "Initialized subsystems:"
  mapM_ printSubsystem initializedSystems

  -- Create a window
  window <- sdlCreateWindow "SDL3 Haskell Audio Example" 800 600 [sdlWindowResizable]
  case window of
    Nothing -> do
      sdlLog "Failed to create window!"
      sdlQuit
      exitFailure
    Just win -> do
      sdlLog "Window created successfully!"
      
      -- Load audio file and setup audio
      -- Use catch to handle any exceptions
      catch
        (bracket setupAudio cleanupAudio $ \audioData -> do
          -- Start event loop with initial time
          startTime <- sdlGetPerformanceCounter
          freq <- sdlGetPerformanceFrequency
          deltaTimeRef <- newIORef 0.0
          eventLoop win startTime freq deltaTimeRef audioData)
        (\e -> do
          sdlLog $ "Error in audio setup or playback: " ++ show (e :: SomeException)
          return ())
        
      -- Cleanup
      sdlDestroyWindow win
      sdlLog "Window destroyed."

  sdlLog "Shutting down SDL..."
  sdlQuit
  sdlLog "Application terminated successfully"
  exitSuccess

-- | Audio data type to hold all our audio resources
data AudioData = AudioData
  { audioDevice :: SDLAudioDeviceID
  , audioStream :: Maybe SDLAudioStream
  , audioBuffer :: [Word8]  -- We'll store buffer as a list
  , audioSpec   :: SDLAudioSpec
  }

-- | Create a valid audio spec with reasonable defaults
createDefaultAudioSpec :: IO SDLAudioSpec
createDefaultAudioSpec = do
  return SDLAudioSpec
    { audioFormat = SDL_AUDIO_S16LE  -- 16-bit signed little-endian
    , audioChannels = 2              -- Stereo
    , audioFreq = 44100              -- 44.1 kHz
    }

-- | Setup audio device and load WAV file
setupAudio :: IO AudioData
setupAudio = do
  sdlLog "Setting up audio..."
  
  -- Load WAV file
  loadResult <- sdlLoadWAV "assets/sound.wav"
  case loadResult of
    Nothing -> do
      sdlLog "Failed to load WAV file! Using default settings..."
      -- Create default audio spec
      spec <- createDefaultAudioSpec
      
      -- Just use a short empty buffer for testing
      let buffer = replicate 1000 0
      
      setupAudioWithSpec spec buffer
      
    Just (spec, buffer) -> do
      -- Validate the audio spec
      let channels = audioChannels spec
          format = audioFormat spec
          freq = audioFreq spec
      
      -- Check if the values are reasonable
      if channels <= 0 || channels > 8 || freq <= 0 || freq > 192000
        then do
          sdlLog $ "Invalid audio spec detected: channels=" ++ show channels ++
                  ", format=" ++ show format ++ ", freq=" ++ show freq
          
          -- Use default spec instead
          defaultSpec <- createDefaultAudioSpec
          sdlLog $ "Using default audio spec: channels=" ++ show (audioChannels defaultSpec) ++
                  ", format=" ++ show (audioFormat defaultSpec) ++ 
                  ", freq=" ++ show (audioFreq defaultSpec)
          
          setupAudioWithSpec defaultSpec buffer
          
        else do
          sdlLog $ "WAV loaded: format=" ++ show format ++ 
                  ", channels=" ++ show channels ++
                  ", freq=" ++ show freq
          
          setupAudioWithSpec spec buffer

-- | Setup audio with a valid spec and buffer
setupAudioWithSpec :: SDLAudioSpec -> [Word8] -> IO AudioData
setupAudioWithSpec spec buffer = do
  -- Open default audio device with the spec
  device <- with spec $ \specPtr ->
            sdlOpenAudioDevice SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK specPtr
  sdlLog $ "Opened audio device ID: " ++ show device
  
  -- Get the format name for display
  formatNamePtr <- sdlGetAudioFormatName (audioFormat spec)
  formatName <- peekCString formatNamePtr
  sdlLog $ "Audio format: " ++ formatName
  
  -- Create audio stream with the provided spec
  streamMaybe <- with spec $ \srcSpecPtr ->
                 sdlOpenAudioDeviceStream device spec Nothing nullPtr
  
  case streamMaybe of
    Nothing -> do
      sdlLog "Failed to create audio stream!"
      exitFailure
    Just stream -> do
      -- Resume the device (start playback)
      resumeResult <- sdlResumeAudioDevice device
      when (not $ toBool resumeResult) $ 
        sdlLog "Warning: Failed to resume audio device"
      
      return $ AudioData 
        { audioDevice = device
        , audioStream = Just stream
        , audioBuffer = buffer
        , audioSpec   = spec
        }

-- | Clean up audio resources
cleanupAudio :: AudioData -> IO ()
cleanupAudio audioData = do
  sdlLog "Cleaning up audio resources..."
  -- Destroy the audio stream if it exists
  case audioStream audioData of
    Just stream -> do
      -- Unbind the stream first, then destroy it
      sdlUnbindAudioStream stream
      
      -- Extract the pointer from the SDLAudioStream wrapper
      let (SDLAudioStream ptr) = stream
      sdlDestroyAudioStream ptr
      
    Nothing -> return ()
  
  -- Close the audio device
  sdlCloseAudioDevice (audioDevice audioData)

-- | Play the sound effect
playSound :: AudioData -> IO ()
playSound audioData = do
  case audioStream audioData of
    Nothing -> sdlLog "No audio stream available!"
    Just stream -> do
      -- Extract the pointer from the SDLAudioStream wrapper
      let (SDLAudioStream ptr) = stream
      
      -- Use the pointer directly with the buffer
      withArray (audioBuffer audioData) $ \bufPtr -> do
        let bufLen = length (audioBuffer audioData)
        
        -- Put audio data into the stream using the raw pointer
        success <- sdlPutAudioStreamData ptr (castPtr bufPtr) (fromIntegral bufLen)
        
        when (toBool success) $
          sdlLog "Playing sound..."
        
        unless (toBool success) $
          sdlLog "Failed to queue audio data!"

-- | Main event loop that tracks FPS and processes events
eventLoop :: SDLWindow -> Word64 -> Word64 -> IORef Double -> AudioData -> IO ()
eventLoop window lastTime freq deltaTimeRef audioData = do
  currentTime <- sdlGetPerformanceCounter
  let deltaTime = fromIntegral (currentTime - lastTime) / fromIntegral freq * 1000.0

  -- Store the new deltaTime
  writeIORef deltaTimeRef deltaTime
  
  -- Event handling
  sdlPumpEvents
  maybeEvent <- sdlPollEvent
  shouldQuit <- case maybeEvent of
    Nothing -> return False
    Just event -> handleEvent event deltaTimeRef audioData

  unless shouldQuit $ do
    -- Small delay to prevent CPU hogging
    threadDelay 1000
    eventLoop window currentTime freq deltaTimeRef audioData

-- | Handle SDL events
handleEvent :: SDLEvent -> IORef Double -> AudioData -> IO Bool
handleEvent event deltaTimeRef audioData = case event of
  SDLEventQuit _ -> do
    sdlLog "Quit event received."
    return True
  
  SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ scancode _ _ _ down _) | down -> do
    deltaTime <- readIORef deltaTimeRef  -- Read delta time
    sdlLog $ printf "Key event received. Delta Time: %.3f ms" deltaTime
    
    -- Play sound on space bar press
    when (scancode == SDL_SCANCODE_SPACE) $ do
      sdlLog "Space pressed, playing sound"
      playSound audioData
    
    -- Quit on Q press
    return $ scancode == SDL_SCANCODE_Q
    
  _ -> return False

-- | Helper function to print subsystem names
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
