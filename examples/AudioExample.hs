{-|
Example     : SDL.Audio
Description : SDL Audio Playback Example with Sine Wave
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3
|-}

module Main where

import SDL hiding (sin)
import SDL.Audio
import Control.Monad (unless, when)
import System.Exit (exitFailure, exitSuccess)
import Foreign (with, toBool, castPtr)
import Foreign.C.String (peekCString)
import Foreign.Ptr (nullPtr)
import Foreign.Marshal.Array (withArray)
import Data.IORef
import Data.Word (Word32, Word64)
import Text.Printf (printf)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.List (unfoldr)

main :: IO ()
main = do
  sdlInit [InitAudio]
  bracket setupAudio cleanupAudio $ \audioData -> do
    startTime <- sdlGetPerformanceCounter
    freq <- sdlGetPerformanceFrequency
    sampleCounterRef <- newIORef 0
    playSineWave (audioStream audioData) sampleCounterRef True
    threadDelay 500000  -- 500ms delay to let device start
    eventLoop startTime freq sampleCounterRef audioData
  sdlQuit
  exitSuccess

data AudioData = AudioData
  { audioDevice :: SDLAudioDeviceID
  , audioStream :: SDLAudioStream
  , audioSpec   :: SDLAudioSpec
  }

createAudioSpec :: IO SDLAudioSpec
createAudioSpec = return SDLAudioSpec
  { audioFormat = SDL_AUDIO_F32    -- Native endian float32
  , audioChannels = 1              -- Mono
  , audioFreq = 8000              -- 8000 Hz
  }

setupAudio :: IO AudioData
setupAudio = do
  desiredSpec <- createAudioSpec
  device <- with desiredSpec $ \specPtr -> do
    dev <- sdlOpenAudioDevice SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK specPtr
    err <- sdlGetError
    unless (err == "") $ sdlLog $ "Open device error: " ++ err
    return dev
  
  streamMaybe <- with desiredSpec $ \srcSpecPtr -> do
    stream <- sdlOpenAudioDeviceStream device desiredSpec Nothing nullPtr
    err <- sdlGetError
    unless (err == "") $ sdlLog $ "Open stream error: " ++ err
    return stream
    
  case streamMaybe of
    Nothing -> do
      sdlLog "Failed to create audio stream!"
      exitFailure
    Just stream -> do
      resumeResult <- sdlResumeAudioDevice device
      err <- sdlGetError
      sdlLog $ "Resume audio device result: " ++ show (toBool resumeResult)
      unless (toBool resumeResult || err == "") $ sdlLog $ "Resume error: " ++ err
      formatNamePtr <- sdlGetAudioFormatName (audioFormat desiredSpec)
      formatName <- peekCString formatNamePtr
      sdlLog $ "Requested audio format: " ++ formatName
      actualSpec <- sdlGetAudioStreamFormat stream
      case actualSpec of
        Just (_inSpec, outSpec) -> do
          actualFormatNamePtr <- sdlGetAudioFormatName (audioFormat outSpec)
          actualFormatName <- peekCString actualFormatNamePtr
          sdlLog $ "Actual audio format: " ++ actualFormatName ++ 
                  ", channels: " ++ show (audioChannels outSpec) ++ 
                  ", freq: " ++ show (audioFreq outSpec)
        Nothing -> sdlLog "Could not retrieve actual audio format"
      return $ AudioData device stream desiredSpec

cleanupAudio :: AudioData -> IO ()
cleanupAudio audioData = do
  let (SDLAudioStream ptr) = audioStream audioData
  sdlUnbindAudioStream (audioStream audioData)
  sdlDestroyAudioStream ptr
  sdlCloseAudioDevice (audioDevice audioData)
  sdlLog "Audio cleaned up"

generateSineWave :: Int -> Int -> [Float]
generateSineWave startSample count = 
  take count $ unfoldr (\i -> Just (0.5 * sin (2 * pi * 440 * fromIntegral i / 8000), i + 1)) startSample

playSineWave :: SDLAudioStream -> IORef Int -> Bool -> IO ()
playSineWave stream sampleCounterRef isInitial = do
  let (SDLAudioStream ptr) = stream
  queued <- sdlGetAudioStreamQueued ptr
  sdlLog $ "Queued bytes: " ++ show queued
  let sampleCount = if isInitial then 8000 else 512
  when (isInitial || True) $ do  -- Force continuous queuing for testing
    currentSample <- readIORef sampleCounterRef
    let samples = generateSineWave currentSample sampleCount
    withArray samples $ \bufPtr -> do
      let bufLen = length samples * 4
      success <- sdlPutAudioStreamData ptr (castPtr bufPtr) (fromIntegral bufLen)
      err <- sdlGetError
      if toBool success
        then do
          let newSample = (currentSample + sampleCount) `mod` 8000
          writeIORef sampleCounterRef newSample
          sdlLog $ "Queued " ++ show (length samples) ++ " samples"
          flushSuccess <- sdlFlushAudioStream ptr
          unless (toBool flushSuccess) $ do
            flushErr <- sdlGetError
            sdlLog $ "Failed to flush audio stream: " ++ flushErr
        else sdlLog $ "Failed to queue audio data: " ++ err

eventLoop :: Word64 -> Word64 -> IORef Int -> AudioData -> IO ()
eventLoop lastTime freq sampleCounterRef audioData = do
  currentTime <- sdlGetPerformanceCounter
  let deltaTime = fromIntegral (currentTime - lastTime) / fromIntegral freq * 1000.0

  sdlPumpEvents
  maybeEvent <- sdlPollEvent
  shouldQuit <- case maybeEvent of
    Nothing -> return False
    Just event -> handleEvent event deltaTime

  playSineWave (audioStream audioData) sampleCounterRef False
  
  unless shouldQuit $ do
    threadDelay 1000  -- 1ms delay per loop
    eventLoop currentTime freq sampleCounterRef audioData

handleEvent :: SDLEvent -> Double -> IO Bool
handleEvent event deltaTime = case event of
  SDLEventQuit _ -> return True
  SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ scancode _ _ _ down _) | down -> do
    sdlLog $ printf "Key pressed. Delta Time: %.3f ms" deltaTime
    return $ scancode == SDL_SCANCODE_Q
  _ -> return False
