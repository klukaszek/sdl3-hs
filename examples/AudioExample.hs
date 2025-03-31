-- {-|
-- Example     : SDL.Audio
-- Description : Simple SDL Audio Playback with Sine Wave
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3

-- | -}
import Foreign
import Foreign.C
import qualified Data.ByteString as BS
import SDL hiding (sin, round)
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = do
  sdlInit [InitAudio]
  maybeDevid <- sdlOpenAudioDevice SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK Nothing
  case maybeDevid of
    Nothing -> putStrLn "Failed to open audio device"
    Just devid -> do
      putStrLn $ "Opened device ID: " ++ show devid

      -- Get device format
      maybeFormat <- sdlGetAudioDeviceFormat devid
      case maybeFormat of
        Just (spec, maybeFrames) -> do
          putStrLn $ "Format: " ++ show spec
          putStrLn $ "Sample Frames: " ++ show maybeFrames
        Nothing -> putStrLn "Failed to get device format"

      -- Check properties
      isPhysical <- sdlIsAudioDevicePhysical devid
      isPlayback <- sdlIsAudioDevicePlayback devid
      putStrLn $ "Is Physical: " ++ show isPhysical
      putStrLn $ "Is Playback: " ++ show isPlayback

      -- Pause and resume
      paused <- sdlPauseAudioDevice devid
      putStrLn $ "Paused: " ++ show paused
      isPaused <- sdlAudioDevicePaused devid
      putStrLn $ "Is Paused: " ++ show isPaused
      resumed <- sdlResumeAudioDevice devid
      putStrLn $ "Resumed: " ++ show resumed

      -- Set and get gain
      setGain <- sdlSetAudioDeviceGain devid 0.5
      putStrLn $ "Set Gain to 0.5: " ++ show setGain
      maybeGain <- sdlGetAudioDeviceGain devid
      putStrLn $ "Gain: " ++ show maybeGain

      -- Close device
      sdlCloseAudioDevice devid
      putStrLn "Device closed"
