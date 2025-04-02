import Foreign
import Foreign.C
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe (unsafePackCStringLen)
import SDL hiding (sin, round)
import Control.Concurrent (threadDelay)
import Foreign.Marshal.Array (withArray)
import Foreign.Storable (sizeOf)

-- | Main function to demonstrate SDL audio playback with a 1-second 440 Hz tone
main :: IO ()
main = do
  -- Initialize SDL with audio support
  sdlInit [InitAudio]

  -- Define the audio specification: 32-bit float, 1 channel (mono), 8000 Hz
  let spec = SDLAudioSpec SDL_AUDIO_F32 1 8000
  
  -- Open the default playback device stream with the specified spec and no callback
  maybeStream <- sdlOpenAudioDeviceStream SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK (Just spec) Nothing
  case maybeStream of
    Nothing -> putStrLn "Failed to open audio stream"
    Just stream -> do
      -- Resume the audio stream to start playback
      success <- sdlResumeAudioStreamDevice stream
      if not success
        then putStrLn "Failed to resume audio stream"
        else do
          -- Generate 8000 samples (1 second at 8000 Hz) of a 440 Hz sine wave with amplitude 0.5
          let samples = [0.5 * sin (2 * pi * 440 * fromIntegral i / 8000) | i <- [0..7999]] :: [Float]

          -- Convert samples to a ByteString and feed them into the audio stream
          withArray samples $ \ptr -> do
            let len = 8000 * sizeOf (undefined :: Float) -- Total byte length (8000 floats * 4 bytes each)
            bs <- unsafePackCStringLen (castPtr ptr, len)
            successPut <- sdlPutAudioStreamData stream bs
            if not successPut
              then putStrLn "Failed to put audio data into stream"
              else do
                -- Wait 5 seconds to ensure the 1-second tone plays fully
                threadDelay (5 * 1000000) -- 5 seconds in microseconds

          -- Clean up by destroying the audio stream (also closes the associated device)
          sdlDestroyAudioStream stream
          sdlLog "Success!"

  sdlQuit
