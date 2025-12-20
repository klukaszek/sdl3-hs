{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : SDL.Audio
Description : SDL audio functionality
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Audio functionality for the SDL library. This module provides bindings for audio
device management, audio streams, and audio format conversion.

For more details, refer to the official SDL3 documentation:
https://wiki.libsdl.org/SDL3/CategoryAudio
-}

module SDL3.Audio
  ( -- * Types
    SDLAudioFormat()
  , SDLAudioSpec(..)
  , SDLAudioDeviceID
  , SDLAudioStream(..)
  , SDLAudioStreamCallback
  , SDLAudioPostmixCallback
    -- * Audio Format Constants
  , pattern SDL_AUDIO_MASK_BITSIZE
  , pattern SDL_AUDIO_MASK_FLOAT
  , pattern SDL_AUDIO_MASK_BIG_ENDIAN
  , pattern SDL_AUDIO_MASK_SIGNED
  , pattern SDL_AUDIO_UNKNOWN
  , pattern SDL_AUDIO_U8
  , pattern SDL_AUDIO_S8
  , pattern SDL_AUDIO_S16LE
  , pattern SDL_AUDIO_S16BE
  , pattern SDL_AUDIO_S32LE
  , pattern SDL_AUDIO_S32BE
  , pattern SDL_AUDIO_F32LE
  , pattern SDL_AUDIO_F32BE
  , pattern SDL_AUDIO_S16
  , pattern SDL_AUDIO_S32
  , pattern SDL_AUDIO_F32
    -- * Default Device Constants
  , pattern SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK
  , pattern SDL_AUDIO_DEVICE_DEFAULT_RECORDING
    -- * Audio Driver Functions
  , sdlGetNumAudioDrivers
  , sdlGetAudioDriver
  , sdlGetCurrentAudioDriver
    -- * Audio Device Functions
  , sdlGetAudioPlaybackDevices
  , sdlGetAudioRecordingDevices
  , sdlGetAudioDeviceName
  , sdlGetAudioDeviceFormat
  , sdlGetAudioDeviceChannelMap
  , sdlOpenAudioDevice
  , sdlIsAudioDevicePhysical
  , sdlIsAudioDevicePlayback
  , sdlPauseAudioDevice
  , sdlResumeAudioDevice
  , sdlAudioDevicePaused
  , sdlGetAudioDeviceGain
  , sdlSetAudioDeviceGain
  , sdlCloseAudioDevice
    -- * Audio Stream Functions
  , sdlBindAudioStreams
  , sdlBindAudioStream
  , sdlUnbindAudioStreams
  , sdlUnbindAudioStream
  , sdlGetAudioStreamDevice
  , sdlCreateAudioStream
  , sdlGetAudioStreamProperties
  , sdlGetAudioStreamFormat
  , sdlSetAudioStreamFormat
  , sdlGetAudioStreamFrequencyRatio
  , sdlSetAudioStreamFrequencyRatio
  , sdlGetAudioStreamGain
  , sdlSetAudioStreamGain
  , sdlGetAudioStreamInputChannelMap
  , sdlGetAudioStreamOutputChannelMap
  , sdlSetAudioStreamInputChannelMap
  , sdlSetAudioStreamOutputChannelMap
  , sdlPutAudioStreamData
  , sdlGetAudioStreamData
  , sdlGetAudioStreamAvailable
  , sdlGetAudioStreamQueued
  , sdlFlushAudioStream
  , sdlClearAudioStream
  , sdlLockAudioStream
  , sdlUnlockAudioStream
  , sdlDestroyAudioStream
    -- * Simplified Audio Functions
  , sdlOpenAudioDeviceStream
  , sdlPauseAudioStreamDevice
  , sdlResumeAudioStreamDevice
  , sdlAudioStreamDevicePaused
    -- * Audio Post-mix Callback
  , sdlSetAudioPostmixCallback
    -- * WAV Loading Functions
  , sdlLoadWAV_IO
  , sdlLoadWAV
    -- * Audio Utilities
  , sdlMixAudio
  , sdlConvertAudioSamples
  , sdlGetAudioFormatName
  , sdlGetSilenceValueForFormat
  ) where

#include <SDL3/SDL_audio.h>

import Foreign
import Foreign.C.Types
import Foreign.C.String
import SDL3.IOStream
import SDL3.Properties
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)

-- | Audio format specification
type SDLAudioFormat = Word16

-- | Audio device instance IDs
type SDLAudioDeviceID = Word32

-- | Audio stream type
newtype SDLAudioStream = SDLAudioStream (Ptr SDLAudioStream)
  deriving (Eq, Show)

-- | Audio specification structure
data SDLAudioSpec = SDLAudioSpec
  { audioFormat   :: SDLAudioFormat  -- ^ Audio data format
  , audioChannels :: Int             -- ^ Number of channels
  , audioFreq     :: Int             -- ^ Sample rate (samples per second)
  } deriving (Show, Eq)

instance Storable SDLAudioSpec where
  sizeOf _ = #{size SDL_AudioSpec}
  alignment _ = #{alignment SDL_AudioSpec}
  peek ptr = do
    fmt <- #{peek SDL_AudioSpec, format} ptr
    ch <- fmap fromIntegral (#{peek SDL_AudioSpec, channels} ptr :: IO CInt)
    fr <- fmap fromIntegral (#{peek SDL_AudioSpec, freq} ptr :: IO CInt)
    return $ SDLAudioSpec fmt ch fr
  poke ptr spec = do
    #{poke SDL_AudioSpec, format} ptr (audioFormat spec)
    #{poke SDL_AudioSpec, channels} ptr (fromIntegral (audioChannels spec) :: CInt)
    #{poke SDL_AudioSpec, freq} ptr (fromIntegral (audioFreq spec) :: CInt)
    -- Debug the poked values
    fmt <- #{peek SDL_AudioSpec, format} ptr :: IO SDLAudioFormat
    ch <- #{peek SDL_AudioSpec, channels} ptr :: IO CInt
    fr <- #{peek SDL_AudioSpec, freq} ptr :: IO CInt
    putStrLn $ "Poked SDLAudioSpec: format=" ++ show fmt ++ ", channels=" ++ show ch ++ ", freq=" ++ show fr
    putStrLn $ "Size: " ++ show (sizeOf (undefined :: SDLAudioSpec))

  -- Audio format constants
pattern SDL_AUDIO_MASK_BITSIZE = (#{const SDL_AUDIO_MASK_BITSIZE}) :: Word16
pattern SDL_AUDIO_MASK_FLOAT = (#{const SDL_AUDIO_MASK_FLOAT}) :: Word16
pattern SDL_AUDIO_MASK_BIG_ENDIAN = (#{const SDL_AUDIO_MASK_BIG_ENDIAN}) :: Word16
pattern SDL_AUDIO_MASK_SIGNED = (#{const SDL_AUDIO_MASK_SIGNED}) :: Word16
pattern SDL_AUDIO_UNKNOWN = (#{const SDL_AUDIO_UNKNOWN}) :: Word16
pattern SDL_AUDIO_U8 = (#{const SDL_AUDIO_U8}) :: Word16
pattern SDL_AUDIO_S8 = (#{const SDL_AUDIO_S8}) :: Word16
pattern SDL_AUDIO_S16LE = (#{const SDL_AUDIO_S16LE}) :: Word16
pattern SDL_AUDIO_S16BE = (#{const SDL_AUDIO_S16BE}) :: Word16
pattern SDL_AUDIO_S32LE = (#{const SDL_AUDIO_S32LE}) :: Word16
pattern SDL_AUDIO_S32BE = (#{const SDL_AUDIO_S32BE}) :: Word16
pattern SDL_AUDIO_F32LE = (#{const SDL_AUDIO_F32LE}) :: Word16
pattern SDL_AUDIO_F32BE = (#{const SDL_AUDIO_F32BE}) :: Word16
pattern SDL_AUDIO_S16 = (#{const SDL_AUDIO_S16}) :: Word16
pattern SDL_AUDIO_S32 = (#{const SDL_AUDIO_S32}) :: Word16
pattern SDL_AUDIO_F32 = (#{const SDL_AUDIO_F32}) :: Word16

-- Default device constants
pattern SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK = (#{const SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK}) :: SDLAudioDeviceID
pattern SDL_AUDIO_DEVICE_DEFAULT_RECORDING = (#{const SDL_AUDIO_DEVICE_DEFAULT_RECORDING}) :: SDLAudioDeviceID

-- Callback types
type SDLAudioStreamCallback = Ptr () -> Ptr SDLAudioStream -> CInt -> CInt -> IO ()
type SDLAudioPostmixCallback = Ptr () -> Ptr SDLAudioSpec -> Ptr CFloat -> CInt -> IO ()

-- Helper functions for wrapping callbacks
foreign import ccall "wrapper"
  wrapAudioStreamCallback :: SDLAudioStreamCallback -> IO (FunPtr SDLAudioStreamCallback)

foreign import ccall "wrapper"
  wrapAudioPostmixCallback :: SDLAudioPostmixCallback -> IO (FunPtr SDLAudioPostmixCallback)

-- Helper to create a FunPtr for audio stream callbacks
makeAudioStreamCallback :: SDLAudioStreamCallback -> IO (FunPtr SDLAudioStreamCallback)
makeAudioStreamCallback = wrapAudioStreamCallback

-- Helper to create a FunPtr for postmix callbacks
makeAudioPostmixCallback :: SDLAudioPostmixCallback -> IO (FunPtr SDLAudioPostmixCallback)
makeAudioPostmixCallback = wrapAudioPostmixCallback

foreign import ccall unsafe "SDL_GetNumAudioDrivers" c_sdlGetNumAudioDrivers :: IO CInt
foreign import ccall unsafe "SDL_GetAudioDriver" c_sdlGetAudioDriver :: CInt -> IO CString
foreign import ccall unsafe "SDL_GetCurrentAudioDriver" c_sdlGetCurrentAudioDriver :: IO CString
foreign import ccall unsafe "SDL_GetAudioPlaybackDevices" c_sdlGetAudioPlaybackDevices :: Ptr CInt -> IO (Ptr SDLAudioDeviceID)
foreign import ccall unsafe "SDL_GetAudioRecordingDevices" c_sdlGetAudioRecordingDevices :: Ptr CInt -> IO (Ptr SDLAudioDeviceID)
foreign import ccall unsafe "SDL_GetAudioDeviceName" c_sdlGetAudioDeviceName :: SDLAudioDeviceID -> IO CString
foreign import ccall unsafe "SDL_GetAudioDeviceFormat" c_sdlGetAudioDeviceFormat :: SDLAudioDeviceID -> Ptr SDLAudioSpec -> Ptr CInt -> IO CBool
foreign import ccall unsafe "SDL_GetAudioDeviceChannelMap" c_sdlGetAudioDeviceChannelMap :: SDLAudioDeviceID -> Ptr CInt -> IO (Ptr CInt)
foreign import ccall unsafe "SDL_OpenAudioDevice" c_sdlOpenAudioDevice :: SDLAudioDeviceID -> Ptr SDLAudioSpec -> IO SDLAudioDeviceID
foreign import ccall unsafe "SDL_IsAudioDevicePhysical" c_sdlIsAudioDevicePhysical :: SDLAudioDeviceID -> IO CBool
foreign import ccall unsafe "SDL_IsAudioDevicePlayback" c_sdlIsAudioDevicePlayback :: SDLAudioDeviceID -> IO CBool
foreign import ccall unsafe "SDL_PauseAudioDevice" c_sdlPauseAudioDevice :: SDLAudioDeviceID -> IO CBool
foreign import ccall unsafe "SDL_ResumeAudioDevice" c_sdlResumeAudioDevice :: SDLAudioDeviceID -> IO CBool
foreign import ccall unsafe "SDL_AudioDevicePaused" c_sdlAudioDevicePaused :: SDLAudioDeviceID -> IO CBool
foreign import ccall unsafe "SDL_GetAudioDeviceGain" c_sdlGetAudioDeviceGain :: SDLAudioDeviceID -> IO CFloat
foreign import ccall unsafe "SDL_SetAudioDeviceGain" c_sdlSetAudioDeviceGain :: SDLAudioDeviceID -> CFloat -> IO CBool
foreign import ccall unsafe "SDL_CloseAudioDevice" c_sdlCloseAudioDevice :: SDLAudioDeviceID -> IO ()
foreign import ccall unsafe "SDL_BindAudioStreams" c_sdlBindAudioStreams :: SDLAudioDeviceID -> Ptr (Ptr SDLAudioStream) -> CInt -> IO CBool
foreign import ccall unsafe "SDL_BindAudioStream" c_sdlBindAudioStream :: SDLAudioDeviceID -> Ptr SDLAudioStream -> IO CBool
foreign import ccall unsafe "SDL_UnbindAudioStreams" c_sdlUnbindAudioStreams :: Ptr (Ptr SDLAudioStream) -> CInt -> IO ()
foreign import ccall unsafe "SDL_UnbindAudioStream" c_sdlUnbindAudioStream :: Ptr SDLAudioStream -> IO ()
foreign import ccall unsafe "SDL_GetAudioStreamDevice" c_sdlGetAudioStreamDevice :: Ptr SDLAudioStream -> IO SDLAudioDeviceID
foreign import ccall unsafe "SDL_CreateAudioStream" c_sdlCreateAudioStream :: Ptr SDLAudioSpec -> Ptr SDLAudioSpec -> IO (Ptr SDLAudioStream)
foreign import ccall unsafe "SDL_GetAudioStreamProperties" c_sdlGetAudioStreamProperties :: Ptr SDLAudioStream -> IO SDLPropertiesID
foreign import ccall unsafe "SDL_GetAudioStreamFormat" c_sdlGetAudioStreamFormat :: Ptr SDLAudioStream -> Ptr SDLAudioSpec -> Ptr SDLAudioSpec -> IO CBool
foreign import ccall unsafe "SDL_SetAudioStreamFormat" c_sdlSetAudioStreamFormat :: Ptr SDLAudioStream -> Ptr SDLAudioSpec -> Ptr SDLAudioSpec -> IO CBool
foreign import ccall unsafe "SDL_GetAudioStreamFrequencyRatio" c_sdlGetAudioStreamFrequencyRatio :: Ptr SDLAudioStream -> IO CFloat
foreign import ccall unsafe "SDL_SetAudioStreamFrequencyRatio" c_sdlSetAudioStreamFrequencyRatio :: Ptr SDLAudioStream -> CFloat -> IO CBool
foreign import ccall unsafe "SDL_GetAudioStreamGain" c_sdlGetAudioStreamGain :: Ptr SDLAudioStream -> IO CFloat
foreign import ccall unsafe "SDL_SetAudioStreamGain" c_sdlSetAudioStreamGain :: Ptr SDLAudioStream -> CFloat -> IO CBool
foreign import ccall unsafe "SDL_GetAudioStreamInputChannelMap" c_sdlGetAudioStreamInputChannelMap :: Ptr SDLAudioStream -> Ptr CInt -> IO (Ptr CInt)
foreign import ccall unsafe "SDL_GetAudioStreamOutputChannelMap" c_sdlGetAudioStreamOutputChannelMap :: Ptr SDLAudioStream -> Ptr CInt -> IO (Ptr CInt)
foreign import ccall unsafe "SDL_SetAudioStreamInputChannelMap" c_sdlSetAudioStreamInputChannelMap :: Ptr SDLAudioStream -> Ptr CInt -> CInt -> IO CBool
foreign import ccall unsafe "SDL_SetAudioStreamOutputChannelMap" c_sdlSetAudioStreamOutputChannelMap :: Ptr SDLAudioStream -> Ptr CInt -> CInt -> IO CBool
foreign import ccall unsafe "SDL_PutAudioStreamData" c_sdlPutAudioStreamData :: Ptr SDLAudioStream -> Ptr () -> CInt -> IO CBool
foreign import ccall unsafe "SDL_GetAudioStreamData" c_sdlGetAudioStreamData :: Ptr SDLAudioStream -> Ptr () -> CInt -> IO CInt
foreign import ccall unsafe "SDL_GetAudioStreamAvailable" c_sdlGetAudioStreamAvailable :: Ptr SDLAudioStream -> IO CInt
foreign import ccall unsafe "SDL_GetAudioStreamQueued" c_sdlGetAudioStreamQueued :: Ptr SDLAudioStream -> IO CInt
foreign import ccall unsafe "SDL_FlushAudioStream" c_sdlFlushAudioStream :: Ptr SDLAudioStream -> IO CBool
foreign import ccall unsafe "SDL_ClearAudioStream" c_sdlClearAudioStream :: Ptr SDLAudioStream -> IO CBool
foreign import ccall unsafe "SDL_PauseAudioStreamDevice" c_sdlPauseAudioStreamDevice :: Ptr SDLAudioStream -> IO CBool
foreign import ccall unsafe "SDL_ResumeAudioStreamDevice" c_sdlResumeAudioStreamDevice :: Ptr SDLAudioStream -> IO CBool
foreign import ccall unsafe "SDL_AudioStreamDevicePaused" c_sdlAudioStreamDevicePaused :: Ptr SDLAudioStream -> IO CBool
foreign import ccall unsafe "SDL_LockAudioStream" c_sdlLockAudioStream :: Ptr SDLAudioStream -> IO CBool
foreign import ccall unsafe "SDL_UnlockAudioStream" c_sdlUnlockAudioStream :: Ptr SDLAudioStream -> IO CBool
foreign import ccall unsafe "SDL_SetAudioStreamGetCallback" c_sdlSetAudioStreamGetCallback :: Ptr SDLAudioStream -> FunPtr SDLAudioStreamCallback -> Ptr () -> IO CBool
foreign import ccall unsafe "SDL_SetAudioStreamPutCallback" c_sdlSetAudioStreamPutCallback :: Ptr SDLAudioStream -> FunPtr SDLAudioStreamCallback -> Ptr () -> IO CBool
foreign import ccall unsafe "SDL_DestroyAudioStream" c_sdlDestroyAudioStream :: Ptr SDLAudioStream -> IO ()
foreign import ccall unsafe "SDL_OpenAudioDeviceStream" c_sdlOpenAudioDeviceStream :: SDLAudioDeviceID -> Ptr SDLAudioSpec -> FunPtr SDLAudioStreamCallback -> Ptr () -> IO (Ptr SDLAudioStream)
foreign import ccall unsafe "SDL_SetAudioPostmixCallback" c_sdlSetAudioPostmixCallback :: SDLAudioDeviceID -> FunPtr SDLAudioPostmixCallback -> Ptr () -> IO CBool
foreign import ccall unsafe "SDL_LoadWAV_IO" c_sdlLoadWAV_IO :: Ptr SDLIOStream -> CBool -> Ptr SDLAudioSpec -> Ptr (Ptr Word8) -> Ptr Word32 -> IO CBool
foreign import ccall unsafe "SDL_LoadWAV" c_sdlLoadWAV :: CString -> Ptr SDLAudioSpec -> Ptr (Ptr Word8) -> Ptr Word32 -> IO CBool
foreign import ccall unsafe "SDL_MixAudio" c_sdlMixAudio :: Ptr Word8 -> Ptr Word8 -> SDLAudioFormat -> Word32 -> CFloat -> IO CBool
foreign import ccall unsafe "SDL_ConvertAudioSamples" c_sdlConvertAudioSamples :: Ptr SDLAudioSpec -> Ptr Word8 -> CInt -> Ptr SDLAudioSpec -> Ptr (Ptr Word8) -> Ptr CInt -> IO CBool
foreign import ccall unsafe "SDL_GetAudioFormatName" c_sdlGetAudioFormatName :: SDLAudioFormat -> IO CString
foreign import ccall unsafe "SDL_GetSilenceValueForFormat" c_sdlGetSilenceValueForFormat :: SDLAudioFormat -> IO CInt
foreign import ccall "string.h memset" memset :: Ptr a -> CInt -> CSize -> IO ()

-- | Foreign imports for missing SDL3 audio stream functions

foreign import ccall unsafe "SDL_PutAudioStreamDataNoCopy"
  c_sdlPutAudioStreamDataNoCopy :: Ptr SDLAudioStream -> Ptr () -> CInt -> FunPtr (Ptr () -> IO ()) -> Ptr () -> IO CBool

foreign import ccall unsafe "SDL_PutAudioStreamPlanarData"
  c_sdlPutAudioStreamPlanarData :: Ptr SDLAudioStream -> Ptr (Ptr ()) -> CInt -> CInt -> IO CBool

foreign import ccall unsafe "SDL_SetAudioIterationCallbacks"
  c_sdlSetAudioIterationCallbacks :: SDLAudioDeviceID -> FunPtr (IO ()) -> FunPtr (IO ()) -> Ptr () -> IO ()

-- | Get the number of built-in audio drivers.
--
-- Returns the number of drivers compiled into SDL. Never negative; returns 0 if no drivers
-- are available. Presence doesn’t guarantee functionality—drivers may fail if their
-- backend isn’t available.
sdlGetNumAudioDrivers :: IO Int
sdlGetNumAudioDrivers = fromIntegral <$> c_sdlGetNumAudioDrivers

-- | Get the name of a built-in audio driver at the specified index.
--
-- Drivers are listed in SDL’s preferred initialization order. Names are simple
-- ASCII identifiers (e.g., "alsa", "coreaudio"). Returns 'Nothing' for invalid indices.
--
-- Index must be between 0 and 'sdlGetNumAudioDrivers' - 1.
sdlGetAudioDriver :: Int -> IO (Maybe String)
sdlGetAudioDriver index = do
  cstr <- c_sdlGetAudioDriver (fromIntegral index)
  if cstr == nullPtr
    then return Nothing
    else Just <$> peekCString cstr

-- | Get the name of the current audio driver.
--
-- Returns the name of the initialized driver (e.g., "alsa", "wasapi") or 'Nothing'
-- if no driver is initialized. Names are simple ASCII identifiers.
sdlGetCurrentAudioDriver :: IO (Maybe String)
sdlGetCurrentAudioDriver = do
  cstr <- c_sdlGetCurrentAudioDriver
  if cstr == nullPtr
    then return Nothing
    else Just <$> peekCString cstr

-- | Get a list of currently-connected audio playback devices.
--
-- Returns a list of device IDs for playback devices (e.g., speakers, headphones).
-- Returns an empty list on error or if no devices are found.
sdlGetAudioPlaybackDevices :: IO [SDLAudioDeviceID]
sdlGetAudioPlaybackDevices =
  bracket (malloc :: IO (Ptr CInt)) free $ \countPtr -> do
    poke countPtr 0  -- Initialize count to 0
    devicesPtr <- c_sdlGetAudioPlaybackDevices countPtr
    if devicesPtr == nullPtr
      then return []
      else do
        count <- fromIntegral <$> peek countPtr
        devices <- peekArray count devicesPtr
        free devicesPtr  -- Free the array
        return devices

-- | Get a list of currently-connected audio recording devices.
--
-- Returns a list of device IDs for recording devices (e.g., microphones).
-- Returns an empty list on error or if no devices are found.
sdlGetAudioRecordingDevices :: IO [SDLAudioDeviceID]
sdlGetAudioRecordingDevices =
  bracket (malloc :: IO (Ptr CInt)) free $ \countPtr -> do
    poke countPtr 0  -- Initialize count to 0
    devicesPtr <- c_sdlGetAudioRecordingDevices countPtr
    if devicesPtr == nullPtr
      then return []
      else do
        count <- fromIntegral <$> peek countPtr
        devices <- peekArray count devicesPtr
        free devicesPtr  -- Free the array
        return devices

-- | Get the human-readable name of a specific audio device.
--
-- Returns the device’s name or 'Nothing' on failure.
sdlGetAudioDeviceName :: SDLAudioDeviceID -> IO (Maybe String)
sdlGetAudioDeviceName devid = do
  cstr <- c_sdlGetAudioDeviceName devid
  if cstr == nullPtr
    then return Nothing
    else Just <$> peekCString cstr

-- | Get the current audio format and buffer size of a specific audio device.
--
-- Returns the device’s current or preferred format and optional sample frames.
-- For unopened devices, this is the preferred format; for opened devices, it’s
-- the active format. Use 'SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK' or
-- 'SDL_AUDIO_DEVICE_DEFAULT_RECORDING' for defaults.
sdlGetAudioDeviceFormat :: SDLAudioDeviceID -> IO (Maybe (SDLAudioSpec, Maybe Int))
sdlGetAudioDeviceFormat devid =
  bracket (malloc :: IO (Ptr SDLAudioSpec)) free $ \specPtr ->
  bracket (malloc :: IO (Ptr CInt)) free $ \framesPtr -> do
    poke framesPtr 0  -- Initialize to 0
    success <- c_sdlGetAudioDeviceFormat devid specPtr framesPtr
    if not (fromCBool success)
      then return Nothing
      else do
        spec <- peek specPtr
        frames <- peek framesPtr
        let sampleFrames = if frames == 0 then Nothing else Just (fromIntegral frames)
        return $ Just (spec, sampleFrames)

-- | Get the current channel map of an audio device.
--
-- Returns the channel map as a list of integers, or 'Nothing' if the default
-- mapping is used (no remapping). The list length matches the device’s channel count.
sdlGetAudioDeviceChannelMap :: SDLAudioDeviceID -> IO (Maybe [Int])
sdlGetAudioDeviceChannelMap devid =
  bracket (malloc :: IO (Ptr CInt)) free $ \countPtr -> do
    poke countPtr 0  -- Initialize to 0
    mapPtr <- c_sdlGetAudioDeviceChannelMap devid countPtr
    if mapPtr == nullPtr
      then return Nothing
      else do
        count <- fromIntegral <$> peek countPtr
        mapData <- peekArray count mapPtr
        free mapPtr
        return $ Just (map fromIntegral mapData)

-- | Open a specific audio device for playback or recording.
--
-- Use 'SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK' or 'SDL_AUDIO_DEVICE_DEFAULT_RECORDING'
-- for defaults. The 'SDLAudioSpec' is a hint; the actual format may differ (check
-- with 'sdlGetAudioDeviceFormat'). Returns 'Nothing' on failure.
sdlOpenAudioDevice :: SDLAudioDeviceID -> Maybe SDLAudioSpec -> IO (Maybe SDLAudioDeviceID)
sdlOpenAudioDevice devid maybeSpec =
  bracket (maybe (return nullPtr) (new . toCAudioSpec) maybeSpec) free $ \specPtr -> do
    newDevid <- c_sdlOpenAudioDevice devid specPtr
    return $ if newDevid == 0 then Nothing else Just newDevid
  where
    toCAudioSpec spec = SDLAudioSpec (audioFormat spec) (audioChannels spec) (audioFreq spec)

-- | Check if an audio device is physical (not logical).
--
-- Physical devices are hardware; logical devices are opened instances.
sdlIsAudioDevicePhysical :: SDLAudioDeviceID -> IO Bool
sdlIsAudioDevicePhysical devid = fromCBool <$> c_sdlIsAudioDevicePhysical devid

-- | Check if an audio device is for playback (not recording).
sdlIsAudioDevicePlayback :: SDLAudioDeviceID -> IO Bool
sdlIsAudioDevicePlayback devid = fromCBool <$> c_sdlIsAudioDevicePlayback devid

-- | Pause audio playback on a specified device.
--
-- Only works on logical devices opened with 'sdlOpenAudioDevice'. Returns 'False' on failure.
sdlPauseAudioDevice :: SDLAudioDeviceID -> IO Bool
sdlPauseAudioDevice devid = fromCBool <$> c_sdlPauseAudioDevice devid

-- | Resume audio playback on a specified device.
--
-- Only works on logical devices. Returns 'False' on failure.
sdlResumeAudioDevice :: SDLAudioDeviceID -> IO Bool
sdlResumeAudioDevice devid = fromCBool <$> c_sdlResumeAudioDevice devid

-- | Check if an audio device is paused.
--
-- Returns 'True' only for valid, paused logical devices.
sdlAudioDevicePaused :: SDLAudioDeviceID -> IO Bool
sdlAudioDevicePaused devid = fromCBool <$> c_sdlAudioDevicePaused devid

-- | Get the gain (volume) of an audio device.
--
-- Returns 'Nothing' on failure or for physical devices (which return -1.0).
-- Default gain is 1.0.
sdlGetAudioDeviceGain :: SDLAudioDeviceID -> IO (Maybe Float)
sdlGetAudioDeviceGain devid = do
  gain <- c_sdlGetAudioDeviceGain devid
  return $ if gain < 0 then Nothing else Just (realToFrac gain)

-- | Set the gain (volume) of an audio device.
--
-- Only works on logical devices. Gain of 1.0 is no change, 0.0 is silence.
-- Returns 'False' on failure.
sdlSetAudioDeviceGain :: SDLAudioDeviceID -> Float -> IO Bool
sdlSetAudioDeviceGain devid gain = fromCBool <$> c_sdlSetAudioDeviceGain devid (realToFrac gain)

-- | Close a previously-opened audio device.
sdlCloseAudioDevice :: SDLAudioDeviceID -> IO ()
sdlCloseAudioDevice = c_sdlCloseAudioDevice

-- | Bind multiple audio streams to an audio device.
--
-- Streams must not already be bound. Returns 'False' on failure.
sdlBindAudioStreams :: SDLAudioDeviceID -> [SDLAudioStream] -> IO Bool
sdlBindAudioStreams devid streams =
  withArrayLen (map (\(SDLAudioStream ptr) -> ptr) streams) $ \len streamPtrs ->
    fromCBool <$> c_sdlBindAudioStreams devid streamPtrs (fromIntegral len)

-- | Bind a single audio stream to an audio device.
--
-- Convenience wrapper for 'sdlBindAudioStreams'. Returns 'False' on failure.
sdlBindAudioStream :: SDLAudioDeviceID -> SDLAudioStream -> IO Bool
sdlBindAudioStream devid (SDLAudioStream stream) = fromCBool <$> c_sdlBindAudioStream devid stream

-- | Unbind multiple audio streams from their devices.
--
-- Streams on the same device are unbound atomically. No-op for unbound streams.
sdlUnbindAudioStreams :: [SDLAudioStream] -> IO ()
sdlUnbindAudioStreams streams =
  withArrayLen (map (\(SDLAudioStream ptr) -> ptr) streams) $ \len streamPtrs ->
    c_sdlUnbindAudioStreams streamPtrs (fromIntegral len)

-- | Unbind a single audio stream from its device.
--
-- Convenience wrapper for 'sdlUnbindAudioStreams'.
sdlUnbindAudioStream :: SDLAudioStream -> IO ()
sdlUnbindAudioStream (SDLAudioStream stream) = c_sdlUnbindAudioStream stream

-- | Query the device an audio stream is bound to.
--
-- Returns 'Nothing' if not bound or invalid.
sdlGetAudioStreamDevice :: SDLAudioStream -> IO (Maybe SDLAudioDeviceID)
sdlGetAudioStreamDevice (SDLAudioStream stream) = do
  devid <- c_sdlGetAudioStreamDevice stream
  return $ if devid == 0 then Nothing else Just devid

-- | Create a new audio stream with specified input and output formats.
--
-- Returns 'Nothing' on failure.
sdlCreateAudioStream :: SDLAudioSpec -> SDLAudioSpec -> IO (Maybe SDLAudioStream)
sdlCreateAudioStream srcSpec dstSpec =
  bracket (new $ toCAudioSpec srcSpec) free $ \srcPtr ->
  bracket (new $ toCAudioSpec dstSpec) free $ \dstPtr -> do
    streamPtr <- c_sdlCreateAudioStream srcPtr dstPtr
    return $ if streamPtr == nullPtr then Nothing else Just (SDLAudioStream streamPtr)
  where
    toCAudioSpec spec = SDLAudioSpec (audioFormat spec) (audioChannels spec) (audioFreq spec)

-- | Get the properties associated with an audio stream.
--
-- Returns 'Nothing' on failure.
sdlGetAudioStreamProperties :: SDLAudioStream -> IO (Maybe SDLPropertiesID)
sdlGetAudioStreamProperties (SDLAudioStream stream) = do
  props <- c_sdlGetAudioStreamProperties stream
  return $ if props == 0 then Nothing else Just props

-- | Query the current input and output formats of an audio stream.
--
-- Returns 'Nothing' on failure.
sdlGetAudioStreamFormat :: SDLAudioStream -> IO (Maybe (SDLAudioSpec, SDLAudioSpec))
sdlGetAudioStreamFormat (SDLAudioStream stream) =
  bracket (malloc :: IO (Ptr SDLAudioSpec)) free $ \srcPtr ->
  bracket (malloc :: IO (Ptr SDLAudioSpec)) free $ \dstPtr -> do
    success <- c_sdlGetAudioStreamFormat stream srcPtr dstPtr
    if not (fromCBool success)
      then return Nothing
      else do
        srcSpec <- peek srcPtr
        dstSpec <- peek dstPtr
        return $ Just (srcSpec, dstSpec)

-- | Change the input and output formats of an audio stream.
--
-- Pass 'Nothing' to leave a format unchanged. Returns 'False' on failure.
sdlSetAudioStreamFormat :: SDLAudioStream -> Maybe SDLAudioSpec -> Maybe SDLAudioSpec -> IO Bool
sdlSetAudioStreamFormat (SDLAudioStream stream) maybeSrc maybeDst =
  bracket (maybe (return nullPtr) (new . toCAudioSpec) maybeSrc) free $ \srcPtr ->
  bracket (maybe (return nullPtr) (new . toCAudioSpec) maybeDst) free $ \dstPtr ->
    fromCBool <$> c_sdlSetAudioStreamFormat stream srcPtr dstPtr
  where
    toCAudioSpec spec = SDLAudioSpec (audioFormat spec) (audioChannels spec) (audioFreq spec)

-- | Get the frequency ratio of an audio stream.
--
-- Returns 'Nothing' on failure (ratio = 0.0).
sdlGetAudioStreamFrequencyRatio :: SDLAudioStream -> IO (Maybe Float)
sdlGetAudioStreamFrequencyRatio (SDLAudioStream stream) = do
  ratio <- c_sdlGetAudioStreamFrequencyRatio stream
  return $ if ratio == 0 then Nothing else Just (realToFrac ratio)

-- | Set the frequency ratio of an audio stream (e.g., speed/pitch adjustment).
--
-- Ratio must be between 0.01 and 100. Returns 'False' on failure.
sdlSetAudioStreamFrequencyRatio :: SDLAudioStream -> Float -> IO Bool
sdlSetAudioStreamFrequencyRatio (SDLAudioStream stream) ratio =
  fromCBool <$> c_sdlSetAudioStreamFrequencyRatio stream (realToFrac ratio)

-- | Get the gain (volume) of an audio stream.
--
-- Returns 'Nothing' on failure (gain = -1.0).
sdlGetAudioStreamGain :: SDLAudioStream -> IO (Maybe Float)
sdlGetAudioStreamGain (SDLAudioStream stream) = do
  gain <- c_sdlGetAudioStreamGain stream
  return $ if gain < 0 then Nothing else Just (realToFrac gain)

-- | Set the gain (volume) of an audio stream.
--
-- Gain of 1.0 is no change, 0.0 is silence. Returns 'False' on failure.
sdlSetAudioStreamGain :: SDLAudioStream -> Float -> IO Bool
sdlSetAudioStreamGain (SDLAudioStream stream) gain =
  fromCBool <$> c_sdlSetAudioStreamGain stream (realToFrac gain)

-- | Get the current input channel map of an audio stream.
--
-- Returns 'Nothing' if default (no remapping).
sdlGetAudioStreamInputChannelMap :: SDLAudioStream -> IO (Maybe [Int])
sdlGetAudioStreamInputChannelMap (SDLAudioStream stream) =
  bracket (malloc :: IO (Ptr CInt)) free $ \countPtr -> do
    poke countPtr 0
    mapPtr <- c_sdlGetAudioStreamInputChannelMap stream countPtr
    if mapPtr == nullPtr
      then return Nothing
      else do
        count <- fromIntegral <$> peek countPtr
        mapData <- peekArray count mapPtr
        free mapPtr
        return $ Just (map fromIntegral mapData)

-- | Get the current output channel map of an audio stream.
--
-- Returns 'Nothing' if default (no remapping).
sdlGetAudioStreamOutputChannelMap :: SDLAudioStream -> IO (Maybe [Int])
sdlGetAudioStreamOutputChannelMap (SDLAudioStream stream) =
  bracket (malloc :: IO (Ptr CInt)) free $ \countPtr -> do
    poke countPtr 0
    mapPtr <- c_sdlGetAudioStreamOutputChannelMap stream countPtr
    if mapPtr == nullPtr
      then return Nothing
      else do
        count <- fromIntegral <$> peek countPtr
        mapData <- peekArray count mapPtr
        free mapPtr
        return $ Just (map fromIntegral mapData)

-- | Set the input channel map of an audio stream.
--
-- Pass 'Nothing' to reset to default. Returns 'False' on failure or mismatch.
sdlSetAudioStreamInputChannelMap :: SDLAudioStream -> Maybe [Int] -> IO Bool
sdlSetAudioStreamInputChannelMap (SDLAudioStream stream) maybeChmap =
  case maybeChmap of
    Nothing -> fromCBool <$> c_sdlSetAudioStreamInputChannelMap stream nullPtr 0
    Just chmap ->
      withArrayLen (map fromIntegral chmap) $ \len chmapPtr ->
        fromCBool <$> c_sdlSetAudioStreamInputChannelMap stream chmapPtr (fromIntegral len)

-- | Set the output channel map of an audio stream.
--
-- Pass 'Nothing' to reset to default. Returns 'False' on failure or mismatch.
sdlSetAudioStreamOutputChannelMap :: SDLAudioStream -> Maybe [Int] -> IO Bool
sdlSetAudioStreamOutputChannelMap (SDLAudioStream stream) maybeChmap =
  case maybeChmap of
    Nothing -> fromCBool <$> c_sdlSetAudioStreamOutputChannelMap stream nullPtr 0
    Just chmap ->
      withArrayLen (map fromIntegral chmap) $ \len chmapPtr ->
        fromCBool <$> c_sdlSetAudioStreamOutputChannelMap stream chmapPtr (fromIntegral len)

-- | Add audio data to the stream.

type NoCopyCallback = Ptr () -> IO ()
foreign import ccall "wrapper"
  wrapNoCopyCallback :: NoCopyCallback -> IO (FunPtr NoCopyCallback)

type IterationCallback = IO ()
foreign import ccall "wrapper"
  wrapIterationCallback :: IterationCallback -> IO (FunPtr IterationCallback)

-- | Add audio data to the stream without copying. The buffer must remain valid until the callback is called.
sdlPutAudioStreamDataNoCopy
  :: SDLAudioStream
  -> Ptr ()
  -> Int
  -> (Ptr () -> IO ()) -- ^ Completion callback
  -> Ptr ()            -- ^ Userdata
  -> IO Bool
sdlPutAudioStreamDataNoCopy (SDLAudioStream stream) buf len callback userdata = do
  cbPtr <- wrapNoCopyCallback callback
  fromCBool <$> c_sdlPutAudioStreamDataNoCopy stream buf (fromIntegral len) cbPtr userdata

-- | Add planar audio data to the stream.
sdlPutAudioStreamPlanarData
  :: SDLAudioStream
  -> Ptr (Ptr ())
  -> Int
  -> Int
  -> IO Bool
sdlPutAudioStreamPlanarData (SDLAudioStream stream) channelBuffers numChannels numSamples =
  fromCBool <$> c_sdlPutAudioStreamPlanarData stream channelBuffers (fromIntegral numChannels) (fromIntegral numSamples)

-- | Set iteration callbacks for an audio device.
sdlSetAudioIterationCallbacks
  :: SDLAudioDeviceID
  -> IO () -- ^ Start callback
  -> IO () -- ^ End callback
  -> Ptr () -- ^ Userdata
  -> IO ()
sdlSetAudioIterationCallbacks devid startCb endCb userdata = do
  startPtr <- wrapIterationCallback startCb
  endPtr <- wrapIterationCallback endCb
  c_sdlSetAudioIterationCallbacks devid startPtr endPtr userdata

--
-- Data must match the current input format. Returns 'False' on failure.
sdlPutAudioStreamData :: SDLAudioStream -> ByteString -> IO Bool
sdlPutAudioStreamData (SDLAudioStream stream) bs =
  unsafeUseAsCStringLen bs $ \(buf, len) ->
    fromCBool <$> c_sdlPutAudioStreamData stream (castPtr buf) (fromIntegral len)

-- | Get converted/resampled data from the stream.
--
-- Returns the data as a ByteString and the number of bytes read, or 'Nothing' on failure.
sdlGetAudioStreamData :: SDLAudioStream -> Int -> IO (Maybe (ByteString, Int))
sdlGetAudioStreamData (SDLAudioStream stream) maxLen =
  bracket (mallocBytes maxLen) free $ \buf -> do
    bytesRead <- c_sdlGetAudioStreamData stream buf (fromIntegral maxLen)
    if bytesRead < 0
      then return Nothing
      else do
        bs <- BS.packCStringLen (castPtr buf, fromIntegral bytesRead)
        return $ Just (bs, fromIntegral bytesRead)

-- | Get the number of converted/resampled bytes available in the stream.
--
-- Returns 'Nothing' on failure.
sdlGetAudioStreamAvailable :: SDLAudioStream -> IO (Maybe Int)
sdlGetAudioStreamAvailable (SDLAudioStream stream) = do
  bytes <- c_sdlGetAudioStreamAvailable stream
  return $ if bytes < 0 then Nothing else Just (fromIntegral bytes)

-- | Get the number of bytes currently queued in the stream (unconverted input).
--
-- Returns 'Nothing' on failure.
sdlGetAudioStreamQueued :: SDLAudioStream -> IO (Maybe Int)
sdlGetAudioStreamQueued (SDLAudioStream stream) = do
  bytes <- c_sdlGetAudioStreamQueued stream
  return $ if bytes < 0 then Nothing else Just (fromIntegral bytes)

-- | Flush the stream to make buffered data available immediately.
--
-- Returns 'False' on failure.
sdlFlushAudioStream :: SDLAudioStream -> IO Bool
sdlFlushAudioStream (SDLAudioStream stream) = fromCBool <$> c_sdlFlushAudioStream stream

-- | Clear any pending data in the stream.
--
-- Returns 'False' on failure.
sdlClearAudioStream :: SDLAudioStream -> IO Bool
sdlClearAudioStream (SDLAudioStream stream) = fromCBool <$> c_sdlClearAudioStream stream

-- | Pause the audio device associated with the stream.
--
-- Returns 'False' on failure.
sdlPauseAudioStreamDevice :: SDLAudioStream -> IO Bool
sdlPauseAudioStreamDevice (SDLAudioStream stream) = fromCBool <$> c_sdlPauseAudioStreamDevice stream

-- | Resume the audio device associated with the stream.
--
-- Returns 'False' on failure.
sdlResumeAudioStreamDevice :: SDLAudioStream -> IO Bool
sdlResumeAudioStreamDevice (SDLAudioStream stream) = fromCBool <$> c_sdlResumeAudioStreamDevice stream

-- | Check if the audio device associated with the stream is paused.
sdlAudioStreamDevicePaused :: SDLAudioStream -> IO Bool
sdlAudioStreamDevicePaused (SDLAudioStream stream) = fromCBool <$> c_sdlAudioStreamDevicePaused stream

-- | Lock the audio stream for serialized access.
--
-- Useful for callback management. Returns 'False' on failure.
sdlLockAudioStream :: SDLAudioStream -> IO Bool
sdlLockAudioStream (SDLAudioStream stream) = fromCBool <$> c_sdlLockAudioStream stream

-- | Unlock the audio stream after locking.
--
-- Must be called from the same thread as 'sdlLockAudioStream'. Returns 'False' on failure.
sdlUnlockAudioStream :: SDLAudioStream -> IO Bool
sdlUnlockAudioStream (SDLAudioStream stream) = fromCBool <$> c_sdlUnlockAudioStream stream

-- | Set a callback that runs when data is requested from an audio stream.
--
-- The callback fires before data is obtained, allowing on-demand addition via 'sdlPutAudioStreamData'.
-- Pass 'Nothing' for the callback to disable it. Returns 'False' on failure (e.g., if stream is null).
sdlSetAudioStreamGetCallback :: SDLAudioStream -> Maybe (SDLAudioStreamCallback, Ptr ()) -> IO Bool
sdlSetAudioStreamGetCallback (SDLAudioStream stream) maybeCallback =
  case maybeCallback of
    Nothing -> fromCBool <$> c_sdlSetAudioStreamGetCallback stream nullFunPtr nullPtr
    Just (callback, userdata) -> do
      callbackPtr <- makeAudioStreamCallback callback
      fromCBool <$> c_sdlSetAudioStreamGetCallback stream callbackPtr userdata

-- | Set a callback that runs when data is added to an audio stream.
--
-- The callback fires after data is added, allowing immediate retrieval via 'sdlGetAudioStreamData'.
-- Pass 'Nothing' for the callback to disable it. Returns 'False' on failure (e.g., if stream is null).
sdlSetAudioStreamPutCallback :: SDLAudioStream -> Maybe (SDLAudioStreamCallback, Ptr ()) -> IO Bool
sdlSetAudioStreamPutCallback (SDLAudioStream stream) maybeCallback =
  case maybeCallback of
    Nothing -> fromCBool <$> c_sdlSetAudioStreamPutCallback stream nullFunPtr nullPtr
    Just (callback, userdata) -> do
      callbackPtr <- makeAudioStreamCallback callback
      fromCBool <$> c_sdlSetAudioStreamPutCallback stream callbackPtr userdata

-- | Free an audio stream, releasing all allocated data and unbinding it from devices.
--
-- If created with 'sdlOpenAudioDeviceStream', the associated device is also closed.
sdlDestroyAudioStream :: SDLAudioStream -> IO ()
sdlDestroyAudioStream (SDLAudioStream stream) = c_sdlDestroyAudioStream stream

-- | Convenience function to open an audio device, create a stream, and bind it.
--
-- The device starts paused; use 'sdlResumeAudioStreamDevice' to start playback.
-- Pass 'Nothing' for 'spec' to let SDL choose the format. Returns 'Nothing' on failure.
sdlOpenAudioDeviceStream :: SDLAudioDeviceID -> Maybe SDLAudioSpec -> Maybe (SDLAudioStreamCallback, Ptr ()) -> IO (Maybe SDLAudioStream)
sdlOpenAudioDeviceStream devid maybeSpec maybeCallback =
  bracket (case maybeSpec of
             Nothing -> return nullPtr
             Just spec -> do
               ptr <- mallocBytes (sizeOf (undefined :: SDLAudioSpec))
               memset ptr 0 (fromIntegral (sizeOf (undefined :: SDLAudioSpec)))
               poke ptr spec
               return ptr
          ) free $ \specPtr -> do
    (callbackPtr, userdata) <- case maybeCallback of
      Nothing -> return (nullFunPtr, nullPtr)
      Just (callback, ud) -> (,ud) <$> makeAudioStreamCallback callback
    streamPtr <- c_sdlOpenAudioDeviceStream devid specPtr callbackPtr userdata
    return $ if streamPtr == nullPtr then Nothing else Just (SDLAudioStream streamPtr)

-- | Set a callback that fires when data is about to be fed to an audio device.
--
-- Useful for final mix adjustments. The callback receives data in 'SDL_AUDIO_F32' format.
-- Pass 'Nothing' to disable the callback. Returns 'False' on failure.
sdlSetAudioPostmixCallback :: SDLAudioDeviceID -> Maybe (SDLAudioPostmixCallback, Ptr ()) -> IO Bool
sdlSetAudioPostmixCallback devid maybeCallback =
  case maybeCallback of
    Nothing -> fromCBool <$> c_sdlSetAudioPostmixCallback devid nullFunPtr nullPtr
    Just (callback, userdata) -> do
      callbackPtr <- makeAudioPostmixCallback callback
      fromCBool <$> c_sdlSetAudioPostmixCallback devid callbackPtr userdata

-- | Load WAV data from an SDL_IOStream.
--
-- Returns the audio spec and data as a ByteString on success, or 'Nothing' on failure.
-- If 'closeio' is True, the stream is closed even on error.
sdlLoadWAV_IO :: SDLIOStream -> Bool -> IO (Maybe (SDLAudioSpec, ByteString))
sdlLoadWAV_IO (SDLIOStream src) closeio =
  bracket (malloc :: IO (Ptr SDLAudioSpec)) free $ \specPtr ->
  bracket (malloc :: IO (Ptr (Ptr Word8))) free $ \bufPtr ->
  bracket (malloc :: IO (Ptr Word32)) free $ \lenPtr -> do
    success <- c_sdlLoadWAV_IO src (fromBool closeio) specPtr bufPtr lenPtr
    if not (fromCBool success)
      then return Nothing
      else do
        spec <- peek specPtr
        buf <- peek bufPtr
        len <- fromIntegral <$> peek lenPtr
        bs <- BS.packCStringLen (castPtr buf, len)
        free buf  -- Caller must free the buffer
        return $ Just (spec, bs)

-- | Load WAV data from a file path.
--
-- Convenience wrapper for 'sdlLoadWAV_IO'. Returns the audio spec and data on success.
sdlLoadWAV :: String -> IO (Maybe (SDLAudioSpec, ByteString))
sdlLoadWAV path =
  withCString path $ \cpath ->
  bracket (malloc :: IO (Ptr SDLAudioSpec)) free $ \specPtr ->
  bracket (malloc :: IO (Ptr (Ptr Word8))) free $ \bufPtr ->
  bracket (malloc :: IO (Ptr Word32)) free $ \lenPtr -> do
    success <- c_sdlLoadWAV cpath specPtr bufPtr lenPtr
    if not (fromCBool success)
      then return Nothing
      else do
        spec <- peek specPtr
        buf <- peek bufPtr
        len <- fromIntegral <$> peek lenPtr
        bs <- BS.packCStringLen (castPtr buf, len)
        free buf  -- Caller must free the buffer
        return $ Just (spec, bs)

-- | Mix audio data into a destination buffer with volume adjustment.
--
-- Both buffers must be in the same format and length. Returns 'False' on failure.
sdlMixAudio :: ByteString -> ByteString -> SDLAudioFormat -> Float -> IO Bool
sdlMixAudio dst src format volume =
  unsafeUseAsCStringLen dst $ \(dstPtr, dstLen) ->
  unsafeUseAsCStringLen src $ \(srcPtr, srcLen) ->
    if dstLen /= srcLen
      then return False
      else fromCBool <$> c_sdlMixAudio (castPtr dstPtr) (castPtr srcPtr) format (fromIntegral dstLen) (realToFrac volume)

-- | Convert audio samples from one format to another.
--
-- Returns the converted data as a ByteString on success, or 'Nothing' on failure.
-- Not suitable for block-wise conversion; use 'SDLAudioStream' for that.
sdlConvertAudioSamples :: SDLAudioSpec -> ByteString -> SDLAudioSpec -> IO (Maybe ByteString)
sdlConvertAudioSamples srcSpec srcData dstSpec =
  bracket (new $ toCAudioSpec srcSpec) free $ \srcSpecPtr ->
  unsafeUseAsCStringLen srcData $ \(srcPtr, srcLen) ->
  bracket (new $ toCAudioSpec dstSpec) free $ \dstSpecPtr ->
  bracket (malloc :: IO (Ptr (Ptr Word8))) free $ \dstPtr ->
  bracket (malloc :: IO (Ptr CInt)) free $ \dstLenPtr -> do
    success <- c_sdlConvertAudioSamples srcSpecPtr (castPtr srcPtr) (fromIntegral srcLen) dstSpecPtr dstPtr dstLenPtr
    if not (fromCBool success)
      then return Nothing
      else do
        dstBuf <- peek dstPtr
        dstLen <- fromIntegral <$> peek dstLenPtr
        bs <- BS.packCStringLen (castPtr dstBuf, dstLen)
        free dstBuf  -- Caller must free the buffer
        return $ Just bs
  where
    toCAudioSpec spec = SDLAudioSpec (audioFormat spec) (audioChannels spec) (audioFreq spec)

-- | Get the human-readable name of an audio format.
--
-- Returns "SDL_AUDIO_UNKNOWN" if the format is unrecognized.
sdlGetAudioFormatName :: SDLAudioFormat -> IO String
sdlGetAudioFormatName format = peekCString =<< c_sdlGetAudioFormatName format

-- | Get the value to use with memset to silence an audio buffer in a specific format.
sdlGetSilenceValueForFormat :: SDLAudioFormat -> IO Int
sdlGetSilenceValueForFormat format = fromIntegral <$> c_sdlGetSilenceValueForFormat format

-- Helper to convert CBool to Bool
fromCBool :: CBool -> Bool
fromCBool 0 = False
fromCBool _ = True
