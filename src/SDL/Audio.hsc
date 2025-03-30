{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ForeignFunctionInterface #-}

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

module SDL.Audio
  ( -- * Types
    SDLAudioFormat(..)
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
import Data.Word
import Data.Int
import SDL.IOStream
import SDL.Properties

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
  peek ptr = SDLAudioSpec
    <$> #{peek SDL_AudioSpec, format} ptr
    <*> (#{peek SDL_AudioSpec, channels} ptr)
    <*> (#{peek SDL_AudioSpec, freq} ptr)
  poke ptr spec = do
    #{poke SDL_AudioSpec, format} ptr (audioFormat spec)
    #{poke SDL_AudioSpec, channels} ptr (audioChannels spec)
    #{poke SDL_AudioSpec, freq} ptr (audioFreq spec)

-- Audio format constants
pattern SDL_AUDIO_MASK_BITSIZE :: Word16
pattern SDL_AUDIO_MASK_BITSIZE = #{const SDL_AUDIO_MASK_BITSIZE}

pattern SDL_AUDIO_MASK_FLOAT :: Word16
pattern SDL_AUDIO_MASK_FLOAT = #{const SDL_AUDIO_MASK_FLOAT}

pattern SDL_AUDIO_MASK_BIG_ENDIAN :: Word16
pattern SDL_AUDIO_MASK_BIG_ENDIAN = #{const SDL_AUDIO_MASK_BIG_ENDIAN}

pattern SDL_AUDIO_MASK_SIGNED :: Word16
pattern SDL_AUDIO_MASK_SIGNED = #{const SDL_AUDIO_MASK_SIGNED}

pattern SDL_AUDIO_UNKNOWN :: Word16
pattern SDL_AUDIO_UNKNOWN = #{const SDL_AUDIO_UNKNOWN}

pattern SDL_AUDIO_U8 :: Word16
pattern SDL_AUDIO_U8 = #{const SDL_AUDIO_U8}

pattern SDL_AUDIO_S8 :: Word16
pattern SDL_AUDIO_S8 = #{const SDL_AUDIO_S8}

pattern SDL_AUDIO_S16LE :: Word16
pattern SDL_AUDIO_S16LE = #{const SDL_AUDIO_S16LE}

pattern SDL_AUDIO_S16BE :: Word16
pattern SDL_AUDIO_S16BE = #{const SDL_AUDIO_S16BE}

pattern SDL_AUDIO_S32LE :: Word16
pattern SDL_AUDIO_S32LE = #{const SDL_AUDIO_S32LE}

pattern SDL_AUDIO_S32BE :: Word16
pattern SDL_AUDIO_S32BE = #{const SDL_AUDIO_S32BE}

pattern SDL_AUDIO_F32LE :: Word16
pattern SDL_AUDIO_F32LE = #{const SDL_AUDIO_F32LE}

pattern SDL_AUDIO_F32BE :: Word16
pattern SDL_AUDIO_F32BE = #{const SDL_AUDIO_F32BE}

pattern SDL_AUDIO_S16 :: Word16
pattern SDL_AUDIO_S16 = #{const SDL_AUDIO_S16}

pattern SDL_AUDIO_S32 :: Word16
pattern SDL_AUDIO_S32 = #{const SDL_AUDIO_S32}

pattern SDL_AUDIO_F32 :: Word16
pattern SDL_AUDIO_F32 = #{const SDL_AUDIO_F32}

-- Default device constants
pattern SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK :: SDLAudioDeviceID
pattern SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK = #{const SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK}

pattern SDL_AUDIO_DEVICE_DEFAULT_RECORDING :: SDLAudioDeviceID
pattern SDL_AUDIO_DEVICE_DEFAULT_RECORDING = #{const SDL_AUDIO_DEVICE_DEFAULT_RECORDING}

-- Callback types
type SDLAudioStreamCallback = Ptr () -> Ptr SDLAudioStream -> CInt -> CInt -> IO ()
type SDLAudioPostmixCallback = Ptr () -> Ptr SDLAudioSpec -> Ptr CFloat -> CInt -> IO ()

-- Foreign imports for audio driver functions
foreign import ccall unsafe "SDL_GetNumAudioDrivers"
  sdlGetNumAudioDrivers :: IO CInt

foreign import ccall unsafe "SDL_GetAudioDriver"
  sdlGetAudioDriver :: CInt -> IO CString

foreign import ccall unsafe "SDL_GetCurrentAudioDriver"
  sdlGetCurrentAudioDriver :: IO CString

-- Foreign imports for audio device functions
foreign import ccall unsafe "SDL_GetAudioPlaybackDevices"
  sdlGetAudioPlaybackDevicesRaw :: Ptr CInt -> IO (Ptr SDLAudioDeviceID)

sdlGetAudioPlaybackDevices :: IO [SDLAudioDeviceID]
sdlGetAudioPlaybackDevices = alloca $ \countPtr -> do
  devicesPtr <- sdlGetAudioPlaybackDevicesRaw countPtr
  count <- peek countPtr
  if devicesPtr == nullPtr
    then return []
    else do
      devices <- peekArray (fromIntegral count) devicesPtr
      free devicesPtr
      return devices

foreign import ccall unsafe "SDL_GetAudioRecordingDevices"
  sdlGetAudioRecordingDevicesRaw :: Ptr CInt -> IO (Ptr SDLAudioDeviceID)

sdlGetAudioRecordingDevices :: IO [SDLAudioDeviceID]
sdlGetAudioRecordingDevices = alloca $ \countPtr -> do
  devicesPtr <- sdlGetAudioRecordingDevicesRaw countPtr
  count <- peek countPtr
  if devicesPtr == nullPtr
    then return []
    else do
      devices <- peekArray (fromIntegral count) devicesPtr
      free devicesPtr
      return devices

foreign import ccall unsafe "SDL_GetAudioDeviceName"
  sdlGetAudioDeviceName :: SDLAudioDeviceID -> IO CString

foreign import ccall unsafe "SDL_GetAudioDeviceFormat"
  sdlGetAudioDeviceFormat :: SDLAudioDeviceID -> Ptr SDLAudioSpec -> Ptr CInt -> IO CBool

foreign import ccall unsafe "SDL_GetAudioDeviceChannelMap"
  sdlGetAudioDeviceChannelMap :: SDLAudioDeviceID -> Ptr CInt -> IO (Ptr CInt)

foreign import ccall unsafe "SDL_OpenAudioDevice"
  sdlOpenAudioDevice :: SDLAudioDeviceID -> Ptr SDLAudioSpec -> IO SDLAudioDeviceID

foreign import ccall unsafe "SDL_IsAudioDevicePhysical"
  sdlIsAudioDevicePhysical :: SDLAudioDeviceID -> IO CBool

foreign import ccall unsafe "SDL_IsAudioDevicePlayback"
  sdlIsAudioDevicePlayback :: SDLAudioDeviceID -> IO CBool

foreign import ccall unsafe "SDL_PauseAudioDevice"
  sdlPauseAudioDevice :: SDLAudioDeviceID -> IO CBool

foreign import ccall unsafe "SDL_ResumeAudioDevice"
  sdlResumeAudioDevice :: SDLAudioDeviceID -> IO CBool

foreign import ccall unsafe "SDL_AudioDevicePaused"
  sdlAudioDevicePaused :: SDLAudioDeviceID -> IO CBool

foreign import ccall unsafe "SDL_GetAudioDeviceGain"
  sdlGetAudioDeviceGain :: SDLAudioDeviceID -> IO CFloat

foreign import ccall unsafe "SDL_SetAudioDeviceGain"
  sdlSetAudioDeviceGain :: SDLAudioDeviceID -> CFloat -> IO CBool

foreign import ccall unsafe "SDL_CloseAudioDevice"
  sdlCloseAudioDevice :: SDLAudioDeviceID -> IO ()

-- Audio Stream Functions
foreign import ccall unsafe "SDL_BindAudioStreams"
  sdlBindAudioStreamsRaw :: SDLAudioDeviceID -> Ptr (Ptr SDLAudioStream) -> CInt -> IO CBool

sdlBindAudioStreams :: SDLAudioDeviceID -> [SDLAudioStream] -> IO Bool
sdlBindAudioStreams devid streams = 
  withArrayLen (map (\(SDLAudioStream ptr) -> ptr) streams) $ \len ptr ->
    toBool <$> sdlBindAudioStreamsRaw devid ptr (fromIntegral len)

foreign import ccall unsafe "SDL_BindAudioStream"
  sdlBindAudioStreamRaw :: SDLAudioDeviceID -> Ptr SDLAudioStream -> IO CBool

sdlBindAudioStream :: SDLAudioDeviceID -> SDLAudioStream -> IO Bool
sdlBindAudioStream devid (SDLAudioStream stream) =
  toBool <$> sdlBindAudioStreamRaw devid stream

foreign import ccall unsafe "SDL_UnbindAudioStreams"
  sdlUnbindAudioStreamsRaw :: Ptr (Ptr SDLAudioStream) -> CInt -> IO ()

sdlUnbindAudioStreams :: [SDLAudioStream] -> IO ()
sdlUnbindAudioStreams streams =
  withArrayLen (map (\(SDLAudioStream ptr) -> ptr) streams) $ \len ptr ->
    sdlUnbindAudioStreamsRaw ptr (fromIntegral len)

foreign import ccall unsafe "SDL_UnbindAudioStream"
  sdlUnbindAudioStreamRaw :: Ptr SDLAudioStream -> IO ()

sdlUnbindAudioStream :: SDLAudioStream -> IO ()
sdlUnbindAudioStream (SDLAudioStream stream) = sdlUnbindAudioStreamRaw stream

foreign import ccall unsafe "SDL_GetAudioStreamDevice"
  sdlGetAudioStreamDeviceRaw :: Ptr SDLAudioStream -> IO SDLAudioDeviceID

sdlGetAudioStreamDevice :: SDLAudioStream -> IO SDLAudioDeviceID
sdlGetAudioStreamDevice (SDLAudioStream stream) = sdlGetAudioStreamDeviceRaw stream

foreign import ccall unsafe "SDL_CreateAudioStream"
  sdlCreateAudioStream :: Ptr SDLAudioSpec -> Ptr SDLAudioSpec -> IO (Ptr SDLAudioStream)

foreign import ccall unsafe "SDL_GetAudioStreamProperties"
  sdlGetAudioStreamPropertiesRaw :: Ptr SDLAudioStream -> IO SDLPropertiesID

sdlGetAudioStreamProperties :: SDLAudioStream -> IO SDLPropertiesID
sdlGetAudioStreamProperties (SDLAudioStream stream) = sdlGetAudioStreamPropertiesRaw stream

foreign import ccall unsafe "SDL_GetAudioStreamFormat"
  sdlGetAudioStreamFormatRaw :: Ptr SDLAudioStream -> Ptr SDLAudioSpec -> Ptr SDLAudioSpec -> IO CBool

sdlGetAudioStreamFormat :: SDLAudioStream -> IO (Maybe (SDLAudioSpec, SDLAudioSpec))
sdlGetAudioStreamFormat (SDLAudioStream stream) = 
  alloca $ \srcSpecPtr ->
  alloca $ \dstSpecPtr -> do
    success <- sdlGetAudioStreamFormatRaw stream srcSpecPtr dstSpecPtr
    if toBool success
      then do
        srcSpec <- peek srcSpecPtr
        dstSpec <- peek dstSpecPtr
        return $ Just (srcSpec, dstSpec)
      else return Nothing

foreign import ccall unsafe "SDL_SetAudioStreamFormat"
  sdlSetAudioStreamFormatRaw :: Ptr SDLAudioStream -> Ptr SDLAudioSpec -> Ptr SDLAudioSpec -> IO CBool

sdlSetAudioStreamFormat :: SDLAudioStream -> Maybe SDLAudioSpec -> Maybe SDLAudioSpec -> IO Bool
sdlSetAudioStreamFormat (SDLAudioStream stream) msrcSpec mdstSpec =
  maybeWith with msrcSpec $ \srcSpecPtr ->
  maybeWith with mdstSpec $ \dstSpecPtr ->
    toBool <$> sdlSetAudioStreamFormatRaw stream srcSpecPtr dstSpecPtr

foreign import ccall unsafe "SDL_GetAudioStreamFrequencyRatio"
  sdlGetAudioStreamFrequencyRatioRaw :: Ptr SDLAudioStream -> IO CFloat

sdlGetAudioStreamFrequencyRatio :: SDLAudioStream -> IO Float
sdlGetAudioStreamFrequencyRatio (SDLAudioStream stream) = 
  realToFrac <$> sdlGetAudioStreamFrequencyRatioRaw stream

foreign import ccall unsafe "SDL_SetAudioStreamFrequencyRatio"
  sdlSetAudioStreamFrequencyRatio :: Ptr SDLAudioStream -> CFloat -> IO CBool

foreign import ccall unsafe "SDL_GetAudioStreamGain"
  sdlGetAudioStreamGainRaw :: Ptr SDLAudioStream -> IO CFloat

sdlGetAudioStreamGain :: SDLAudioStream -> IO Float
sdlGetAudioStreamGain (SDLAudioStream stream) = 
  realToFrac <$> sdlGetAudioStreamGainRaw stream

foreign import ccall unsafe "SDL_SetAudioStreamGain"
  sdlSetAudioStreamGainRaw :: Ptr SDLAudioStream -> CFloat -> IO CBool

sdlSetAudioStreamGain :: SDLAudioStream -> Float -> IO Bool
sdlSetAudioStreamGain (SDLAudioStream stream) gain =
  toBool <$> sdlSetAudioStreamGainRaw stream (realToFrac gain)

-- Channel map functions
foreign import ccall unsafe "SDL_GetAudioStreamInputChannelMap"
  sdlGetAudioStreamInputChannelMapRaw :: Ptr SDLAudioStream -> Ptr CInt -> IO (Ptr CInt)

sdlGetAudioStreamInputChannelMap :: SDLAudioStream -> IO [Int]
sdlGetAudioStreamInputChannelMap (SDLAudioStream stream) = alloca $ \countPtr -> do
  mapPtr <- sdlGetAudioStreamInputChannelMapRaw (castPtr stream) countPtr
  if mapPtr == nullPtr
    then return []
    else do
      count <- peek countPtr
      map fromIntegral <$> peekArray (fromIntegral count) mapPtr

foreign import ccall unsafe "SDL_GetAudioStreamOutputChannelMap"
  sdlGetAudioStreamOutputChannelMapRaw :: Ptr SDLAudioStream -> Ptr CInt -> IO (Ptr CInt)

sdlGetAudioStreamOutputChannelMap :: SDLAudioStream -> IO [Int]
sdlGetAudioStreamOutputChannelMap (SDLAudioStream stream) = alloca $ \countPtr -> do
  mapPtr <- sdlGetAudioStreamOutputChannelMapRaw (castPtr stream) countPtr
  if mapPtr == nullPtr
    then return []
    else do
      count <- peek countPtr
      map fromIntegral <$> peekArray (fromIntegral count) mapPtr

foreign import ccall unsafe "SDL_SetAudioStreamInputChannelMap"
  sdlSetAudioStreamInputChannelMapRaw :: Ptr SDLAudioStream -> Ptr CInt -> CInt -> IO CBool

sdlSetAudioStreamInputChannelMap :: SDLAudioStream -> [Int] -> IO Bool
sdlSetAudioStreamInputChannelMap (SDLAudioStream stream) chmap =
  withArrayLen (map fromIntegral chmap) $ \len ptr ->
    toBool <$> sdlSetAudioStreamInputChannelMapRaw (castPtr stream) ptr (fromIntegral len)

foreign import ccall unsafe "SDL_SetAudioStreamOutputChannelMap"
  sdlSetAudioStreamOutputChannelMapRaw :: Ptr SDLAudioStream -> Ptr CInt -> CInt -> IO CBool

sdlSetAudioStreamOutputChannelMap :: SDLAudioStream -> [Int] -> IO Bool
sdlSetAudioStreamOutputChannelMap (SDLAudioStream stream) chmap =
  withArrayLen (map fromIntegral chmap) $ \len ptr ->
    toBool <$> sdlSetAudioStreamOutputChannelMapRaw (castPtr stream) ptr (fromIntegral len)

-- Stream data functions
foreign import ccall unsafe "SDL_PutAudioStreamData"
  sdlPutAudioStreamData :: Ptr SDLAudioStream -> Ptr () -> CInt -> IO CBool

foreign import ccall unsafe "SDL_GetAudioStreamData"
  sdlGetAudioStreamData :: Ptr SDLAudioStream -> Ptr () -> CInt -> IO CInt

foreign import ccall unsafe "SDL_GetAudioStreamAvailable"
  sdlGetAudioStreamAvailable :: Ptr SDLAudioStream -> IO CInt

foreign import ccall unsafe "SDL_GetAudioStreamQueued"
  sdlGetAudioStreamQueued :: Ptr SDLAudioStream -> IO CInt

foreign import ccall unsafe "SDL_FlushAudioStream"
  sdlFlushAudioStream :: Ptr SDLAudioStream -> IO CBool

foreign import ccall unsafe "SDL_ClearAudioStream"
  sdlClearAudioStream :: Ptr SDLAudioStream -> IO CBool

foreign import ccall unsafe "SDL_LockAudioStream"
  sdlLockAudioStream :: Ptr SDLAudioStream -> IO CBool

foreign import ccall unsafe "SDL_UnlockAudioStream"
  sdlUnlockAudioStream :: Ptr SDLAudioStream -> IO CBool

foreign import ccall unsafe "SDL_DestroyAudioStream"
  sdlDestroyAudioStream :: Ptr SDLAudioStream -> IO ()

-- Simplified audio device stream functions
foreign import ccall unsafe "SDL_OpenAudioDeviceStream"
  sdlOpenAudioDeviceStreamRaw :: SDLAudioDeviceID -> Ptr SDLAudioSpec -> FunPtr SDLAudioStreamCallback -> Ptr () -> IO (SDLAudioStream)

sdlOpenAudioDeviceStream :: SDLAudioDeviceID -> SDLAudioSpec -> Maybe SDLAudioStreamCallback -> Ptr () -> IO (Maybe SDLAudioStream)
sdlOpenAudioDeviceStream devid spec mcallback userdata = do
  callback <- maybe (return nullFunPtr) wrapAudioStreamCallback mcallback
  with spec $ \specPtr -> do
    streamPtr <- sdlOpenAudioDeviceStreamRaw devid specPtr callback userdata
    return $ Just streamPtr
    -- return $ if streamPtr == nullPtr
    --   then Nothing
    --   else Just (castPtr streamPtr)

foreign import ccall unsafe "SDL_PauseAudioStreamDevice"
  sdlPauseAudioStreamDevice :: Ptr SDLAudioStream -> IO CBool

foreign import ccall unsafe "SDL_ResumeAudioStreamDevice"
  sdlResumeAudioStreamDevice :: Ptr SDLAudioStream -> IO CBool

foreign import ccall unsafe "SDL_AudioStreamDevicePaused"
  sdlAudioStreamDevicePaused :: Ptr SDLAudioStream -> IO CBool

-- Audio postmix callback
foreign import ccall unsafe "SDL_SetAudioPostmixCallback"
  sdlSetAudioPostmixCallbackRaw :: SDLAudioDeviceID -> FunPtr SDLAudioPostmixCallback -> Ptr () -> IO CBool

sdlSetAudioPostmixCallback :: SDLAudioDeviceID -> Maybe SDLAudioPostmixCallback -> Ptr () -> IO Bool
sdlSetAudioPostmixCallback devid mcallback userdata = do
  callback <- maybe (return nullFunPtr) wrapAudioPostmixCallback mcallback
  toBool <$> sdlSetAudioPostmixCallbackRaw devid callback userdata

-- WAV loading functions
foreign import ccall unsafe "SDL_LoadWAV_IO"
  sdlLoadWAV_IORaw :: Ptr SDLIOStream -> CBool -> Ptr SDLAudioSpec -> Ptr (Ptr Word8) -> Ptr Word32 -> IO CBool

sdlLoadWAV_IO :: SDLIOStream -> Bool -> IO (Maybe (SDLAudioSpec, [Word8]))
sdlLoadWAV_IO (SDLIOStream stream) closeio =
  alloca $ \specPtr ->
  alloca $ \bufPtr ->
  alloca $ \lenPtr -> do
    success <- sdlLoadWAV_IORaw stream (fromBool closeio) specPtr bufPtr lenPtr
    if toBool success
      then do
        spec <- peek specPtr
        len <- peek lenPtr
        buf <- peek bufPtr
        audio <- peekArray (fromIntegral len) buf
        free buf
        return $ Just (spec, audio)
      else return Nothing

foreign import ccall unsafe "SDL_LoadWAV"
  sdlLoadWAVRaw :: CString -> Ptr SDLAudioSpec -> Ptr (Ptr Word8) -> Ptr Word32 -> IO CBool

sdlLoadWAV :: String -> IO (Maybe (SDLAudioSpec, [Word8]))
sdlLoadWAV path =
  withCString path $ \cpath ->
  alloca $ \specPtr ->
  alloca $ \bufPtr ->
  alloca $ \lenPtr -> do
    success <- sdlLoadWAVRaw cpath specPtr bufPtr lenPtr
    if toBool success
      then do
        spec <- peek specPtr
        len <- peek lenPtr
        buf <- peek bufPtr
        audio <- peekArray (fromIntegral len) buf
        free buf
        return $ Just (spec, audio)
      else return Nothing

-- Audio utility functions
foreign import ccall unsafe "SDL_MixAudio"
  sdlMixAudio :: Ptr Word8 -> Ptr Word8 -> SDLAudioFormat -> Word32 -> CFloat -> IO CBool

foreign import ccall unsafe "SDL_ConvertAudioSamples"
  sdlConvertAudioSamples :: Ptr SDLAudioSpec -> Ptr Word8 -> CInt -> Ptr SDLAudioSpec -> Ptr (Ptr Word8) -> Ptr CInt -> IO CBool

foreign import ccall unsafe "SDL_GetAudioFormatName"
  sdlGetAudioFormatName :: SDLAudioFormat -> IO CString

foreign import ccall unsafe "SDL_GetSilenceValueForFormat"
  sdlGetSilenceValueForFormat :: SDLAudioFormat -> IO CInt

-- Helper functions for wrapping callbacks
foreign import ccall "wrapper"
  wrapAudioStreamCallback :: SDLAudioStreamCallback -> IO (FunPtr SDLAudioStreamCallback)

foreign import ccall "wrapper"
  wrapAudioPostmixCallback :: SDLAudioPostmixCallback -> IO (FunPtr SDLAudioPostmixCallback)
