{-# LANGUAGE NoMonomorphismRestriction #-}

module SDL3.Wrapped.Audio
  ( module SDL3.Raw.Audio
  , sdlGetNumAudioDrivers
  , sdlGetAudioDriver
  , sdlGetCurrentAudioDriver
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
  , sdlOpenAudioDeviceStream
  , sdlPauseAudioStreamDevice
  , sdlResumeAudioStreamDevice
  , sdlAudioStreamDevicePaused
  , sdlSetAudioPostmixCallback
  , sdlLoadWAV_IO
  , sdlLoadWAV
  , sdlMixAudio
  , sdlConvertAudioSamples
  , sdlGetAudioFormatName
  , sdlGetSilenceValueForFormat
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Foreign.Ptr (FunPtr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import System.IO.Unsafe (unsafePerformIO)
import SDL3.Raw.Audio hiding
  ( sdlAudioDevicePaused
  , sdlAudioStreamDevicePaused
  , sdlBindAudioStream
  , sdlBindAudioStreams
  , sdlClearAudioStream
  , sdlCloseAudioDevice
  , sdlConvertAudioSamples
  , sdlCreateAudioStream
  , sdlDestroyAudioStream
  , sdlFlushAudioStream
  , sdlGetAudioDeviceChannelMap
  , sdlGetAudioDeviceFormat
  , sdlGetAudioDeviceGain
  , sdlGetAudioDeviceName
  , sdlGetAudioDriver
  , sdlGetAudioFormatName
  , sdlGetAudioPlaybackDevices
  , sdlGetAudioRecordingDevices
  , sdlGetAudioStreamAvailable
  , sdlGetAudioStreamData
  , sdlGetAudioStreamDevice
  , sdlGetAudioStreamFormat
  , sdlGetAudioStreamFrequencyRatio
  , sdlGetAudioStreamGain
  , sdlGetAudioStreamInputChannelMap
  , sdlGetAudioStreamOutputChannelMap
  , sdlGetAudioStreamProperties
  , sdlGetAudioStreamQueued
  , sdlGetCurrentAudioDriver
  , sdlGetNumAudioDrivers
  , sdlGetSilenceValueForFormat
  , sdlIsAudioDevicePhysical
  , sdlIsAudioDevicePlayback
  , sdlLoadWAV
  , sdlLoadWAV_IO
  , sdlLockAudioStream
  , sdlMixAudio
  , sdlOpenAudioDevice
  , sdlOpenAudioDeviceStream
  , sdlPauseAudioDevice
  , sdlPauseAudioStreamDevice
  , sdlPutAudioStreamData
  , sdlResumeAudioDevice
  , sdlResumeAudioStreamDevice
  , sdlSetAudioDeviceGain
  , sdlSetAudioPostmixCallback
  , sdlSetAudioStreamFormat
  , sdlSetAudioStreamFrequencyRatio
  , sdlSetAudioStreamGain
  , sdlSetAudioStreamInputChannelMap
  , sdlSetAudioStreamOutputChannelMap
  , sdlUnbindAudioStream
  , sdlUnbindAudioStreams
  , sdlUnlockAudioStream
  )
import qualified SDL3.Raw.Audio as Raw

streamCallbackRefs :: IORef [(SDLAudioStream, FunPtr Raw.SDLAudioStreamCallback)]
streamCallbackRefs = unsafePerformIO (newIORef [])
{-# NOINLINE streamCallbackRefs #-}

postmixCallbackRefs :: IORef [(SDLAudioDeviceID, FunPtr Raw.SDLAudioPostmixCallback)]
postmixCallbackRefs = unsafePerformIO (newIORef [])
{-# NOINLINE postmixCallbackRefs #-}

replaceStreamCallback :: SDLAudioStream -> Maybe (FunPtr Raw.SDLAudioStreamCallback) -> IO [FunPtr Raw.SDLAudioStreamCallback]
replaceStreamCallback stream newCallback =
  atomicModifyIORef' streamCallbackRefs $ \entries ->
    let (matches, rest) = foldr partition ([], []) entries
        updated = maybe rest (\callbackPtr -> (stream, callbackPtr) : rest) newCallback
    in (updated, map snd matches)
  where
    partition pair@(currentStream, _) (hits, misses)
      | currentStream == stream = (pair : hits, misses)
      | otherwise = (hits, pair : misses)

replacePostmixCallback :: SDLAudioDeviceID -> Maybe (FunPtr Raw.SDLAudioPostmixCallback) -> IO [FunPtr Raw.SDLAudioPostmixCallback]
replacePostmixCallback devid newCallback =
  atomicModifyIORef' postmixCallbackRefs $ \entries ->
    let (matches, rest) = foldr partition ([], []) entries
        updated = maybe rest (\callbackPtr -> (devid, callbackPtr) : rest) newCallback
    in (updated, map snd matches)
  where
    partition pair@(currentDevice, _) (hits, misses)
      | currentDevice == devid = (pair : hits, misses)
      | otherwise = (hits, pair : misses)

releaseCallbacks :: [FunPtr a] -> IO ()
releaseCallbacks = mapM_ freeHaskellFunPtr

sdlGetNumAudioDrivers = liftIO Raw.sdlGetNumAudioDrivers
sdlGetAudioDriver = liftIO . Raw.sdlGetAudioDriver
sdlGetCurrentAudioDriver = liftIO Raw.sdlGetCurrentAudioDriver
sdlGetAudioPlaybackDevices = liftIO Raw.sdlGetAudioPlaybackDevices
sdlGetAudioRecordingDevices = liftIO Raw.sdlGetAudioRecordingDevices
sdlGetAudioDeviceName = liftIO . Raw.sdlGetAudioDeviceName
sdlGetAudioDeviceFormat = liftIO . Raw.sdlGetAudioDeviceFormat
sdlGetAudioDeviceChannelMap = liftIO . Raw.sdlGetAudioDeviceChannelMap
sdlOpenAudioDevice devid maybeSpec = liftIO $ Raw.sdlOpenAudioDevice devid maybeSpec
sdlIsAudioDevicePhysical = liftIO . Raw.sdlIsAudioDevicePhysical
sdlIsAudioDevicePlayback = liftIO . Raw.sdlIsAudioDevicePlayback
sdlPauseAudioDevice = liftIO . Raw.sdlPauseAudioDevice
sdlResumeAudioDevice = liftIO . Raw.sdlResumeAudioDevice
sdlAudioDevicePaused = liftIO . Raw.sdlAudioDevicePaused
sdlGetAudioDeviceGain = liftIO . Raw.sdlGetAudioDeviceGain
sdlSetAudioDeviceGain devid gain = liftIO $ Raw.sdlSetAudioDeviceGain devid gain
sdlCloseAudioDevice devid =
  liftIO $ do
    Raw.sdlCloseAudioDevice devid
    oldCallbacks <- replacePostmixCallback devid Nothing
    releaseCallbacks oldCallbacks
sdlBindAudioStreams devid streams = liftIO $ Raw.sdlBindAudioStreams devid streams
sdlBindAudioStream devid stream = liftIO $ Raw.sdlBindAudioStream devid stream
sdlUnbindAudioStreams = liftIO . Raw.sdlUnbindAudioStreams
sdlUnbindAudioStream = liftIO . Raw.sdlUnbindAudioStream
sdlGetAudioStreamDevice = liftIO . Raw.sdlGetAudioStreamDevice
sdlCreateAudioStream srcSpec dstSpec = liftIO $ Raw.sdlCreateAudioStream srcSpec dstSpec
sdlGetAudioStreamProperties = liftIO . Raw.sdlGetAudioStreamProperties
sdlGetAudioStreamFormat = liftIO . Raw.sdlGetAudioStreamFormat
sdlSetAudioStreamFormat stream maybeSrc maybeDst = liftIO $ Raw.sdlSetAudioStreamFormat stream maybeSrc maybeDst
sdlGetAudioStreamFrequencyRatio = liftIO . Raw.sdlGetAudioStreamFrequencyRatio
sdlSetAudioStreamFrequencyRatio stream ratio = liftIO $ Raw.sdlSetAudioStreamFrequencyRatio stream ratio
sdlGetAudioStreamGain = liftIO . Raw.sdlGetAudioStreamGain
sdlSetAudioStreamGain stream gain = liftIO $ Raw.sdlSetAudioStreamGain stream gain
sdlGetAudioStreamInputChannelMap = liftIO . Raw.sdlGetAudioStreamInputChannelMap
sdlGetAudioStreamOutputChannelMap = liftIO . Raw.sdlGetAudioStreamOutputChannelMap
sdlSetAudioStreamInputChannelMap stream maybeChmap = liftIO $ Raw.sdlSetAudioStreamInputChannelMap stream maybeChmap
sdlSetAudioStreamOutputChannelMap stream maybeChmap = liftIO $ Raw.sdlSetAudioStreamOutputChannelMap stream maybeChmap
sdlPutAudioStreamData stream bs = liftIO $ Raw.sdlPutAudioStreamData stream bs
sdlGetAudioStreamData stream maxLen = liftIO $ Raw.sdlGetAudioStreamData stream maxLen
sdlGetAudioStreamAvailable = liftIO . Raw.sdlGetAudioStreamAvailable
sdlGetAudioStreamQueued = liftIO . Raw.sdlGetAudioStreamQueued
sdlFlushAudioStream = liftIO . Raw.sdlFlushAudioStream
sdlClearAudioStream = liftIO . Raw.sdlClearAudioStream
sdlLockAudioStream = liftIO . Raw.sdlLockAudioStream
sdlUnlockAudioStream = liftIO . Raw.sdlUnlockAudioStream
sdlDestroyAudioStream stream =
  liftIO $ do
    Raw.sdlDestroyAudioStream stream
    oldCallbacks <- replaceStreamCallback stream Nothing
    releaseCallbacks oldCallbacks
sdlOpenAudioDeviceStream devid maybeSpec maybeCallback =
  liftIO $ do
    (callbackPtr, userdata) <- case maybeCallback of
      Nothing -> pure (nullFunPtr, nullPtr)
      Just (callback, ud) -> do
        callbackPtr <- Raw.wrapAudioStreamCallback callback
        pure (callbackPtr, ud)
    result <- Raw.sdlOpenAudioDeviceStream devid maybeSpec callbackPtr userdata
    case result of
      Nothing -> do
        if callbackPtr == nullFunPtr
          then pure ()
          else freeHaskellFunPtr callbackPtr
        pure Nothing
      Just stream -> do
        if callbackPtr == nullFunPtr
          then pure ()
          else do
            oldCallbacks <- replaceStreamCallback stream (Just callbackPtr)
            releaseCallbacks oldCallbacks
        pure (Just stream)
sdlPauseAudioStreamDevice = liftIO . Raw.sdlPauseAudioStreamDevice
sdlResumeAudioStreamDevice = liftIO . Raw.sdlResumeAudioStreamDevice
sdlAudioStreamDevicePaused = liftIO . Raw.sdlAudioStreamDevicePaused
sdlSetAudioPostmixCallback devid Nothing =
  liftIO $ do
    success <- Raw.sdlSetAudioPostmixCallback devid nullFunPtr nullPtr
    if success
      then do
        oldCallbacks <- replacePostmixCallback devid Nothing
        releaseCallbacks oldCallbacks
        pure True
      else pure False
sdlSetAudioPostmixCallback devid (Just (callback, userdata)) =
  liftIO $ do
    callbackPtr <- Raw.wrapAudioPostmixCallback callback
    success <- Raw.sdlSetAudioPostmixCallback devid callbackPtr userdata
    if success
      then do
        oldCallbacks <- replacePostmixCallback devid (Just callbackPtr)
        releaseCallbacks oldCallbacks
        pure True
      else do
        freeHaskellFunPtr callbackPtr
        pure False
sdlLoadWAV_IO stream closeIo = liftIO $ Raw.sdlLoadWAV_IO stream closeIo
sdlLoadWAV = liftIO . Raw.sdlLoadWAV
sdlMixAudio dst src format volume = liftIO $ Raw.sdlMixAudio dst src format volume
sdlConvertAudioSamples srcSpec srcData dstSpec = liftIO $ Raw.sdlConvertAudioSamples srcSpec srcData dstSpec
sdlGetAudioFormatName = liftIO . Raw.sdlGetAudioFormatName
sdlGetSilenceValueForFormat = liftIO . Raw.sdlGetSilenceValueForFormat
