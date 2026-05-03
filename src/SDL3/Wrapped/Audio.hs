{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SDL3.Wrapped.Audio
  ( SDLAudioFormatUnknown(..)
  , SDLAudioFormatU8(..)
  , SDLAudioFormatS8(..)
  , SDLAudioFormatS16LE(..)
  , SDLAudioFormatS16BE(..)
  , SDLAudioFormatS32LE(..)
  , SDLAudioFormatS32BE(..)
  , SDLAudioFormatF32LE(..)
  , SDLAudioFormatF32BE(..)
  , SDLAudioFormatS16(..)
  , SDLAudioFormatS32(..)
  , SDLAudioFormatF32(..)
  , SDLAudioSpec(..)
  , SomeSDLAudioSpec(..)
  , toSomeSDLAudioSpec
  , sdlAudioSpec
  , SDLAudioStream
  , SDLAudioDeviceID
  , sdlPutAudioStreamDataBytes
  , sdlGetAudioStreamDataBytes
  , sdlDefaultDevicePlayback
  , sdlDefaultDeviceRecording
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
  , SDLAudioStreamPut(..)
  , SDLAudioStreamGet(..)
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

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Int
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Proxy
import Data.Traversable
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Void
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)
import SDL3.Wrapped.IOStream (SDLIOStream)
import qualified SDL3.Raw.Audio as Raw


--------------------------------
-- Audio Format
--------------------------------

data SDLAudioFormatUnknown = SDLAudioFormatUnknown
data SDLAudioFormatU8 = SDLAudioFormatU8
data SDLAudioFormatS8 = SDLAudioFormatS8
data SDLAudioFormatS16LE = SDLAudioFormatS16LE
data SDLAudioFormatS16BE = SDLAudioFormatS16BE
data SDLAudioFormatS32LE = SDLAudioFormatS32LE
data SDLAudioFormatS32BE = SDLAudioFormatS32BE
data SDLAudioFormatF32LE = SDLAudioFormatF32LE
data SDLAudioFormatF32BE = SDLAudioFormatF32BE
data SDLAudioFormatS16 = SDLAudioFormatS16
data SDLAudioFormatS32 = SDLAudioFormatS32
data SDLAudioFormatF32 = SDLAudioFormatF32


class SDLAudioFormat s where
  type SampleFor s
  getSample :: Proxy s -> B.Get (SampleFor s)
  putSample :: Proxy s -> SampleFor s -> B.Put
  sdlAudioFormatFor :: s -> Raw.SDLAudioFormat

instance SDLAudioFormat SDLAudioFormatUnknown where
  type SampleFor SDLAudioFormatUnknown = Void
  getSample _ = fail "Cannot read unknown sample type"
  putSample _ = absurd
  sdlAudioFormatFor = const Raw.SDL_AUDIO_UNKNOWN

instance SDLAudioFormat SDLAudioFormatU8 where
  type SampleFor SDLAudioFormatU8 = Word8
  getSample _ = B.getWord8
  putSample _ = B.putWord8
  sdlAudioFormatFor = const Raw.SDL_AUDIO_U8

instance SDLAudioFormat SDLAudioFormatS8 where
  type SampleFor SDLAudioFormatS8 = Int8
  getSample _ = B.getInt8
  putSample _ = B.putInt8
  sdlAudioFormatFor = const Raw.SDL_AUDIO_S8

instance SDLAudioFormat SDLAudioFormatS16LE where
  type SampleFor SDLAudioFormatS16LE = Int16
  getSample _ = B.getInt16le
  putSample _ = B.putInt16le
  sdlAudioFormatFor = const Raw.SDL_AUDIO_S16LE

instance SDLAudioFormat SDLAudioFormatS16BE where
  type SampleFor SDLAudioFormatS16BE = Int16
  getSample _ = B.getInt16be
  putSample _ = B.putInt16be
  sdlAudioFormatFor = const Raw.SDL_AUDIO_S16BE

instance SDLAudioFormat SDLAudioFormatS32LE where
  type SampleFor SDLAudioFormatS32LE = Int32
  getSample _ = B.getInt32le
  putSample _ = B.putInt32le
  sdlAudioFormatFor = const Raw.SDL_AUDIO_S32LE

instance SDLAudioFormat SDLAudioFormatS32BE where
  type SampleFor SDLAudioFormatS32BE = Int32
  getSample _ = B.getInt32be
  putSample _ = B.putInt32be
  sdlAudioFormatFor = const Raw.SDL_AUDIO_S32BE

instance SDLAudioFormat SDLAudioFormatF32LE where
  type SampleFor SDLAudioFormatF32LE = Float
  getSample _ = B.getFloatle
  putSample _ = B.putFloatle
  sdlAudioFormatFor = const Raw.SDL_AUDIO_F32LE

instance SDLAudioFormat SDLAudioFormatF32BE where
  type SampleFor SDLAudioFormatF32BE = Float
  getSample _ = B.getFloatbe
  putSample _ = B.putFloatbe
  sdlAudioFormatFor = const Raw.SDL_AUDIO_F32BE

instance SDLAudioFormat SDLAudioFormatS16 where
  type SampleFor SDLAudioFormatS16 = Int16
  getSample _ = B.getInt16host
  putSample _ = B.putInt16host
  sdlAudioFormatFor = const Raw.SDL_AUDIO_S16

instance SDLAudioFormat SDLAudioFormatS32 where
  type SampleFor SDLAudioFormatS32 = Int32
  getSample _ = B.getInt32host
  putSample _ = B.putInt32host
  sdlAudioFormatFor = const Raw.SDL_AUDIO_S32

instance SDLAudioFormat SDLAudioFormatF32 where
  type SampleFor SDLAudioFormatF32 = Float
  getSample _ = B.getFloathost
  putSample _ = B.putFloathost
  sdlAudioFormatFor = const Raw.SDL_AUDIO_F32


-- Two typeclasses for defining whether a format is Endian or Native
-- to use when implementing the Get and Set typeclasses below

class Endian format where {}
class Native format where {}

-- No instance for SDLAudioFormatUnknown
instance Native SDLAudioFormatU8 where {}
instance Native SDLAudioFormatS8 where {}
instance Endian SDLAudioFormatS16LE where {}
instance Endian SDLAudioFormatS16BE where {}
instance Endian SDLAudioFormatS32LE where {}
instance Endian SDLAudioFormatS32BE where {}
instance Endian SDLAudioFormatF32LE where {}
instance Endian SDLAudioFormatF32BE where {}
instance Native SDLAudioFormatS16 where {}
instance Native SDLAudioFormatS32 where {}
instance Native SDLAudioFormatF32 where {}


-- | Existential wrapper for 'SDLAudioSpec' when the format is only known at runtime.
data SomeSDLAudioSpec where
  SomeSDLAudioSpec :: SDLAudioSpec f -> SomeSDLAudioSpec

deriving instance Show SomeSDLAudioSpec

-- | Convert a raw 'SDLAudioSpec' into an existentially-quantified wrapped spec
-- based on its runtime audio format.
toSomeSDLAudioSpec :: Raw.SDLAudioSpec -> SomeSDLAudioSpec
toSomeSDLAudioSpec raw =
  case Raw.audioFormat raw of
    f | f == Raw.SDL_AUDIO_U8    -> SomeSDLAudioSpec (SDLAudioSpec raw :: SDLAudioSpec SDLAudioFormatU8)
      | f == Raw.SDL_AUDIO_S8    -> SomeSDLAudioSpec (SDLAudioSpec raw :: SDLAudioSpec SDLAudioFormatS8)
      | f == Raw.SDL_AUDIO_S16LE -> SomeSDLAudioSpec (SDLAudioSpec raw :: SDLAudioSpec SDLAudioFormatS16LE)
      | f == Raw.SDL_AUDIO_S16BE -> SomeSDLAudioSpec (SDLAudioSpec raw :: SDLAudioSpec SDLAudioFormatS16BE)
      | f == Raw.SDL_AUDIO_S32LE -> SomeSDLAudioSpec (SDLAudioSpec raw :: SDLAudioSpec SDLAudioFormatS32LE)
      | f == Raw.SDL_AUDIO_S32BE -> SomeSDLAudioSpec (SDLAudioSpec raw :: SDLAudioSpec SDLAudioFormatS32BE)
      | f == Raw.SDL_AUDIO_F32LE -> SomeSDLAudioSpec (SDLAudioSpec raw :: SDLAudioSpec SDLAudioFormatF32LE)
      | f == Raw.SDL_AUDIO_F32BE -> SomeSDLAudioSpec (SDLAudioSpec raw :: SDLAudioSpec SDLAudioFormatF32BE)
      | f == Raw.SDL_AUDIO_S16   -> SomeSDLAudioSpec (SDLAudioSpec raw :: SDLAudioSpec SDLAudioFormatS16)
      | f == Raw.SDL_AUDIO_S32   -> SomeSDLAudioSpec (SDLAudioSpec raw :: SDLAudioSpec SDLAudioFormatS32)
      | f == Raw.SDL_AUDIO_F32   -> SomeSDLAudioSpec (SDLAudioSpec raw :: SDLAudioSpec SDLAudioFormatF32)
      | otherwise                -> SomeSDLAudioSpec (SDLAudioSpec raw :: SDLAudioSpec SDLAudioFormatUnknown)


-- For the typeclasses below, we can split the implementations into two sets,
-- `Endian` and `Native` (by adding constraints on the instances), because
-- some `Native` versions could be done more efficiently than the `Endian`.
-- For some we can generalize to common typeclass constraints, e.g. `Traversable`.


-- +-----------------------------------------+
-- |          GET IMPLEMENTATIONS (so far)   |
-- +-----------------------+--------+--------+
-- | CONTAINER             | Endian | Native |
-- +-----------------------+--------+--------+
-- | Data.Array            |        |        |
-- +-----------------------+--------+--------+
-- | Data.List             |    X   |    X   | -- `Applicative`, `Monoid`, `Traversable`
-- +-----------------------+--------+--------+
-- | Vector                |    X   |    X   | -- `Applicative`, `Monoid`, `Traversable`
-- +-----------------------+--------+--------+
-- | Vector.Storable       |        |    X   |
-- +-----------------------+--------+--------+
-- | Vector.Strict         |    X   |    X   | -- `Applicative`, `Monoid`, `Traversable`
-- +-----------------------+--------+--------+
-- | Vector.Unboxed        |        |        |
-- +-----------------------+--------+--------+

class (SDLAudioFormat getFormat) => SDLAudioStreamGet container getFormat where
  sdlGetAudioStreamData
    :: forall m putFormat . (MonadIO m)
    => SDLAudioStream getFormat putFormat -> Int
    -> m (Maybe (container (SampleFor getFormat)))

instance
  -- samples
  ( SDLAudioFormat getFormat
  , Storable (SampleFor getFormat)
  -- container
  , Applicative container
  , Monoid (container (B.Get (SampleFor getFormat)))
  , Traversable container
  ) => SDLAudioStreamGet container getFormat where
  sdlGetAudioStreamData (SDLAudioStream stream) maxLen = liftIO do
    streamData <- Raw.sdlGetAudioStreamData stream maxLen
    forM streamData \(bytes, bytesRead) -> do
      let proxy = Proxy :: Proxy getFormat
      let numSamples = bytesRead `div` sizeOf (undefined :: SampleFor getFormat)
      -- this implementation could be more efficient for Data.Vector(.Strict)
      let get = sequence . mconcat $ replicate numSamples (pure (getSample proxy))
      return . B.runGet get $ BS.fromStrict bytes

-- TODO Endian/Native Data.Array???

-- TODO Endian Vector.Storable

instance (Native getFormat, SDLAudioFormat getFormat, Storable (SampleFor getFormat))
  => SDLAudioStreamGet VS.Vector getFormat where
  sdlGetAudioStreamData (SDLAudioStream stream) maxLen = liftIO do
    let maxSamples = maxLen `div` sizeOf (undefined :: SampleFor getFormat)
    mvec <- VSM.new maxSamples
    VSM.unsafeWith mvec $ \ptr -> do
      bytesRead <- Raw.sdlGetAudioStreamDataPtr stream (castPtr ptr) maxLen
      if bytesRead < 0
        then return Nothing
        else do
          let samplesRead = bytesRead `div` sizeOf (undefined :: SampleFor getFormat)
          vec <- VS.unsafeFreeze (VSM.slice 0 samplesRead mvec)
          return (Just vec)

-- TODO Endian Vector.Unboxed
-- TODO Native Vector.Unboxed


-- +-----------------------------------------+
-- |          PUT IMPLEMENTATIONS (so far)   |
-- +-----------------------+--------+--------+
-- | CONTAINER             | Endian | Native |
-- +-----------------------+--------+--------+
-- | Data.Array            |    X   |    X   | -- `Traversable` when `Ix i` 
-- +-----------------------+--------+--------+
-- | Data.List             |    X   |    X   | -- `Traversable`
-- +-----------------------+--------+--------+
-- | Vector                |    X   |    X   | -- `Traversable`
-- +-----------------------+--------+--------+
-- | Vector.Storable       |        |    X   |
-- +-----------------------+--------+--------+
-- | Vector.Strict         |    X   |    X   | -- `Traversable`
-- +-----------------------+--------+--------+
-- | Vector.Unboxed        |        |        |
-- +-----------------------+--------+--------+

class (SDLAudioFormat putFormat) => SDLAudioStreamPut container putFormat where
  sdlPutAudioStreamData
    :: forall m getFormat . (MonadIO m)
    => SDLAudioStream getFormat putFormat -> container (SampleFor putFormat)
    -> m Bool

instance (SDLAudioFormat putFormat, Traversable container)
  => SDLAudioStreamPut container putFormat where
  sdlPutAudioStreamData (SDLAudioStream stream) vec = liftIO
    . Raw.sdlPutAudioStreamData stream . BS.toStrict . B.runPut . void
    $ traverse (putSample (Proxy :: Proxy putFormat)) vec

-- TODO Endian Vector.Storable

instance (Native putFormat, SDLAudioFormat putFormat, Storable (SampleFor putFormat))
  => SDLAudioStreamPut VS.Vector putFormat where
  sdlPutAudioStreamData (SDLAudioStream stream) vec = liftIO do
    let numBytes = VS.length vec * sizeOf (undefined :: SampleFor putFormat)
    VS.unsafeWith vec \audioPtr -> do
      Raw.sdlPutAudioStreamDataPtr stream (castPtr audioPtr) numBytes

-- TODO Endian Vector.Unboxed
-- TODO Native Vector.Unboxed


-- | Escape-hatch for putting raw 'ByteString' data into a stream. Useful when
-- the data originates from runtime-loaded sources (e.g. 'sdlLoadWAV') where the
-- phantom type can't easily be matched.
sdlPutAudioStreamDataBytes :: MonadIO m => SDLAudioStream getFormat putFormat -> BS.ByteString -> m Bool
sdlPutAudioStreamDataBytes (SDLAudioStream stream) bs = liftIO $ Raw.sdlPutAudioStreamData stream bs

-- | Escape-hatch for reading raw 'ByteString' data from a stream, returning the
-- number of bytes read alongside the data.
sdlGetAudioStreamDataBytes :: MonadIO m => SDLAudioStream getFormat putFormat -> Int -> m (Maybe (BS.ByteString, Int))
sdlGetAudioStreamDataBytes (SDLAudioStream stream) maxLen = liftIO $ Raw.sdlGetAudioStreamData stream maxLen


--------------------------------
-- Audio Spec, Stream, Device
--------------------------------

newtype SDLAudioSpec format = SDLAudioSpec { toRawAudioSpec :: Raw.SDLAudioSpec }
  deriving (Eq, Show)

-- TODO `newtype` these?
type ChannelCount = Int
type SampleRate = Int

sdlAudioSpec :: (SDLAudioFormat a) => a -> ChannelCount -> SampleRate -> SDLAudioSpec a
sdlAudioSpec format channelCount sampleRate =
  SDLAudioSpec $ Raw.SDLAudioSpec (sdlAudioFormatFor format) channelCount sampleRate


newtype SDLAudioStream getFormat putFormat = SDLAudioStream { toRawAudioStream :: Raw.SDLAudioStream }
  deriving (Eq, Show)


type SDLAudioDeviceID = Raw.SDLAudioDeviceID

sdlDefaultDevicePlayback :: SDLAudioDeviceID
sdlDefaultDevicePlayback = Raw.SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK

sdlDefaultDeviceRecording :: SDLAudioDeviceID
sdlDefaultDeviceRecording = Raw.SDL_AUDIO_DEVICE_DEFAULT_RECORDING


-- Callback types

type SDLAudioStreamCallback getFormat putFormat =
  Ptr () -> SDLAudioStream getFormat putFormat -> Int -> Int -> IO ()

makeRawStreamCallback
  :: SDLAudioStreamCallback getFormat putFormat -> Raw.SDLAudioStreamCallback
makeRawStreamCallback callback = \userData stream additionalAmount totalAmount ->
  callback userData (SDLAudioStream stream)
    (fromIntegral additionalAmount) (fromIntegral totalAmount)


-- | Callback for reading or manipulating data just before it's written to a device.
--
-- This callback is always given data in the SDLAudioFormatF32 format.
-- The mutable vector argument should never be grown or shrunken.
type SDLAudioPostmixCallback =
  Ptr () -> SDLAudioSpec SDLAudioFormatF32 -> VSM.IOVector Float -> IO ()


wrapAudioStreamCallback
  :: (MonadIO m)
  => SDLAudioStreamCallback getFormat putFormat
  -> m (FunPtr (SDLAudioStreamCallback getFormat putFormat))
wrapAudioStreamCallback callback = liftIO $
  castFunPtr <$> Raw.wrapAudioStreamCallback (makeRawStreamCallback callback)

wrapAudioPostmixCallback
  :: (MonadIO m)
  => SDLAudioPostmixCallback
  -> m (FunPtr SDLAudioPostmixCallback)
wrapAudioPostmixCallback callback = liftIO $
  castFunPtr <$> Raw.wrapAudioPostmixCallback \userData specPtr bufPtr buflen -> do
    spec <- SDLAudioSpec <$> peek specPtr
    -- cast from CFloat to Float. Easier here than after creating the vector
    bufForeignPtr <- newForeignPtr_ (castPtr bufPtr :: Ptr Float)
    let len = (fromIntegral buflen `div` sizeOf (0::Float))
    let buffer = VSM.unsafeFromForeignPtr0 bufForeignPtr len
    callback userData spec buffer


--------------------------------


streamCallbackRefs :: IORef [(Raw.SDLAudioStream, FunPtr Raw.SDLAudioStreamCallback)]
streamCallbackRefs = unsafePerformIO (newIORef [])
{-# NOINLINE streamCallbackRefs #-}

streamPutCallbackRefs :: IORef [(Raw.SDLAudioStream, FunPtr Raw.SDLAudioStreamCallback)]
streamPutCallbackRefs = unsafePerformIO (newIORef [])
{-# NOINLINE streamPutCallbackRefs #-}

postmixCallbackRefs :: IORef [(SDLAudioDeviceID, FunPtr SDLAudioPostmixCallback)]
postmixCallbackRefs = unsafePerformIO (newIORef [])
{-# NOINLINE postmixCallbackRefs #-}

replaceStreamCallback
  :: (MonadIO m)
  => Raw.SDLAudioStream
  -> Maybe (FunPtr Raw.SDLAudioStreamCallback)
  -> m [FunPtr Raw.SDLAudioStreamCallback]
replaceStreamCallback stream newCallback = liftIO $
  atomicModifyIORef' streamCallbackRefs $ \entries ->
    let (matches, rest) = foldr partition ([], []) entries
        updated = maybe rest (\callbackPtr -> (stream, callbackPtr) : rest) newCallback
    in (updated, map snd matches)
  where
    partition pair@(currentStream, _) (hits, misses)
      | currentStream == stream = (pair : hits, misses)
      | otherwise = (hits, pair : misses)

replaceStreamPutCallback
  :: (MonadIO m)
  => Raw.SDLAudioStream
  -> Maybe (FunPtr Raw.SDLAudioStreamCallback)
  -> m [FunPtr Raw.SDLAudioStreamCallback]
replaceStreamPutCallback stream newCallback = liftIO $
  atomicModifyIORef' streamPutCallbackRefs $ \entries ->
    let (matches, rest) = foldr partition ([], []) entries
        updated = maybe rest (\callbackPtr -> (stream, callbackPtr) : rest) newCallback
    in (updated, map snd matches)
  where
    partition pair@(currentStream, _) (hits, misses)
      | currentStream == stream = (pair : hits, misses)
      | otherwise = (hits, pair : misses)

replacePostmixCallback
  :: (MonadIO m)
  => SDLAudioDeviceID -> Maybe (FunPtr SDLAudioPostmixCallback)
  -> m [FunPtr SDLAudioPostmixCallback]
replacePostmixCallback devid newCallback = liftIO $
  atomicModifyIORef' postmixCallbackRefs $ \entries ->
    let (matches, rest) = foldr partition ([], []) entries
        updated = maybe rest (\callbackPtr -> (devid, callbackPtr) : rest) newCallback
    in (updated, map snd matches)
  where
    partition pair@(currentDevice, _) (hits, misses)
      | currentDevice == devid = (pair : hits, misses)
      | otherwise = (hits, pair : misses)

releaseCallbacks :: (MonadIO m) => [FunPtr a] -> m ()
releaseCallbacks = liftIO . mapM_ freeHaskellFunPtr

sdlGetNumAudioDrivers = liftIO Raw.sdlGetNumAudioDrivers
sdlGetAudioDriver = liftIO . Raw.sdlGetAudioDriver
sdlGetCurrentAudioDriver = liftIO Raw.sdlGetCurrentAudioDriver

sdlGetAudioPlaybackDevices = liftIO Raw.sdlGetAudioPlaybackDevices
sdlGetAudioRecordingDevices = liftIO Raw.sdlGetAudioRecordingDevices
sdlGetAudioDeviceName = liftIO . Raw.sdlGetAudioDeviceName
sdlGetAudioDeviceFormat :: MonadIO m => SDLAudioDeviceID -> m (Maybe (SomeSDLAudioSpec, Maybe Int))
sdlGetAudioDeviceFormat devid = liftIO $ do
  result <- Raw.sdlGetAudioDeviceFormat devid
  pure $ case result of
    Nothing -> Nothing
    Just (rawSpec, mFrames) -> Just (toSomeSDLAudioSpec rawSpec, mFrames)

sdlGetAudioDeviceChannelMap = liftIO . Raw.sdlGetAudioDeviceChannelMap
sdlOpenAudioDevice :: MonadIO m => SDLAudioDeviceID -> Maybe (SDLAudioSpec format) -> m (Maybe SDLAudioDeviceID)
sdlOpenAudioDevice devid maybeSpec = liftIO $ Raw.sdlOpenAudioDevice devid (toRawAudioSpec <$> maybeSpec)
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

sdlBindAudioStreams devid = liftIO . Raw.sdlBindAudioStreams devid . map toRawAudioStream
sdlBindAudioStream devid = liftIO . Raw.sdlBindAudioStream devid . toRawAudioStream
sdlUnbindAudioStreams = liftIO . Raw.sdlUnbindAudioStreams . map toRawAudioStream
sdlUnbindAudioStream = liftIO . Raw.sdlUnbindAudioStream . toRawAudioStream
sdlGetAudioStreamDevice = liftIO . Raw.sdlGetAudioStreamDevice . toRawAudioStream

sdlCreateAudioStream
  :: (MonadIO m)
  => SDLAudioSpec getFormat -> SDLAudioSpec putFormat
  -> m (Maybe (SDLAudioStream getFormat putFormat))
sdlCreateAudioStream srcSpec dstSpec = liftIO $
  fmap SDLAudioStream <$>
  Raw.sdlCreateAudioStream (toRawAudioSpec srcSpec) (toRawAudioSpec dstSpec)

sdlGetAudioStreamProperties = liftIO . Raw.sdlGetAudioStreamProperties . toRawAudioStream

sdlGetAudioStreamFormat
  :: (MonadIO m)
  => SDLAudioStream getFormat putFormat
  -> m (Maybe (SDLAudioSpec getFormat, SDLAudioSpec putFormat))
sdlGetAudioStreamFormat = liftIO
  . fmap (fmap \(g, p) -> (SDLAudioSpec g, SDLAudioSpec p))
  . Raw.sdlGetAudioStreamFormat . toRawAudioStream

sdlSetAudioStreamFormat
  :: (MonadIO m)
  => SDLAudioStream getFormat putFormat
  -> Maybe (SDLAudioSpec getFormat)
  -> Maybe (SDLAudioSpec putFormat)
  -> m Bool
sdlSetAudioStreamFormat stream maybeSrc maybeDst = liftIO $
  Raw.sdlSetAudioStreamFormat
    (toRawAudioStream stream)
    (toRawAudioSpec <$> maybeSrc)
    (toRawAudioSpec <$> maybeDst)

sdlGetAudioStreamFrequencyRatio = liftIO . Raw.sdlGetAudioStreamFrequencyRatio . toRawAudioStream
sdlSetAudioStreamFrequencyRatio stream ratio = liftIO $ Raw.sdlSetAudioStreamFrequencyRatio (toRawAudioStream stream) ratio
sdlGetAudioStreamGain = liftIO . Raw.sdlGetAudioStreamGain . toRawAudioStream
sdlSetAudioStreamGain stream gain = liftIO $ Raw.sdlSetAudioStreamGain (toRawAudioStream stream) gain
sdlGetAudioStreamInputChannelMap = liftIO . Raw.sdlGetAudioStreamInputChannelMap . toRawAudioStream
sdlGetAudioStreamOutputChannelMap = liftIO . Raw.sdlGetAudioStreamOutputChannelMap . toRawAudioStream
sdlSetAudioStreamInputChannelMap stream maybeChmap = liftIO $ Raw.sdlSetAudioStreamInputChannelMap (toRawAudioStream stream) maybeChmap
sdlSetAudioStreamOutputChannelMap stream maybeChmap = liftIO $ Raw.sdlSetAudioStreamOutputChannelMap (toRawAudioStream stream) maybeChmap
sdlGetAudioStreamAvailable = liftIO . Raw.sdlGetAudioStreamAvailable . toRawAudioStream
sdlGetAudioStreamQueued = liftIO . Raw.sdlGetAudioStreamQueued . toRawAudioStream
sdlFlushAudioStream = liftIO . Raw.sdlFlushAudioStream . toRawAudioStream
sdlClearAudioStream = liftIO . Raw.sdlClearAudioStream . toRawAudioStream
sdlLockAudioStream = liftIO . Raw.sdlLockAudioStream . toRawAudioStream
sdlUnlockAudioStream = liftIO . Raw.sdlUnlockAudioStream . toRawAudioStream

sdlSetAudioStreamGetCallback
  :: (MonadIO m)
  => SDLAudioStream getFormat putFormat
  -> Maybe (SDLAudioStreamCallback getFormat putFormat, Ptr ())
  -> m Bool
sdlSetAudioStreamGetCallback stream maybeCallback = liftIO $ do
  let rawStream = toRawAudioStream stream
  case maybeCallback of
    Nothing -> do
      success <- Raw.sdlSetAudioStreamGetCallback rawStream Nothing
      when success $ do
        oldCallbacks <- replaceStreamCallback rawStream Nothing
        releaseCallbacks oldCallbacks
      pure success
    Just (callback, userdata) -> do
      callbackPtr <- wrapAudioStreamCallback callback
      success <- Raw.sdlSetAudioStreamGetCallback rawStream (Just (castFunPtr callbackPtr, userdata))
      if success
        then do
          oldCallbacks <- replaceStreamCallback rawStream (Just (castFunPtr callbackPtr))
          releaseCallbacks oldCallbacks
          pure True
        else do
          freeHaskellFunPtr callbackPtr
          pure False

sdlSetAudioStreamPutCallback
  :: (MonadIO m)
  => SDLAudioStream getFormat putFormat
  -> Maybe (SDLAudioStreamCallback getFormat putFormat, Ptr ())
  -> m Bool
sdlSetAudioStreamPutCallback stream maybeCallback = liftIO $ do
  let rawStream = toRawAudioStream stream
  case maybeCallback of
    Nothing -> do
      success <- Raw.sdlSetAudioStreamPutCallback rawStream Nothing
      when success $ do
        oldCallbacks <- replaceStreamPutCallback rawStream Nothing
        releaseCallbacks oldCallbacks
      pure success
    Just (callback, userdata) -> do
      callbackPtr <- wrapAudioStreamCallback callback
      success <- Raw.sdlSetAudioStreamPutCallback rawStream (Just (castFunPtr callbackPtr, userdata))
      if success
        then do
          oldCallbacks <- replaceStreamPutCallback rawStream (Just (castFunPtr callbackPtr))
          releaseCallbacks oldCallbacks
          pure True
        else do
          freeHaskellFunPtr callbackPtr
          pure False

sdlDestroyAudioStream :: (MonadIO m) => SDLAudioStream getFormat putFormat -> m ()
sdlDestroyAudioStream stream = liftIO do
  let rawStream = toRawAudioStream stream
  Raw.sdlDestroyAudioStream rawStream
  oldGetCallbacks <- replaceStreamCallback rawStream Nothing
  oldPutCallbacks <- replaceStreamPutCallback rawStream Nothing
  releaseCallbacks (oldGetCallbacks ++ oldPutCallbacks)

sdlOpenAudioDeviceStream
  :: (MonadIO m)
  => SDLAudioDeviceID
  -> Maybe (SDLAudioSpec format)
  -> Maybe (SDLAudioStreamCallback format format, Ptr ())
  -> m (Maybe (SDLAudioStream format format))
sdlOpenAudioDeviceStream devid maybeSpec maybeCallback = liftIO do
  (callbackPtr, userdata) <-
    case maybeCallback of
      Nothing -> pure (nullFunPtr, nullPtr)
      Just (callback, ud) -> do
        callbackPtr <- wrapAudioStreamCallback callback
        pure (callbackPtr, ud)
  result <- Raw.sdlOpenAudioDeviceStream devid
    (toRawAudioSpec <$> maybeSpec) (castFunPtr callbackPtr) userdata
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
          oldCallbacks <- replaceStreamCallback stream (Just (castFunPtr callbackPtr))
          releaseCallbacks oldCallbacks
      pure (Just (SDLAudioStream stream))

sdlPauseAudioStreamDevice :: MonadIO m => SDLAudioStream getFormat putFormat -> m Bool
sdlPauseAudioStreamDevice = liftIO . Raw.sdlPauseAudioStreamDevice . toRawAudioStream

sdlResumeAudioStreamDevice :: MonadIO m => SDLAudioStream getFormat putFormat -> m Bool
sdlResumeAudioStreamDevice = liftIO . Raw.sdlResumeAudioStreamDevice . toRawAudioStream

sdlAudioStreamDevicePaused :: MonadIO m => SDLAudioStream getFormat putFormat -> m Bool
sdlAudioStreamDevicePaused = liftIO . Raw.sdlAudioStreamDevicePaused . toRawAudioStream

sdlSetAudioPostmixCallback
  :: (MonadIO m)
  => SDLAudioDeviceID -> Maybe (SDLAudioPostmixCallback, Ptr ()) -> m Bool
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
    callbackPtr <- wrapAudioPostmixCallback callback
    success <- Raw.sdlSetAudioPostmixCallback devid (castFunPtr callbackPtr) userdata
    if success
      then do
        oldCallbacks <- replacePostmixCallback devid (Just callbackPtr)
        releaseCallbacks oldCallbacks
        pure True
      else do
        freeHaskellFunPtr callbackPtr
        pure False

sdlLoadWAV_IO :: MonadIO m => SDLIOStream -> Bool -> m (Maybe (SomeSDLAudioSpec, BS.ByteString))
sdlLoadWAV_IO stream closeIo = liftIO $ fmap (\(spec, bs) -> (toSomeSDLAudioSpec spec, bs)) <$> Raw.sdlLoadWAV_IO stream closeIo

sdlLoadWAV :: MonadIO m => FilePath -> m (Maybe (SomeSDLAudioSpec, BS.ByteString))
sdlLoadWAV = liftIO . fmap (fmap (\(spec, bs) -> (toSomeSDLAudioSpec spec, bs))) . Raw.sdlLoadWAV

sdlMixAudio :: forall m format. (MonadIO m, SDLAudioFormat format, Storable (SampleFor format)) => VSM.IOVector (SampleFor format) -> VS.Vector (SampleFor format) -> format -> Float -> m Bool
sdlMixAudio dst src fmt volume = liftIO $
  VSM.unsafeWith dst $ \dstPtr ->
  VS.unsafeWith src $ \srcPtr ->
    if VSM.length dst /= VS.length src
      then return False
      else Raw.sdlMixAudio (castPtr dstPtr) (castPtr srcPtr) (VS.length src * sizeOf (undefined :: SampleFor format)) (sdlAudioFormatFor fmt) volume

sdlConvertAudioSamples :: MonadIO m => SomeSDLAudioSpec -> BS.ByteString -> SomeSDLAudioSpec -> m (Maybe BS.ByteString)
sdlConvertAudioSamples (SomeSDLAudioSpec srcSpec) srcData (SomeSDLAudioSpec dstSpec) =
  liftIO $ Raw.sdlConvertAudioSamples (toRawAudioSpec srcSpec) srcData (toRawAudioSpec dstSpec)

sdlGetAudioFormatName :: (MonadIO m, SDLAudioFormat format) => format -> m String
sdlGetAudioFormatName fmt = liftIO $ Raw.sdlGetAudioFormatName (sdlAudioFormatFor fmt)

sdlGetSilenceValueForFormat :: (MonadIO m, SDLAudioFormat format) => format -> m Int
sdlGetSilenceValueForFormat fmt = liftIO $ Raw.sdlGetSilenceValueForFormat (sdlAudioFormatFor fmt)
