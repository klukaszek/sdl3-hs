{-# LANGUAGE ScopedTypeVariables #-}

module SDL3.Wrapped.IOStream
  ( SDLIOStream
  , SDLIOStatus(..)
  , SDLIOWhence(..)
  , SDLIOStreamInterface(..)
  , withIOStreamPtr
  , sdlUnsafeFromRawIOStream
  , sdlUnsafeToRawIOStream
  , sdlIOFromFile
  , sdlIOFromMem
  , sdlIOFromConstMem
  , sdlIOFromDynamicMem
  , sdlOpenIO
  , sdlCloseIO
  , sdlGetIOProperties
  , sdlGetIOStatus
  , sdlGetIOSize
  , sdlSeekIO
  , sdlTellIO
  , sdlReadIO
  , sdlWriteIO
  , sdlIOprintf
  , sdlIOvprintf
  , sdlFlushIO
  , sdlLoadFile_IO
  , sdlLoadFile
  , sdlSaveFile_IO
  , sdlSaveFile
  , sdlReadU8
  , sdlReadS8
  , sdlReadU16LE
  , sdlReadS16LE
  , sdlReadU16BE
  , sdlReadS16BE
  , sdlReadU32LE
  , sdlReadS32LE
  , sdlReadU32BE
  , sdlReadS32BE
  , sdlReadU64LE
  , sdlReadS64LE
  , sdlReadU64BE
  , sdlReadS64BE
  , sdlWriteU8
  , sdlWriteS8
  , sdlWriteU16LE
  , sdlWriteS16LE
  , sdlWriteU16BE
  , sdlWriteS16BE
  , sdlWriteU32LE
  , sdlWriteS32LE
  , sdlWriteU32BE
  , sdlWriteS32BE
  , sdlWriteU64LE
  , sdlWriteS64LE
  , sdlWriteU64BE
  , sdlWriteS64BE
  , sdlPropIOStreamWindowsHandle
  , sdlPropIOStreamStdioFile
  , sdlPropIOStreamFileDescriptor
  , sdlPropIOStreamAndroidAAsset
  , sdlPropIOStreamMemoryPointer
  , sdlPropIOStreamMemorySize
  , sdlPropIOStreamDynamicMemory
  , sdlPropIOStreamDynamicChunksize
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable
import SDL3.Raw.IOStream
  ( SDLIOStatus(..)
  , SDLIOStream
  , SDLIOStreamInterface(..)
  , SDLIOWhence(..)
  , sdlPropIOStreamAndroidAAsset
  , sdlPropIOStreamDynamicChunksize
  , sdlPropIOStreamDynamicMemory
  , sdlPropIOStreamFileDescriptor
  , sdlPropIOStreamMemoryPointer
  , sdlPropIOStreamMemorySize
  , sdlPropIOStreamStdioFile
  , sdlPropIOStreamWindowsHandle
  )
import SDL3.Raw.Properties (SDLPropertiesID)
import qualified SDL3.Raw.IOStream as Raw

ioWhenceToCInt :: SDLIOWhence -> CInt
ioWhenceToCInt SDLIOSeekSet = 0
ioWhenceToCInt SDLIOSeekCur = 1
ioWhenceToCInt SDLIOSeekEnd = 2

cIntToIOStatus :: CInt -> SDLIOStatus
cIntToIOStatus 0 = SDLIOStatusReady
cIntToIOStatus 1 = SDLIOStatusError
cIntToIOStatus 2 = SDLIOStatusEOF
cIntToIOStatus 3 = SDLIOStatusNotReady
cIntToIOStatus 4 = SDLIOStatusReadonly
cIntToIOStatus 5 = SDLIOStatusWriteonly
cIntToIOStatus _ = SDLIOStatusError

wrapIOStream :: Ptr Raw.SDLIOStream -> Maybe SDLIOStream
wrapIOStream = sdlUnsafeFromRawIOStream

wrapIOStreamIO :: IO (Ptr Raw.SDLIOStream) -> IO (Maybe SDLIOStream)
wrapIOStreamIO action = wrapIOStream <$> action

sdlUnsafeFromRawIOStream :: Ptr Raw.SDLIOStream -> Maybe SDLIOStream
sdlUnsafeFromRawIOStream streamPtr
  | streamPtr == nullPtr = Nothing
  | otherwise = Just (Raw.SDLIOStream streamPtr)

sdlUnsafeToRawIOStream :: SDLIOStream -> Ptr Raw.SDLIOStream
sdlUnsafeToRawIOStream (Raw.SDLIOStream streamPtr) = streamPtr

withIOStreamPtr :: MonadIO m => SDLIOStream -> (Ptr Raw.SDLIOStream -> IO a) -> m a
withIOStreamPtr stream action = liftIO $ action (sdlUnsafeToRawIOStream stream)

sdlIOFromFile :: MonadIO m => String -> String -> m (Maybe SDLIOStream)
sdlIOFromFile file mode = liftIO $
  withCString file $ \cfile ->
    withCString mode $ \cmode ->
      wrapIOStreamIO (Raw.sdlIOFromFileRaw cfile cmode)

sdlIOFromMem :: MonadIO m => Ptr a -> CSize -> m (Maybe SDLIOStream)
sdlIOFromMem a b = liftIO $ wrapIOStreamIO (Raw.sdlIOFromMemRaw a b)

sdlIOFromConstMem :: MonadIO m => Ptr a -> CSize -> m (Maybe SDLIOStream)
sdlIOFromConstMem a b = liftIO $ wrapIOStreamIO (Raw.sdlIOFromConstMemRaw a b)

sdlIOFromDynamicMem :: MonadIO m => m (Maybe SDLIOStream)
sdlIOFromDynamicMem = liftIO $ wrapIOStreamIO Raw.sdlIOFromDynamicMemRaw

sdlOpenIO :: MonadIO m => Ptr SDLIOStreamInterface -> Ptr a -> m (Maybe SDLIOStream)
sdlOpenIO a b = liftIO $ wrapIOStreamIO (Raw.sdlOpenIORaw a b)

sdlCloseIO :: MonadIO m => SDLIOStream -> m Bool
sdlCloseIO stream = liftIO $ Raw.sdlCloseIORaw (sdlUnsafeToRawIOStream stream)

sdlGetIOProperties :: MonadIO m => SDLIOStream -> m SDLPropertiesID
sdlGetIOProperties stream = liftIO $ Raw.sdlGetIOPropertiesRaw (sdlUnsafeToRawIOStream stream)

sdlGetIOStatus :: MonadIO m => SDLIOStream -> m SDLIOStatus
sdlGetIOStatus stream = liftIO $ do
  statusInt <- Raw.sdlGetIOStatusRaw (sdlUnsafeToRawIOStream stream)
  return $ cIntToIOStatus statusInt

sdlGetIOSize :: MonadIO m => SDLIOStream -> m Int64
sdlGetIOSize stream = liftIO $ Raw.sdlGetIOSizeRaw (sdlUnsafeToRawIOStream stream)

sdlSeekIO :: MonadIO m => SDLIOStream -> Int64 -> SDLIOWhence -> m Int64
sdlSeekIO stream offset whence =
  liftIO $ Raw.sdlSeekIORaw (sdlUnsafeToRawIOStream stream) offset (ioWhenceToCInt whence)

sdlTellIO :: MonadIO m => SDLIOStream -> m Int64
sdlTellIO stream = liftIO $ Raw.sdlTellIORaw (sdlUnsafeToRawIOStream stream)

sdlReadIO :: MonadIO m => SDLIOStream -> Ptr a -> CSize -> m CSize
sdlReadIO stream buffer size = liftIO $ Raw.sdlReadIORaw (sdlUnsafeToRawIOStream stream) buffer size

sdlWriteIO :: MonadIO m => SDLIOStream -> Ptr a -> CSize -> m CSize
sdlWriteIO stream buffer size = liftIO $ Raw.sdlWriteIORaw (sdlUnsafeToRawIOStream stream) buffer size

sdlIOprintf :: MonadIO m => SDLIOStream -> String -> m CSize
sdlIOprintf stream str = liftIO $
  withCString str $ \cstr ->
    Raw.sdlIOprintfRaw (sdlUnsafeToRawIOStream stream) cstr

sdlIOvprintf :: MonadIO m => SDLIOStream -> String -> Ptr () -> m CSize
sdlIOvprintf stream format vaListPtr = liftIO $
  withCString format $ \cstr ->
    Raw.sdlIOvprintfRaw (sdlUnsafeToRawIOStream stream) cstr vaListPtr

sdlFlushIO :: MonadIO m => SDLIOStream -> m Bool
sdlFlushIO stream = liftIO $ Raw.sdlFlushIORaw (sdlUnsafeToRawIOStream stream)

sdlLoadFile_IO :: MonadIO m => SDLIOStream -> Ptr CSize -> Bool -> m (Ptr ())
sdlLoadFile_IO stream sizePtr closeIo =
  liftIO $ Raw.sdlLoadFile_IORaw (sdlUnsafeToRawIOStream stream) sizePtr closeIo

sdlLoadFile :: MonadIO m => String -> m (Maybe (Ptr (), Word32))
sdlLoadFile file = liftIO $
  alloca $ \(sizePtr :: Ptr CSize) ->
    withCString file $ \cfile -> do
      dataPtr <- castPtr <$> Raw.sdlLoadFileRaw cfile sizePtr
      if dataPtr == nullPtr
        then return Nothing
        else do
          actualSize <- peek sizePtr
          return (Just (dataPtr, fromIntegral actualSize))

sdlSaveFile_IO :: MonadIO m => SDLIOStream -> Ptr a -> CSize -> Bool -> m Bool
sdlSaveFile_IO stream dataPtr dataSize closeIo =
  liftIO $ Raw.sdlSaveFile_IORaw (sdlUnsafeToRawIOStream stream) dataPtr dataSize closeIo

sdlSaveFile :: MonadIO m => String -> Ptr a -> CSize -> m Bool
sdlSaveFile file dataPtr dataSize = liftIO $
  withCString file $ \cfile ->
    Raw.sdlSaveFileRaw cfile dataPtr dataSize

sdlReadU8 :: MonadIO m => SDLIOStream -> Ptr Word8 -> m Bool
sdlReadU8 stream valuePtr = liftIO $ Raw.sdlReadU8Raw (sdlUnsafeToRawIOStream stream) valuePtr

sdlReadS8 :: MonadIO m => SDLIOStream -> Ptr Int8 -> m Bool
sdlReadS8 stream valuePtr = liftIO $ Raw.sdlReadS8Raw (sdlUnsafeToRawIOStream stream) valuePtr

sdlReadU16LE :: MonadIO m => SDLIOStream -> Ptr Word16 -> m Bool
sdlReadU16LE stream valuePtr = liftIO $ Raw.sdlReadU16LERaw (sdlUnsafeToRawIOStream stream) valuePtr

sdlReadS16LE :: MonadIO m => SDLIOStream -> Ptr Int16 -> m Bool
sdlReadS16LE stream valuePtr = liftIO $ Raw.sdlReadS16LERaw (sdlUnsafeToRawIOStream stream) valuePtr

sdlReadU16BE :: MonadIO m => SDLIOStream -> Ptr Word16 -> m Bool
sdlReadU16BE stream valuePtr = liftIO $ Raw.sdlReadU16BERaw (sdlUnsafeToRawIOStream stream) valuePtr

sdlReadS16BE :: MonadIO m => SDLIOStream -> Ptr Int16 -> m Bool
sdlReadS16BE stream valuePtr = liftIO $ Raw.sdlReadS16BERaw (sdlUnsafeToRawIOStream stream) valuePtr

sdlReadU32LE :: MonadIO m => SDLIOStream -> Ptr Word32 -> m Bool
sdlReadU32LE stream valuePtr = liftIO $ Raw.sdlReadU32LERaw (sdlUnsafeToRawIOStream stream) valuePtr

sdlReadS32LE :: MonadIO m => SDLIOStream -> Ptr Int32 -> m Bool
sdlReadS32LE stream valuePtr = liftIO $ Raw.sdlReadS32LERaw (sdlUnsafeToRawIOStream stream) valuePtr

sdlReadU32BE :: MonadIO m => SDLIOStream -> Ptr Word32 -> m Bool
sdlReadU32BE stream valuePtr = liftIO $ Raw.sdlReadU32BERaw (sdlUnsafeToRawIOStream stream) valuePtr

sdlReadS32BE :: MonadIO m => SDLIOStream -> Ptr Int32 -> m Bool
sdlReadS32BE stream valuePtr = liftIO $ Raw.sdlReadS32BERaw (sdlUnsafeToRawIOStream stream) valuePtr

sdlReadU64LE :: MonadIO m => SDLIOStream -> Ptr Word64 -> m Bool
sdlReadU64LE stream valuePtr = liftIO $ Raw.sdlReadU64LERaw (sdlUnsafeToRawIOStream stream) valuePtr

sdlReadS64LE :: MonadIO m => SDLIOStream -> Ptr Int64 -> m Bool
sdlReadS64LE stream valuePtr = liftIO $ Raw.sdlReadS64LERaw (sdlUnsafeToRawIOStream stream) valuePtr

sdlReadU64BE :: MonadIO m => SDLIOStream -> Ptr Word64 -> m Bool
sdlReadU64BE stream valuePtr = liftIO $ Raw.sdlReadU64BERaw (sdlUnsafeToRawIOStream stream) valuePtr

sdlReadS64BE :: MonadIO m => SDLIOStream -> Ptr Int64 -> m Bool
sdlReadS64BE stream valuePtr = liftIO $ Raw.sdlReadS64BERaw (sdlUnsafeToRawIOStream stream) valuePtr

sdlWriteU8 :: MonadIO m => SDLIOStream -> Word8 -> m Bool
sdlWriteU8 stream value = liftIO $ Raw.sdlWriteU8Raw (sdlUnsafeToRawIOStream stream) value

sdlWriteS8 :: MonadIO m => SDLIOStream -> Int8 -> m Bool
sdlWriteS8 stream value = liftIO $ Raw.sdlWriteS8Raw (sdlUnsafeToRawIOStream stream) value

sdlWriteU16LE :: MonadIO m => SDLIOStream -> Word16 -> m Bool
sdlWriteU16LE stream value = liftIO $ Raw.sdlWriteU16LERaw (sdlUnsafeToRawIOStream stream) value

sdlWriteS16LE :: MonadIO m => SDLIOStream -> Int16 -> m Bool
sdlWriteS16LE stream value = liftIO $ Raw.sdlWriteS16LERaw (sdlUnsafeToRawIOStream stream) value

sdlWriteU16BE :: MonadIO m => SDLIOStream -> Word16 -> m Bool
sdlWriteU16BE stream value = liftIO $ Raw.sdlWriteU16BERaw (sdlUnsafeToRawIOStream stream) value

sdlWriteS16BE :: MonadIO m => SDLIOStream -> Int16 -> m Bool
sdlWriteS16BE stream value = liftIO $ Raw.sdlWriteS16BERaw (sdlUnsafeToRawIOStream stream) value

sdlWriteU32LE :: MonadIO m => SDLIOStream -> Word32 -> m Bool
sdlWriteU32LE stream value = liftIO $ Raw.sdlWriteU32LERaw (sdlUnsafeToRawIOStream stream) value

sdlWriteS32LE :: MonadIO m => SDLIOStream -> Int32 -> m Bool
sdlWriteS32LE stream value = liftIO $ Raw.sdlWriteS32LERaw (sdlUnsafeToRawIOStream stream) value

sdlWriteU32BE :: MonadIO m => SDLIOStream -> Word32 -> m Bool
sdlWriteU32BE stream value = liftIO $ Raw.sdlWriteU32BERaw (sdlUnsafeToRawIOStream stream) value

sdlWriteS32BE :: MonadIO m => SDLIOStream -> Int32 -> m Bool
sdlWriteS32BE stream value = liftIO $ Raw.sdlWriteS32BERaw (sdlUnsafeToRawIOStream stream) value

sdlWriteU64LE :: MonadIO m => SDLIOStream -> Word64 -> m Bool
sdlWriteU64LE stream value = liftIO $ Raw.sdlWriteU64LERaw (sdlUnsafeToRawIOStream stream) value

sdlWriteS64LE :: MonadIO m => SDLIOStream -> Int64 -> m Bool
sdlWriteS64LE stream value = liftIO $ Raw.sdlWriteS64LERaw (sdlUnsafeToRawIOStream stream) value

sdlWriteU64BE :: MonadIO m => SDLIOStream -> Word64 -> m Bool
sdlWriteU64BE stream value = liftIO $ Raw.sdlWriteU64BERaw (sdlUnsafeToRawIOStream stream) value

sdlWriteS64BE :: MonadIO m => SDLIOStream -> Int64 -> m Bool
sdlWriteS64BE stream value = liftIO $ Raw.sdlWriteS64BERaw (sdlUnsafeToRawIOStream stream) value
