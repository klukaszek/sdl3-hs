{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : SDL.IOStream
Description : I/O stream functionality for SDL
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

SDL provides an abstract interface for reading and writing data streams. It offers 
implementations for files, memory, etc, and the app can provide their own implementations, too.

SDL_IOStream is not related to the standard C++ iostream class, other than both are 
abstract interfaces to read/write data.
-}

module SDL.IOStream
  ( -- * I/O Stream Types
    SDLIOStream(..)
  , SDLIOStatus(..)
  , SDLIOWhence(..)
  , SDLIOStreamInterface(..)
    
    -- * I/O Stream Creation
  , sdlIOFromFile
  , sdlIOFromMem
  , sdlIOFromConstMem
  , sdlIOFromDynamicMem
  , sdlOpenIO
    
    -- * I/O Stream Operations
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
    
    -- * File Loading/Saving
  , sdlLoadFile_IO
  , sdlLoadFile
  , sdlSaveFile_IO
  , sdlSaveFile
    
    -- * Endian-Aware I/O Functions
    -- ** Reading
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
    
    -- ** Writing
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
    
    -- * Property Constants
  , sdlPropIOStreamWindowsHandle
  , sdlPropIOStreamStdioFile
  , sdlPropIOStreamFileDescriptor
  , sdlPropIOStreamAndroidAAsset
  , sdlPropIOStreamMemoryPointer
  , sdlPropIOStreamMemorySize
  , sdlPropIOStreamDynamicMemory
  , sdlPropIOStreamDynamicChunksize
  ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Data.Word
import Data.Int
import SDL.Properties (SDLPropertiesID(..))
import SDL.Stdinc (SDLCall)

-- | SDL_IOStream status, set by a read or write operation
data SDLIOStatus
  = SDLIOStatusReady     -- ^ Everything is ready (no errors and not EOF)
  | SDLIOStatusError     -- ^ Read or write I/O error
  | SDLIOStatusEOF       -- ^ End of file
  | SDLIOStatusNotReady  -- ^ Non blocking I/O, not ready
  | SDLIOStatusReadonly  -- ^ Tried to write a read-only buffer
  | SDLIOStatusWriteonly -- ^ Tried to read a write-only buffer
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Convert SDLIOStatus to CInt for FFI
ioStatusToCInt :: SDLIOStatus -> CInt
ioStatusToCInt SDLIOStatusReady     = 0
ioStatusToCInt SDLIOStatusError     = 1
ioStatusToCInt SDLIOStatusEOF       = 2
ioStatusToCInt SDLIOStatusNotReady  = 3
ioStatusToCInt SDLIOStatusReadonly  = 4
ioStatusToCInt SDLIOStatusWriteonly = 5

-- | Convert CInt to SDLIOStatus for FFI
cIntToIOStatus :: CInt -> SDLIOStatus
cIntToIOStatus 0 = SDLIOStatusReady
cIntToIOStatus 1 = SDLIOStatusError
cIntToIOStatus 2 = SDLIOStatusEOF
cIntToIOStatus 3 = SDLIOStatusNotReady
cIntToIOStatus 4 = SDLIOStatusReadonly
cIntToIOStatus 5 = SDLIOStatusWriteonly
cIntToIOStatus _ = SDLIOStatusError  -- Default case for unexpected values

-- | Possible 'whence' values for SDL_IOStream seeking
data SDLIOWhence
  = SDLIOSeekSet  -- ^ Seek from the beginning of data
  | SDLIOSeekCur  -- ^ Seek relative to current read point
  | SDLIOSeekEnd  -- ^ Seek relative to the end of data
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Convert SDLIOWhence to CInt for FFI
ioWhenceToCInt :: SDLIOWhence -> CInt
ioWhenceToCInt SDLIOSeekSet = 0
ioWhenceToCInt SDLIOSeekCur = 1
ioWhenceToCInt SDLIOSeekEnd = 2

-- | The function pointers that drive an SDL_IOStream
data SDLIOStreamInterface = SDLIOStreamInterface
  { sdlIOStreamVersion :: Word32
  , sdlIOStreamSize :: FunPtr (Ptr () -> IO Int64)
  , sdlIOStreamSeek :: FunPtr (Ptr () -> Int64 -> CInt -> IO Int64)
  , sdlIOStreamRead :: FunPtr (Ptr () -> Ptr () -> Word32 -> Ptr CInt -> IO Word32)
  , sdlIOStreamWrite :: FunPtr (Ptr () -> Ptr () -> Word32 -> Ptr CInt -> IO Word32)
  , sdlIOStreamFlush :: FunPtr (Ptr () -> Ptr CInt -> IO Bool)
  , sdlIOStreamClose :: FunPtr (Ptr () -> IO Bool)
  }

-- | The read/write operation structure
newtype SDLIOStream = SDLIOStream (Ptr SDLIOStream)
  deriving (Eq, Show)

-- | Property constants
sdlPropIOStreamWindowsHandle :: String
sdlPropIOStreamWindowsHandle = "SDL.iostream.windows.handle"

sdlPropIOStreamStdioFile :: String
sdlPropIOStreamStdioFile = "SDL.iostream.stdio.file"

sdlPropIOStreamFileDescriptor :: String
sdlPropIOStreamFileDescriptor = "SDL.iostream.file_descriptor"

sdlPropIOStreamAndroidAAsset :: String
sdlPropIOStreamAndroidAAsset = "SDL.iostream.android.aasset"

sdlPropIOStreamMemoryPointer :: String
sdlPropIOStreamMemoryPointer = "SDL.iostream.memory.base"

sdlPropIOStreamMemorySize :: String
sdlPropIOStreamMemorySize = "SDL.iostream.memory.size"

sdlPropIOStreamDynamicMemory :: String
sdlPropIOStreamDynamicMemory = "SDL.iostream.dynamic.memory"

sdlPropIOStreamDynamicChunksize :: String
sdlPropIOStreamDynamicChunksize = "SDL.iostream.dynamic.chunksize"

-- | Create a new SDL_IOStream structure for reading from and/or writing to a named file
--
-- @since 3.2.0
foreign import ccall "SDL_IOFromFile" 
  sdlIOFromFileC :: CString -> CString -> IO (Ptr SDLIOStream)

-- | Create a new SDL_IOStream structure for reading from and/or writing to a named file
--
-- @since 3.2.0
sdlIOFromFile :: String -> String -> IO (Maybe (Ptr SDLIOStream))
sdlIOFromFile file mode =
  withCString file $ \cfile ->
  withCString mode $ \cmode -> do
    streamPtr <- sdlIOFromFileC cfile cmode
    if streamPtr == nullPtr
      then return Nothing
      else return (Just streamPtr)

-- | Prepare a read-write memory buffer for use with SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_IOFromMem" 
  sdlIOFromMem :: Ptr a -> CSize -> IO (Ptr SDLIOStream)

-- | Prepare a read-only memory buffer for use with SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_IOFromConstMem" 
  sdlIOFromConstMem :: Ptr a -> CSize -> IO (Ptr SDLIOStream)

-- | Create an SDL_IOStream that is backed by dynamically allocated memory
--
-- @since 3.2.0
foreign import ccall "SDL_IOFromDynamicMem" 
  sdlIOFromDynamicMem :: IO (Ptr SDLIOStream)

-- | Create a custom SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_OpenIO" 
  sdlOpenIO :: Ptr SDLIOStreamInterface -> Ptr a -> IO (Ptr SDLIOStream)

-- | Close and free an allocated SDL_IOStream structure
--
-- @since 3.2.0
foreign import ccall "SDL_CloseIO" 
  sdlCloseIO :: Ptr SDLIOStream -> IO Bool

-- | Get the properties associated with an SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_GetIOProperties" 
  sdlGetIOProperties :: Ptr SDLIOStream -> IO SDLPropertiesID
     
-- | Query the stream status of an SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_GetIOStatus" 
  sdlGetIOStatusRaw :: Ptr SDLIOStream -> IO CInt

-- | Query the stream status of an SDL_IOStream
-- 
-- @since 3.2.0
sdlGetIOStatus :: Ptr SDLIOStream -> IO SDLIOStatus
sdlGetIOStatus stream = do
  statusInt <- sdlGetIOStatusRaw stream
  return $ cIntToIOStatus statusInt

-- | Get the size of the data stream in an SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_GetIOSize" 
  sdlGetIOSize :: Ptr SDLIOStream -> IO Int64

-- | Seek within an SDL_IOStream data stream
--
-- @since 3.2.0
foreign import ccall "SDL_SeekIO" 
  sdlSeekIORaw :: Ptr SDLIOStream -> Int64 -> CInt -> IO Int64

-- | Seek within an SDL_IOStream data stream
--
-- @since 3.2.0
sdlSeekIO :: Ptr SDLIOStream -> Int64 -> SDLIOWhence -> IO Int64
sdlSeekIO stream offset whence = sdlSeekIORaw stream offset (ioWhenceToCInt whence)

-- | Determine the current read/write offset in an SDL_IOStream data stream
--
-- @since 3.2.0
foreign import ccall "SDL_TellIO" 
  sdlTellIO :: Ptr SDLIOStream -> IO Int64

-- | Read from a data source
--
-- @since 3.2.0
foreign import ccall "SDL_ReadIO" 
  sdlReadIO :: Ptr SDLIOStream -> Ptr a -> CSize -> IO CSize

-- | Write to an SDL_IOStream data stream
--
-- @since 3.2.0
foreign import ccall "SDL_WriteIO" 
  sdlWriteIO :: Ptr SDLIOStream -> Ptr a -> CSize -> IO CSize

-- | Print to an SDL_IOStream data stream
--
-- This is just a stub. In Haskell, you would typically use sdlWriteIO instead.
-- If you need formatted output, you would format the string in Haskell and then write it.
sdlIOprintf :: Ptr SDLIOStream -> String -> IO CSize
sdlIOprintf stream format = do
  formattedStr <- return format  -- In a real application, you would format the string here
  withCString formattedStr $ \cstr -> do
    let len = length formattedStr
    sdlWriteIO stream (castPtr cstr) (fromIntegral len)

-- | Print to an SDL_IOStream data stream with va_list
--
-- This is just a stub. In Haskell, you would typically use sdlWriteIO instead.
sdlIOvprintf :: Ptr SDLIOStream -> String -> IO CSize
sdlIOvprintf = sdlIOprintf

-- | Flush any buffered data in the stream
--
-- @since 3.2.0
foreign import ccall "SDL_FlushIO" 
  sdlFlushIO :: Ptr SDLIOStream -> IO Bool

-- | Load all the data from an SDL data stream
--
-- @since 3.2.0
foreign import ccall "SDL_LoadFile_IO" 
  sdlLoadFile_IO :: Ptr SDLIOStream -> Ptr CSize -> Bool -> IO (Ptr ())

-- | Load all the data from a file path
--
-- @since 3.2.0
foreign import ccall "SDL_LoadFile" 
  sdlLoadFileC :: CString -> Ptr CSize -> IO (Ptr ())

-- | Load all the data from a file path
--
-- @since 3.2.0
sdlLoadFile :: String -> IO (Maybe (Ptr (), Word32))
sdlLoadFile file =
  alloca $ \(sizePtr :: Ptr CSize) ->    -- Allocate space for the size output parameter
  withCString file $ \cfile -> do       -- Convert FilePath to CString
    -- Call the raw C function, casting result to Ptr () for clarity
    dataPtr <- castPtr <$> sdlLoadFileC cfile sizePtr
    if dataPtr == nullPtr               -- Check for NULL *before* peeking size
      then return Nothing               -- Return Nothing on failure
      else do
        actualSize <- peek sizePtr        -- Peek the size *only if* dataPtr is valid
        return (Just (dataPtr, fromIntegral actualSize)) -- Return the Ptr () and the size

-- | Save all the data into an SDL data stream
--
-- @since 3.2.0
foreign import ccall "SDL_SaveFile_IO" 
  sdlSaveFile_IO :: Ptr SDLIOStream -> Ptr a -> CSize -> Bool -> IO Bool

-- | Save all the data into a file path
--
-- @since 3.2.0
foreign import ccall "SDL_SaveFile" 
  sdlSaveFileC :: CString -> Ptr a -> CSize -> IO Bool

-- | Save all the data into a file path
--
-- @since 3.2.0
sdlSaveFile :: String -> Ptr a -> CSize -> IO Bool
sdlSaveFile file data' datasize = 
  withCString file $ \cfile ->
    sdlSaveFileC cfile data' datasize

-- | Read a byte from an SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_ReadU8" 
  sdlReadU8 :: Ptr SDLIOStream -> Ptr Word8 -> IO Bool

-- | Read a signed byte from an SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_ReadS8" 
  sdlReadS8 :: Ptr SDLIOStream -> Ptr Int8 -> IO Bool

-- | Read 16 bits of little-endian data from an SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_ReadU16LE" 
  sdlReadU16LE :: Ptr SDLIOStream -> Ptr Word16 -> IO Bool

-- | Read 16 bits of little-endian data from an SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_ReadS16LE" 
  sdlReadS16LE :: Ptr SDLIOStream -> Ptr Int16 -> IO Bool

-- | Read 16 bits of big-endian data from an SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_ReadU16BE" 
  sdlReadU16BE :: Ptr SDLIOStream -> Ptr Word16 -> IO Bool

-- | Read 16 bits of big-endian data from an SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_ReadS16BE" 
  sdlReadS16BE :: Ptr SDLIOStream -> Ptr Int16 -> IO Bool

-- | Read 32 bits of little-endian data from an SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_ReadU32LE" 
  sdlReadU32LE :: Ptr SDLIOStream -> Ptr Word32 -> IO Bool

-- | Read 32 bits of little-endian data from an SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_ReadS32LE" 
  sdlReadS32LE :: Ptr SDLIOStream -> Ptr Int32 -> IO Bool

-- | Read 32 bits of big-endian data from an SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_ReadU32BE" 
  sdlReadU32BE :: Ptr SDLIOStream -> Ptr Word32 -> IO Bool

-- | Read 32 bits of big-endian data from an SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_ReadS32BE" 
  sdlReadS32BE :: Ptr SDLIOStream -> Ptr Int32 -> IO Bool

-- | Read 64 bits of little-endian data from an SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_ReadU64LE" 
  sdlReadU64LE :: Ptr SDLIOStream -> Ptr Word64 -> IO Bool

-- | Read 64 bits of little-endian data from an SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_ReadS64LE" 
  sdlReadS64LE :: Ptr SDLIOStream -> Ptr Int64 -> IO Bool

-- | Read 64 bits of big-endian data from an SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_ReadU64BE" 
  sdlReadU64BE :: Ptr SDLIOStream -> Ptr Word64 -> IO Bool

-- | Read 64 bits of big-endian data from an SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_ReadS64BE" 
  sdlReadS64BE :: Ptr SDLIOStream -> Ptr Int64 -> IO Bool

-- | Write a byte to an SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_WriteU8" 
  sdlWriteU8 :: Ptr SDLIOStream -> Word8 -> IO Bool

-- | Write a signed byte to an SDL_IOStream
--
-- @since 3.2.0
foreign import ccall "SDL_WriteS8" 
  sdlWriteS8 :: Ptr SDLIOStream -> Int8 -> IO Bool

-- | Write 16 bits in native format to an SDL_IOStream as little-endian data
--
-- @since 3.2.0
foreign import ccall "SDL_WriteU16LE" 
  sdlWriteU16LE :: Ptr SDLIOStream -> Word16 -> IO Bool

-- | Write 16 bits in native format to an SDL_IOStream as little-endian data
--
-- @since 3.2.0
foreign import ccall "SDL_WriteS16LE" 
  sdlWriteS16LE :: Ptr SDLIOStream -> Int16 -> IO Bool

-- | Write 16 bits in native format to an SDL_IOStream as big-endian data
--
-- @since 3.2.0
foreign import ccall "SDL_WriteU16BE" 
  sdlWriteU16BE :: Ptr SDLIOStream -> Word16 -> IO Bool

-- | Write 16 bits in native format to an SDL_IOStream as big-endian data
--
-- @since 3.2.0
foreign import ccall "SDL_WriteS16BE" 
  sdlWriteS16BE :: Ptr SDLIOStream -> Int16 -> IO Bool

-- | Write 32 bits in native format to an SDL_IOStream as little-endian data
--
-- @since 3.2.0
foreign import ccall "SDL_WriteU32LE" 
  sdlWriteU32LE :: Ptr SDLIOStream -> Word32 -> IO Bool

-- | Write 32 bits in native format to an SDL_IOStream as little-endian data
--
-- @since 3.2.0
foreign import ccall "SDL_WriteS32LE" 
  sdlWriteS32LE :: Ptr SDLIOStream -> Int32 -> IO Bool

-- | Write 32 bits in native format to an SDL_IOStream as big-endian data
--
-- @since 3.2.0
foreign import ccall "SDL_WriteU32BE" 
  sdlWriteU32BE :: Ptr SDLIOStream -> Word32 -> IO Bool

-- | Write 32 bits in native format to an SDL_IOStream as big-endian data
--
-- @since 3.2.0
foreign import ccall "SDL_WriteS32BE" 
  sdlWriteS32BE :: Ptr SDLIOStream -> Int32 -> IO Bool

-- | Write 64 bits in native format to an SDL_IOStream as little-endian data
--
-- @since 3.2.0
foreign import ccall "SDL_WriteU64LE" 
  sdlWriteU64LE :: Ptr SDLIOStream -> Word64 -> IO Bool

-- | Write 64 bits in native format to an SDL_IOStream as little-endian data
--
-- @since 3.2.0
foreign import ccall "SDL_WriteS64LE" 
  sdlWriteS64LE :: Ptr SDLIOStream -> Int64 -> IO Bool

-- | Write 64 bits in native format to an SDL_IOStream as big-endian data
--
-- @since 3.2.0
foreign import ccall "SDL_WriteU64BE" 
  sdlWriteU64BE :: Ptr SDLIOStream -> Word64 -> IO Bool

-- | Write 64 bits in native format to an SDL_IOStream as big-endian data
--
-- @since 3.2.0
foreign import ccall "SDL_WriteS64BE" 
  sdlWriteS64BE :: Ptr SDLIOStream -> Int64 -> IO Bool
