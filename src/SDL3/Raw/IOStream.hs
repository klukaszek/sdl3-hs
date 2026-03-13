{-# LANGUAGE ScopedTypeVariables #-}

module SDL3.Raw.IOStream
  ( SDLIOStream(..)
  , SDLIOStatus(..)
  , SDLIOWhence(..)
  , SDLIOStreamInterface(..)
  , sdlIOFromFileRaw
  , sdlIOFromMemRaw
  , sdlIOFromConstMemRaw
  , sdlIOFromDynamicMemRaw
  , sdlOpenIORaw
  , sdlCloseIORaw
  , sdlGetIOPropertiesRaw
  , sdlGetIOStatusRaw
  , sdlGetIOSizeRaw
  , sdlSeekIORaw
  , sdlTellIORaw
  , sdlReadIORaw
  , sdlWriteIORaw
  , sdlIOprintfRaw
  , sdlIOvprintfRaw
  , sdlFlushIORaw
  , sdlLoadFile_IORaw
  , sdlLoadFileRaw
  , sdlSaveFile_IORaw
  , sdlSaveFileRaw
  , sdlReadU8Raw
  , sdlReadS8Raw
  , sdlReadU16LERaw
  , sdlReadS16LERaw
  , sdlReadU16BERaw
  , sdlReadS16BERaw
  , sdlReadU32LERaw
  , sdlReadS32LERaw
  , sdlReadU32BERaw
  , sdlReadS32BERaw
  , sdlReadU64LERaw
  , sdlReadS64LERaw
  , sdlReadU64BERaw
  , sdlReadS64BERaw
  , sdlWriteU8Raw
  , sdlWriteS8Raw
  , sdlWriteU16LERaw
  , sdlWriteS16LERaw
  , sdlWriteU16BERaw
  , sdlWriteS16BERaw
  , sdlWriteU32LERaw
  , sdlWriteS32LERaw
  , sdlWriteU32BERaw
  , sdlWriteS32BERaw
  , sdlWriteU64LERaw
  , sdlWriteS64LERaw
  , sdlWriteU64BERaw
  , sdlWriteS64BERaw
  , sdlPropIOStreamWindowsHandle
  , sdlPropIOStreamStdioFile
  , sdlPropIOStreamFileDescriptor
  , sdlPropIOStreamAndroidAAsset
  , sdlPropIOStreamMemoryPointer
  , sdlPropIOStreamMemorySize
  , sdlPropIOStreamDynamicMemory
  , sdlPropIOStreamDynamicChunksize
  ) where

import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import SDL3.Raw.Properties (SDLPropertiesID)

data SDLIOStatus
  = SDLIOStatusReady
  | SDLIOStatusError
  | SDLIOStatusEOF
  | SDLIOStatusNotReady
  | SDLIOStatusReadonly
  | SDLIOStatusWriteonly
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data SDLIOWhence
  = SDLIOSeekSet
  | SDLIOSeekCur
  | SDLIOSeekEnd
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data SDLIOStreamInterface = SDLIOStreamInterface
  { sdlIOStreamVersion :: Word32
  , sdlIOStreamSize :: FunPtr (Ptr () -> IO Int64)
  , sdlIOStreamSeek :: FunPtr (Ptr () -> Int64 -> CInt -> IO Int64)
  , sdlIOStreamRead :: FunPtr (Ptr () -> Ptr () -> Word32 -> Ptr CInt -> IO Word32)
  , sdlIOStreamWrite :: FunPtr (Ptr () -> Ptr () -> Word32 -> Ptr CInt -> IO Word32)
  , sdlIOStreamFlush :: FunPtr (Ptr () -> Ptr CInt -> IO Bool)
  , sdlIOStreamClose :: FunPtr (Ptr () -> IO Bool)
  }

newtype SDLIOStream = SDLIOStream (Ptr SDLIOStream)
  deriving (Eq, Show)

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

foreign import ccall "SDL_IOFromFile"
  sdlIOFromFileRaw :: CString -> CString -> IO (Ptr SDLIOStream)

foreign import ccall "SDL_IOFromMem"
  sdlIOFromMemRaw :: Ptr a -> CSize -> IO (Ptr SDLIOStream)

foreign import ccall "SDL_IOFromConstMem"
  sdlIOFromConstMemRaw :: Ptr a -> CSize -> IO (Ptr SDLIOStream)

foreign import ccall "SDL_IOFromDynamicMem"
  sdlIOFromDynamicMemRaw :: IO (Ptr SDLIOStream)

foreign import ccall "SDL_OpenIO"
  sdlOpenIORaw :: Ptr SDLIOStreamInterface -> Ptr a -> IO (Ptr SDLIOStream)

foreign import ccall "SDL_CloseIO"
  sdlCloseIORaw :: Ptr SDLIOStream -> IO Bool

foreign import ccall "SDL_GetIOProperties"
  sdlGetIOPropertiesRaw :: Ptr SDLIOStream -> IO SDLPropertiesID

foreign import ccall "SDL_GetIOStatus"
  sdlGetIOStatusRaw :: Ptr SDLIOStream -> IO CInt

foreign import ccall "SDL_GetIOSize"
  sdlGetIOSizeRaw :: Ptr SDLIOStream -> IO Int64

foreign import ccall "SDL_SeekIO"
  sdlSeekIORaw :: Ptr SDLIOStream -> Int64 -> CInt -> IO Int64

foreign import ccall "SDL_TellIO"
  sdlTellIORaw :: Ptr SDLIOStream -> IO Int64

foreign import ccall "SDL_ReadIO"
  sdlReadIORaw :: Ptr SDLIOStream -> Ptr a -> CSize -> IO CSize

foreign import ccall "SDL_WriteIO"
  sdlWriteIORaw :: Ptr SDLIOStream -> Ptr a -> CSize -> IO CSize

foreign import ccall unsafe "wrapper_SDL_IOprintf"
  sdlIOprintfRaw :: Ptr SDLIOStream -> CString -> IO CSize

foreign import ccall unsafe "SDL_IOvprintf"
  sdlIOvprintfRaw :: Ptr SDLIOStream -> CString -> Ptr () -> IO CSize

foreign import ccall "SDL_FlushIO"
  sdlFlushIORaw :: Ptr SDLIOStream -> IO Bool

foreign import ccall "SDL_LoadFile_IO"
  sdlLoadFile_IORaw :: Ptr SDLIOStream -> Ptr CSize -> Bool -> IO (Ptr ())

foreign import ccall "SDL_LoadFile"
  sdlLoadFileRaw :: CString -> Ptr CSize -> IO (Ptr ())

foreign import ccall "SDL_SaveFile_IO"
  sdlSaveFile_IORaw :: Ptr SDLIOStream -> Ptr a -> CSize -> Bool -> IO Bool

foreign import ccall "SDL_SaveFile"
  sdlSaveFileRaw :: CString -> Ptr a -> CSize -> IO Bool

foreign import ccall "SDL_ReadU8"
  sdlReadU8Raw :: Ptr SDLIOStream -> Ptr Word8 -> IO Bool

foreign import ccall "SDL_ReadS8"
  sdlReadS8Raw :: Ptr SDLIOStream -> Ptr Int8 -> IO Bool

foreign import ccall "SDL_ReadU16LE"
  sdlReadU16LERaw :: Ptr SDLIOStream -> Ptr Word16 -> IO Bool

foreign import ccall "SDL_ReadS16LE"
  sdlReadS16LERaw :: Ptr SDLIOStream -> Ptr Int16 -> IO Bool

foreign import ccall "SDL_ReadU16BE"
  sdlReadU16BERaw :: Ptr SDLIOStream -> Ptr Word16 -> IO Bool

foreign import ccall "SDL_ReadS16BE"
  sdlReadS16BERaw :: Ptr SDLIOStream -> Ptr Int16 -> IO Bool

foreign import ccall "SDL_ReadU32LE"
  sdlReadU32LERaw :: Ptr SDLIOStream -> Ptr Word32 -> IO Bool

foreign import ccall "SDL_ReadS32LE"
  sdlReadS32LERaw :: Ptr SDLIOStream -> Ptr Int32 -> IO Bool

foreign import ccall "SDL_ReadU32BE"
  sdlReadU32BERaw :: Ptr SDLIOStream -> Ptr Word32 -> IO Bool

foreign import ccall "SDL_ReadS32BE"
  sdlReadS32BERaw :: Ptr SDLIOStream -> Ptr Int32 -> IO Bool

foreign import ccall "SDL_ReadU64LE"
  sdlReadU64LERaw :: Ptr SDLIOStream -> Ptr Word64 -> IO Bool

foreign import ccall "SDL_ReadS64LE"
  sdlReadS64LERaw :: Ptr SDLIOStream -> Ptr Int64 -> IO Bool

foreign import ccall "SDL_ReadU64BE"
  sdlReadU64BERaw :: Ptr SDLIOStream -> Ptr Word64 -> IO Bool

foreign import ccall "SDL_ReadS64BE"
  sdlReadS64BERaw :: Ptr SDLIOStream -> Ptr Int64 -> IO Bool

foreign import ccall "SDL_WriteU8"
  sdlWriteU8Raw :: Ptr SDLIOStream -> Word8 -> IO Bool

foreign import ccall "SDL_WriteS8"
  sdlWriteS8Raw :: Ptr SDLIOStream -> Int8 -> IO Bool

foreign import ccall "SDL_WriteU16LE"
  sdlWriteU16LERaw :: Ptr SDLIOStream -> Word16 -> IO Bool

foreign import ccall "SDL_WriteS16LE"
  sdlWriteS16LERaw :: Ptr SDLIOStream -> Int16 -> IO Bool

foreign import ccall "SDL_WriteU16BE"
  sdlWriteU16BERaw :: Ptr SDLIOStream -> Word16 -> IO Bool

foreign import ccall "SDL_WriteS16BE"
  sdlWriteS16BERaw :: Ptr SDLIOStream -> Int16 -> IO Bool

foreign import ccall "SDL_WriteU32LE"
  sdlWriteU32LERaw :: Ptr SDLIOStream -> Word32 -> IO Bool

foreign import ccall "SDL_WriteS32LE"
  sdlWriteS32LERaw :: Ptr SDLIOStream -> Int32 -> IO Bool

foreign import ccall "SDL_WriteU32BE"
  sdlWriteU32BERaw :: Ptr SDLIOStream -> Word32 -> IO Bool

foreign import ccall "SDL_WriteS32BE"
  sdlWriteS32BERaw :: Ptr SDLIOStream -> Int32 -> IO Bool

foreign import ccall "SDL_WriteU64LE"
  sdlWriteU64LERaw :: Ptr SDLIOStream -> Word64 -> IO Bool

foreign import ccall "SDL_WriteS64LE"
  sdlWriteS64LERaw :: Ptr SDLIOStream -> Int64 -> IO Bool

foreign import ccall "SDL_WriteU64BE"
  sdlWriteU64BERaw :: Ptr SDLIOStream -> Word64 -> IO Bool

foreign import ccall "SDL_WriteS64BE"
  sdlWriteS64BERaw :: Ptr SDLIOStream -> Int64 -> IO Bool
