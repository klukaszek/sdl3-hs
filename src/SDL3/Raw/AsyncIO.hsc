{-# LANGUAGE ForeignFunctionInterface #-}

#include <SDL3/SDL_asyncio.h>

module SDL3.Raw.AsyncIO
  ( SDLAsyncIO
  , SDLAsyncIOTaskType(..)
  , SDLAsyncIOResult(..)
  , SDLAsyncIOOutcome(..)
  , SDLAsyncIOQueue
  , sdlAsyncIOFromFileRaw
  , sdlGetAsyncIOSizeRaw
  , sdlReadAsyncIORaw
  , sdlWriteAsyncIORaw
  , sdlCloseAsyncIORaw
  , sdlCreateAsyncIOQueueRaw
  , sdlDestroyAsyncIOQueueRaw
  , sdlGetAsyncIOResultRaw
  , sdlWaitAsyncIOResultRaw
  , sdlSignalAsyncIOQueueRaw
  , sdlLoadFileAsyncRaw
  ) where

import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types (CInt(..))
import Foreign.Ptr
import Foreign.Storable
import SDL3.Raw.Stdinc (SDLBool)

data SDLAsyncIO = SDLAsyncIO

data SDLAsyncIOTaskType
  = SDL_ASYNCIO_TASK_READ
  | SDL_ASYNCIO_TASK_WRITE
  | SDL_ASYNCIO_TASK_CLOSE
  deriving (Eq, Show, Enum)

data SDLAsyncIOResult
  = SDL_ASYNCIO_COMPLETE
  | SDL_ASYNCIO_FAILURE
  | SDL_ASYNCIO_CANCELED
  deriving (Eq, Show, Enum)

data SDLAsyncIOOutcome = SDLAsyncIOOutcome
  { asyncio :: Ptr SDLAsyncIO
  , taskType :: SDLAsyncIOTaskType
  , result :: SDLAsyncIOResult
  , buffer :: Ptr ()
  , offset :: Word64
  , bytesRequested :: Word64
  , bytesTransferred :: Word64
  , userdata :: Ptr ()
  } deriving (Eq, Show)

instance Storable SDLAsyncIOOutcome where
  sizeOf _ = #size SDL_AsyncIOOutcome
  alignment _ = #alignment SDL_AsyncIOOutcome
  peek ptr = do
    asyncioPtr <- #{peek SDL_AsyncIOOutcome, asyncio} ptr
    taskTypeValue <- (#{peek SDL_AsyncIOOutcome, type} ptr) :: IO CInt
    resultValue <- (#{peek SDL_AsyncIOOutcome, result} ptr) :: IO CInt
    bufferPtr <- #{peek SDL_AsyncIOOutcome, buffer} ptr
    offsetValue <- #{peek SDL_AsyncIOOutcome, offset} ptr
    bytesRequestedValue <- #{peek SDL_AsyncIOOutcome, bytes_requested} ptr
    bytesTransferredValue <- #{peek SDL_AsyncIOOutcome, bytes_transferred} ptr
    userdataPtr <- #{peek SDL_AsyncIOOutcome, userdata} ptr
    pure $
      SDLAsyncIOOutcome
        asyncioPtr
        (toEnum $ fromIntegral taskTypeValue)
        (toEnum $ fromIntegral resultValue)
        bufferPtr
        offsetValue
        bytesRequestedValue
        bytesTransferredValue
        userdataPtr
  poke ptr outcome = do
    #{poke SDL_AsyncIOOutcome, asyncio} ptr (asyncio outcome)
    #{poke SDL_AsyncIOOutcome, type} ptr (fromIntegral $ fromEnum $ taskType outcome :: CInt)
    #{poke SDL_AsyncIOOutcome, result} ptr (fromIntegral $ fromEnum $ result outcome :: CInt)
    #{poke SDL_AsyncIOOutcome, buffer} ptr (buffer outcome)
    #{poke SDL_AsyncIOOutcome, offset} ptr (offset outcome)
    #{poke SDL_AsyncIOOutcome, bytes_requested} ptr (bytesRequested outcome)
    #{poke SDL_AsyncIOOutcome, bytes_transferred} ptr (bytesTransferred outcome)
    #{poke SDL_AsyncIOOutcome, userdata} ptr (userdata outcome)

data SDLAsyncIOQueue

foreign import ccall unsafe "SDL_AsyncIOFromFile"
  sdlAsyncIOFromFileRaw :: CString -> CString -> IO (Ptr SDLAsyncIO)

foreign import ccall unsafe "SDL_GetAsyncIOSize"
  sdlGetAsyncIOSizeRaw :: Ptr SDLAsyncIO -> IO Int64

foreign import ccall unsafe "SDL_ReadAsyncIO"
  sdlReadAsyncIORaw :: Ptr SDLAsyncIO -> Ptr () -> Word64 -> Word64 -> Ptr SDLAsyncIOQueue -> Ptr () -> IO SDLBool

foreign import ccall unsafe "SDL_WriteAsyncIO"
  sdlWriteAsyncIORaw :: Ptr SDLAsyncIO -> Ptr () -> Word64 -> Word64 -> Ptr SDLAsyncIOQueue -> Ptr () -> IO SDLBool

foreign import ccall unsafe "SDL_CloseAsyncIO"
  sdlCloseAsyncIORaw :: Ptr SDLAsyncIO -> SDLBool -> Ptr SDLAsyncIOQueue -> Ptr () -> IO SDLBool

foreign import ccall unsafe "SDL_CreateAsyncIOQueue"
  sdlCreateAsyncIOQueueRaw :: IO (Ptr SDLAsyncIOQueue)

foreign import ccall unsafe "SDL_DestroyAsyncIOQueue"
  sdlDestroyAsyncIOQueueRaw :: Ptr SDLAsyncIOQueue -> IO ()

foreign import ccall unsafe "SDL_GetAsyncIOResult"
  sdlGetAsyncIOResultRaw :: Ptr SDLAsyncIOQueue -> Ptr SDLAsyncIOOutcome -> IO SDLBool

foreign import ccall unsafe "SDL_WaitAsyncIOResult"
  sdlWaitAsyncIOResultRaw :: Ptr SDLAsyncIOQueue -> Ptr SDLAsyncIOOutcome -> Int32 -> IO SDLBool

foreign import ccall unsafe "SDL_SignalAsyncIOQueue"
  sdlSignalAsyncIOQueueRaw :: Ptr SDLAsyncIOQueue -> IO ()

foreign import ccall unsafe "SDL_LoadFileAsync"
  sdlLoadFileAsyncRaw :: CString -> Ptr SDLAsyncIOQueue -> Ptr () -> IO SDLBool
