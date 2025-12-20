-- |
-- Module      : SDL.AsyncIO
-- Description : Bindings to SDL asynchronous I/O functionality
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BS3
--
-- This module provides Haskell bindings to SDL's asynchronous I/O functionality.
-- SDL offers a way to perform I/O asynchronously, allowing an app to read
-- or write files without waiting for data to actually transfer.
module SDL3.AsyncIO
  ( -- * Types
    SDLAsyncIO,
    SDLAsyncIOTaskType (..),
    SDLAsyncIOResult (..),
    SDLAsyncIOOutcome (..),
    SDLAsyncIOQueue,

    -- * Functions
    sdlAsyncIOFromFile,
    sdlGetAsyncIOSize,
    sdlReadAsyncIO,
    sdlWriteAsyncIO,
    sdlCloseAsyncIO,
    sdlCreateAsyncIOQueue,
    sdlDestroyAsyncIOQueue,
    sdlGetAsyncIOResult,
    sdlWaitAsyncIOResult,
    sdlSignalAsyncIOQueue,
    sdlLoadFileAsync,
  )
where

import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import SDL3.Stdinc (SDLBool)

-- | The asynchronous I/O operation structure.
data SDLAsyncIO = SDLAsyncIO

-- | Types of asynchronous I/O tasks.
data SDLAsyncIOTaskType
  = -- | A read operation.
    SDL_ASYNCIO_TASK_READ
  | -- | A write operation.
    SDL_ASYNCIO_TASK_WRITE
  | -- | A close operation.
    SDL_ASYNCIO_TASK_CLOSE
  deriving (Eq, Show, Enum)

-- | Possible outcomes of an asynchronous I/O task.
data SDLAsyncIOResult
  = -- | Request was completed without error
    SDL_ASYNCIO_COMPLETE
  | -- | Request failed for some reason; check SDL_GetError()!
    SDL_ASYNCIO_FAILURE
  | -- | Request was canceled before completing.
    SDL_ASYNCIO_CANCELED
  deriving (Eq, Show, Enum)

-- | Information about a completed asynchronous I/O request.
data SDLAsyncIOOutcome = SDL_AsyncIOOutcome
  { -- | What generated this task. This pointer will be invalid if it was closed!
    asyncio :: Ptr SDLAsyncIO,
    -- | What sort of task was this? Read, write, etc?
    taskType :: SDLAsyncIOTaskType,
    -- | The result of the work (success, failure, cancellation).
    result :: SDLAsyncIOResult,
    -- | Buffer where data was read/written.
    buffer :: Ptr (),
    -- | Offset in the SDL_AsyncIO where data was read/written.
    offset :: Word64,
    -- | Number of bytes the task was to read/write.
    bytesRequested :: Word64,
    -- | Actual number of bytes that were read/written.
    bytesTransferred :: Word64,
    -- | Pointer provided by the app when starting the task
    userdata :: Ptr ()
  }

-- | A queue of completed asynchronous I/O tasks.
data SDLAsyncIOQueue

-- Foreign structure for SDL_AsyncIOOutcome
data SDLAsyncIOOutcomeFFI

-- | Create a new SDL_AsyncIO object for reading from and/or writing to a named file.
foreign import ccall unsafe "SDL_AsyncIOFromFile"
  sdlAsyncIOFromFile :: CString -> CString -> IO (Ptr SDLAsyncIO)

-- | Get the size of the data stream in an SDL_AsyncIO.
foreign import ccall unsafe "SDL_GetAsyncIOSize"
  sdlGetAsyncIOSize :: Ptr SDLAsyncIO -> IO Int64

-- | Start an async read.
foreign import ccall unsafe "SDL_ReadAsyncIO"
  sdlReadAsyncIO :: Ptr SDLAsyncIO -> Ptr () -> Word64 -> Word64 -> Ptr SDLAsyncIOQueue -> Ptr () -> IO SDLBool

-- | Start an async write.
foreign import ccall unsafe "SDL_WriteAsyncIO"
  sdlWriteAsyncIO :: Ptr SDLAsyncIO -> Ptr () -> Word64 -> Word64 -> Ptr SDLAsyncIOQueue -> Ptr () -> IO SDLBool

-- | Close and free any allocated resources for an async I/O object.
foreign import ccall unsafe "SDL_CloseAsyncIO"
  sdlCloseAsyncIO :: Ptr SDLAsyncIO -> SDLBool -> Ptr SDLAsyncIOQueue -> Ptr () -> IO SDLBool

-- | Create a task queue for tracking multiple I/O operations.
foreign import ccall unsafe "SDL_CreateAsyncIOQueue"
  sdlCreateAsyncIOQueue :: IO (Ptr SDLAsyncIOQueue)

-- | Destroy a previously-created async I/O task queue.
foreign import ccall unsafe "SDL_DestroyAsyncIOQueue"
  sdlDestroyAsyncIOQueue :: Ptr SDLAsyncIOQueue -> IO ()

-- | Query an async I/O task queue for completed tasks.
foreign import ccall unsafe "SDL_GetAsyncIOResult"
  sdlGetAsyncIOResult :: Ptr SDLAsyncIOQueue -> Ptr SDLAsyncIOOutcomeFFI -> IO SDLBool

-- | Block until an async I/O task queue has a completed task.
foreign import ccall unsafe "SDL_WaitAsyncIOResult"
  sdlWaitAsyncIOResult :: Ptr SDLAsyncIOQueue -> Ptr SDLAsyncIOOutcomeFFI -> Int32 -> IO SDLBool

-- | Wake up any threads that are blocking in SDL_WaitAsyncIOResult().
foreign import ccall unsafe "SDL_SignalAsyncIOQueue"
  sdlSignalAsyncIOQueue :: Ptr SDLAsyncIOQueue -> IO ()

-- | Load all the data from a file path, asynchronously.
foreign import ccall unsafe "SDL_LoadFileAsync"
  sdlLoadFileAsync :: CString -> Ptr SDLAsyncIOQueue -> Ptr () -> IO SDLBool
