{-|
Module      : SDL.AsyncIO
Description : Bindings to SDL asynchronous I/O functionality
Copyright   : (c) Kyle Lukaszek, 2025
License     : BS3
Maintainer  : kylelukaszek@example.com

This module provides Haskell bindings to SDL's asynchronous I/O functionality.
SDL offers a way to perform I/O asynchronously, allowing an app to read
or write files without waiting for data to actually transfer.
-}

module SDL.AsyncIO
  (
  -- * Types
    SDL_AsyncIO
  , SDL_AsyncIOTaskType(..)
  , SDL_AsyncIOResult(..)
  , SDL_AsyncIOOutcome(..)
  , SDL_AsyncIOQueue
  
  -- * Functions
  , sdlAsyncIOFromFile
  , sdlGetAsyncIOSize
  , sdlReadAsyncIO
  , sdlWriteAsyncIO
  , sdlCloseAsyncIO
  , sdlCreateAsyncIOQueue
  , sdlDestroyAsyncIOQueue
  , sdlGetAsyncIOResult
  , sdlWaitAsyncIOResult
  , sdlSignalAsyncIOQueue
  , sdlLoadFileAsync
  ) where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import SDL.Stdinc (SDLBool)
import Data.Word
import Data.Int

-- | The asynchronous I/O operation structure.
data SDL_AsyncIO = SDL_AsyncIO

-- | Types of asynchronous I/O tasks.
data SDL_AsyncIOTaskType = 
    SDL_ASYNCIO_TASK_READ    -- ^ A read operation.
  | SDL_ASYNCIO_TASK_WRITE   -- ^ A write operation.
  | SDL_ASYNCIO_TASK_CLOSE   -- ^ A close operation.
  deriving (Eq, Show, Enum)

-- | Possible outcomes of an asynchronous I/O task.
data SDL_AsyncIOResult =
    SDL_ASYNCIO_COMPLETE   -- ^ Request was completed without error
  | SDL_ASYNCIO_FAILURE    -- ^ Request failed for some reason; check SDL_GetError()!
  | SDL_ASYNCIO_CANCELED   -- ^ Request was canceled before completing.
  deriving (Eq, Show, Enum)

-- | Information about a completed asynchronous I/O request.
data SDL_AsyncIOOutcome = SDL_AsyncIOOutcome
  { asyncio :: Ptr SDL_AsyncIO            -- ^ What generated this task. This pointer will be invalid if it was closed!
  , taskType :: SDL_AsyncIOTaskType       -- ^ What sort of task was this? Read, write, etc?
  , result :: SDL_AsyncIOResult           -- ^ The result of the work (success, failure, cancellation).
  , buffer :: Ptr ()                      -- ^ Buffer where data was read/written.
  , offset :: Word64                      -- ^ Offset in the SDL_AsyncIO where data was read/written.
  , bytesRequested :: Word64              -- ^ Number of bytes the task was to read/write.
  , bytesTransferred :: Word64            -- ^ Actual number of bytes that were read/written.
  , userdata :: Ptr ()                    -- ^ Pointer provided by the app when starting the task
  }

-- | A queue of completed asynchronous I/O tasks.
data SDL_AsyncIOQueue = SDL_AsyncIOQueue

-- Foreign structure for SDL_AsyncIOOutcome
data SDL_AsyncIOOutcome_FFI

-- | Create a new SDL_AsyncIO object for reading from and/or writing to a named file.
foreign import ccall unsafe "SDL_AsyncIOFromFile"
  sdlAsyncIOFromFile :: CString -> CString -> IO (Ptr SDL_AsyncIO)

-- | Get the size of the data stream in an SDL_AsyncIO.
foreign import ccall unsafe "SDL_GetAsyncIOSize"
  sdlGetAsyncIOSize :: Ptr SDL_AsyncIO -> IO Int64

-- | Start an async read.
foreign import ccall unsafe "SDL_ReadAsyncIO"
  sdlReadAsyncIO :: Ptr SDL_AsyncIO -> Ptr () -> Word64 -> Word64 -> Ptr SDL_AsyncIOQueue -> Ptr () -> IO SDLBool

-- | Start an async write.
foreign import ccall unsafe "SDL_WriteAsyncIO"
  sdlWriteAsyncIO :: Ptr SDL_AsyncIO -> Ptr () -> Word64 -> Word64 -> Ptr SDL_AsyncIOQueue -> Ptr () -> IO SDLBool

-- | Close and free any allocated resources for an async I/O object.
foreign import ccall unsafe "SDL_CloseAsyncIO"
  sdlCloseAsyncIO :: Ptr SDL_AsyncIO -> SDLBool -> Ptr SDL_AsyncIOQueue -> Ptr () -> IO SDLBool

-- | Create a task queue for tracking multiple I/O operations.
foreign import ccall unsafe "SDL_CreateAsyncIOQueue"
  sdlCreateAsyncIOQueue :: IO (Ptr SDL_AsyncIOQueue)

-- | Destroy a previously-created async I/O task queue.
foreign import ccall unsafe "SDL_DestroyAsyncIOQueue"
  sdlDestroyAsyncIOQueue :: Ptr SDL_AsyncIOQueue -> IO ()

-- | Query an async I/O task queue for completed tasks.
foreign import ccall unsafe "SDL_GetAsyncIOResult"
  sdlGetAsyncIOResult :: Ptr SDL_AsyncIOQueue -> Ptr SDL_AsyncIOOutcome_FFI -> IO SDLBool

-- | Block until an async I/O task queue has a completed task.
foreign import ccall unsafe "SDL_WaitAsyncIOResult"
  sdlWaitAsyncIOResult :: Ptr SDL_AsyncIOQueue -> Ptr SDL_AsyncIOOutcome_FFI -> Int32 -> IO SDLBool

-- | Wake up any threads that are blocking in SDL_WaitAsyncIOResult().
foreign import ccall unsafe "SDL_SignalAsyncIOQueue"
  sdlSignalAsyncIOQueue :: Ptr SDL_AsyncIOQueue -> IO ()

-- | Load all the data from a file path, asynchronously.
foreign import ccall unsafe "SDL_LoadFileAsync"
  sdlLoadFileAsync :: CString -> Ptr SDL_AsyncIOQueue -> Ptr () -> IO SDLBool
