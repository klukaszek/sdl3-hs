module SDL3.Wrapped.AsyncIO
  ( SDLAsyncIO
  , SDLAsyncIOTaskType(..)
  , SDLAsyncIOResult(..)
  , SDLAsyncIOOutcome(..)
  , SDLAsyncIOQueue
  , withAsyncIOPtr
  , withAsyncIOQueuePtr
  , sdlUnsafeFromRawAsyncIO
  , sdlUnsafeToRawAsyncIO
  , sdlUnsafeFromRawAsyncIOQueue
  , sdlUnsafeToRawAsyncIOQueue
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

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int
import Data.Word
import Foreign.C.String (withCString)
import Foreign.Marshal.Utils (toBool)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import SDL3.Raw.AsyncIO
  ( SDLAsyncIOTaskType(..)
  , SDLAsyncIOResult(..)
  )
import qualified SDL3.Raw.AsyncIO as Raw
import SDL3.Stdinc (SDLBool)

newtype SDLAsyncIO = SDLAsyncIO (Ptr Raw.SDLAsyncIO)
  deriving (Eq)

instance Show SDLAsyncIO where
  show (SDLAsyncIO asyncioPtr) = "SDLAsyncIO " ++ show asyncioPtr

newtype SDLAsyncIOQueue = SDLAsyncIOQueue (Ptr Raw.SDLAsyncIOQueue)
  deriving (Eq)

instance Show SDLAsyncIOQueue where
  show (SDLAsyncIOQueue queuePtr) = "SDLAsyncIOQueue " ++ show queuePtr

data SDLAsyncIOOutcome = SDLAsyncIOOutcome
  { asyncio :: Maybe SDLAsyncIO
  , taskType :: SDLAsyncIOTaskType
  , result :: SDLAsyncIOResult
  , buffer :: Ptr ()
  , offset :: Word64
  , bytesRequested :: Word64
  , bytesTransferred :: Word64
  , userdata :: Ptr ()
  } deriving (Eq, Show)

sdlUnsafeFromRawAsyncIO :: Ptr Raw.SDLAsyncIO -> Maybe SDLAsyncIO
sdlUnsafeFromRawAsyncIO asyncioPtr
  | asyncioPtr == nullPtr = Nothing
  | otherwise = Just (SDLAsyncIO asyncioPtr)

sdlUnsafeToRawAsyncIO :: SDLAsyncIO -> Ptr Raw.SDLAsyncIO
sdlUnsafeToRawAsyncIO (SDLAsyncIO asyncioPtr) = asyncioPtr

sdlUnsafeFromRawAsyncIOQueue :: Ptr Raw.SDLAsyncIOQueue -> Maybe SDLAsyncIOQueue
sdlUnsafeFromRawAsyncIOQueue queuePtr
  | queuePtr == nullPtr = Nothing
  | otherwise = Just (SDLAsyncIOQueue queuePtr)

sdlUnsafeToRawAsyncIOQueue :: SDLAsyncIOQueue -> Ptr Raw.SDLAsyncIOQueue
sdlUnsafeToRawAsyncIOQueue (SDLAsyncIOQueue queuePtr) = queuePtr

withAsyncIOPtr :: MonadIO m => SDLAsyncIO -> (Ptr Raw.SDLAsyncIO -> IO a) -> m a
withAsyncIOPtr asyncio action = liftIO $ action (sdlUnsafeToRawAsyncIO asyncio)

withAsyncIOQueuePtr :: MonadIO m => SDLAsyncIOQueue -> (Ptr Raw.SDLAsyncIOQueue -> IO a) -> m a
withAsyncIOQueuePtr queue action = liftIO $ action (sdlUnsafeToRawAsyncIOQueue queue)

fromRawOutcome :: Raw.SDLAsyncIOOutcome -> SDLAsyncIOOutcome
fromRawOutcome outcome =
  SDLAsyncIOOutcome
    { asyncio = sdlUnsafeFromRawAsyncIO (Raw.asyncio outcome)
    , taskType = Raw.taskType outcome
    , result = Raw.result outcome
    , buffer = Raw.buffer outcome
    , offset = Raw.offset outcome
    , bytesRequested = Raw.bytesRequested outcome
    , bytesTransferred = Raw.bytesTransferred outcome
    , userdata = Raw.userdata outcome
    }

sdlAsyncIOFromFile :: MonadIO m => String -> String -> m (Maybe SDLAsyncIO)
sdlAsyncIOFromFile file mode = liftIO $
  withCString file $ \cfile ->
    withCString mode $ \cmode ->
      sdlUnsafeFromRawAsyncIO <$> Raw.sdlAsyncIOFromFileRaw cfile cmode

sdlGetAsyncIOSize :: MonadIO m => SDLAsyncIO -> m Int64
sdlGetAsyncIOSize asyncio = liftIO $ Raw.sdlGetAsyncIOSizeRaw (sdlUnsafeToRawAsyncIO asyncio)

sdlReadAsyncIO :: MonadIO m => SDLAsyncIO -> Ptr () -> Word64 -> Word64 -> SDLAsyncIOQueue -> Ptr () -> m SDLBool
sdlReadAsyncIO asyncio bufferPtr offset bytes queue userdata =
  liftIO $ Raw.sdlReadAsyncIORaw
    (sdlUnsafeToRawAsyncIO asyncio)
    bufferPtr
    offset
    bytes
    (sdlUnsafeToRawAsyncIOQueue queue)
    userdata

sdlWriteAsyncIO :: MonadIO m => SDLAsyncIO -> Ptr () -> Word64 -> Word64 -> SDLAsyncIOQueue -> Ptr () -> m SDLBool
sdlWriteAsyncIO asyncio bufferPtr offset bytes queue userdata =
  liftIO $ Raw.sdlWriteAsyncIORaw
    (sdlUnsafeToRawAsyncIO asyncio)
    bufferPtr
    offset
    bytes
    (sdlUnsafeToRawAsyncIOQueue queue)
    userdata

sdlCloseAsyncIO :: MonadIO m => SDLAsyncIO -> SDLBool -> SDLAsyncIOQueue -> Ptr () -> m SDLBool
sdlCloseAsyncIO asyncio flush queue userdata =
  liftIO $ Raw.sdlCloseAsyncIORaw
    (sdlUnsafeToRawAsyncIO asyncio)
    flush
    (sdlUnsafeToRawAsyncIOQueue queue)
    userdata

sdlCreateAsyncIOQueue :: MonadIO m => m (Maybe SDLAsyncIOQueue)
sdlCreateAsyncIOQueue = liftIO $ sdlUnsafeFromRawAsyncIOQueue <$> Raw.sdlCreateAsyncIOQueueRaw

sdlDestroyAsyncIOQueue :: MonadIO m => SDLAsyncIOQueue -> m ()
sdlDestroyAsyncIOQueue queue = liftIO $ Raw.sdlDestroyAsyncIOQueueRaw (sdlUnsafeToRawAsyncIOQueue queue)

sdlGetAsyncIOResult :: MonadIO m => SDLAsyncIOQueue -> m (Maybe SDLAsyncIOOutcome)
sdlGetAsyncIOResult queue = liftIO $
  alloca $ \outcomePtr -> do
    success <- Raw.sdlGetAsyncIOResultRaw (sdlUnsafeToRawAsyncIOQueue queue) outcomePtr
    if toBool success
      then Just . fromRawOutcome <$> peek outcomePtr
      else pure Nothing

sdlWaitAsyncIOResult :: MonadIO m => SDLAsyncIOQueue -> Int32 -> m (Maybe SDLAsyncIOOutcome)
sdlWaitAsyncIOResult queue timeoutMs = liftIO $
  alloca $ \outcomePtr -> do
    success <- Raw.sdlWaitAsyncIOResultRaw (sdlUnsafeToRawAsyncIOQueue queue) outcomePtr timeoutMs
    if toBool success
      then Just . fromRawOutcome <$> peek outcomePtr
      else pure Nothing

sdlSignalAsyncIOQueue :: MonadIO m => SDLAsyncIOQueue -> m ()
sdlSignalAsyncIOQueue queue = liftIO $ Raw.sdlSignalAsyncIOQueueRaw (sdlUnsafeToRawAsyncIOQueue queue)

sdlLoadFileAsync :: MonadIO m => String -> SDLAsyncIOQueue -> Ptr () -> m SDLBool
sdlLoadFileAsync file queue userdata = liftIO $
  withCString file $ \cfile ->
    Raw.sdlLoadFileAsyncRaw cfile (sdlUnsafeToRawAsyncIOQueue queue) userdata
