module SDL3.Wrapped.Clipboard
  ( sdlSetClipboardText
  , sdlGetClipboardText
  , sdlHasClipboardText
  , sdlSetPrimarySelectionText
  , sdlGetPrimarySelectionText
  , sdlHasPrimarySelectionText
  , SDLClipboardDataCallback
  , SDLClipboardCleanupCallback
  , sdlSetClipboardData
  , sdlClearClipboardData
  , sdlGetClipboardData
  , sdlHasClipboardData
  , sdlGetClipboardMimeTypes
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Foreign
import Foreign.C
import Foreign.Marshal.Utils (withMany)
import System.IO.Unsafe (unsafePerformIO)
import qualified SDL3.Raw.Clipboard as Raw

type SDLClipboardDataCallback = Raw.SDLClipboardDataCallback
type SDLClipboardCleanupCallback = Raw.SDLClipboardCleanupCallback

clipboardCallbackRef :: IORef (Maybe (FunPtr Raw.SDLClipboardDataCallback, FunPtr Raw.SDLClipboardCleanupCallback))
clipboardCallbackRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE clipboardCallbackRef #-}

cbool :: CBool -> Bool
cbool = (/= 0)

peekMaybeCString :: CString -> IO (Maybe String)
peekMaybeCString ptr
  | ptr == nullPtr = pure Nothing
  | otherwise = Just <$> peekCString ptr

releaseClipboardCallbacks :: Maybe (FunPtr Raw.SDLClipboardDataCallback, FunPtr Raw.SDLClipboardCleanupCallback) -> IO ()
releaseClipboardCallbacks Nothing = pure ()
releaseClipboardCallbacks (Just (callbackPtr, cleanupPtr)) = do
  freeHaskellFunPtr callbackPtr
  freeHaskellFunPtr cleanupPtr

sdlSetClipboardText :: MonadIO m => String -> m Bool
sdlSetClipboardText text =
  liftIO $ withCString text (fmap cbool . Raw.sdlSetClipboardText)

sdlGetClipboardText :: MonadIO m => m (Maybe String)
sdlGetClipboardText = liftIO $ Raw.sdlGetClipboardText >>= peekMaybeCString

sdlHasClipboardText :: MonadIO m => m Bool
sdlHasClipboardText = liftIO $ cbool <$> Raw.sdlHasClipboardText

sdlSetPrimarySelectionText :: MonadIO m => String -> m Bool
sdlSetPrimarySelectionText text =
  liftIO $ withCString text (fmap cbool . Raw.sdlSetPrimarySelectionText)

sdlGetPrimarySelectionText :: MonadIO m => m (Maybe String)
sdlGetPrimarySelectionText = liftIO $ Raw.sdlGetPrimarySelectionText >>= peekMaybeCString

sdlHasPrimarySelectionText :: MonadIO m => m Bool
sdlHasPrimarySelectionText = liftIO $ cbool <$> Raw.sdlHasPrimarySelectionText

sdlSetClipboardData
  :: MonadIO m
  => SDLClipboardDataCallback
  -> SDLClipboardCleanupCallback
  -> Ptr ()
  -> [String]
  -> m Bool
sdlSetClipboardData callback cleanup userdata mimeTypes =
  liftIO $
    withMany withCString mimeTypes $ \mimeTypePtrs ->
      withArrayLen mimeTypePtrs $ \len mimeArray -> do
        callbackPtr <- Raw.makeCallback callback
        cleanupPtr <- Raw.makeCleanup cleanup
        result <- Raw.sdlSetClipboardData callbackPtr cleanupPtr userdata mimeArray (fromIntegral len)
        if cbool result
          then do
            previous <- atomicModifyIORef' clipboardCallbackRef (\current -> (Just (callbackPtr, cleanupPtr), current))
            releaseClipboardCallbacks previous
            pure True
          else do
            freeHaskellFunPtr callbackPtr
            freeHaskellFunPtr cleanupPtr
            pure False

sdlClearClipboardData :: MonadIO m => m Bool
sdlClearClipboardData =
  liftIO $ do
    result <- Raw.sdlClearClipboardData
    if cbool result
      then do
        previous <- atomicModifyIORef' clipboardCallbackRef (\current -> (Nothing, current))
        releaseClipboardCallbacks previous
        pure True
      else pure False

sdlGetClipboardData :: MonadIO m => String -> m (Maybe ByteString)
sdlGetClipboardData mimeType =
  liftIO $
    alloca $ \sizePtr ->
      withCString mimeType $ \cstr -> do
        dataPtr <- Raw.sdlGetClipboardData cstr sizePtr
        if dataPtr == nullPtr
          then pure Nothing
          else do
            size <- peek sizePtr
            Just <$> BS.packCStringLen (castPtr dataPtr, fromIntegral size)

sdlHasClipboardData :: MonadIO m => String -> m Bool
sdlHasClipboardData mimeType =
  liftIO $ withCString mimeType (\cstr -> fmap cbool (Raw.sdlHasClipboardData cstr))

sdlGetClipboardMimeTypes :: MonadIO m => m [String]
sdlGetClipboardMimeTypes =
  liftIO $
    alloca $ \countPtr -> do
      poke countPtr 0
      mimeTypesPtr <- Raw.sdlGetClipboardMimeTypes countPtr
      if mimeTypesPtr == nullPtr
        then pure []
        else do
          count <- peek countPtr
          mimeTypesCStr <- peekArray (fromIntegral count) mimeTypesPtr
          mapM peekCString mimeTypesCStr
