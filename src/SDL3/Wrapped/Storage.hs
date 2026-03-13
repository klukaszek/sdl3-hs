module SDL3.Wrapped.Storage
  ( SDLStorageInterface(..)
  , SDLStorage
  , withStoragePtr
  , sdlUnsafeFromRawStorage
  , sdlUnsafeToRawStorage
  , sdlOpenTitleStorage
  , sdlOpenUserStorage
  , sdlOpenFileStorage
  , sdlOpenStorage
  , sdlCloseStorage
  , sdlStorageReady
  , sdlGetStorageFileSize
  , sdlReadStorageFile
  , sdlWriteStorageFile
  , sdlCreateStorageDirectory
  , sdlEnumerateStorageDirectory
  , sdlRemoveStoragePath
  , sdlRenameStoragePath
  , sdlCopyStorageFile
  , sdlGetStoragePathInfo
  , sdlGetStorageSpaceRemaining
  , sdlGlobStorageDirectory
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign
import Foreign.C
import SDL3.Filesystem (SDLGlobFlags, SDLEnumerationResult, SDLPathInfo)
import SDL3.Properties (SDLPropertiesID)
import SDL3.Raw.Storage (SDLStorageInterface(..))
import qualified SDL3.Raw.Storage as Raw

newtype SDLStorage = SDLStorage (Ptr Raw.SDLStorage)
  deriving (Eq)

instance Show SDLStorage where
  show (SDLStorage storagePtr) = "SDLStorage " ++ show storagePtr

sdlUnsafeFromRawStorage :: Ptr Raw.SDLStorage -> Maybe SDLStorage
sdlUnsafeFromRawStorage storagePtr
  | storagePtr == nullPtr = Nothing
  | otherwise = Just (SDLStorage storagePtr)

sdlUnsafeToRawStorage :: SDLStorage -> Ptr Raw.SDLStorage
sdlUnsafeToRawStorage (SDLStorage storagePtr) = storagePtr

withStoragePtr :: MonadIO m => SDLStorage -> (Ptr Raw.SDLStorage -> IO a) -> m a
withStoragePtr storage action = liftIO $ action (sdlUnsafeToRawStorage storage)

sdlOpenTitleStorage :: MonadIO m => Maybe String -> SDLPropertiesID -> m (Maybe SDLStorage)
sdlOpenTitleStorage override props =
  liftIO $
    maybeWith withCString override $ \overridePtr ->
      sdlUnsafeFromRawStorage <$> Raw.sdlOpenTitleStorage overridePtr props

sdlOpenUserStorage :: MonadIO m => String -> String -> SDLPropertiesID -> m (Maybe SDLStorage)
sdlOpenUserStorage org app props =
  liftIO $
    withCString org $ \orgPtr ->
      withCString app $ \appPtr ->
        sdlUnsafeFromRawStorage <$> Raw.sdlOpenUserStorage orgPtr appPtr props

sdlOpenFileStorage :: MonadIO m => Maybe String -> m (Maybe SDLStorage)
sdlOpenFileStorage path =
  liftIO $
    maybeWith withCString path $ \pathPtr ->
      sdlUnsafeFromRawStorage <$> Raw.sdlOpenFileStorage pathPtr

sdlOpenStorage :: MonadIO m => SDLStorageInterface -> Ptr () -> m (Maybe SDLStorage)
sdlOpenStorage iface userdata =
  liftIO $
    with iface $ \ifacePtr ->
      sdlUnsafeFromRawStorage <$> Raw.sdlOpenStorage ifacePtr userdata

sdlCloseStorage :: MonadIO m => SDLStorage -> m Bool
sdlCloseStorage storage = liftIO $ Raw.sdlCloseStorage (sdlUnsafeToRawStorage storage)

sdlStorageReady :: MonadIO m => SDLStorage -> m Bool
sdlStorageReady storage = liftIO $ Raw.sdlStorageReady (sdlUnsafeToRawStorage storage)

sdlGetStorageFileSize :: MonadIO m => SDLStorage -> String -> m (Maybe Word64)
sdlGetStorageFileSize storage path =
  liftIO $
    withCString path $ \pathPtr ->
      alloca $ \lengthPtr -> do
        success <- Raw.sdlGetStorageFileSize (sdlUnsafeToRawStorage storage) pathPtr lengthPtr
        if success
          then Just <$> peek lengthPtr
          else pure Nothing

sdlReadStorageFile :: MonadIO m => SDLStorage -> String -> Ptr () -> Word64 -> m Bool
sdlReadStorageFile storage path destination len =
  liftIO $
    withCString path $ \pathPtr ->
      Raw.sdlReadStorageFile (sdlUnsafeToRawStorage storage) pathPtr destination len

sdlWriteStorageFile :: MonadIO m => SDLStorage -> String -> Ptr () -> Word64 -> m Bool
sdlWriteStorageFile storage path source len =
  liftIO $
    withCString path $ \pathPtr ->
      Raw.sdlWriteStorageFile (sdlUnsafeToRawStorage storage) pathPtr source len

sdlCreateStorageDirectory :: MonadIO m => SDLStorage -> String -> m Bool
sdlCreateStorageDirectory storage path =
  liftIO $
    withCString path $ \pathPtr ->
      Raw.sdlCreateStorageDirectory (sdlUnsafeToRawStorage storage) pathPtr

sdlEnumerateStorageDirectory
  :: MonadIO m
  => SDLStorage
  -> Maybe String
  -> (Ptr () -> String -> String -> IO SDLEnumerationResult)
  -> Ptr ()
  -> m Bool
sdlEnumerateStorageDirectory storage path callback userdata =
  liftIO $
    maybeWith withCString path $ \pathPtr -> do
      callbackPtr <- Raw.mkEnumerateCallback $ \ud p n -> do
        dir <- peekCString p
        name <- peekCString n
        fromIntegral . fromEnum <$> callback ud dir name
      result <- Raw.sdlEnumerateStorageDirectory (sdlUnsafeToRawStorage storage) pathPtr callbackPtr userdata
      freeHaskellFunPtr callbackPtr
      pure result

sdlRemoveStoragePath :: MonadIO m => SDLStorage -> String -> m Bool
sdlRemoveStoragePath storage path =
  liftIO $
    withCString path $ \pathPtr ->
      Raw.sdlRemoveStoragePath (sdlUnsafeToRawStorage storage) pathPtr

sdlRenameStoragePath :: MonadIO m => SDLStorage -> String -> String -> m Bool
sdlRenameStoragePath storage oldPath newPath =
  liftIO $
    withCString oldPath $ \oldPathPtr ->
      withCString newPath $ \newPathPtr ->
        Raw.sdlRenameStoragePath (sdlUnsafeToRawStorage storage) oldPathPtr newPathPtr

sdlCopyStorageFile :: MonadIO m => SDLStorage -> String -> String -> m Bool
sdlCopyStorageFile storage oldPath newPath =
  liftIO $
    withCString oldPath $ \oldPathPtr ->
      withCString newPath $ \newPathPtr ->
        Raw.sdlCopyStorageFile (sdlUnsafeToRawStorage storage) oldPathPtr newPathPtr

sdlGetStoragePathInfo :: MonadIO m => SDLStorage -> String -> m (Maybe SDLPathInfo)
sdlGetStoragePathInfo storage path =
  liftIO $
    withCString path $ \pathPtr ->
      alloca $ \infoPtr -> do
        success <- Raw.sdlGetStoragePathInfo (sdlUnsafeToRawStorage storage) pathPtr infoPtr
        if success
          then Just <$> peek infoPtr
          else pure Nothing

sdlGetStorageSpaceRemaining :: MonadIO m => SDLStorage -> m Word64
sdlGetStorageSpaceRemaining storage =
  liftIO $ Raw.sdlGetStorageSpaceRemaining (sdlUnsafeToRawStorage storage)

sdlGlobStorageDirectory
  :: MonadIO m
  => SDLStorage
  -> Maybe String
  -> Maybe String
  -> SDLGlobFlags
  -> m (Maybe [String])
sdlGlobStorageDirectory storage path patternValue flags =
  liftIO $
    maybeWith withCString path $ \pathPtr ->
      maybeWith withCString patternValue $ \patternPtr ->
        alloca $ \countPtr -> do
          result <- Raw.sdlGlobStorageDirectory (sdlUnsafeToRawStorage storage) pathPtr patternPtr flags countPtr
          if result == nullPtr
            then pure Nothing
            else do
              count <- peek countPtr
              strings <- peekArray (fromIntegral count) result >>= mapM peekCString
              free result
              pure (Just strings)
