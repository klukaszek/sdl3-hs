{-# LANGUAGE PatternSynonyms #-}

module SDL3.Wrapped.Filesystem
  ( sdlGetBasePath
  , sdlGetPrefPath
  , sdlGetUserFolder
  , sdlGetCurrentDirectory
  , sdlCreateDirectory
  , sdlEnumerateDirectory
  , sdlRemovePath
  , sdlRenamePath
  , sdlCopyFile
  , sdlGetPathInfo
  , sdlGlobDirectory
  , SDLFolder(..)
  , SDLPathType(..)
  , SDLPathInfo(..)
  , SDLGlobFlags(..)
  , pattern SDL_GLOB_CASEINSENSITIVE
  , SDLEnumerationResult(..)
  , SDLEnumerateDirectoryCallback
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign
import Foreign.C
import SDL3.Raw.Filesystem
  ( SDLFolder(..)
  , SDLPathType(..)
  , SDLPathInfo(..)
  , SDLGlobFlags(..)
  , pattern SDL_GLOB_CASEINSENSITIVE
  , SDLEnumerationResult(..)
  )
import qualified SDL3.Raw.Filesystem as Raw

type SDLEnumerateDirectoryCallback = Ptr () -> String -> String -> IO SDLEnumerationResult

peekMaybeCString :: CString -> IO (Maybe String)
peekMaybeCString ptr
  | ptr == nullPtr = pure Nothing
  | otherwise = Just <$> peekCString ptr

sdlGetBasePath :: MonadIO m => m (Maybe String)
sdlGetBasePath = liftIO $ Raw.sdlGetBasePath >>= peekMaybeCString

sdlGetPrefPath :: MonadIO m => String -> String -> m (Maybe String)
sdlGetPrefPath org app =
  liftIO $
    withCString org $ \orgPtr ->
      withCString app $ \appPtr ->
        Raw.sdlGetPrefPath orgPtr appPtr >>= peekMaybeCString

sdlGetUserFolder :: MonadIO m => SDLFolder -> m (Maybe String)
sdlGetUserFolder folder = liftIO $ Raw.sdlGetUserFolder folder >>= peekMaybeCString

sdlGetCurrentDirectory :: MonadIO m => m (Maybe String)
sdlGetCurrentDirectory = liftIO $ Raw.sdlGetCurrentDirectory >>= peekMaybeCString

sdlCreateDirectory :: MonadIO m => String -> m Bool
sdlCreateDirectory path = liftIO $ withCString path Raw.sdlCreateDirectory

sdlEnumerateDirectory :: MonadIO m => String -> SDLEnumerateDirectoryCallback -> Ptr () -> m Bool
sdlEnumerateDirectory path callback userdata =
  liftIO $
    withCString path $ \pathPtr -> do
      callbackPtr <- Raw.makeEnumerateCallback $ \rawUserdata dirPtr entryPtr -> do
        dir <- peekCString dirPtr
        entry <- peekCString entryPtr
        fromIntegral . fromEnum <$> callback rawUserdata dir entry
      result <- Raw.sdlEnumerateDirectory pathPtr callbackPtr userdata
      freeHaskellFunPtr callbackPtr
      pure result

sdlRemovePath :: MonadIO m => String -> m Bool
sdlRemovePath path = liftIO $ withCString path Raw.sdlRemovePath

sdlRenamePath :: MonadIO m => String -> String -> m Bool
sdlRenamePath oldPath newPath =
  liftIO $
    withCString oldPath $ \oldPathPtr ->
      withCString newPath $ \newPathPtr ->
        Raw.sdlRenamePath oldPathPtr newPathPtr

sdlCopyFile :: MonadIO m => String -> String -> m Bool
sdlCopyFile oldPath newPath =
  liftIO $
    withCString oldPath $ \oldPathPtr ->
      withCString newPath $ \newPathPtr ->
        Raw.sdlCopyFile oldPathPtr newPathPtr

sdlGetPathInfo :: MonadIO m => String -> m (Maybe SDLPathInfo)
sdlGetPathInfo path =
  liftIO $
    withCString path $ \pathPtr ->
      alloca $ \infoPtr -> do
        success <- Raw.sdlGetPathInfo pathPtr infoPtr
        if success
          then Just <$> peek infoPtr
          else pure Nothing

sdlGlobDirectory :: MonadIO m => String -> Maybe String -> SDLGlobFlags -> m (Maybe [String])
sdlGlobDirectory path patternValue flags =
  liftIO $
    withCString path $ \pathPtr ->
      maybeWith withCString patternValue $ \patternPtr ->
        alloca $ \countPtr -> do
          results <- Raw.sdlGlobDirectory pathPtr patternPtr flags countPtr
          if results == nullPtr
            then pure Nothing
            else do
              count <- peek countPtr
              paths <- peekArray (fromIntegral count) results
              Just <$> mapM peekCString paths
