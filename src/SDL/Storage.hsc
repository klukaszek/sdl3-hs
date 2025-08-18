{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- SDL/Storage.hsc
{-|
Module      : SDL.Storage
Description : Storage control functions for SDL3.
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

This module provides Haskell bindings to the SDL3 storage functionality.
-}

module SDL.Storage
  (
    -- * Storage Interface
    SDLStorageInterface(..)
  , SDLStorage

    -- * Storage Creation and Management
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

#include <SDL3/SDL_storage.h>

import Foreign
import Foreign.C
import SDL.Filesystem (SDLPathInfo, SDLEnumerationResult(..), SDLGlobFlags(..))
import SDL.Properties (SDLPropertiesID)

-- | An opaque handle representing a storage container.
data SDLStorage

-- | Interface for custom SDL_Storage implementations.
data SDLStorageInterface = SDLStorageInterface
  { version          :: Word32
  , close            :: FunPtr (Ptr () -> IO Bool)
  , ready            :: FunPtr (Ptr () -> IO Bool)
  , enumerate        :: FunPtr (Ptr () -> CString -> FunPtr (Ptr () -> CString -> CString -> IO CInt) -> Ptr () -> IO Bool)
  , info             :: FunPtr (Ptr () -> CString -> Ptr SDLPathInfo -> IO Bool)
  , readFile         :: FunPtr (Ptr () -> CString -> Ptr () -> Word64 -> IO Bool)
  , writeFile        :: FunPtr (Ptr () -> CString -> Ptr () -> Word64 -> IO Bool)
  , mkdir            :: FunPtr (Ptr () -> CString -> IO Bool)
  , remove           :: FunPtr (Ptr () -> CString -> IO Bool)
  , rename           :: FunPtr (Ptr () -> CString -> CString -> IO Bool)
  , copy             :: FunPtr (Ptr () -> CString -> CString -> IO Bool)
  , spaceRemaining   :: FunPtr (Ptr () -> IO Word64)
  } deriving (Show)

instance Storable SDLStorageInterface where
  sizeOf _ = #{size SDL_StorageInterface}
  alignment _ = #{alignment SDL_StorageInterface}
  peek ptr = do
    v <- #{peek SDL_StorageInterface, version} ptr
    c <- #{peek SDL_StorageInterface, close} ptr
    r <- #{peek SDL_StorageInterface, ready} ptr
    e <- #{peek SDL_StorageInterface, enumerate} ptr
    i <- #{peek SDL_StorageInterface, info} ptr
    rf <- #{peek SDL_StorageInterface, read_file} ptr
    wf <- #{peek SDL_StorageInterface, write_file} ptr
    m <- #{peek SDL_StorageInterface, mkdir} ptr
    rm <- #{peek SDL_StorageInterface, remove} ptr
    rn <- #{peek SDL_StorageInterface, rename} ptr
    cp <- #{peek SDL_StorageInterface, copy} ptr
    sr <- #{peek SDL_StorageInterface, space_remaining} ptr
    return $ SDLStorageInterface v c r e i rf wf m rm rn cp sr
  poke ptr (SDLStorageInterface v c r e i rf wf m rm rn cp sr) = do
    #{poke SDL_StorageInterface, version} ptr v
    #{poke SDL_StorageInterface, close} ptr c
    #{poke SDL_StorageInterface, ready} ptr r
    #{poke SDL_StorageInterface, enumerate} ptr e
    #{poke SDL_StorageInterface, info} ptr i
    #{poke SDL_StorageInterface, read_file} ptr rf
    #{poke SDL_StorageInterface, write_file} ptr wf
    #{poke SDL_StorageInterface, mkdir} ptr m
    #{poke SDL_StorageInterface, remove} ptr rm
    #{poke SDL_StorageInterface, rename} ptr rn
    #{poke SDL_StorageInterface, copy} ptr cp
    #{poke SDL_StorageInterface, space_remaining} ptr sr

-- FFI Imports

foreign import ccall unsafe "SDL_OpenTitleStorage" sdlOpenTitleStorage_ :: CString -> Word32 -> IO (Ptr SDLStorage)
foreign import ccall unsafe "SDL_OpenUserStorage" sdlOpenUserStorage_ :: CString -> CString -> Word32 -> IO (Ptr SDLStorage)
foreign import ccall unsafe "SDL_OpenFileStorage" sdlOpenFileStorage_ :: CString -> IO (Ptr SDLStorage)
foreign import ccall unsafe "SDL_OpenStorage" sdlOpenStorage_ :: Ptr SDLStorageInterface -> Ptr () -> IO (Ptr SDLStorage)
foreign import ccall unsafe "SDL_CloseStorage" sdlCloseStorage_ :: Ptr SDLStorage -> IO Bool
foreign import ccall unsafe "SDL_StorageReady" sdlStorageReady_ :: Ptr SDLStorage -> IO Bool
foreign import ccall unsafe "SDL_GetStorageFileSize" sdlGetStorageFileSize_ :: Ptr SDLStorage -> CString -> Ptr Word64 -> IO Bool
foreign import ccall unsafe "SDL_ReadStorageFile" sdlReadStorageFile_ :: Ptr SDLStorage -> CString -> Ptr () -> Word64 -> IO Bool
foreign import ccall unsafe "SDL_WriteStorageFile" sdlWriteStorageFile_ :: Ptr SDLStorage -> CString -> Ptr () -> Word64 -> IO Bool
foreign import ccall unsafe "SDL_CreateStorageDirectory" sdlCreateStorageDirectory_ :: Ptr SDLStorage -> CString -> IO Bool
foreign import ccall safe "SDL_EnumerateStorageDirectory" sdlEnumerateStorageDirectory_ :: Ptr SDLStorage -> CString -> FunPtr (Ptr () -> CString -> CString -> IO CInt) -> Ptr () -> IO Bool
foreign import ccall unsafe "SDL_RemoveStoragePath" sdlRemoveStoragePath_ :: Ptr SDLStorage -> CString -> IO Bool
foreign import ccall unsafe "SDL_RenameStoragePath" sdlRenameStoragePath_ :: Ptr SDLStorage -> CString -> CString -> IO Bool
foreign import ccall unsafe "SDL_CopyStorageFile" sdlCopyStorageFile_ :: Ptr SDLStorage -> CString -> CString -> IO Bool
foreign import ccall unsafe "SDL_GetStoragePathInfo" sdlGetStoragePathInfo_ :: Ptr SDLStorage -> CString -> Ptr SDLPathInfo -> IO Bool
foreign import ccall unsafe "SDL_GetStorageSpaceRemaining" sdlGetStorageSpaceRemaining_ :: Ptr SDLStorage -> IO Word64
foreign import ccall unsafe "SDL_GlobStorageDirectory" sdlGlobStorageDirectory_ :: Ptr SDLStorage -> CString -> CString -> CInt -> Ptr CInt -> IO (Ptr CString)

-- Dynamic FunPtr wrapper for callbacks
foreign import ccall "wrapper" mkEnumerateCallback :: (Ptr () -> CString -> CString -> IO CInt) -> IO (FunPtr (Ptr () -> CString -> CString -> IO CInt))

-- Haskell Wrappers

-- | Open a read-only container for the application's filesystem.
sdlOpenTitleStorage :: Maybe String -> SDLPropertiesID -> IO (Maybe (Ptr SDLStorage))
sdlOpenTitleStorage override props =
  maybeWith withCString override $ \overridePtr -> do
    storage <- sdlOpenTitleStorage_ overridePtr props
    return $ if storage == nullPtr then Nothing else Just storage

-- | Open a container for a user's unique read/write filesystem.
sdlOpenUserStorage :: String -> String -> SDLPropertiesID -> IO (Maybe (Ptr SDLStorage))
sdlOpenUserStorage org app props =
  withCString org $ \orgPtr ->
  withCString app $ \appPtr -> do
    storage <- sdlOpenUserStorage_ orgPtr appPtr props
    return $ if storage == nullPtr then Nothing else Just storage

-- | Open a container for local filesystem storage.
sdlOpenFileStorage :: Maybe String -> IO (Maybe (Ptr SDLStorage))
sdlOpenFileStorage path =
  maybeWith withCString path $ \pathPtr -> do
    storage <- sdlOpenFileStorage_ pathPtr
    return $ if storage == nullPtr then Nothing else Just storage

-- | Open a container using a client-provided storage interface.
sdlOpenStorage :: SDLStorageInterface -> Ptr () -> IO (Maybe (Ptr SDLStorage))
sdlOpenStorage iface userdata =
  with iface $ \ifacePtr -> do
    storage <- sdlOpenStorage_ ifacePtr userdata
    return $ if storage == nullPtr then Nothing else Just storage

-- | Close and free a storage container.
sdlCloseStorage :: Ptr SDLStorage -> IO Bool
sdlCloseStorage = sdlCloseStorage_

-- | Check if the storage container is ready to use.
sdlStorageReady :: Ptr SDLStorage -> IO Bool
sdlStorageReady = sdlStorageReady_

-- | Query the size of a file within a storage container.
sdlGetStorageFileSize :: Ptr SDLStorage -> String -> IO (Maybe Word64)
sdlGetStorageFileSize storage path =
  withCString path $ \pathPtr ->
  alloca $ \lengthPtr -> do
    success <- sdlGetStorageFileSize_ storage pathPtr lengthPtr
    if success
      then Just <$> peek lengthPtr
      else return Nothing

-- | Synchronously read a file from a storage container into a buffer.
sdlReadStorageFile :: Ptr SDLStorage -> String -> Ptr () -> Word64 -> IO Bool
sdlReadStorageFile storage path destination len =
  withCString path $ \pathPtr ->
    sdlReadStorageFile_ storage pathPtr destination len

-- | Synchronously write a file from memory into a storage container.
sdlWriteStorageFile :: Ptr SDLStorage -> String -> Ptr () -> Word64 -> IO Bool
sdlWriteStorageFile storage path source len =
  withCString path $ \pathPtr ->
    sdlWriteStorageFile_ storage pathPtr source len

-- | Create a directory in a writable storage container.
sdlCreateStorageDirectory :: Ptr SDLStorage -> String -> IO Bool
sdlCreateStorageDirectory storage path =
  withCString path $ \pathPtr ->
    sdlCreateStorageDirectory_ storage pathPtr

-- | Enumerate a directory in a storage container through a callback.
sdlEnumerateStorageDirectory :: Ptr SDLStorage -> Maybe String -> (Ptr () -> CString -> CString -> IO SDLEnumerationResult) -> Ptr () -> IO Bool
sdlEnumerateStorageDirectory storage path callback userdata =
  maybeWith withCString path $ \pathPtr -> do
    callbackPtr <- mkEnumerateCallback $ \ud p n -> fromIntegral . fromEnum <$> callback ud p n
    result <- sdlEnumerateStorageDirectory_ storage pathPtr callbackPtr userdata
    freeHaskellFunPtr callbackPtr
    return result

-- | Remove a file or an empty directory in a writable storage container.
sdlRemoveStoragePath :: Ptr SDLStorage -> String -> IO Bool
sdlRemoveStoragePath storage path =
  withCString path $ \pathPtr ->
    sdlRemoveStoragePath_ storage pathPtr

-- | Rename a file or directory in a writable storage container.
sdlRenameStoragePath :: Ptr SDLStorage -> String -> String -> IO Bool
sdlRenameStoragePath storage oldpath newpath =
  withCString oldpath $ \oldPtr ->
  withCString newpath $ \newPtr ->
    sdlRenameStoragePath_ storage oldPtr newPtr

-- | Copy a file in a writable storage container.
sdlCopyStorageFile :: Ptr SDLStorage -> String -> String -> IO Bool
sdlCopyStorageFile storage oldpath newpath =
  withCString oldpath $ \oldPtr ->
  withCString newpath $ \newPtr ->
    sdlCopyStorageFile_ storage oldPtr newPtr

-- | Get information about a filesystem path in a storage container.
sdlGetStoragePathInfo :: Ptr SDLStorage -> String -> IO (Maybe SDLPathInfo)
sdlGetStoragePathInfo storage path =
  withCString path $ \pathPtr ->
  alloca $ \infoPtr -> do
    success <- sdlGetStoragePathInfo_ storage pathPtr infoPtr
    if success
      then Just <$> peek infoPtr
      else return Nothing

-- | Query the remaining space in a storage container.
sdlGetStorageSpaceRemaining :: Ptr SDLStorage -> IO Word64
sdlGetStorageSpaceRemaining = sdlGetStorageSpaceRemaining_

-- | Enumerate a directory tree, filtered by pattern, and return a list.
sdlGlobStorageDirectory :: Ptr SDLStorage -> Maybe String -> Maybe String -> SDLGlobFlags -> IO (Maybe [String])
sdlGlobStorageDirectory storage path pattern (SDLGlobFlags flags) =
  maybeWith withCString path $ \pathPtr ->
  maybeWith withCString pattern $ \patternPtr ->
  alloca $ \countPtr -> do
    result <- sdlGlobStorageDirectory_ storage pathPtr patternPtr (fromIntegral flags) countPtr
    if result == nullPtr
      then return Nothing
      else do
        count <- peek countPtr
        strings <- peekArray (fromIntegral count) result >>= mapM peekCString
        free result
        return $ Just strings
