{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SDL3.Raw.Storage
  ( SDLStorageInterface(..)
  , SDLStorage
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
  , mkEnumerateCallback
  ) where

#include <SDL3/SDL_storage.h>

import Foreign
import Foreign.C
import SDL3.Raw.Filesystem (SDLGlobFlags(..), SDLPathInfo)
import SDL3.Raw.Properties (SDLPropertiesID)

data SDLStorage

data SDLStorageInterface = SDLStorageInterface
  { version :: Word32
  , close :: FunPtr (Ptr () -> IO Bool)
  , ready :: FunPtr (Ptr () -> IO Bool)
  , enumerate :: FunPtr (Ptr () -> CString -> FunPtr (Ptr () -> CString -> CString -> IO CInt) -> Ptr () -> IO Bool)
  , info :: FunPtr (Ptr () -> CString -> Ptr SDLPathInfo -> IO Bool)
  , readFile :: FunPtr (Ptr () -> CString -> Ptr () -> Word64 -> IO Bool)
  , writeFile :: FunPtr (Ptr () -> CString -> Ptr () -> Word64 -> IO Bool)
  , mkdir :: FunPtr (Ptr () -> CString -> IO Bool)
  , remove :: FunPtr (Ptr () -> CString -> IO Bool)
  , rename :: FunPtr (Ptr () -> CString -> CString -> IO Bool)
  , copy :: FunPtr (Ptr () -> CString -> CString -> IO Bool)
  , spaceRemaining :: FunPtr (Ptr () -> IO Word64)
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
    pure $ SDLStorageInterface v c r e i rf wf m rm rn cp sr

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

foreign import ccall unsafe "SDL_OpenTitleStorage"
  sdlOpenTitleStorage :: CString -> SDLPropertiesID -> IO (Ptr SDLStorage)

foreign import ccall unsafe "SDL_OpenUserStorage"
  sdlOpenUserStorage :: CString -> CString -> SDLPropertiesID -> IO (Ptr SDLStorage)

foreign import ccall unsafe "SDL_OpenFileStorage"
  sdlOpenFileStorage :: CString -> IO (Ptr SDLStorage)

foreign import ccall unsafe "SDL_OpenStorage"
  sdlOpenStorage :: Ptr SDLStorageInterface -> Ptr () -> IO (Ptr SDLStorage)

foreign import ccall unsafe "SDL_CloseStorage"
  sdlCloseStorage :: Ptr SDLStorage -> IO Bool

foreign import ccall unsafe "SDL_StorageReady"
  sdlStorageReady :: Ptr SDLStorage -> IO Bool

foreign import ccall unsafe "SDL_GetStorageFileSize"
  sdlGetStorageFileSize :: Ptr SDLStorage -> CString -> Ptr Word64 -> IO Bool

foreign import ccall unsafe "SDL_ReadStorageFile"
  sdlReadStorageFile :: Ptr SDLStorage -> CString -> Ptr () -> Word64 -> IO Bool

foreign import ccall unsafe "SDL_WriteStorageFile"
  sdlWriteStorageFile :: Ptr SDLStorage -> CString -> Ptr () -> Word64 -> IO Bool

foreign import ccall unsafe "SDL_CreateStorageDirectory"
  sdlCreateStorageDirectory :: Ptr SDLStorage -> CString -> IO Bool

foreign import ccall safe "SDL_EnumerateStorageDirectory"
  sdlEnumerateStorageDirectory :: Ptr SDLStorage -> CString -> FunPtr (Ptr () -> CString -> CString -> IO CInt) -> Ptr () -> IO Bool

foreign import ccall unsafe "SDL_RemoveStoragePath"
  sdlRemoveStoragePath :: Ptr SDLStorage -> CString -> IO Bool

foreign import ccall unsafe "SDL_RenameStoragePath"
  sdlRenameStoragePath :: Ptr SDLStorage -> CString -> CString -> IO Bool

foreign import ccall unsafe "SDL_CopyStorageFile"
  sdlCopyStorageFile :: Ptr SDLStorage -> CString -> CString -> IO Bool

foreign import ccall unsafe "SDL_GetStoragePathInfo"
  sdlGetStoragePathInfo :: Ptr SDLStorage -> CString -> Ptr SDLPathInfo -> IO Bool

foreign import ccall unsafe "SDL_GetStorageSpaceRemaining"
  sdlGetStorageSpaceRemaining :: Ptr SDLStorage -> IO Word64

foreign import ccall unsafe "SDL_GlobStorageDirectory"
  c_sdlGlobStorageDirectory :: Ptr SDLStorage -> CString -> CString -> CInt -> Ptr CInt -> IO (Ptr CString)

sdlGlobStorageDirectory :: Ptr SDLStorage -> CString -> CString -> SDLGlobFlags -> Ptr CInt -> IO (Ptr CString)
sdlGlobStorageDirectory storage path patternPtr (SDLGlobFlags flags) countPtr =
  c_sdlGlobStorageDirectory storage path patternPtr (fromIntegral flags) countPtr

foreign import ccall "wrapper"
  mkEnumerateCallback :: (Ptr () -> CString -> CString -> IO CInt) -> IO (FunPtr (Ptr () -> CString -> CString -> IO CInt))
