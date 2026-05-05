{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

module SDL3.Raw.Filesystem
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
  , makeEnumerateCallback
  ) where

#include <SDL3/SDL_filesystem.h>

import Data.Bits (Bits)
import Data.Word (Word32, Word64)
import Foreign
import Foreign.C
import SDL3.Raw.Stdinc (SDLTime)

data SDLFolder
  = SDL_FOLDER_HOME
  | SDL_FOLDER_DESKTOP
  | SDL_FOLDER_DOCUMENTS
  | SDL_FOLDER_DOWNLOADS
  | SDL_FOLDER_MUSIC
  | SDL_FOLDER_PICTURES
  | SDL_FOLDER_PUBLICSHARE
  | SDL_FOLDER_SAVEDGAMES
  | SDL_FOLDER_SCREENSHOTS
  | SDL_FOLDER_TEMPLATES
  | SDL_FOLDER_VIDEOS
  deriving (Bounded, Enum, Eq, Show)

data SDLPathType
  = SDL_PATHTYPE_NONE
  | SDL_PATHTYPE_FILE
  | SDL_PATHTYPE_DIRECTORY
  | SDL_PATHTYPE_OTHER
  deriving (Bounded, Enum, Eq, Show)

data SDLPathInfo = SDLPathInfo
  { pathType :: SDLPathType
  , size :: Word64
  , createTime :: SDLTime
  , modifyTime :: SDLTime
  , accessTime :: SDLTime
  } deriving (Eq, Show)

instance Storable SDLPathInfo where
  sizeOf _ = #{size SDL_PathInfo}
  alignment _ = #{alignment SDL_PathInfo}

  peek ptr = do
    pType <- #{peek SDL_PathInfo, type} ptr :: IO CInt
    pSize <- #{peek SDL_PathInfo, size} ptr
    pCreate <- #{peek SDL_PathInfo, create_time} ptr
    pModify <- #{peek SDL_PathInfo, modify_time} ptr
    pAccess <- #{peek SDL_PathInfo, access_time} ptr
    pure $ SDLPathInfo (toEnum (fromIntegral pType)) pSize pCreate pModify pAccess

  poke ptr (SDLPathInfo pType pSize pCreate pModify pAccess) = do
    #{poke SDL_PathInfo, type} ptr (fromEnum pType)
    #{poke SDL_PathInfo, size} ptr pSize
    #{poke SDL_PathInfo, create_time} ptr pCreate
    #{poke SDL_PathInfo, modify_time} ptr pModify
    #{poke SDL_PathInfo, access_time} ptr pAccess

newtype SDLGlobFlags = SDLGlobFlags Word32
  deriving (Bits, Eq, Num, Show)

pattern SDL_GLOB_CASEINSENSITIVE = #{const SDL_GLOB_CASEINSENSITIVE} :: SDLGlobFlags

data SDLEnumerationResult
  = SDL_ENUM_CONTINUE
  | SDL_ENUM_SUCCESS
  | SDL_ENUM_FAILURE
  deriving (Bounded, Enum, Eq, Show)

type SDLEnumerateDirectoryCallback = Ptr () -> CString -> CString -> IO CInt

foreign import ccall unsafe "SDL_GetBasePath"
  sdlGetBasePath :: IO CString

foreign import ccall unsafe "SDL_GetPrefPath"
  c_sdlGetPrefPath :: CString -> CString -> IO CString

sdlGetPrefPath :: CString -> CString -> IO CString
sdlGetPrefPath = c_sdlGetPrefPath

foreign import ccall unsafe "SDL_GetUserFolder"
  c_sdlGetUserFolder :: CInt -> IO CString

sdlGetUserFolder :: SDLFolder -> IO CString
sdlGetUserFolder folder = c_sdlGetUserFolder (fromIntegral (fromEnum folder))

foreign import ccall unsafe "SDL_CreateDirectory"
  sdlCreateDirectory :: CString -> IO Bool

foreign import ccall unsafe "SDL_EnumerateDirectory"
  sdlEnumerateDirectory :: CString -> FunPtr SDLEnumerateDirectoryCallback -> Ptr () -> IO Bool

foreign import ccall unsafe "SDL_RemovePath"
  sdlRemovePath :: CString -> IO Bool

foreign import ccall unsafe "SDL_RenamePath"
  sdlRenamePath :: CString -> CString -> IO Bool

foreign import ccall unsafe "SDL_CopyFile"
  sdlCopyFile :: CString -> CString -> IO Bool

foreign import ccall unsafe "SDL_GetPathInfo"
  sdlGetPathInfo :: CString -> Ptr SDLPathInfo -> IO Bool

foreign import ccall unsafe "SDL_GlobDirectory"
  c_sdlGlobDirectory :: CString -> CString -> Word32 -> Ptr CInt -> IO (Ptr CString)

sdlGlobDirectory :: CString -> CString -> SDLGlobFlags -> Ptr CInt -> IO (Ptr CString)
sdlGlobDirectory path patternPtr (SDLGlobFlags flags) countPtr =
  c_sdlGlobDirectory path patternPtr flags countPtr

foreign import ccall unsafe "SDL_GetCurrentDirectory"
  sdlGetCurrentDirectory :: IO CString

foreign import ccall "wrapper"
  makeEnumerateCallback :: SDLEnumerateDirectoryCallback -> IO (FunPtr SDLEnumerateDirectoryCallback)
