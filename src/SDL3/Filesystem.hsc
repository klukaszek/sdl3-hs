{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

-- SDL/Filesystem.hsc
{-|
Module      : SDL.Filesystem
Description : Filesystem manipulation functions for SDL3.
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

This module provides Haskell bindings to the SDL3 filesystem functionality.
-}

module SDL3.Filesystem
  (
    -- * Path Functions
    sdlGetBasePath
  , sdlGetPrefPath
  , sdlGetUserFolder
  , sdlGetCurrentDirectory

    -- * Filesystem Operations
  , sdlCreateDirectory
  , sdlEnumerateDirectory
  , sdlRemovePath
  , sdlRenamePath
  , sdlCopyFile
  , sdlGetPathInfo
  , sdlGlobDirectory

    -- * Types and Enums
  , SDLFolder(..)

  , SDLPathType(..)

  , SDLPathInfo(..)
  , SDLGlobFlags(..)
  , pattern SDL_GLOB_CASEINSENSITIVE
  , SDLEnumerationResult(..)
  , SDLEnumerateDirectoryCallback
  ) where

#include <SDL3/SDL_filesystem.h>

import Foreign hiding (free)
import Foreign.C
import SDL3.Stdinc (SDLTime)

-- | Type of OS-provided default folder (enum).
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
  deriving (Show, Eq, Enum, Bounded)

-- | Types of filesystem entries (enum).
data SDLPathType
  = SDL_PATHTYPE_NONE
  | SDL_PATHTYPE_FILE
  | SDL_PATHTYPE_DIRECTORY
  | SDL_PATHTYPE_OTHER
  deriving (Show, Eq, Enum, Bounded)

-- | Information about a path on the filesystem
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
    return $ SDLPathInfo (toEnum $ fromIntegral pType) pSize pCreate pModify pAccess
  poke ptr (SDLPathInfo pType pSize pCreate pModify pAccess) = do
    #{poke SDL_PathInfo, type} ptr (fromEnum pType)
    #{poke SDL_PathInfo, size} ptr pSize
    #{poke SDL_PathInfo, create_time} ptr pCreate
    #{poke SDL_PathInfo, modify_time} ptr pModify
    #{poke SDL_PathInfo, access_time} ptr pAccess

-- | Flags for path matching (bitfield).
newtype SDLGlobFlags = SDLGlobFlags Word32
  deriving (Show, Eq, Bits, Num)

pattern SDL_GLOB_CASEINSENSITIVE = #{const SDL_GLOB_CASEINSENSITIVE} :: SDLGlobFlags

-- | Results from enumeration callback (enum).
data SDLEnumerationResult
  = SDL_ENUM_CONTINUE
  | SDL_ENUM_SUCCESS
  | SDL_ENUM_FAILURE
  deriving (Show, Eq, Enum, Bounded)

-- | Callback for directory enumeration
type SDLEnumerateDirectoryCallback = Ptr () -> CString -> CString -> IO CInt

-- FFI Imports

foreign import ccall unsafe "SDL_GetBasePath" sdlGetBasePath_ :: IO CString
foreign import ccall unsafe "SDL_GetPrefPath" sdlGetPrefPath_ :: CString -> CString -> IO CString
foreign import ccall unsafe "SDL_GetUserFolder" sdlGetUserFolder_ :: CInt -> IO CString
foreign import ccall unsafe "SDL_CreateDirectory" sdlCreateDirectory_ :: CString -> IO Bool
foreign import ccall unsafe "SDL_EnumerateDirectory" sdlEnumerateDirectory_ :: CString -> FunPtr SDLEnumerateDirectoryCallback -> Ptr () -> IO Bool
foreign import ccall unsafe "SDL_RemovePath" sdlRemovePath_ :: CString -> IO Bool
foreign import ccall unsafe "SDL_RenamePath" sdlRenamePath_ :: CString -> CString -> IO Bool
foreign import ccall unsafe "SDL_CopyFile" sdlCopyFile_ :: CString -> CString -> IO Bool
foreign import ccall unsafe "SDL_GetPathInfo" sdlGetPathInfo_ :: CString -> Ptr SDLPathInfo -> IO Bool
foreign import ccall unsafe "SDL_GlobDirectory" sdlGlobDirectory_ :: CString -> CString -> Word32 -> Ptr CInt -> IO (Ptr CString)
foreign import ccall unsafe "SDL_GetCurrentDirectory" sdlGetCurrentDirectory_ :: IO CString

-- | Create a C function pointer from a Haskell SDLEnumerateDirectoryCallback
foreign import ccall "wrapper"
  makeEnumerateCallback :: SDLEnumerateDirectoryCallback -> IO (FunPtr SDLEnumerateDirectoryCallback)

-- Haskell Wrappers

-- | Get the directory where the application was run from
-- Memory is managed by SDL, no manual cleanup required
sdlGetBasePath :: IO (Maybe String)
sdlGetBasePath = do
  path <- sdlGetBasePath_
  if path == nullPtr
    then return Nothing
    else Just <$> peekCString path

-- | Get the user-and-app-specific path where files can be written
sdlGetPrefPath :: String -> String -> IO (Maybe String)
sdlGetPrefPath org app =
  withCString org $ \orgPtr ->
  withCString app $ \appPtr -> do
    path <- sdlGetPrefPath_ orgPtr appPtr
    if path == nullPtr
      then return Nothing
      else do
        result <- peekCString path
        return $ Just result

-- | Find the most suitable user folder for a specific purpose
sdlGetUserFolder :: SDLFolder -> IO (Maybe String)
sdlGetUserFolder folder = do
  path <- sdlGetUserFolder_ (fromIntegral $ fromEnum folder)
  if path == nullPtr
    then return Nothing
    else Just <$> peekCString path

-- | Create a directory and any missing parent directories
sdlCreateDirectory :: String -> IO Bool
sdlCreateDirectory path = withCString path sdlCreateDirectory_

-- | Enumerate a directory through a callback function
sdlEnumerateDirectory :: String -> SDLEnumerateDirectoryCallback -> Ptr () -> IO Bool
sdlEnumerateDirectory path callback userdata =
  withCString path $ \pathPtr -> do
    callbackPtr <- makeEnumerateCallback callback
    result <- sdlEnumerateDirectory_ pathPtr callbackPtr userdata
    freeHaskellFunPtr callbackPtr
    return result

-- | Remove a file or empty directory
sdlRemovePath :: String -> IO Bool
sdlRemovePath path = withCString path sdlRemovePath_

-- | Rename a file or directory
sdlRenamePath :: String -> String -> IO Bool
sdlRenamePath oldpath newpath =
  withCString oldpath $ \oldPtr ->
  withCString newpath $ \newPtr ->
    sdlRenamePath_ oldPtr newPtr

-- | Copy a file
sdlCopyFile :: String -> String -> IO Bool
sdlCopyFile oldpath newpath =
  withCString oldpath $ \oldPtr ->
  withCString newpath $ \newPtr ->
    sdlCopyFile_ oldPtr newPtr

-- | Get information about a filesystem path
sdlGetPathInfo :: String -> IO (Maybe SDLPathInfo)
sdlGetPathInfo path =
  withCString path $ \pathPtr ->
  alloca $ \infoPtr -> do
    success <- sdlGetPathInfo_ pathPtr infoPtr
    if success
      then Just <$> peek infoPtr
      else return Nothing

-- | Enumerate a directory tree, filtered by pattern, and return a list
sdlGlobDirectory :: String -> Maybe String -> SDLGlobFlags -> IO (Maybe [String])
sdlGlobDirectory path pat (SDLGlobFlags flags) =
  withCString path $ \pathPtr ->
  maybeWith withCString pat $ \patternPtr ->
  alloca $ \countPtr -> do
    results <- sdlGlobDirectory_ pathPtr patternPtr flags countPtr
    if results == nullPtr
      then return Nothing
      else do
        count <- peek countPtr
        paths <- peekArray (fromIntegral count) results
        strings <- mapM peekCString paths
        return $ Just strings

-- | Get the current working directory
sdlGetCurrentDirectory :: IO (Maybe String)
sdlGetCurrentDirectory = do
  path <- sdlGetCurrentDirectory_
  if path == nullPtr
    then return Nothing
    else Just <$> peekCString path
