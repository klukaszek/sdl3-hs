{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- SDL/Filesystem.hsc
{-|
Module      : SDL.Filesystem
Description : Filesystem manipulation functions for SDL3.
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

This module provides Haskell bindings to the SDL3 filesystem functionality.
-}

module SDL.Filesystem
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
  , sdlFolderHOME
  , sdlFolderDESKTOP
  , sdlFolderDOCUMENTS
  , sdlFolderDOWNLOADS
  , sdlFolderMUSIC
  , sdlFolderPICTURES
  , sdlFolderPUBLICSHARE
  , sdlFolderSAVEDGAMES
  , sdlFolderSCREENSHOTS
  , sdlFolderTEMPLATES
  , sdlFolderVIDEOS
  , SDLPathType(..)
  , sdlPathTypeNONE
  , sdlPathTypeFILE
  , sdlPathTypeDIRECTORY
  , sdlPathTypeOTHER
  , SDLPathInfo(..)
  , SDLGlobFlags(..)
  , sdlGlobCaseInsensitive
  , SDLEnumerationResult(..)
  , sdlEnumCONTINUE
  , sdlEnumSUCCESS
  , sdlEnumFAILURE
  , SDLEnumerateDirectoryCallback
  ) where

#include <SDL3/SDL_filesystem.h>

import Foreign hiding (free)
import Foreign.C
import SDL.Stdinc (SDLTime, free)
import Data.Bits (Bits)
import Control.Exception (bracket)
import Control.Monad (when)

-- | Type of OS-provided default folder (enum).
newtype SDLFolder = SDLFolder { unSDLFolder :: CInt }
  deriving (Show, Eq, Bits)

#{enum SDLFolder, SDLFolder
 , sdlFolderHOME = SDL_FOLDER_HOME
 , sdlFolderDESKTOP = SDL_FOLDER_DESKTOP
 , sdlFolderDOCUMENTS = SDL_FOLDER_DOCUMENTS
 , sdlFolderDOWNLOADS = SDL_FOLDER_DOWNLOADS
 , sdlFolderMUSIC = SDL_FOLDER_MUSIC
 , sdlFolderPICTURES = SDL_FOLDER_PICTURES
 , sdlFolderPUBLICSHARE = SDL_FOLDER_PUBLICSHARE
 , sdlFolderSAVEDGAMES = SDL_FOLDER_SAVEDGAMES
 , sdlFolderSCREENSHOTS = SDL_FOLDER_SCREENSHOTS
 , sdlFolderTEMPLATES = SDL_FOLDER_TEMPLATES
 , sdlFolderVIDEOS = SDL_FOLDER_VIDEOS
 }

-- | Types of filesystem entries (enum).
newtype SDLPathType = SDLPathType { unSDLPathType :: CInt }
  deriving (Show, Eq, Bits)

#{enum SDLPathType, SDLPathType
 , sdlPathTypeNONE = SDL_PATHTYPE_NONE
 , sdlPathTypeFILE = SDL_PATHTYPE_FILE
 , sdlPathTypeDIRECTORY = SDL_PATHTYPE_DIRECTORY
 , sdlPathTypeOTHER = SDL_PATHTYPE_OTHER
 }

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
    return $ SDLPathInfo (SDLPathType pType) pSize pCreate pModify pAccess
  poke ptr (SDLPathInfo pType pSize pCreate pModify pAccess) = do
    #{poke SDL_PathInfo, type} ptr (unSDLPathType pType :: CInt)
    #{poke SDL_PathInfo, size} ptr pSize
    #{poke SDL_PathInfo, create_time} ptr pCreate
    #{poke SDL_PathInfo, modify_time} ptr pModify
    #{poke SDL_PathInfo, access_time} ptr pAccess

-- | Flags for path matching (bitfield).
newtype SDLGlobFlags = SDLGlobFlags { unSDLGlobFlags :: Word32 }
  deriving (Show, Eq, Bits)

sdlGlobCaseInsensitive :: SDLGlobFlags
sdlGlobCaseInsensitive = SDLGlobFlags #{const SDL_GLOB_CASEINSENSITIVE}

-- | Results from enumeration callback (enum).
newtype SDLEnumerationResult = SDLEnumerationResult { unSDLEnumerationResult :: CInt }
  deriving (Show, Eq, Bits)

#{enum SDLEnumerationResult, SDLEnumerationResult
 , sdlEnumCONTINUE = SDL_ENUM_CONTINUE
 , sdlEnumSUCCESS = SDL_ENUM_SUCCESS
 , sdlEnumFAILURE = SDL_ENUM_FAILURE
 }

-- | Callback for directory enumeration
type SDLEnumerateDirectoryCallback = Ptr () -> CString -> CString -> IO SDLEnumerationResult

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
-- Requires cleanup via 'free' from SDL.Stdinc
sdlGetBasePath :: IO (Maybe String)
sdlGetBasePath = bracket
    -- Acquire: Get the pointer from SDL
    sdlGetBasePath_
    -- Release: Free the pointer if it wasn't null
    (\ptr -> when (ptr /= nullPtr) (free ptr))
    -- Use: If pointer is valid, copy it into a Haskell String.
    --      If pointer is null, result is Nothing.
    (\ptr -> if ptr == nullPtr
                then return Nothing -- Propagate failure as Nothing
                else Just <$> peekCString ptr -- Copy to Haskell String
    )

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
        free path
        return $ Just result

-- | Find the most suitable user folder for a specific purpose
sdlGetUserFolder :: SDLFolder -> IO (Maybe String)
sdlGetUserFolder folder = do
  path <- sdlGetUserFolder_ (unSDLFolder folder)
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
sdlGlobDirectory path pattern flags =
  withCString path $ \pathPtr ->
  maybeWith withCString pattern $ \patternPtr ->
  alloca $ \countPtr -> do
    results <- sdlGlobDirectory_ pathPtr patternPtr (unSDLGlobFlags flags) countPtr
    if results == nullPtr
      then return Nothing
      else do
        count <- peek countPtr
        paths <- peekArray (fromIntegral count) results
        strings <- mapM peekCString paths
        free results
        return $ Just strings

-- | Get the current working directory
sdlGetCurrentDirectory :: IO (Maybe String)
sdlGetCurrentDirectory = do
  path <- sdlGetCurrentDirectory_
  if path == nullPtr
    then return Nothing
    else do
      result <- peekCString path
      free path
      return $ Just result
