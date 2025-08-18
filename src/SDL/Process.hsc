{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}

-- SDL/Process.hsc
{-|
Module      : SDL.Process
Description : Process control functions for SDL3.
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

This module provides Haskell bindings to the SDL3 process functionality.
-}

module SDL.Process
  (
    -- * Process Creation and Management
    SDLProcess
  , sdlCreateProcess
  , sdlCreateProcessWithProperties
  , sdlGetProcessProperties
  , sdlReadProcess
  , sdlGetProcessInput
  , sdlGetProcessOutput
  , sdlKillProcess
  , sdlWaitProcess
  , sdlDestroyProcess

    -- * Process I/O Options
  , SDLProcessIO(..)

    -- * Property Constants
  , pattern SDL_PROP_PROCESS_CREATE_ARGS_POINTER
  , pattern SDL_PROP_PROCESS_CREATE_ENVIRONMENT_POINTER
  , pattern SDL_PROP_PROCESS_CREATE_STDIN_NUMBER
  , pattern SDL_PROP_PROCESS_CREATE_STDIN_POINTER
  , pattern SDL_PROP_PROCESS_CREATE_STDOUT_NUMBER
  , pattern SDL_PROP_PROCESS_CREATE_STDOUT_POINTER
  , pattern SDL_PROP_PROCESS_CREATE_STDERR_NUMBER
  , pattern SDL_PROP_PROCESS_CREATE_STDERR_POINTER
  , pattern SDL_PROP_PROCESS_CREATE_STDERR_TO_STDOUT_BOOLEAN
  , pattern SDL_PROP_PROCESS_CREATE_BACKGROUND_BOOLEAN
  , pattern SDL_PROP_PROCESS_PID_NUMBER
  , pattern SDL_PROP_PROCESS_STDIN_POINTER
  , pattern SDL_PROP_PROCESS_STDOUT_POINTER
  , pattern SDL_PROP_PROCESS_STDERR_POINTER
  , pattern SDL_PROP_PROCESS_BACKGROUND_BOOLEAN

  ) where

#include <SDL3/SDL_process.h>

import Foreign
import Foreign.C
import SDL.IOStream (SDLIOStream)
import SDL.Properties (SDLPropertiesID)

-- | An opaque handle representing a system process.
data SDLProcess

-- | Description of where standard I/O should be directed when creating a process.
-- C header decl starts at 0, can derive Enum
data SDLProcessIO
  = SDL_PROCESS_STDIO_INHERITED
  | SDL_PROCESS_STDIO_NULL
  | SDL_PROCESS_STDIO_APP
  | SDL_PROCESS_STDIO_REDIRECT
  deriving (Show, Eq, Bounded, Enum)

-- FFI Imports

foreign import ccall unsafe "SDL_CreateProcess" sdlCreateProcess_ :: Ptr CString -> Bool -> IO (Ptr SDLProcess)
foreign import ccall unsafe "SDL_CreateProcessWithProperties" sdlCreateProcessWithProperties :: SDLPropertiesID -> IO (Ptr SDLProcess)
foreign import ccall unsafe "SDL_GetProcessProperties" sdlGetProcessProperties :: Ptr SDLProcess -> IO SDLPropertiesID
foreign import ccall unsafe "SDL_ReadProcess" sdlReadProcess_ :: Ptr SDLProcess -> Ptr CSize -> Ptr CInt -> IO (Ptr ())
foreign import ccall unsafe "SDL_GetProcessInput" sdlGetProcessInput_ :: Ptr SDLProcess -> IO (Ptr SDLIOStream)
foreign import ccall unsafe "SDL_GetProcessOutput" sdlGetProcessOutput_ :: Ptr SDLProcess -> IO (Ptr SDLIOStream)
foreign import ccall unsafe "SDL_KillProcess" sdlKillProcess_ :: Ptr SDLProcess -> Bool -> IO Bool
foreign import ccall unsafe "SDL_WaitProcess" sdlWaitProcess_ :: Ptr SDLProcess -> Bool -> Ptr CInt -> IO Bool
foreign import ccall unsafe "SDL_DestroyProcess" sdlDestroyProcess_ :: Ptr SDLProcess -> IO ()

-- Haskell Wrappers

-- | Create a new process with the given arguments and stdio piping option.
sdlCreateProcess :: [String] -> Bool -> IO (Maybe (Ptr SDLProcess))
sdlCreateProcess args pipeStdio = do
  -- Convert each Haskell String to a CString
  cstrings <- mapM newCString args
  -- Use withArray0 to create a null-terminated array of CStrings
  withArray0 nullPtr cstrings $ \argsPtr -> do
    process <- sdlCreateProcess_ argsPtr pipeStdio
    return $ if process == nullPtr then Nothing else Just process

-- | Read all output from a process, returning the data and exit code.
sdlReadProcess :: Ptr SDLProcess -> IO (Maybe (String, Int))
sdlReadProcess process =
  alloca $ \sizePtr ->
  alloca $ \exitcodePtr -> do
    dataPtr <- sdlReadProcess_ process sizePtr exitcodePtr
    if dataPtr == nullPtr
      then return Nothing
      else do
        size <- peek sizePtr
        str <- peekCStringLen (castPtr dataPtr, fromIntegral size)
        exitcode <- peek exitcodePtr
        free dataPtr
        return $ Just (str, fromIntegral exitcode)

-- | Get the SDL_IOStream for process standard input.
sdlGetProcessInput :: Ptr SDLProcess -> IO (Maybe (Ptr SDLIOStream))
sdlGetProcessInput process = do
  stream <- sdlGetProcessInput_ process
  return $ if stream == nullPtr then Nothing else Just stream

-- | Get the SDL_IOStream for process standard output.
sdlGetProcessOutput :: Ptr SDLProcess -> IO (Maybe (Ptr SDLIOStream))
sdlGetProcessOutput process = do
  stream <- sdlGetProcessOutput_ process
  return $ if stream == nullPtr then Nothing else Just stream

-- | Stop a process.
sdlKillProcess :: Ptr SDLProcess -> Bool -> IO Bool
sdlKillProcess process force = sdlKillProcess_ process force

-- | Wait for a process to finish, optionally blocking.
sdlWaitProcess :: Ptr SDLProcess -> Bool -> IO (Bool, Maybe Int)
sdlWaitProcess process block =
  alloca $ \exitcodePtr -> do
    exited <- sdlWaitProcess_ process block exitcodePtr
    if exited
      then do
        exitcode <- peek exitcodePtr
        return (True, Just $ fromIntegral exitcode)
      else return (False, Nothing)

-- | Destroy a previously created process object.
sdlDestroyProcess :: Ptr SDLProcess -> IO ()
sdlDestroyProcess = sdlDestroyProcess_

-- Property Constants

pattern SDL_PROP_PROCESS_CREATE_ARGS_POINTER :: String
pattern SDL_PROP_PROCESS_CREATE_ARGS_POINTER = #{const_str SDL_PROP_PROCESS_CREATE_ARGS_POINTER}

pattern SDL_PROP_PROCESS_CREATE_ENVIRONMENT_POINTER :: String
pattern SDL_PROP_PROCESS_CREATE_ENVIRONMENT_POINTER = #{const_str SDL_PROP_PROCESS_CREATE_ENVIRONMENT_POINTER}

pattern SDL_PROP_PROCESS_CREATE_STDIN_NUMBER :: String
pattern SDL_PROP_PROCESS_CREATE_STDIN_NUMBER = #{const_str SDL_PROP_PROCESS_CREATE_STDIN_NUMBER}

pattern SDL_PROP_PROCESS_CREATE_STDIN_POINTER :: String
pattern SDL_PROP_PROCESS_CREATE_STDIN_POINTER = #{const_str SDL_PROP_PROCESS_CREATE_STDIN_POINTER}

pattern SDL_PROP_PROCESS_CREATE_STDOUT_NUMBER :: String
pattern SDL_PROP_PROCESS_CREATE_STDOUT_NUMBER = #{const_str SDL_PROP_PROCESS_CREATE_STDOUT_NUMBER}

pattern SDL_PROP_PROCESS_CREATE_STDOUT_POINTER :: String
pattern SDL_PROP_PROCESS_CREATE_STDOUT_POINTER = #{const_str SDL_PROP_PROCESS_CREATE_STDOUT_POINTER}

pattern SDL_PROP_PROCESS_CREATE_STDERR_NUMBER :: String
pattern SDL_PROP_PROCESS_CREATE_STDERR_NUMBER = #{const_str SDL_PROP_PROCESS_CREATE_STDERR_NUMBER}

pattern SDL_PROP_PROCESS_CREATE_STDERR_POINTER :: String
pattern SDL_PROP_PROCESS_CREATE_STDERR_POINTER = #{const_str SDL_PROP_PROCESS_CREATE_STDERR_POINTER}

pattern SDL_PROP_PROCESS_CREATE_STDERR_TO_STDOUT_BOOLEAN :: String
pattern SDL_PROP_PROCESS_CREATE_STDERR_TO_STDOUT_BOOLEAN = #{const_str SDL_PROP_PROCESS_CREATE_STDERR_TO_STDOUT_BOOLEAN}

pattern SDL_PROP_PROCESS_CREATE_BACKGROUND_BOOLEAN :: String
pattern SDL_PROP_PROCESS_CREATE_BACKGROUND_BOOLEAN = #{const_str SDL_PROP_PROCESS_CREATE_BACKGROUND_BOOLEAN}

pattern SDL_PROP_PROCESS_PID_NUMBER:: String
pattern SDL_PROP_PROCESS_PID_NUMBER = #{const_str SDL_PROP_PROCESS_PID_NUMBER}

pattern SDL_PROP_PROCESS_STDIN_POINTER :: String
pattern SDL_PROP_PROCESS_STDIN_POINTER = #{const_str SDL_PROP_PROCESS_STDIN_POINTER}

pattern SDL_PROP_PROCESS_STDOUT_POINTER :: String
pattern SDL_PROP_PROCESS_STDOUT_POINTER = #{const_str SDL_PROP_PROCESS_STDOUT_POINTER}

pattern SDL_PROP_PROCESS_STDERR_POINTER :: String
pattern SDL_PROP_PROCESS_STDERR_POINTER = #{const_str SDL_PROP_PROCESS_STDERR_POINTER}

pattern SDL_PROP_PROCESS_BACKGROUND_BOOLEAN :: String
pattern SDL_PROP_PROCESS_BACKGROUND_BOOLEAN = #{const_str SDL_PROP_PROCESS_BACKGROUND_BOOLEAN}
