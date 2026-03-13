{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}

#include <SDL3/SDL_process.h>

module SDL3.Raw.Process
  ( SDLProcess
  , SDLProcessIO(..)
  , sdlCreateProcessRaw
  , sdlCreateProcessWithPropertiesRaw
  , sdlGetProcessPropertiesRaw
  , sdlReadProcessRaw
  , sdlGetProcessInputRaw
  , sdlGetProcessOutputRaw
  , sdlKillProcessRaw
  , sdlWaitProcessRaw
  , sdlDestroyProcessRaw
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

import Foreign
import Foreign.C
import SDL3.Raw.IOStream (SDLIOStream)
import SDL3.Raw.Properties (SDLPropertiesID)

data SDLProcess

data SDLProcessIO
  = SDL_PROCESS_STDIO_INHERITED
  | SDL_PROCESS_STDIO_NULL
  | SDL_PROCESS_STDIO_APP
  | SDL_PROCESS_STDIO_REDIRECT
  deriving (Show, Eq, Bounded, Enum)

foreign import ccall unsafe "SDL_CreateProcess"
  sdlCreateProcessRaw :: Ptr CString -> Bool -> IO (Ptr SDLProcess)

foreign import ccall unsafe "SDL_CreateProcessWithProperties"
  sdlCreateProcessWithPropertiesRaw :: SDLPropertiesID -> IO (Ptr SDLProcess)

foreign import ccall unsafe "SDL_GetProcessProperties"
  sdlGetProcessPropertiesRaw :: Ptr SDLProcess -> IO SDLPropertiesID

foreign import ccall unsafe "SDL_ReadProcess"
  sdlReadProcessRaw :: Ptr SDLProcess -> Ptr CSize -> Ptr CInt -> IO (Ptr ())

foreign import ccall unsafe "SDL_GetProcessInput"
  sdlGetProcessInputRaw :: Ptr SDLProcess -> IO (Ptr SDLIOStream)

foreign import ccall unsafe "SDL_GetProcessOutput"
  sdlGetProcessOutputRaw :: Ptr SDLProcess -> IO (Ptr SDLIOStream)

foreign import ccall unsafe "SDL_KillProcess"
  sdlKillProcessRaw :: Ptr SDLProcess -> Bool -> IO Bool

foreign import ccall unsafe "SDL_WaitProcess"
  sdlWaitProcessRaw :: Ptr SDLProcess -> Bool -> Ptr CInt -> IO Bool

foreign import ccall unsafe "SDL_DestroyProcess"
  sdlDestroyProcessRaw :: Ptr SDLProcess -> IO ()

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

pattern SDL_PROP_PROCESS_PID_NUMBER :: String
pattern SDL_PROP_PROCESS_PID_NUMBER = #{const_str SDL_PROP_PROCESS_PID_NUMBER}

pattern SDL_PROP_PROCESS_STDIN_POINTER :: String
pattern SDL_PROP_PROCESS_STDIN_POINTER = #{const_str SDL_PROP_PROCESS_STDIN_POINTER}

pattern SDL_PROP_PROCESS_STDOUT_POINTER :: String
pattern SDL_PROP_PROCESS_STDOUT_POINTER = #{const_str SDL_PROP_PROCESS_STDOUT_POINTER}

pattern SDL_PROP_PROCESS_STDERR_POINTER :: String
pattern SDL_PROP_PROCESS_STDERR_POINTER = #{const_str SDL_PROP_PROCESS_STDERR_POINTER}

pattern SDL_PROP_PROCESS_BACKGROUND_BOOLEAN :: String
pattern SDL_PROP_PROCESS_BACKGROUND_BOOLEAN = #{const_str SDL_PROP_PROCESS_BACKGROUND_BOOLEAN}
