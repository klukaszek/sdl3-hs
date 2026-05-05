{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

#include <SDL3/SDL_log.h>

module SDL3.Raw.Log
  ( SDLLogCategory(..)
  , pattern SDL_LOG_CATEGORY_APPLICATION
  , pattern SDL_LOG_CATEGORY_ERROR
  , pattern SDL_LOG_CATEGORY_ASSERT
  , pattern SDL_LOG_CATEGORY_SYSTEM
  , pattern SDL_LOG_CATEGORY_AUDIO
  , pattern SDL_LOG_CATEGORY_VIDEO
  , pattern SDL_LOG_CATEGORY_RENDER
  , pattern SDL_LOG_CATEGORY_INPUT
  , pattern SDL_LOG_CATEGORY_TEST
  , pattern SDL_LOG_CATEGORY_GPU
  , pattern SDL_LOG_CATEGORY_CUSTOM
  , SDLLogPriority(..)
  , pattern SDL_LOG_PRIORITY_INVALID
  , pattern SDL_LOG_PRIORITY_TRACE
  , pattern SDL_LOG_PRIORITY_VERBOSE
  , pattern SDL_LOG_PRIORITY_DEBUG
  , pattern SDL_LOG_PRIORITY_INFO
  , pattern SDL_LOG_PRIORITY_WARN
  , pattern SDL_LOG_PRIORITY_ERROR
  , pattern SDL_LOG_PRIORITY_CRITICAL
  , SDLLogOutputFunction
  , sdlSetLogPrioritiesRaw
  , sdlSetLogPriorityRaw
  , sdlGetLogPriorityRaw
  , sdlResetLogPrioritiesRaw
  , sdlSetLogPriorityPrefixRaw
  , sdlLogRaw
  , sdlLogTraceRaw
  , sdlLogVerboseRaw
  , sdlLogDebugRaw
  , sdlLogInfoRaw
  , sdlLogWarnRaw
  , sdlLogErrorRaw
  , sdlLogCriticalRaw
  , sdlLogMessageRaw
  , sdlGetDefaultLogOutputFunctionRaw
  , sdlGetLogOutputFunctionRaw
  , sdlSetLogOutputFunctionRaw
  ) where

import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CUInt(..))
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Storable (Storable)

newtype SDLLogCategory = SDLLogCategory CInt
  deriving newtype (Show, Eq, Ord, Storable, Enum)

pattern SDL_LOG_CATEGORY_APPLICATION = SDLLogCategory #{const SDL_LOG_CATEGORY_APPLICATION}
pattern SDL_LOG_CATEGORY_ERROR = SDLLogCategory #{const SDL_LOG_CATEGORY_ERROR}
pattern SDL_LOG_CATEGORY_ASSERT = SDLLogCategory #{const SDL_LOG_CATEGORY_ASSERT}
pattern SDL_LOG_CATEGORY_SYSTEM = SDLLogCategory #{const SDL_LOG_CATEGORY_SYSTEM}
pattern SDL_LOG_CATEGORY_AUDIO = SDLLogCategory #{const SDL_LOG_CATEGORY_AUDIO}
pattern SDL_LOG_CATEGORY_VIDEO = SDLLogCategory #{const SDL_LOG_CATEGORY_VIDEO}
pattern SDL_LOG_CATEGORY_RENDER = SDLLogCategory #{const SDL_LOG_CATEGORY_RENDER}
pattern SDL_LOG_CATEGORY_INPUT = SDLLogCategory #{const SDL_LOG_CATEGORY_INPUT}
pattern SDL_LOG_CATEGORY_TEST = SDLLogCategory #{const SDL_LOG_CATEGORY_TEST}
pattern SDL_LOG_CATEGORY_GPU = SDLLogCategory #{const SDL_LOG_CATEGORY_GPU}
pattern SDL_LOG_CATEGORY_CUSTOM = SDLLogCategory #{const SDL_LOG_CATEGORY_CUSTOM}

newtype SDLLogPriority = SDLLogPriority CInt
  deriving newtype (Show, Eq, Ord, Storable, Enum)

pattern SDL_LOG_PRIORITY_INVALID = SDLLogPriority #{const SDL_LOG_PRIORITY_INVALID}
pattern SDL_LOG_PRIORITY_TRACE = SDLLogPriority #{const SDL_LOG_PRIORITY_TRACE}
pattern SDL_LOG_PRIORITY_VERBOSE = SDLLogPriority #{const SDL_LOG_PRIORITY_VERBOSE}
pattern SDL_LOG_PRIORITY_DEBUG = SDLLogPriority #{const SDL_LOG_PRIORITY_DEBUG}
pattern SDL_LOG_PRIORITY_INFO = SDLLogPriority #{const SDL_LOG_PRIORITY_INFO}
pattern SDL_LOG_PRIORITY_WARN = SDLLogPriority #{const SDL_LOG_PRIORITY_WARN}
pattern SDL_LOG_PRIORITY_ERROR = SDLLogPriority #{const SDL_LOG_PRIORITY_ERROR}
pattern SDL_LOG_PRIORITY_CRITICAL = SDLLogPriority #{const SDL_LOG_PRIORITY_CRITICAL}

type SDLLogOutputFunction = Ptr () -> CInt -> CInt -> CString -> IO ()

foreign import ccall unsafe "SDL_SetLogPriorities"
  sdlSetLogPrioritiesRaw :: CInt -> IO ()

foreign import ccall unsafe "SDL_SetLogPriority"
  sdlSetLogPriorityRaw :: CInt -> CInt -> IO ()

foreign import ccall unsafe "SDL_GetLogPriority"
  sdlGetLogPriorityRaw :: CInt -> IO CInt

foreign import ccall unsafe "SDL_ResetLogPriorities"
  sdlResetLogPrioritiesRaw :: IO ()

foreign import ccall unsafe "SDL_SetLogPriorityPrefix"
  sdlSetLogPriorityPrefixRaw :: CInt -> CString -> IO CUInt

foreign import ccall unsafe "SDL_Log"
  sdlLogRaw :: CString -> IO ()

foreign import ccall unsafe "SDL_LogTrace"
  sdlLogTraceRaw :: CInt -> CString -> IO ()

foreign import ccall unsafe "SDL_LogVerbose"
  sdlLogVerboseRaw :: CInt -> CString -> IO ()

foreign import ccall unsafe "SDL_LogDebug"
  sdlLogDebugRaw :: CInt -> CString -> IO ()

foreign import ccall unsafe "SDL_LogInfo"
  sdlLogInfoRaw :: CInt -> CString -> IO ()

foreign import ccall unsafe "SDL_LogWarn"
  sdlLogWarnRaw :: CInt -> CString -> IO ()

foreign import ccall unsafe "SDL_LogError"
  sdlLogErrorRaw :: CInt -> CString -> IO ()

foreign import ccall unsafe "SDL_LogCritical"
  sdlLogCriticalRaw :: CInt -> CString -> IO ()

foreign import ccall unsafe "SDL_LogMessage"
  sdlLogMessageRaw :: CInt -> CInt -> CString -> IO ()

foreign import ccall unsafe "SDL_GetDefaultLogOutputFunction"
  sdlGetDefaultLogOutputFunctionRaw :: IO (FunPtr SDLLogOutputFunction)

foreign import ccall unsafe "SDL_GetLogOutputFunction"
  sdlGetLogOutputFunctionRaw :: Ptr (FunPtr SDLLogOutputFunction) -> Ptr (Ptr ()) -> IO ()

foreign import ccall unsafe "SDL_SetLogOutputFunction"
  sdlSetLogOutputFunctionRaw :: FunPtr SDLLogOutputFunction -> Ptr () -> IO ()
