{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : SDL.Log
Description : SDL logging system
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Simple log messages with priorities and categories. A message's
SDL_LogPriority signifies how important the message is. A message's
SDL_LogCategory signifies from what domain it belongs to. Every category
has a minimum priority specified: when a message belongs to that category,
it will only be sent out if it has that minimum priority or higher.

SDL's own logs are sent below the default priority threshold, so they are
quiet by default.

You can change the log verbosity programmatically using 'sdlSetLogPriority'
or with environment variables via SDL hints.
-}

module SDL.Log
  ( -- * Types
    SDLLogCategory(..)
  , SDLLogPriority(..)
  , SDLLogOutputFunction

    -- * Log Management Functions
  , sdlSetLogPriorities
  , sdlSetLogPriority
  , sdlGetLogPriority
  , sdlResetLogPriorities
  , sdlSetLogPriorityPrefix

    -- * Logging Functions
  , sdlLog
  , sdlLogTrace
  , sdlLogVerbose
  , sdlLogDebug
  , sdlLogInfo
  , sdlLogWarn
  , sdlLogError
  , sdlLogCritical
  , sdlLogMessage

    -- * Output Function Management
  , sdlGetDefaultLogOutputFunction
  , sdlGetLogOutputFunction
  , sdlSetLogOutputFunction
  ) where

#include <SDL3/SDL_log.h>

import Foreign.C.Types (CInt(..), CChar(..))
import Foreign.Ptr (Ptr, FunPtr, nullPtr, nullFunPtr)
import Foreign.C.String (CString, withCString)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (alloca)
import Data.Word (Word32)
import Data.Bits ((.|.))

-- | Predefined log categories
data SDLLogCategory
  = SDLLogCategoryApplication
  | SDLLogCategoryError
  | SDLLogCategoryAssert
  | SDLLogCategorySystem
  | SDLLogCategoryAudio
  | SDLLogCategoryVideo
  | SDLLogCategoryRender
  | SDLLogCategoryInput
  | SDLLogCategoryTest
  | SDLLogCategoryGPU
  | SDLLogCategoryCustom
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Convert category to CInt
toSDLLogCategory :: SDLLogCategory -> CInt
toSDLLogCategory cat = case cat of
  SDLLogCategoryApplication -> #const SDL_LOG_CATEGORY_APPLICATION
  SDLLogCategoryError       -> #const SDL_LOG_CATEGORY_ERROR
  SDLLogCategoryAssert      -> #const SDL_LOG_CATEGORY_ASSERT
  SDLLogCategorySystem      -> #const SDL_LOG_CATEGORY_SYSTEM
  SDLLogCategoryAudio       -> #const SDL_LOG_CATEGORY_AUDIO
  SDLLogCategoryVideo       -> #const SDL_LOG_CATEGORY_VIDEO
  SDLLogCategoryRender      -> #const SDL_LOG_CATEGORY_RENDER
  SDLLogCategoryInput       -> #const SDL_LOG_CATEGORY_INPUT
  SDLLogCategoryTest        -> #const SDL_LOG_CATEGORY_TEST
  SDLLogCategoryGPU         -> #const SDL_LOG_CATEGORY_GPU
  SDLLogCategoryCustom      -> #const SDL_LOG_CATEGORY_CUSTOM

-- | Predefined log priorities
data SDLLogPriority
  = SDLLogPriorityInvalid
  | SDLLogPriorityTrace
  | SDLLogPriorityVerbose
  | SDLLogPriorityDebug
  | SDLLogPriorityInfo
  | SDLLogPriorityWarn
  | SDLLogPriorityError
  | SDLLogPriorityCritical
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Convert priority to CInt
toSDLLogPriority :: SDLLogPriority -> CInt
toSDLLogPriority pri = case pri of
  SDLLogPriorityInvalid  -> #const SDL_LOG_PRIORITY_INVALID
  SDLLogPriorityTrace    -> #const SDL_LOG_PRIORITY_TRACE
  SDLLogPriorityVerbose  -> #const SDL_LOG_PRIORITY_VERBOSE
  SDLLogPriorityDebug    -> #const SDL_LOG_PRIORITY_DEBUG
  SDLLogPriorityInfo     -> #const SDL_LOG_PRIORITY_INFO
  SDLLogPriorityWarn     -> #const SDL_LOG_PRIORITY_WARN
  SDLLogPriorityError    -> #const SDL_LOG_PRIORITY_ERROR
  SDLLogPriorityCritical -> #const SDL_LOG_PRIORITY_CRITICAL

-- | Log management functions

foreign import ccall "SDL_SetLogPriorities"
  sdlSetLogPrioritiesRaw :: CInt -> IO ()

sdlSetLogPriorities :: SDLLogPriority -> IO ()
sdlSetLogPriorities priority = sdlSetLogPrioritiesRaw (toSDLLogPriority priority)

foreign import ccall "SDL_SetLogPriority"
  sdlSetLogPriorityRaw :: CInt -> CInt -> IO ()

sdlSetLogPriority :: SDLLogCategory -> SDLLogPriority -> IO ()
sdlSetLogPriority category priority = 
  sdlSetLogPriorityRaw (toSDLLogCategory category) (toSDLLogPriority priority)

foreign import ccall "SDL_GetLogPriority"
  sdlGetLogPriorityRaw :: CInt -> IO CInt

sdlGetLogPriority :: SDLLogCategory -> IO SDLLogPriority
sdlGetLogPriority category = do
  pri <- sdlGetLogPriorityRaw (toSDLLogCategory category)
  return $ toEnum (fromIntegral pri)

foreign import ccall "SDL_ResetLogPriorities"
  sdlResetLogPriorities :: IO ()

foreign import ccall "SDL_SetLogPriorityPrefix"
  sdlSetLogPriorityPrefixRaw :: CInt -> CString -> IO Word32

sdlSetLogPriorityPrefix :: SDLLogPriority -> Maybe String -> IO Bool
sdlSetLogPriorityPrefix priority prefix = do
  res <- case prefix of
    Nothing -> sdlSetLogPriorityPrefixRaw (toSDLLogPriority priority) nullPtr
    Just str -> withCString str $ \cstr -> 
      sdlSetLogPriorityPrefixRaw (toSDLLogPriority priority) cstr
  return (res /= 0)

-- | Logging functions

foreign import ccall "SDL_Log"
  sdlLogRaw :: CString -> IO ()

sdlLog :: String -> IO ()
sdlLog msg = withCString msg sdlLogRaw

foreign import ccall "SDL_LogTrace"
  sdlLogTraceRaw :: CInt -> CString -> IO ()

sdlLogTrace :: SDLLogCategory -> String -> IO ()
sdlLogTrace category msg = 
  withCString msg $ sdlLogTraceRaw (toSDLLogCategory category)

foreign import ccall "SDL_LogVerbose"
  sdlLogVerboseRaw :: CInt -> CString -> IO ()

sdlLogVerbose :: SDLLogCategory -> String -> IO ()
sdlLogVerbose category msg = 
  withCString msg $ sdlLogVerboseRaw (toSDLLogCategory category)

foreign import ccall "SDL_LogDebug"
  sdlLogDebugRaw :: CInt -> CString -> IO ()

sdlLogDebug :: SDLLogCategory -> String -> IO ()
sdlLogDebug category msg = 
  withCString msg $ sdlLogDebugRaw (toSDLLogCategory category)

foreign import ccall "SDL_LogInfo"
  sdlLogInfoRaw :: CInt -> CString -> IO ()

sdlLogInfo :: SDLLogCategory -> String -> IO ()
sdlLogInfo category msg = 
  withCString msg $ sdlLogInfoRaw (toSDLLogCategory category)

foreign import ccall "SDL_LogWarn"
  sdlLogWarnRaw :: CInt -> CString -> IO ()

sdlLogWarn :: SDLLogCategory -> String -> IO ()
sdlLogWarn category msg = 
  withCString msg $ sdlLogWarnRaw (toSDLLogCategory category)

foreign import ccall "SDL_LogError"
  sdlLogErrorRaw :: CInt -> CString -> IO ()

sdlLogError :: SDLLogCategory -> String -> IO ()
sdlLogError category msg = 
  withCString msg $ sdlLogErrorRaw (toSDLLogCategory category)

foreign import ccall "SDL_LogCritical"
  sdlLogCriticalRaw :: CInt -> CString -> IO ()

sdlLogCritical :: SDLLogCategory -> String -> IO ()
sdlLogCritical category msg = 
  withCString msg $ sdlLogCriticalRaw (toSDLLogCategory category)

foreign import ccall "SDL_LogMessage"
  sdlLogMessageRaw :: CInt -> CInt -> CString -> IO ()

sdlLogMessage :: SDLLogCategory -> SDLLogPriority -> String -> IO ()
sdlLogMessage category priority msg = 
  withCString msg $ \cstr ->
    sdlLogMessageRaw (toSDLLogCategory category) (toSDLLogPriority priority) cstr

-- | Log output function management

type SDLLogOutputFunction = Ptr () -> CInt -> CInt -> CString -> IO ()

foreign import ccall "SDL_GetDefaultLogOutputFunction"
  sdlGetDefaultLogOutputFunction :: IO (FunPtr SDLLogOutputFunction)

foreign import ccall "SDL_GetLogOutputFunction"
  sdlGetLogOutputFunctionRaw :: Ptr (FunPtr SDLLogOutputFunction) -> Ptr (Ptr ()) -> IO ()

sdlGetLogOutputFunction :: IO (FunPtr SDLLogOutputFunction, Ptr ())
sdlGetLogOutputFunction = do
  alloca $ \callbackPtr -> do
    alloca $ \userdataPtr -> do
      sdlGetLogOutputFunctionRaw callbackPtr userdataPtr
      callback <- peek callbackPtr
      userdata <- peek userdataPtr
      return (callback, userdata)

foreign import ccall "SDL_SetLogOutputFunction"
  sdlSetLogOutputFunctionRaw :: FunPtr SDLLogOutputFunction -> Ptr () -> IO ()

sdlSetLogOutputFunction :: FunPtr SDLLogOutputFunction -> Ptr () -> IO ()
sdlSetLogOutputFunction callback userdata = 
  sdlSetLogOutputFunctionRaw callback userdata
