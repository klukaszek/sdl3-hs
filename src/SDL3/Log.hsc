-- SDL/Log.hsc
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : SDL.Log
-- Description : SDL logging system
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Simple log messages with priorities and categories. A message's
-- SDL_LogPriority signifies how important the message is. A message's
-- SDL_LogCategory signifies from what domain it belongs to. Every category
-- has a minimum priority specified: when a message belongs to that category,
-- it will only be sent out if it has that minimum priority or higher.
--
-- SDL's own logs are sent below the default priority threshold, so they are
-- quiet by default.
--
-- You can change the log verbosity programmatically using 'sdlSetLogPriority'
-- or with environment variables via SDL hints.

module SDL3.Log
  ( -- * Types
    SDLLogCategory(..)
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

import Foreign.C.Types (CInt(..), CUInt(..))
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.String (CString, withCString)
import Foreign.Storable (Storable(..), peek)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (maybeWith)

-- | Log categories. Use pattern synonyms for predefined categories.
-- Custom categories start from `SDL_LOG_CATEGORY_CUSTOM`.
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

-- | Log priorities.
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

-- | The prototype for the log output function
type SDLLogOutputFunction = Ptr () -> CInt -> CInt -> CString -> IO ()

-- * Log Management Functions

-- | Set the priority of all log categories.
foreign import ccall unsafe "SDL_SetLogPriorities"
  c_sdlSetLogPriorities :: CInt -> IO ()

sdlSetLogPriorities :: SDLLogPriority -> IO ()
sdlSetLogPriorities priority = c_sdlSetLogPriorities (fromIntegral $ fromEnum priority) -- Int -> CInt

-- | Set the priority of a particular log category.
foreign import ccall unsafe "SDL_SetLogPriority"
  c_sdlSetLogPriority :: CInt -> CInt -> IO ()

sdlSetLogPriority :: SDLLogCategory -> SDLLogPriority -> IO ()
sdlSetLogPriority category priority =
  c_sdlSetLogPriority (fromIntegral $ fromEnum category) (fromIntegral $ fromEnum priority) -- Int -> CInt

-- | Get the priority of a particular log category.
foreign import ccall unsafe "SDL_GetLogPriority"
  c_sdlGetLogPriority :: CInt -> IO CInt

sdlGetLogPriority :: SDLLogCategory -> IO SDLLogPriority
sdlGetLogPriority category = toEnum . fromIntegral <$> c_sdlGetLogPriority (fromIntegral $ fromEnum category) -- Int -> CInt

-- | Reset all priorities to default.
foreign import ccall unsafe "SDL_ResetLogPriorities"
  sdlResetLogPriorities :: IO ()

-- | Set the prefix for a log priority.
-- Returns True if the prefix was set, False otherwise (invalid priority).
foreign import ccall unsafe "SDL_SetLogPriorityPrefix"
  c_sdlSetLogPriorityPrefix :: CInt -> CString -> IO CUInt

sdlSetLogPriorityPrefix :: SDLLogPriority -> Maybe String -> IO Bool
sdlSetLogPriorityPrefix priority mPrefix = do
  res <- maybeWith withCString mPrefix $ \cPrefix ->
           c_sdlSetLogPriorityPrefix (fromIntegral $ fromEnum priority) cPrefix -- Int -> CInt
  return (res /= 0)

-- * Logging Functions

-- | Log a message with SDL_LOG_CATEGORY_APPLICATION and SDL_LOG_PRIORITY_INFO.
foreign import ccall unsafe "SDL_Log"
  c_sdlLog :: CString -> IO ()

sdlLog :: String -> IO ()
sdlLog msg = withCString msg c_sdlLog

-- | Log a message with SDL_LOG_PRIORITY_TRACE.
foreign import ccall unsafe "SDL_LogTrace"
  c_sdlLogTrace :: CInt -> CString -> IO ()

sdlLogTrace :: SDLLogCategory -> String -> IO ()
sdlLogTrace category msg =
  withCString msg $ \cMsg ->
    c_sdlLogTrace (fromIntegral $ fromEnum category) cMsg -- Int -> CInt

-- | Log a message with SDL_LOG_PRIORITY_VERBOSE.
foreign import ccall unsafe "SDL_LogVerbose"
  c_sdlLogVerbose :: CInt -> CString -> IO ()

sdlLogVerbose :: SDLLogCategory -> String -> IO ()
sdlLogVerbose category msg =
  withCString msg $ \cMsg ->
    c_sdlLogVerbose (fromIntegral $ fromEnum category) cMsg -- Int -> CInt

-- | Log a message with SDL_LOG_PRIORITY_DEBUG.
foreign import ccall unsafe "SDL_LogDebug"
  c_sdlLogDebug :: CInt -> CString -> IO ()

sdlLogDebug :: SDLLogCategory -> String -> IO ()
sdlLogDebug category msg =
  withCString msg $ \cMsg ->
    c_sdlLogDebug (fromIntegral $ fromEnum category) cMsg -- Int -> CInt

-- | Log a message with SDL_LOG_PRIORITY_INFO.
foreign import ccall unsafe "SDL_LogInfo"
  c_sdlLogInfo :: CInt -> CString -> IO ()

sdlLogInfo :: SDLLogCategory -> String -> IO ()
sdlLogInfo category msg =
  withCString msg $ \cMsg ->
    c_sdlLogInfo (fromIntegral $ fromEnum category) cMsg -- Int -> CInt

-- | Log a message with SDL_LOG_PRIORITY_WARN.
foreign import ccall unsafe "SDL_LogWarn"
  c_sdlLogWarn :: CInt -> CString -> IO ()

sdlLogWarn :: SDLLogCategory -> String -> IO ()
sdlLogWarn category msg =
  withCString msg $ \cMsg ->
    c_sdlLogWarn (fromIntegral $ fromEnum category) cMsg -- Int -> CInt

-- | Log a message with SDL_LOG_PRIORITY_ERROR.
foreign import ccall unsafe "SDL_LogError"
  c_sdlLogError :: CInt -> CString -> IO ()

sdlLogError :: SDLLogCategory -> String -> IO ()
sdlLogError category msg =
  withCString msg $ \cMsg ->
    c_sdlLogError (fromIntegral $ fromEnum category) cMsg -- Int -> CInt

-- | Log a message with SDL_LOG_PRIORITY_CRITICAL.
foreign import ccall unsafe "SDL_LogCritical"
  c_sdlLogCritical :: CInt -> CString -> IO ()

sdlLogCritical :: SDLLogCategory -> String -> IO ()
sdlLogCritical category msg =
  withCString msg $ \cMsg ->
    c_sdlLogCritical (fromIntegral $ fromEnum category) cMsg -- Int -> CInt

-- | Log a message with the specified category and priority.
foreign import ccall unsafe "SDL_LogMessage"
  c_sdlLogMessage :: CInt -> CInt -> CString -> IO ()

sdlLogMessage :: SDLLogCategory -> SDLLogPriority -> String -> IO ()
sdlLogMessage category priority msg =
  withCString msg $ \cMsg ->
    c_sdlLogMessage (fromIntegral $ fromEnum category) (fromIntegral $ fromEnum priority) cMsg -- Int -> CInt

-- * Output Function Management

-- | Get the default log output function.
foreign import ccall unsafe "SDL_GetDefaultLogOutputFunction"
  sdlGetDefaultLogOutputFunction :: IO (FunPtr SDLLogOutputFunction)

-- | Get the current log output function.
foreign import ccall unsafe "SDL_GetLogOutputFunction"
  c_sdlGetLogOutputFunction :: Ptr (FunPtr SDLLogOutputFunction) -> Ptr (Ptr ()) -> IO ()

sdlGetLogOutputFunction :: IO (FunPtr SDLLogOutputFunction, Ptr ())
sdlGetLogOutputFunction =
  alloca $ \callbackPtr ->
  alloca $ \userdataPtr -> do
    c_sdlGetLogOutputFunction callbackPtr userdataPtr
    (,) <$> peek callbackPtr <*> peek userdataPtr

-- | Set the log output function.
foreign import ccall unsafe "SDL_SetLogOutputFunction"
  c_sdlSetLogOutputFunction :: FunPtr SDLLogOutputFunction -> Ptr () -> IO ()

sdlSetLogOutputFunction :: FunPtr SDLLogOutputFunction -> Ptr () -> IO ()
sdlSetLogOutputFunction = c_sdlSetLogOutputFunction -- Direct passthrough
