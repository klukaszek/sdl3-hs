{-# LANGUAGE PatternSynonyms #-}

module SDL3.Wrapped.Log
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
  , sdlSetLogPriorities
  , sdlSetLogPriority
  , sdlGetLogPriority
  , sdlResetLogPriorities
  , sdlSetLogPriorityPrefix
  , sdlLog
  , sdlLogTrace
  , sdlLogVerbose
  , sdlLogDebug
  , sdlLogInfo
  , sdlLogWarn
  , sdlLogError
  , sdlLogCritical
  , sdlLogMessage
  , sdlGetDefaultLogOutputFunction
  , sdlGetLogOutputFunction
  , sdlSetLogOutputFunction
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.String (withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (maybeWith)
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Storable (peek)
import SDL3.Raw.Log
  ( SDLLogCategory(..)
  , pattern SDL_LOG_CATEGORY_APPLICATION
  , pattern SDL_LOG_CATEGORY_ASSERT
  , pattern SDL_LOG_CATEGORY_AUDIO
  , pattern SDL_LOG_CATEGORY_CUSTOM
  , pattern SDL_LOG_CATEGORY_ERROR
  , pattern SDL_LOG_CATEGORY_GPU
  , pattern SDL_LOG_CATEGORY_INPUT
  , pattern SDL_LOG_CATEGORY_RENDER
  , pattern SDL_LOG_CATEGORY_SYSTEM
  , pattern SDL_LOG_CATEGORY_TEST
  , pattern SDL_LOG_CATEGORY_VIDEO
  , SDLLogOutputFunction
  , SDLLogPriority(..)
  , pattern SDL_LOG_PRIORITY_CRITICAL
  , pattern SDL_LOG_PRIORITY_DEBUG
  , pattern SDL_LOG_PRIORITY_ERROR
  , pattern SDL_LOG_PRIORITY_INFO
  , pattern SDL_LOG_PRIORITY_INVALID
  , pattern SDL_LOG_PRIORITY_TRACE
  , pattern SDL_LOG_PRIORITY_VERBOSE
  , pattern SDL_LOG_PRIORITY_WARN
  )
import qualified SDL3.Raw.Log as Raw

sdlSetLogPriorities :: MonadIO m => SDLLogPriority -> m ()
sdlSetLogPriorities priority =
  liftIO $ Raw.sdlSetLogPrioritiesRaw (fromIntegral $ fromEnum priority)

sdlSetLogPriority :: MonadIO m => SDLLogCategory -> SDLLogPriority -> m ()
sdlSetLogPriority category priority = liftIO $
  Raw.sdlSetLogPriorityRaw
    (fromIntegral $ fromEnum category)
    (fromIntegral $ fromEnum priority)

sdlGetLogPriority :: MonadIO m => SDLLogCategory -> m SDLLogPriority
sdlGetLogPriority category = liftIO $
  toEnum . fromIntegral <$> Raw.sdlGetLogPriorityRaw (fromIntegral $ fromEnum category)

sdlResetLogPriorities :: MonadIO m => m ()
sdlResetLogPriorities = liftIO Raw.sdlResetLogPrioritiesRaw

sdlSetLogPriorityPrefix :: MonadIO m => SDLLogPriority -> Maybe String -> m Bool
sdlSetLogPriorityPrefix priority mPrefix = liftIO $ do
  res <-
    maybeWith withCString mPrefix $
      Raw.sdlSetLogPriorityPrefixRaw (fromIntegral $ fromEnum priority)
  return (res /= 0)

sdlLog :: MonadIO m => String -> m ()
sdlLog msg = liftIO $ withCString msg Raw.sdlLogRaw

sdlLogTrace :: MonadIO m => SDLLogCategory -> String -> m ()
sdlLogTrace category msg = liftIO $
  withCString msg $ \cMsg ->
    Raw.sdlLogTraceRaw (fromIntegral $ fromEnum category) cMsg

sdlLogVerbose :: MonadIO m => SDLLogCategory -> String -> m ()
sdlLogVerbose category msg = liftIO $
  withCString msg $ \cMsg ->
    Raw.sdlLogVerboseRaw (fromIntegral $ fromEnum category) cMsg

sdlLogDebug :: MonadIO m => SDLLogCategory -> String -> m ()
sdlLogDebug category msg = liftIO $
  withCString msg $ \cMsg ->
    Raw.sdlLogDebugRaw (fromIntegral $ fromEnum category) cMsg

sdlLogInfo :: MonadIO m => SDLLogCategory -> String -> m ()
sdlLogInfo category msg = liftIO $
  withCString msg $ \cMsg ->
    Raw.sdlLogInfoRaw (fromIntegral $ fromEnum category) cMsg

sdlLogWarn :: MonadIO m => SDLLogCategory -> String -> m ()
sdlLogWarn category msg = liftIO $
  withCString msg $ \cMsg ->
    Raw.sdlLogWarnRaw (fromIntegral $ fromEnum category) cMsg

sdlLogError :: MonadIO m => SDLLogCategory -> String -> m ()
sdlLogError category msg = liftIO $
  withCString msg $ \cMsg ->
    Raw.sdlLogErrorRaw (fromIntegral $ fromEnum category) cMsg

sdlLogCritical :: MonadIO m => SDLLogCategory -> String -> m ()
sdlLogCritical category msg = liftIO $
  withCString msg $ \cMsg ->
    Raw.sdlLogCriticalRaw (fromIntegral $ fromEnum category) cMsg

sdlLogMessage :: MonadIO m => SDLLogCategory -> SDLLogPriority -> String -> m ()
sdlLogMessage category priority msg = liftIO $
  withCString msg $ \cMsg ->
    Raw.sdlLogMessageRaw
      (fromIntegral $ fromEnum category)
      (fromIntegral $ fromEnum priority)
      cMsg

sdlGetDefaultLogOutputFunction :: MonadIO m => m (FunPtr SDLLogOutputFunction)
sdlGetDefaultLogOutputFunction = liftIO Raw.sdlGetDefaultLogOutputFunctionRaw

sdlGetLogOutputFunction :: MonadIO m => m (FunPtr SDLLogOutputFunction, Ptr ())
sdlGetLogOutputFunction = liftIO $
  alloca $ \callbackPtr ->
    alloca $ \userdataPtr -> do
      Raw.sdlGetLogOutputFunctionRaw callbackPtr userdataPtr
      (,) <$> peek callbackPtr <*> peek userdataPtr

sdlSetLogOutputFunction :: MonadIO m => FunPtr SDLLogOutputFunction -> Ptr () -> m ()
sdlSetLogOutputFunction callback userdata =
  liftIO $ Raw.sdlSetLogOutputFunctionRaw callback userdata
