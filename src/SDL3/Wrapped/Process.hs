{-# LANGUAGE PatternSynonyms #-}

module SDL3.Wrapped.Process
  ( SDLProcess
  , withProcessPtr
  , sdlUnsafeFromRawProcess
  , sdlUnsafeToRawProcess
  , sdlCreateProcess
  , sdlCreateProcessWithProperties
  , sdlGetProcessProperties
  , sdlReadProcess
  , sdlGetProcessInput
  , sdlGetProcessOutput
  , sdlKillProcess
  , sdlWaitProcess
  , sdlDestroyProcess
  , SDLProcessIO(..)
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

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign
import Foreign.C
import Foreign.Marshal.Utils (withMany)
import SDL3.IOStream (SDLIOStream, sdlUnsafeFromRawIOStream)
import SDL3.Properties (SDLPropertiesID)
import SDL3.Raw.Process
  ( SDLProcessIO(..)
  , pattern SDL_PROP_PROCESS_BACKGROUND_BOOLEAN
  , pattern SDL_PROP_PROCESS_CREATE_ARGS_POINTER
  , pattern SDL_PROP_PROCESS_CREATE_BACKGROUND_BOOLEAN
  , pattern SDL_PROP_PROCESS_CREATE_ENVIRONMENT_POINTER
  , pattern SDL_PROP_PROCESS_CREATE_STDERR_NUMBER
  , pattern SDL_PROP_PROCESS_CREATE_STDERR_POINTER
  , pattern SDL_PROP_PROCESS_CREATE_STDERR_TO_STDOUT_BOOLEAN
  , pattern SDL_PROP_PROCESS_CREATE_STDIN_NUMBER
  , pattern SDL_PROP_PROCESS_CREATE_STDIN_POINTER
  , pattern SDL_PROP_PROCESS_CREATE_STDOUT_NUMBER
  , pattern SDL_PROP_PROCESS_CREATE_STDOUT_POINTER
  , pattern SDL_PROP_PROCESS_PID_NUMBER
  , pattern SDL_PROP_PROCESS_STDERR_POINTER
  , pattern SDL_PROP_PROCESS_STDIN_POINTER
  , pattern SDL_PROP_PROCESS_STDOUT_POINTER
  )
import qualified SDL3.Raw.Process as Raw

newtype SDLProcess = SDLProcess (Ptr Raw.SDLProcess)
  deriving (Eq)

instance Show SDLProcess where
  show (SDLProcess processPtr) = "SDLProcess " ++ show processPtr

sdlUnsafeFromRawProcess :: Ptr Raw.SDLProcess -> Maybe SDLProcess
sdlUnsafeFromRawProcess processPtr
  | processPtr == nullPtr = Nothing
  | otherwise = Just (SDLProcess processPtr)

sdlUnsafeToRawProcess :: SDLProcess -> Ptr Raw.SDLProcess
sdlUnsafeToRawProcess (SDLProcess processPtr) = processPtr

withProcessPtr :: MonadIO m => SDLProcess -> (Ptr Raw.SDLProcess -> IO a) -> m a
withProcessPtr process action = liftIO $ action (sdlUnsafeToRawProcess process)

sdlCreateProcess :: MonadIO m => [String] -> Bool -> m (Maybe SDLProcess)
sdlCreateProcess args pipeStdio = liftIO $
  withMany withCString args $ \cstrings ->
    withArray0 nullPtr cstrings $ \argsPtr -> do
      process <- Raw.sdlCreateProcessRaw argsPtr pipeStdio
      return $ sdlUnsafeFromRawProcess process

sdlCreateProcessWithProperties :: MonadIO m => SDLPropertiesID -> m (Maybe SDLProcess)
sdlCreateProcessWithProperties props =
  liftIO $ sdlUnsafeFromRawProcess <$> Raw.sdlCreateProcessWithPropertiesRaw props

sdlGetProcessProperties :: MonadIO m => SDLProcess -> m SDLPropertiesID
sdlGetProcessProperties process = liftIO $ Raw.sdlGetProcessPropertiesRaw (sdlUnsafeToRawProcess process)

sdlReadProcess :: MonadIO m => SDLProcess -> m (Maybe (String, Int))
sdlReadProcess process = liftIO $
  alloca $ \sizePtr ->
    alloca $ \exitcodePtr -> do
      dataPtr <- Raw.sdlReadProcessRaw (sdlUnsafeToRawProcess process) sizePtr exitcodePtr
      if dataPtr == nullPtr
        then return Nothing
        else do
          size <- peek sizePtr
          str <- peekCStringLen (castPtr dataPtr, fromIntegral size)
          exitcode <- peek exitcodePtr
          free dataPtr
          return $ Just (str, fromIntegral exitcode)

sdlGetProcessInput :: MonadIO m => SDLProcess -> m (Maybe SDLIOStream)
sdlGetProcessInput process = liftIO $ do
  stream <- Raw.sdlGetProcessInputRaw (sdlUnsafeToRawProcess process)
  return $ sdlUnsafeFromRawIOStream stream

sdlGetProcessOutput :: MonadIO m => SDLProcess -> m (Maybe SDLIOStream)
sdlGetProcessOutput process = liftIO $ do
  stream <- Raw.sdlGetProcessOutputRaw (sdlUnsafeToRawProcess process)
  return $ sdlUnsafeFromRawIOStream stream

sdlKillProcess :: MonadIO m => SDLProcess -> Bool -> m Bool
sdlKillProcess process force = liftIO $ Raw.sdlKillProcessRaw (sdlUnsafeToRawProcess process) force

sdlWaitProcess :: MonadIO m => SDLProcess -> Bool -> m (Bool, Maybe Int)
sdlWaitProcess process block = liftIO $
  alloca $ \exitcodePtr -> do
    exited <- Raw.sdlWaitProcessRaw (sdlUnsafeToRawProcess process) block exitcodePtr
    if exited
      then do
        exitcode <- peek exitcodePtr
        return (True, Just $ fromIntegral exitcode)
      else return (False, Nothing)

sdlDestroyProcess :: MonadIO m => SDLProcess -> m ()
sdlDestroyProcess process = liftIO $ Raw.sdlDestroyProcessRaw (sdlUnsafeToRawProcess process)
