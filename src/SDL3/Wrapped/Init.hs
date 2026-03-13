{-# LANGUAGE PatternSynonyms #-}

module SDL3.Wrapped.Init
  ( SDLInitFlags
  , pattern SDL_INIT_AUDIO
  , pattern SDL_INIT_VIDEO
  , pattern SDL_INIT_JOYSTICK
  , pattern SDL_INIT_HAPTIC
  , pattern SDL_INIT_GAMEPAD
  , pattern SDL_INIT_EVENTS
  , pattern SDL_INIT_SENSOR
  , pattern SDL_INIT_CAMERA
  , sdlInit
  , sdlInitSubSystem
  , sdlQuitSubSystem
  , sdlWasInit
  , sdlQuit
  , sdlIsMainThread
  , MainThreadCallback
  , sdlRunOnMainThread
  , SDLAppResult(..)
  , SDLAppInitFunc
  , SDLAppIterateFunc
  , SDLAppEventFunc
  , SDLAppQuitFunc
  , sdlSetAppMetadata
  , sdlSetAppMetadataProperty
  , sdlGetAppMetadataProperty
  , propAppMetadataName
  , propAppMetadataVersion
  , propAppMetadataIdentifier
  , propAppMetadataCreator
  , propAppMetadataCopyright
  , propAppMetadataUrl
  , propAppMetadataType
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.&.), (.|.))
import Data.IORef (newIORef, atomicModifyIORef', writeIORef)
import Foreign
import Foreign.C
import qualified SDL3.Raw.Init as Raw
import SDL3.Raw.Init
  ( SDLInitFlags
  , pattern SDL_INIT_AUDIO
  , pattern SDL_INIT_VIDEO
  , pattern SDL_INIT_JOYSTICK
  , pattern SDL_INIT_HAPTIC
  , pattern SDL_INIT_GAMEPAD
  , pattern SDL_INIT_EVENTS
  , pattern SDL_INIT_SENSOR
  , pattern SDL_INIT_CAMERA
  , MainThreadCallback
  , SDLAppResult(..)
  , SDLAppInitFunc
  , SDLAppIterateFunc
  , SDLAppEventFunc
  , SDLAppQuitFunc
  , propAppMetadataName
  , propAppMetadataVersion
  , propAppMetadataIdentifier
  , propAppMetadataCreator
  , propAppMetadataCopyright
  , propAppMetadataUrl
  , propAppMetadataType
  )

combineInitFlags :: [SDLInitFlags] -> Word32
combineInitFlags = foldr (.|.) 0

knownInitFlags :: [SDLInitFlags]
knownInitFlags =
  [ SDL_INIT_AUDIO
  , SDL_INIT_VIDEO
  , SDL_INIT_JOYSTICK
  , SDL_INIT_HAPTIC
  , SDL_INIT_GAMEPAD
  , SDL_INIT_EVENTS
  , SDL_INIT_SENSOR
  , SDL_INIT_CAMERA
  ]

sdlInit :: MonadIO m => [SDLInitFlags] -> m Bool
sdlInit = liftIO . Raw.sdlInit . combineInitFlags

sdlInitSubSystem :: MonadIO m => [SDLInitFlags] -> m Bool
sdlInitSubSystem = liftIO . Raw.sdlInitSubSystem . combineInitFlags

sdlQuitSubSystem :: MonadIO m => [SDLInitFlags] -> m ()
sdlQuitSubSystem = liftIO . Raw.sdlQuitSubSystem . combineInitFlags

sdlWasInit :: MonadIO m => [SDLInitFlags] -> m [SDLInitFlags]
sdlWasInit flags =
  liftIO $ do
    result <- Raw.sdlWasInit (combineInitFlags flags)
    pure $ filter (\flag -> (result .&. flag) /= 0) knownInitFlags

sdlQuit :: MonadIO m => m ()
sdlQuit = liftIO Raw.sdlQuit

sdlIsMainThread :: MonadIO m => m Bool
sdlIsMainThread = liftIO Raw.sdlIsMainThread

sdlRunOnMainThread :: MonadIO m => MainThreadCallback -> Ptr () -> Bool -> m Bool
sdlRunOnMainThread callback userdata waitComplete =
  liftIO $ do
    callbackRef <- newIORef Nothing
    let managedCallback ptr = do
          callback ptr
          funPtr <- atomicModifyIORef' callbackRef (\current -> (Nothing, current))
          maybe (pure ()) freeHaskellFunPtr funPtr
    callbackPtr <- Raw.makeMainThreadCallback managedCallback
    writeIORef callbackRef (Just callbackPtr)
    success <- Raw.sdlRunOnMainThread callbackPtr userdata waitComplete
    if success
      then pure True
      else do
        funPtr <- atomicModifyIORef' callbackRef (\current -> (Nothing, current))
        maybe (pure ()) freeHaskellFunPtr funPtr
        pure False

sdlSetAppMetadata :: MonadIO m => String -> String -> String -> m Bool
sdlSetAppMetadata appName appVersion appIdentifier =
  liftIO $
    withCString appName $ \appNamePtr ->
      withCString appVersion $ \appVersionPtr ->
        withCString appIdentifier $ \appIdentifierPtr ->
          Raw.sdlSetAppMetadata appNamePtr appVersionPtr appIdentifierPtr

sdlSetAppMetadataProperty :: MonadIO m => String -> String -> m Bool
sdlSetAppMetadataProperty name value =
  liftIO $
    withCString name $ \namePtr ->
      withCString value $ \valuePtr ->
        Raw.sdlSetAppMetadataProperty namePtr valuePtr

sdlGetAppMetadataProperty :: MonadIO m => String -> m (Maybe String)
sdlGetAppMetadataProperty name =
  liftIO $
    withCString name $ \namePtr -> do
      valuePtr <- Raw.sdlGetAppMetadataProperty namePtr
      if valuePtr == nullPtr
        then pure Nothing
        else Just <$> peekCString valuePtr
