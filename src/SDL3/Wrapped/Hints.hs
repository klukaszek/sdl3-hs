{-# LANGUAGE PatternSynonyms #-}

module SDL3.Wrapped.Hints
  ( module SDL3.Raw.Hints
  , SDLHintCallback
  , SDLHintCallbackHandle(..)
  , sdlSetHintWithPriority
  , sdlSetHint
  , sdlResetHint
  , sdlResetHints
  , sdlGetHint
  , sdlGetHintBoolean
  , sdlAddHintCallback
  , sdlRemoveHintCallback
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign
import Foreign.C
import SDL3.Raw.Hints hiding
  ( SDLHintCallbackRaw
  , mkHintCallback
  , sdlSetHintWithPriority
  , sdlSetHint
  , sdlResetHint
  , sdlResetHints
  , sdlGetHint
  , sdlGetHintBoolean
  , sdlAddHintCallback
  , sdlRemoveHintCallback
  )
import qualified SDL3.Raw.Hints as Raw

type SDLHintCallback = Ptr () -> String -> String -> String -> IO ()

newtype SDLHintCallbackHandle = SDLHintCallbackHandle (FunPtr Raw.SDLHintCallbackRaw)

sdlSetHintWithPriority :: MonadIO m => String -> String -> SDLHintPriority -> m Bool
sdlSetHintWithPriority name value priority =
  liftIO $
    withCString name $ \namePtr ->
      withCString value $ \valuePtr ->
        Raw.sdlSetHintWithPriority namePtr valuePtr priority

sdlSetHint :: MonadIO m => String -> String -> m Bool
sdlSetHint name value =
  liftIO $
    withCString name $ \namePtr ->
      withCString value $ \valuePtr ->
        Raw.sdlSetHint namePtr valuePtr

sdlResetHint :: MonadIO m => String -> m Bool
sdlResetHint name = liftIO $ withCString name Raw.sdlResetHint

sdlResetHints :: MonadIO m => m ()
sdlResetHints = liftIO Raw.sdlResetHints

sdlGetHint :: MonadIO m => String -> m (Maybe String)
sdlGetHint name =
  liftIO $
    withCString name $ \namePtr -> do
      result <- Raw.sdlGetHint namePtr
      if result == nullPtr
        then pure Nothing
        else Just <$> peekCString result

sdlGetHintBoolean :: MonadIO m => String -> Bool -> m Bool
sdlGetHintBoolean name defaultValue =
  liftIO $ withCString name (\namePtr -> Raw.sdlGetHintBoolean namePtr defaultValue)

sdlAddHintCallback :: MonadIO m => String -> SDLHintCallback -> Ptr () -> m (Maybe SDLHintCallbackHandle)
sdlAddHintCallback name callback userdata =
  liftIO $
    withCString name $ \namePtr -> do
      callbackPtr <- Raw.mkHintCallback $ \ud ptrName oldVal newVal -> do
        nameStr <- peekCString ptrName
        oldStr <- peekCString oldVal
        newStr <- peekCString newVal
        callback ud nameStr oldStr newStr
      added <- Raw.sdlAddHintCallback namePtr callbackPtr userdata
      if added
        then pure (Just (SDLHintCallbackHandle callbackPtr))
        else do
          freeHaskellFunPtr callbackPtr
          pure Nothing

sdlRemoveHintCallback :: MonadIO m => String -> SDLHintCallbackHandle -> Ptr () -> m ()
sdlRemoveHintCallback name (SDLHintCallbackHandle callbackPtr) userdata =
  liftIO $
    withCString name $ \namePtr -> do
      Raw.sdlRemoveHintCallback namePtr callbackPtr userdata
      freeHaskellFunPtr callbackPtr
