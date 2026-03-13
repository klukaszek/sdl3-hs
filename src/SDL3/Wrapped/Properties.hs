{-# LANGUAGE PatternSynonyms #-}

module SDL3.Wrapped.Properties
  ( SDLPropertiesID
  , SDLPropertyType(..)
  , SDLCleanupPropertyCallback
  , SDLEnumeratePropertiesCallback
  , sdlGetGlobalProperties
  , sdlCreateProperties
  , sdlCopyProperties
  , sdlLockProperties
  , sdlUnlockProperties
  , sdlDestroyProperties
  , sdlSetPointerPropertyWithCleanup
  , sdlSetPointerProperty
  , sdlSetStringProperty
  , sdlSetNumberProperty
  , sdlSetFloatProperty
  , sdlSetBooleanProperty
  , sdlHasProperty
  , sdlGetPropertyType
  , sdlGetPointerProperty
  , sdlGetStringProperty
  , sdlGetNumberProperty
  , sdlGetFloatProperty
  , sdlGetBooleanProperty
  , sdlClearProperty
  , sdlEnumerateProperties
  , pattern SDL_PROP_NAME_STRING
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import SDL3.Raw.Properties
  ( SDLPropertiesID
  , SDLPropertyType(..)
  , SDLCleanupPropertyCallback
  , SDLEnumeratePropertiesCallback
  , pattern SDL_PROP_NAME_STRING
  )
import qualified SDL3.Raw.Properties as Raw

sdlGetGlobalProperties :: MonadIO m => m SDLPropertiesID
sdlGetGlobalProperties = liftIO Raw.sdlGetGlobalPropertiesRaw

sdlCreateProperties :: MonadIO m => m SDLPropertiesID
sdlCreateProperties = liftIO Raw.sdlCreatePropertiesRaw

sdlCopyProperties :: MonadIO m => SDLPropertiesID -> SDLPropertiesID -> m Bool
sdlCopyProperties src dst = liftIO $ Raw.sdlCopyPropertiesRaw src dst

sdlLockProperties :: MonadIO m => SDLPropertiesID -> m Bool
sdlLockProperties = liftIO . Raw.sdlLockPropertiesRaw

sdlUnlockProperties :: MonadIO m => SDLPropertiesID -> m ()
sdlUnlockProperties = liftIO . Raw.sdlUnlockPropertiesRaw

sdlDestroyProperties :: MonadIO m => SDLPropertiesID -> m ()
sdlDestroyProperties = liftIO . Raw.sdlDestroyPropertiesRaw

sdlSetPointerPropertyWithCleanup ::
  MonadIO m =>
  SDLPropertiesID ->
  String ->
  Ptr () ->
  SDLCleanupPropertyCallback ->
  Ptr () ->
  m Bool
sdlSetPointerPropertyWithCleanup props name value cleanup userdata = liftIO $
  withCString name $ \cname ->
    Raw.sdlSetPointerPropertyWithCleanupRaw props cname value cleanup userdata

sdlSetPointerProperty :: MonadIO m => SDLPropertiesID -> String -> Ptr () -> m Bool
sdlSetPointerProperty props name value = liftIO $
  withCString name $ \cname ->
    Raw.sdlSetPointerPropertyRaw props cname value

sdlSetStringProperty :: MonadIO m => SDLPropertiesID -> String -> String -> m Bool
sdlSetStringProperty props name value = liftIO $
  withCString name $ \cname ->
    withCString value $ \cvalue ->
      Raw.sdlSetStringPropertyRaw props cname cvalue

sdlSetNumberProperty :: MonadIO m => SDLPropertiesID -> String -> Int64 -> m Bool
sdlSetNumberProperty props name value = liftIO $
  withCString name $ \cname ->
    Raw.sdlSetNumberPropertyRaw props cname value

sdlSetFloatProperty :: MonadIO m => SDLPropertiesID -> String -> Float -> m Bool
sdlSetFloatProperty props name value = liftIO $
  withCString name $ \cname ->
    Raw.sdlSetFloatPropertyRaw props cname (CFloat value)

sdlSetBooleanProperty :: MonadIO m => SDLPropertiesID -> String -> Bool -> m Bool
sdlSetBooleanProperty props name value = liftIO $
  withCString name $ \cname ->
    Raw.sdlSetBooleanPropertyRaw props cname value

sdlHasProperty :: MonadIO m => SDLPropertiesID -> String -> m Bool
sdlHasProperty props name = liftIO $
  withCString name $ \cname ->
    Raw.sdlHasPropertyRaw props cname

sdlGetPropertyType :: MonadIO m => SDLPropertiesID -> String -> m SDLPropertyType
sdlGetPropertyType props name = liftIO $
  withCString name $ \cname -> do
    typeInt <- Raw.sdlGetPropertyTypeRaw props cname
    return $ toEnum (fromIntegral typeInt)

sdlGetPointerProperty :: MonadIO m => SDLPropertiesID -> String -> Ptr () -> m (Ptr ())
sdlGetPointerProperty props name defaultValue = liftIO $
  withCString name $ \cname ->
    Raw.sdlGetPointerPropertyRaw props cname defaultValue

sdlGetStringProperty :: MonadIO m => SDLPropertiesID -> String -> String -> m String
sdlGetStringProperty props name defaultValue = liftIO $
  withCString name $ \cname ->
    withCString defaultValue $ \cdefault -> do
      cresult <- Raw.sdlGetStringPropertyRaw props cname cdefault
      peekCString cresult

sdlGetNumberProperty :: MonadIO m => SDLPropertiesID -> String -> Int64 -> m Int64
sdlGetNumberProperty props name defaultValue = liftIO $
  withCString name $ \cname ->
    Raw.sdlGetNumberPropertyRaw props cname defaultValue

sdlGetFloatProperty :: MonadIO m => SDLPropertiesID -> String -> Float -> m Float
sdlGetFloatProperty props name defaultValue = liftIO $
  withCString name $ \cname -> do
    CFloat result <- Raw.sdlGetFloatPropertyRaw props cname (CFloat defaultValue)
    return result

sdlGetBooleanProperty :: MonadIO m => SDLPropertiesID -> String -> Bool -> m Bool
sdlGetBooleanProperty props name defaultValue = liftIO $
  withCString name $ \cname ->
    Raw.sdlGetBooleanPropertyRaw props cname defaultValue

sdlClearProperty :: MonadIO m => SDLPropertiesID -> String -> m Bool
sdlClearProperty props name = liftIO $
  withCString name $ \cname ->
    Raw.sdlClearPropertyRaw props cname

sdlEnumerateProperties ::
  MonadIO m =>
  SDLPropertiesID ->
  SDLEnumeratePropertiesCallback ->
  Ptr () ->
  m Bool
sdlEnumerateProperties props callback userdata =
  liftIO $ Raw.sdlEnumeratePropertiesRaw props callback userdata
