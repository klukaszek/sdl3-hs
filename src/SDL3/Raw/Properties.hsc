{-# LANGUAGE PatternSynonyms #-}

#include <SDL3/SDL_properties.h>

module SDL3.Raw.Properties
  ( SDLPropertiesID
  , SDLPropertyType(..)
  , SDLCleanupPropertyCallback
  , SDLEnumeratePropertiesCallback
  , sdlGetGlobalPropertiesRaw
  , sdlCreatePropertiesRaw
  , sdlCopyPropertiesRaw
  , sdlLockPropertiesRaw
  , sdlUnlockPropertiesRaw
  , sdlDestroyPropertiesRaw
  , sdlSetPointerPropertyWithCleanupRaw
  , sdlSetPointerPropertyRaw
  , sdlSetStringPropertyRaw
  , sdlSetNumberPropertyRaw
  , sdlSetFloatPropertyRaw
  , sdlSetBooleanPropertyRaw
  , sdlHasPropertyRaw
  , sdlGetPropertyTypeRaw
  , sdlGetPointerPropertyRaw
  , sdlGetStringPropertyRaw
  , sdlGetNumberPropertyRaw
  , sdlGetFloatPropertyRaw
  , sdlGetBooleanPropertyRaw
  , sdlClearPropertyRaw
  , sdlEnumeratePropertiesRaw
  , pattern SDL_PROP_NAME_STRING
  ) where

import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

pattern SDL_PROP_NAME_STRING = #{const_str SDL_PROP_NAME_STRING}

type SDLPropertiesID = Word32

data SDLPropertyType
  = SDLPropertyTypeInvalid
  | SDLPropertyTypePointer
  | SDLPropertyTypeString
  | SDLPropertyTypeNumber
  | SDLPropertyTypeFloat
  | SDLPropertyTypeBoolean
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type SDLCleanupPropertyCallback = FunPtr (Ptr () -> Ptr () -> IO ())

type SDLEnumeratePropertiesCallback = FunPtr (Ptr () -> SDLPropertiesID -> CString -> IO ())

foreign import ccall "SDL_GetGlobalProperties"
  sdlGetGlobalPropertiesRaw :: IO SDLPropertiesID

foreign import ccall "SDL_CreateProperties"
  sdlCreatePropertiesRaw :: IO SDLPropertiesID

foreign import ccall "SDL_CopyProperties"
  sdlCopyPropertiesRaw :: SDLPropertiesID -> SDLPropertiesID -> IO Bool

foreign import ccall "SDL_LockProperties"
  sdlLockPropertiesRaw :: SDLPropertiesID -> IO Bool

foreign import ccall "SDL_UnlockProperties"
  sdlUnlockPropertiesRaw :: SDLPropertiesID -> IO ()

foreign import ccall "SDL_SetPointerPropertyWithCleanup"
  sdlSetPointerPropertyWithCleanupRaw ::
    SDLPropertiesID ->
    CString ->
    Ptr () ->
    SDLCleanupPropertyCallback ->
    Ptr () ->
    IO Bool

foreign import ccall "SDL_SetPointerProperty"
  sdlSetPointerPropertyRaw :: SDLPropertiesID -> CString -> Ptr () -> IO Bool

foreign import ccall "SDL_SetStringProperty"
  sdlSetStringPropertyRaw :: SDLPropertiesID -> CString -> CString -> IO Bool

foreign import ccall "SDL_SetNumberProperty"
  sdlSetNumberPropertyRaw :: SDLPropertiesID -> CString -> Int64 -> IO Bool

foreign import ccall "SDL_SetFloatProperty"
  sdlSetFloatPropertyRaw :: SDLPropertiesID -> CString -> CFloat -> IO Bool

foreign import ccall "SDL_SetBooleanProperty"
  sdlSetBooleanPropertyRaw :: SDLPropertiesID -> CString -> Bool -> IO Bool

foreign import ccall "SDL_HasProperty"
  sdlHasPropertyRaw :: SDLPropertiesID -> CString -> IO Bool

foreign import ccall "SDL_GetPropertyType"
  sdlGetPropertyTypeRaw :: SDLPropertiesID -> CString -> IO CInt

foreign import ccall "SDL_GetPointerProperty"
  sdlGetPointerPropertyRaw :: SDLPropertiesID -> CString -> Ptr () -> IO (Ptr ())

foreign import ccall "SDL_GetStringProperty"
  sdlGetStringPropertyRaw :: SDLPropertiesID -> CString -> CString -> IO CString

foreign import ccall "SDL_GetNumberProperty"
  sdlGetNumberPropertyRaw :: SDLPropertiesID -> CString -> Int64 -> IO Int64

foreign import ccall "SDL_GetFloatProperty"
  sdlGetFloatPropertyRaw :: SDLPropertiesID -> CString -> CFloat -> IO CFloat

foreign import ccall "SDL_GetBooleanProperty"
  sdlGetBooleanPropertyRaw :: SDLPropertiesID -> CString -> Bool -> IO Bool

foreign import ccall "SDL_ClearProperty"
  sdlClearPropertyRaw :: SDLPropertiesID -> CString -> IO Bool

foreign import ccall "SDL_EnumerateProperties"
  sdlEnumeratePropertiesRaw :: SDLPropertiesID -> SDLEnumeratePropertiesCallback -> Ptr () -> IO Bool

foreign import ccall "SDL_DestroyProperties"
  sdlDestroyPropertiesRaw :: SDLPropertiesID -> IO ()
