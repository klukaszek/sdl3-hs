-- |
-- Module      : SDL.Properties
-- Description : Dynamic property management
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- A property is a variable that can be created and retrieved by name at runtime.
--
-- All properties are part of a property group (SDLPropertiesID). A property group can
-- be created with the 'sdlCreateProperties' function and destroyed with the
-- 'sdlDestroyProperties' function.
--
-- Properties can be added to and retrieved from a property group through the following functions:
--
-- - 'sdlSetPointerProperty' and 'sdlGetPointerProperty' operate on pointer types.
-- - 'sdlSetStringProperty' and 'sdlGetStringProperty' operate on string types.
-- - 'sdlSetNumberProperty' and 'sdlGetNumberProperty' operate on signed 64-bit integer types.
-- - 'sdlSetFloatProperty' and 'sdlGetFloatProperty' operate on floating point types.
-- - 'sdlSetBooleanProperty' and 'sdlGetBooleanProperty' operate on boolean types.
--
-- Properties can be removed from a group by using 'sdlClearProperty'.
module SDL.Properties
  ( -- * Property Types
    SDLPropertiesID,
    SDLPropertyType (..),
    SDLCleanupPropertyCallback,
    SDLEnumeratePropertiesCallback,

    -- * Property Group Management
    sdlGetGlobalProperties,
    sdlCreateProperties,
    sdlCopyProperties,
    sdlLockProperties,
    sdlUnlockProperties,
    sdlDestroyProperties,

    -- * Property Operations
    sdlSetPointerPropertyWithCleanup,
    sdlSetPointerProperty,
    sdlSetStringProperty,
    sdlSetNumberProperty,
    sdlSetFloatProperty,
    sdlSetBooleanProperty,
    sdlHasProperty,
    sdlGetPropertyType,
    sdlGetPointerProperty,
    sdlGetStringProperty,
    sdlGetNumberProperty,
    sdlGetFloatProperty,
    sdlGetBooleanProperty,
    sdlClearProperty,
    sdlEnumerateProperties,
  )
where

import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

-- | SDL properties ID
type SDLPropertiesID = Word32

-- | SDL property type
data SDLPropertyType
  = -- | The property doesn't exist or is of an unknown type
    SDLPropertyTypeInvalid
  | -- | The property is a pointer
    SDLPropertyTypePointer
  | -- | The property is a string
    SDLPropertyTypeString
  | -- | The property is a number
    SDLPropertyTypeNumber
  | -- | The property is a float
    SDLPropertyTypeFloat
  | -- | The property is a boolean
    SDLPropertyTypeBoolean
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | A callback used to free resources when a property is deleted
type SDLCleanupPropertyCallback = FunPtr (Ptr () -> Ptr () -> IO ())

-- | A callback used to enumerate all the properties in a group of properties
type SDLEnumeratePropertiesCallback = FunPtr (Ptr () -> SDLPropertiesID -> CString -> IO ())

-- | Get the global SDL properties
--
-- @since 3.2.0
foreign import ccall "SDL_GetGlobalProperties"
  sdlGetGlobalProperties :: IO SDLPropertiesID

-- | Create a group of properties
--
-- All properties are automatically destroyed when SDL_Quit() is called.
--
-- @since 3.2.0
foreign import ccall "SDL_CreateProperties"
  sdlCreateProperties :: IO SDLPropertiesID

-- | Copy a group of properties
--
-- Copy all the properties from one group of properties to another, with the
-- exception of properties requiring cleanup (set using
-- 'sdlSetPointerPropertyWithCleanup'), which will not be copied. Any
-- property that already exists on `dst` will be overwritten.
--
-- @since 3.2.0
foreign import ccall "SDL_CopyProperties"
  sdlCopyProperties :: SDLPropertiesID -> SDLPropertiesID -> IO Bool

-- | Lock a group of properties
--
-- Obtain a multi-threaded lock for these properties. Other threads will wait
-- while trying to lock these properties until they are unlocked. Properties
-- must be unlocked before they are destroyed.
--
-- The lock is automatically taken when setting individual properties, this
-- function is only needed when you want to set several properties atomically
-- or want to guarantee that properties being queried aren't freed in another
-- thread.
--
-- @since 3.2.0
foreign import ccall "SDL_LockProperties"
  sdlLockProperties :: SDLPropertiesID -> IO Bool

-- | Unlock a group of properties
--
-- @since 3.2.0
foreign import ccall "SDL_UnlockProperties"
  sdlUnlockProperties :: SDLPropertiesID -> IO ()

-- | Set a pointer property in a group of properties with a cleanup function
-- that is called when the property is deleted
--
-- The cleanup function is also called if setting the property fails for any
-- reason.
--
-- For simply setting basic data types, like numbers, bools, or strings, use
-- 'sdlSetNumberProperty', 'sdlSetBooleanProperty', or 'sdlSetStringProperty'
-- instead, as those functions will handle cleanup on your behalf. This
-- function is only for more complex, custom data.
--
-- @since 3.2.0
foreign import ccall "SDL_SetPointerPropertyWithCleanup"
  sdlSetPointerPropertyWithCleanup :: SDLPropertiesID -> CString -> Ptr () -> SDLCleanupPropertyCallback -> Ptr () -> IO Bool

-- | Set a pointer property in a group of properties
--
-- @since 3.2.0
foreign import ccall "SDL_SetPointerProperty"
  sdlSetPointerPropertyC :: SDLPropertiesID -> CString -> Ptr () -> IO Bool

-- | Set a pointer property in a group of properties
--
-- @since 3.2.0
sdlSetPointerProperty :: SDLPropertiesID -> String -> Ptr () -> IO Bool
sdlSetPointerProperty props name value = withCString name $ \cname ->
  sdlSetPointerPropertyC props cname value

-- | Set a string property in a group of properties
--
-- This function makes a copy of the string; the caller does not have to
-- preserve the data after this call completes.
--
-- @since 3.2.0
foreign import ccall "SDL_SetStringProperty"
  sdlSetStringPropertyC :: SDLPropertiesID -> CString -> CString -> IO Bool

-- | Set a string property in a group of properties
--
-- This function makes a copy of the string; the caller does not have to
-- preserve the data after this call completes.
--
-- @since 3.2.0
sdlSetStringProperty :: SDLPropertiesID -> String -> String -> IO Bool
sdlSetStringProperty props name value =
  withCString name $ \cname ->
    withCString value $ \cvalue ->
      sdlSetStringPropertyC props cname cvalue

-- | Set an integer property in a group of properties
--
-- @since 3.2.0
foreign import ccall "SDL_SetNumberProperty"
  sdlSetNumberPropertyC :: SDLPropertiesID -> CString -> Int64 -> IO Bool

-- | Set an integer property in a group of properties
--
-- @since 3.2.0
sdlSetNumberProperty :: SDLPropertiesID -> String -> Int64 -> IO Bool
sdlSetNumberProperty props name value = withCString name $ \cname ->
  sdlSetNumberPropertyC props cname value

-- | Set a floating point property in a group of properties
--
-- @since 3.2.0
foreign import ccall "SDL_SetFloatProperty"
  sdlSetFloatPropertyC :: SDLPropertiesID -> CString -> CFloat -> IO Bool

-- | Set a floating point property in a group of properties
--
-- @since 3.2.0
sdlSetFloatProperty :: SDLPropertiesID -> String -> Float -> IO Bool
sdlSetFloatProperty props name value = withCString name $ \cname ->
  sdlSetFloatPropertyC props cname (CFloat value)

-- | Set a boolean property in a group of properties
--
-- @since 3.2.0
foreign import ccall "SDL_SetBooleanProperty"
  sdlSetBooleanPropertyC :: SDLPropertiesID -> CString -> Bool -> IO Bool

-- | Set a boolean property in a group of properties
--
-- @since 3.2.0
sdlSetBooleanProperty :: SDLPropertiesID -> String -> Bool -> IO Bool
sdlSetBooleanProperty props name value = withCString name $ \cname ->
  sdlSetBooleanPropertyC props cname value

-- | Return whether a property exists in a group of properties
--
-- @since 3.2.0
foreign import ccall "SDL_HasProperty"
  sdlHasPropertyC :: SDLPropertiesID -> CString -> IO Bool

-- | Return whether a property exists in a group of properties
--
-- @since 3.2.0
sdlHasProperty :: SDLPropertiesID -> String -> IO Bool
sdlHasProperty props name = withCString name $ \cname ->
  sdlHasPropertyC props cname

-- | Get the type of a property in a group of properties
--
-- @since 3.2.0
foreign import ccall "SDL_GetPropertyType"
  sdlGetPropertyTypeC :: SDLPropertiesID -> CString -> IO CInt

-- | Get the type of a property in a group of properties
--
-- @since 3.2.0
sdlGetPropertyType :: SDLPropertiesID -> String -> IO SDLPropertyType
sdlGetPropertyType props name = withCString name $ \cname -> do
  typeInt <- sdlGetPropertyTypeC props cname
  return $ toEnum (fromIntegral typeInt)

-- | Get a pointer property from a group of properties
--
-- By convention, the names of properties that SDL exposes on objects will
-- start with "SDL.", and properties that SDL uses internally will start with
-- "SDL.internal.". These should be considered read-only and should not be
-- modified by applications.
--
-- @since 3.2.0
foreign import ccall "SDL_GetPointerProperty"
  sdlGetPointerPropertyC :: SDLPropertiesID -> CString -> Ptr () -> IO (Ptr ())

-- | Get a pointer property from a group of properties
--
-- @since 3.2.0
sdlGetPointerProperty :: SDLPropertiesID -> String -> Ptr () -> IO (Ptr ())
sdlGetPointerProperty props name defaultValue = withCString name $ \cname ->
  sdlGetPointerPropertyC props cname defaultValue

-- | Get a string property from a group of properties
--
-- @since 3.2.0
foreign import ccall "SDL_GetStringProperty"
  sdlGetStringPropertyC :: SDLPropertiesID -> CString -> CString -> IO CString

-- | Get a string property from a group of properties
--
-- @since 3.2.0
sdlGetStringProperty :: SDLPropertiesID -> String -> String -> IO String
sdlGetStringProperty props name defaultValue =
  withCString name $ \cname ->
    withCString defaultValue $ \cdefault -> do
      cresult <- sdlGetStringPropertyC props cname cdefault
      peekCString cresult

-- | Get a number property from a group of properties
--
-- You can use 'sdlGetPropertyType' to query whether the property exists and
-- is a number property.
--
-- @since 3.2.0
foreign import ccall "SDL_GetNumberProperty"
  sdlGetNumberPropertyC :: SDLPropertiesID -> CString -> Int64 -> IO Int64

-- | Get a number property from a group of properties
--
-- @since 3.2.0
sdlGetNumberProperty :: SDLPropertiesID -> String -> Int64 -> IO Int64
sdlGetNumberProperty props name defaultValue = withCString name $ \cname ->
  sdlGetNumberPropertyC props cname defaultValue

-- | Get a floating point property from a group of properties
--
-- You can use 'sdlGetPropertyType' to query whether the property exists and
-- is a floating point property.
--
-- @since 3.2.0
foreign import ccall "SDL_GetFloatProperty"
  sdlGetFloatPropertyC :: SDLPropertiesID -> CString -> CFloat -> IO CFloat

-- | Get a floating point property from a group of properties
--
-- @since 3.2.0
sdlGetFloatProperty :: SDLPropertiesID -> String -> Float -> IO Float
sdlGetFloatProperty props name defaultValue = withCString name $ \cname -> do
  CFloat result <- sdlGetFloatPropertyC props cname (CFloat defaultValue)
  return result

-- | Get a boolean property from a group of properties
--
-- You can use 'sdlGetPropertyType' to query whether the property exists and
-- is a boolean property.
--
-- @since 3.2.0
foreign import ccall "SDL_GetBooleanProperty"
  sdlGetBooleanPropertyC :: SDLPropertiesID -> CString -> Bool -> IO Bool

-- | Get a boolean property from a group of properties
--
-- @since 3.2.0
sdlGetBooleanProperty :: SDLPropertiesID -> String -> Bool -> IO Bool
sdlGetBooleanProperty props name defaultValue = withCString name $ \cname ->
  sdlGetBooleanPropertyC props cname defaultValue

-- | Clear a property from a group of properties
--
-- @since 3.2.0
foreign import ccall "SDL_ClearProperty"
  sdlClearPropertyC :: SDLPropertiesID -> CString -> IO Bool

-- | Clear a property from a group of properties
--
-- @since 3.2.0
sdlClearProperty :: SDLPropertiesID -> String -> IO Bool
sdlClearProperty props name = withCString name $ \cname ->
  sdlClearPropertyC props cname

-- | Enumerate the properties contained in a group of properties
--
-- The callback function is called for each property in the group of
-- properties. The properties are locked during enumeration.
--
-- @since 3.2.0
foreign import ccall "SDL_EnumerateProperties"
  sdlEnumerateProperties :: SDLPropertiesID -> SDLEnumeratePropertiesCallback -> Ptr () -> IO Bool

-- | Destroy a group of properties
--
-- All properties are deleted and their cleanup functions will be called, if
-- any.
--
-- @since 3.2.0
foreign import ccall "SDL_DestroyProperties"
  sdlDestroyProperties :: SDLPropertiesID -> IO ()
