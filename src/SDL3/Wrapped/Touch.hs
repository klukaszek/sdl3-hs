{-# LANGUAGE PatternSynonyms #-}

module SDL3.Wrapped.Touch
  ( SDLTouchID
  , SDLFingerID
  , SDLTouchDeviceType(..)
  , pattern SDL_TOUCH_DEVICE_INVALID
  , pattern SDL_TOUCH_DEVICE_DIRECT
  , pattern SDL_TOUCH_DEVICE_INDIRECT_ABSOLUTE
  , pattern SDL_TOUCH_DEVICE_INDIRECT_RELATIVE
  , SDLFinger(..)
  , pattern SDL_TOUCH_MOUSEID
  , pattern SDL_MOUSE_TOUCHID
  , sdlGetTouchDevices
  , sdlGetTouchDeviceName
  , sdlGetTouchDeviceType
  , sdlGetTouchFingers
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (peek)
import SDL3.Raw.Touch
  ( SDLFinger(..)
  , SDLFingerID
  , SDLTouchDeviceType(..)
  , SDLTouchID
  , pattern SDL_MOUSE_TOUCHID
  , pattern SDL_TOUCH_DEVICE_DIRECT
  , pattern SDL_TOUCH_DEVICE_INDIRECT_ABSOLUTE
  , pattern SDL_TOUCH_DEVICE_INDIRECT_RELATIVE
  , pattern SDL_TOUCH_DEVICE_INVALID
  , pattern SDL_TOUCH_MOUSEID
  )
import qualified SDL3.Raw.Touch as Raw

sdlGetTouchDevices :: MonadIO m => m [SDLTouchID]
sdlGetTouchDevices = liftIO $ alloca $ \countPtr -> do
  idPtr <- Raw.sdlGetTouchDevicesRaw countPtr
  if idPtr == nullPtr
    then return []
    else do
      count <- peek countPtr
      ids <- peekArray (fromIntegral count) idPtr
      free (castPtr idPtr)
      return ids

sdlGetTouchDeviceName :: MonadIO m => SDLTouchID -> m (Maybe String)
sdlGetTouchDeviceName touchID = liftIO $ do
  cStr <- Raw.sdlGetTouchDeviceNameRaw touchID
  if cStr == nullPtr
    then return Nothing
    else Just <$> peekCString cStr

sdlGetTouchDeviceType :: MonadIO m => SDLTouchID -> m SDLTouchDeviceType
sdlGetTouchDeviceType touchID =
  liftIO $ toEnum . fromIntegral <$> Raw.sdlGetTouchDeviceTypeRaw touchID

sdlGetTouchFingers :: MonadIO m => SDLTouchID -> m [SDLFinger]
sdlGetTouchFingers touchID = liftIO $ alloca $ \countPtr -> do
  fingersPtrArrayPtr <- Raw.sdlGetTouchFingersRaw touchID countPtr
  if fingersPtrArrayPtr == nullPtr
    then return []
    else do
      count <- peek countPtr
      if count <= 0
        then return []
        else do
          fingerPtrs <- peekArray (fromIntegral count) fingersPtrArrayPtr
          mapM peek fingerPtrs
