{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : SDL.Touch
Description : SDL touch input management
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

SDL offers touch input management on platforms that support it, handling multiple touch devices
and tracking multiple fingers on those devices.

This module provides functions to query touch device information (e.g., 'sdlGetTouchDevices',
'sdlGetTouchDeviceName') and retrieve active finger data (e.g., 'sdlGetTouchFingers'). Touch
events are primarily handled through the SDL event system (SDL_EVENT_FINGER_DOWN, etc.), but
this module complements that by offering direct access to touch hardware details.

By default, SDL simulates mouse events from touch input, which can be identified by the
'sdlTouchMouseID' constant. Applications needing to distinguish touch from mouse input should
filter out mouse events with this ID.
-}

module SDL.Touch
  ( -- * Types
    SDLTouchID(..)
  , SDLFingerID
  , SDLTouchDeviceType(..)
  , SDLFinger(..)

    -- * Constants
  , sdlTouchMouseID
  , sdlMouseTouchID

    -- * Touch Device Functions
  , sdlGetTouchDevices
  , sdlGetTouchDeviceName
  , sdlGetTouchDeviceType
  , sdlGetTouchFingers
  ) where

#include <SDL3/SDL_touch.h>

import Foreign.C.Types
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.C.String (CString, peekCString)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (free, alloca)  -- Added alloca
import Foreign.Marshal.Array (peekArray)
import Data.Word (Word32, Word64)
import Control.Monad (when)
import SDL.Mouse

-- | A unique ID for a touch device (SDL_TouchID).
newtype SDLTouchID = SDLTouchID { unSDLTouchID :: Word64 }
  deriving (Show, Eq, Ord)

-- | A unique ID for a single finger on a touch device (SDL_FingerID).
type SDLFingerID = Word64

-- | An enum describing the type of a touch device (SDL_TouchDeviceType).
data SDLTouchDeviceType
  = SDLTouchDeviceInvalid           -- ^ SDL_TOUCH_DEVICE_INVALID
  | SDLTouchDeviceDirect            -- ^ SDL_TOUCH_DEVICE_DIRECT
  | SDLTouchDeviceIndirectAbsolute  -- ^ SDL_TOUCH_DEVICE_INDIRECT_ABSOLUTE
  | SDLTouchDeviceIndirectRelative  -- ^ SDL_TOUCH_DEVICE_INDIRECT_RELATIVE
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Convert C enum values to Haskell SDLTouchDeviceType.
fromCSDLTouchDeviceType :: CInt -> SDLTouchDeviceType
fromCSDLTouchDeviceType (-1) = SDLTouchDeviceInvalid
fromCSDLTouchDeviceType 0    = SDLTouchDeviceDirect
fromCSDLTouchDeviceType 1    = SDLTouchDeviceIndirectAbsolute
fromCSDLTouchDeviceType 2    = SDLTouchDeviceIndirectRelative
fromCSDLTouchDeviceType _    = SDLTouchDeviceInvalid  -- Fallback for unknown values

-- | Data about a single finger in a multitouch event (SDL_Finger).
data SDLFinger = SDLFinger
  { fingerID       :: SDLFingerID  -- ^ The finger ID
  , fingerX        :: Float        -- ^ X-axis location, normalized (0...1)
  , fingerY        :: Float        -- ^ Y-axis location, normalized (0...1)
  , fingerPressure :: Float        -- ^ Pressure applied, normalized (0...1)
  } deriving (Eq, Show)

instance Storable SDLFinger where
  sizeOf _ = #size SDL_Finger
  alignment _ = #alignment SDL_Finger
  peek ptr = SDLFinger
    <$> (#peek SDL_Finger, id) ptr
    <*> (#peek SDL_Finger, x) ptr
    <*> (#peek SDL_Finger, y) ptr
    <*> (#peek SDL_Finger, pressure) ptr
  poke ptr (SDLFinger fid fx fy fp) = do
    (#poke SDL_Finger, id) ptr fid
    (#poke SDL_Finger, x) ptr fx
    (#poke SDL_Finger, y) ptr fy
    (#poke SDL_Finger, pressure) ptr fp

-- | The SDL_MouseID for mouse events simulated with touch input (SDL_TOUCH_MOUSEID).
sdlTouchMouseID :: SDLMouseID
sdlTouchMouseID = SDLMouseID(#const SDL_TOUCH_MOUSEID)

-- | The SDL_TouchID for touch events simulated with mouse input (SDL_MOUSE_TOUCHID).
sdlMouseTouchID :: SDLTouchID
sdlMouseTouchID = SDLTouchID(#const SDL_MOUSE_TOUCHID)

-- | Get a list of registered touch devices (SDL_GetTouchDevices).
foreign import ccall "SDL_GetTouchDevices"
  sdlGetTouchDevicesRaw :: Ptr CInt -> IO (Ptr Word64)

-- | Haskell wrapper for SDL_GetTouchDevices.
sdlGetTouchDevices :: IO [SDLTouchID]
sdlGetTouchDevices = alloca $ \countPtr -> do
  pArr <- sdlGetTouchDevicesRaw countPtr
  if pArr == nullPtr
    then return []
    else do
      count <- peek countPtr
      arr <- peekArray (fromIntegral count) pArr
      free (castPtr pArr)
      return $ map SDLTouchID arr

-- | Get the touch device name as reported from the driver (SDL_GetTouchDeviceName).
foreign import ccall "SDL_GetTouchDeviceName"
  sdlGetTouchDeviceNameRaw :: SDLTouchID -> IO CString

-- | Haskell wrapper for SDL_GetTouchDeviceName.
sdlGetTouchDeviceName :: SDLTouchID -> IO (Maybe String)
sdlGetTouchDeviceName touchID = do
  cstr <- sdlGetTouchDeviceNameRaw touchID
  if cstr == nullPtr
    then return Nothing
    else Just <$> peekCString cstr

-- | Get the type of the given touch device (SDL_GetTouchDeviceType).
foreign import ccall "SDL_GetTouchDeviceType"
  sdlGetTouchDeviceTypeRaw :: SDLTouchID -> IO CInt

-- | Haskell wrapper for SDL_GetTouchDeviceType.
sdlGetTouchDeviceType :: SDLTouchID -> IO SDLTouchDeviceType
sdlGetTouchDeviceType touchID = fromCSDLTouchDeviceType <$> sdlGetTouchDeviceTypeRaw touchID

-- | Get a list of active fingers for a given touch device (SDL_GetTouchFingers).
foreign import ccall "SDL_GetTouchFingers"
  sdlGetTouchFingersRaw :: SDLTouchID -> Ptr CInt -> IO (Ptr (Ptr SDLFinger))

-- | Haskell wrapper for SDL_GetTouchFingers.
sdlGetTouchFingers :: SDLTouchID -> IO [SDLFinger]
sdlGetTouchFingers touchID = alloca $ \countPtr -> do
  poke countPtr 0  -- Initialize count to 0
  fingersPtrPtr <- sdlGetTouchFingersRaw touchID countPtr
  if fingersPtrPtr == nullPtr
    then return []
    else do
      count <- peek countPtr
      fingersPtr <- peek fingersPtrPtr
      fingers <- peekArray (fromIntegral count) fingersPtr
      free fingersPtrPtr  -- Free the array of pointers
      free fingersPtr     -- Free the finger data
      return fingers
