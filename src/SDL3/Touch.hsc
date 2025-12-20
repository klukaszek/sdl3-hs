-- SDL/Touch.hsc
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-} -- Added for Storable SDLFinger

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
'SDL_TOUCH_MOUSEID' constant. Applications needing to distinguish touch from mouse input should
filter out mouse events with this ID.
-}

#include <SDL3/SDL_touch.h>

module SDL3.Touch
  ( -- * Types
    SDLTouchID
  , SDLFingerID
  , SDLTouchDeviceType(..)
  , pattern SDL_TOUCH_DEVICE_INVALID
  , pattern SDL_TOUCH_DEVICE_DIRECT
  , pattern SDL_TOUCH_DEVICE_INDIRECT_ABSOLUTE
  , pattern SDL_TOUCH_DEVICE_INDIRECT_RELATIVE
  , SDLFinger(..)

    -- * Patterns / Constants
  , pattern SDL_TOUCH_MOUSEID
  , pattern SDL_MOUSE_TOUCHID

    -- * Touch Device Functions
  , sdlGetTouchDevices
  , sdlGetTouchDeviceName
  , sdlGetTouchDeviceType
  , sdlGetTouchFingers
  ) where

import Foreign.C.Types
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.C.String (CString, peekCString)
import Foreign.Storable (Storable(..), peek, poke)
import Foreign.Marshal.Alloc (free, alloca)
import Foreign.Marshal.Array (peekArray)
import Data.Word (Word64)
import SDL3.Mouse (SDLMouseID) -- Assuming defined as Word32

-- | A unique ID for a touch device (SDL_TouchID).
type SDLTouchID = Word64

-- | A unique ID for a single finger on a touch device (SDL_FingerID).
type SDLFingerID = Word64

-- | An enum describing the type of a touch device (SDL_TouchDeviceType).
newtype SDLTouchDeviceType = SDLTouchDeviceType CInt
  deriving newtype (Show, Eq, Ord, Storable, Enum, Bounded)

pattern SDL_TOUCH_DEVICE_INVALID :: SDLTouchDeviceType
pattern SDL_TOUCH_DEVICE_INVALID           = SDLTouchDeviceType (#{const SDL_TOUCH_DEVICE_INVALID})
pattern SDL_TOUCH_DEVICE_DIRECT :: SDLTouchDeviceType
pattern SDL_TOUCH_DEVICE_DIRECT            = SDLTouchDeviceType #{const SDL_TOUCH_DEVICE_DIRECT}
pattern SDL_TOUCH_DEVICE_INDIRECT_ABSOLUTE :: SDLTouchDeviceType
pattern SDL_TOUCH_DEVICE_INDIRECT_ABSOLUTE  = SDLTouchDeviceType #{const SDL_TOUCH_DEVICE_INDIRECT_ABSOLUTE}
pattern SDL_TOUCH_DEVICE_INDIRECT_RELATIVE :: SDLTouchDeviceType
pattern SDL_TOUCH_DEVICE_INDIRECT_RELATIVE  = SDLTouchDeviceType #{const SDL_TOUCH_DEVICE_INDIRECT_RELATIVE}

-- | Data about a single finger in a multitouch event (SDL_Finger).
data SDLFinger = SDLFinger
  { fingerID       :: SDLFingerID  -- ^ The finger ID
  , fingerX        :: CFloat       -- ^ X-axis location, normalized (0...1)
  , fingerY        :: CFloat       -- ^ Y-axis location, normalized (0...1)
  , fingerPressure :: CFloat       -- ^ Pressure applied, normalized (0...1)
  } deriving (Eq, Show)

instance Storable SDLFinger where
  sizeOf _ = #{size SDL_Finger}
  alignment _ = #{alignment SDL_Finger}
  peek ptr = do -- Use RecordWildCards if preferred
    fingerID       <- #{peek SDL_Finger, id} ptr
    fingerX        <- #{peek SDL_Finger, x} ptr
    fingerY        <- #{peek SDL_Finger, y} ptr
    fingerPressure <- #{peek SDL_Finger, pressure} ptr
    return SDLFinger{..}
  poke ptr SDLFinger{..} = do -- Use RecordWildCards
    #{poke SDL_Finger, id} ptr fingerID
    #{poke SDL_Finger, x} ptr fingerX
    #{poke SDL_Finger, y} ptr fingerY
    #{poke SDL_Finger, pressure} ptr fingerPressure

-- | The SDL_MouseID for mouse events simulated with touch input (SDL_TOUCH_MOUSEID).
pattern SDL_TOUCH_MOUSEID :: SDLMouseID
pattern SDL_TOUCH_MOUSEID = #{const SDL_TOUCH_MOUSEID}

-- | The SDL_TouchID for touch events simulated with mouse input (SDL_MOUSE_TOUCHID).
pattern SDL_MOUSE_TOUCHID :: SDLTouchID
pattern SDL_MOUSE_TOUCHID = #{const SDL_MOUSE_TOUCHID}

-- | Get a list of registered touch devices (SDL_GetTouchDevices).
-- Memory allocated by SDL must be freed by the caller.
foreign import ccall unsafe "SDL_GetTouchDevices"
  c_sdlGetTouchDevices :: Ptr CInt -> IO (Ptr SDLTouchID) -- SDL_TouchID is Uint64

-- | Haskell wrapper for SDL_GetTouchDevices.
sdlGetTouchDevices :: IO [SDLTouchID]
sdlGetTouchDevices = alloca $ \countPtr -> do
  idPtr <- c_sdlGetTouchDevices countPtr
  if idPtr == nullPtr
    then return []
    else do
      count <- peek countPtr
      ids <- peekArray (fromIntegral count) idPtr
      -- Free the memory allocated by SDL for the ID list
      free (castPtr idPtr)
      return ids

-- | Get the touch device name as reported from the driver (SDL_GetTouchDeviceName).
-- The returned string is owned by SDL and should not be freed.
foreign import ccall unsafe "SDL_GetTouchDeviceName"
  c_sdlGetTouchDeviceName :: SDLTouchID -> IO CString

-- | Haskell wrapper for SDL_GetTouchDeviceName.
sdlGetTouchDeviceName :: SDLTouchID -> IO (Maybe String)
sdlGetTouchDeviceName touchID = do
  cStr <- c_sdlGetTouchDeviceName touchID
  if cStr == nullPtr
    then return Nothing
    else Just <$> peekCString cStr

-- | Get the type of the given touch device (SDL_GetTouchDeviceType).
foreign import ccall unsafe "SDL_GetTouchDeviceType"
  c_sdlGetTouchDeviceType :: SDLTouchID -> IO CInt

-- | Haskell wrapper for SDL_GetTouchDeviceType.
sdlGetTouchDeviceType :: SDLTouchID -> IO SDLTouchDeviceType
sdlGetTouchDeviceType touchID =
  toEnum . fromIntegral <$> c_sdlGetTouchDeviceType touchID -- Use derived Enum

-- | Get a list of active fingers for a given touch device (SDL_GetTouchFingers).
-- Returns pointers into SDL's internal state - DO NOT FREE the returned pointers.
foreign import ccall unsafe "SDL_GetTouchFingers"
  c_sdlGetTouchFingers :: SDLTouchID -> Ptr CInt -> IO (Ptr (Ptr SDLFinger)) -- Returns SDL_Finger**

-- | Haskell wrapper for SDL_GetTouchFingers.
-- Reads the finger data from SDL's internal state.
sdlGetTouchFingers :: SDLTouchID -> IO [SDLFinger]
sdlGetTouchFingers touchID = alloca $ \countPtr -> do
  -- SDL returns a pointer to an array of SDL_Finger pointers (SDL_Finger**)
  fingersPtrArrayPtr <- c_sdlGetTouchFingers touchID countPtr
  if fingersPtrArrayPtr == nullPtr
    then return []
    else do
      count <- peek countPtr
      if count <= 0
        then return []
        else do
          -- Peek the array of Ptr SDL_Finger
          fingerPtrs <- peekArray (fromIntegral count) fingersPtrArrayPtr
          -- Peek each SDLFinger struct from the pointers
          mapM peek fingerPtrs
