{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

{-|
Module      : SDL.Events
Description : SDL event queue management
Copyright   : (c) Kyle Lukaszek 2025
License     : BSD3

SDL event queue management functions. This module provides functionality for
handling the SDL event queue, including polling events, pushing custom events,
and managing event filters.

For more details, refer to the official SDL3 documentation:
https://wiki.libsdl.org/SDL3/CategoryEvents

Basic usage:
- Poll events with 'sdlPollEvent'
- Wait for events with 'sdlWaitEvent' or 'sdlWaitEventTimeout'
- Push custom events with 'sdlPushEvent'
- Manage event filters with 'sdlSetEventFilter' and 'sdlAddEventWatch'
-}

module SDL.Events
  ( -- * Types
    SDLEventType
  , SDLEventAction
  , pattern SDL_EVENT_QUIT
  , pattern SDL_EVENT_TERMINATING
  , pattern SDL_EVENT_LOW_MEMORY
  , pattern SDL_EVENT_WILL_ENTER_BACKGROUND
  , pattern SDL_EVENT_DID_ENTER_BACKGROUND
  , pattern SDL_EVENT_WILL_ENTER_FOREGROUND
  , pattern SDL_EVENT_DID_ENTER_FOREGROUND
  , pattern SDL_EVENT_LOCALE_CHANGED
  , pattern SDL_EVENT_SYSTEM_THEME_CHANGED
  , pattern SDL_EVENT_DISPLAY_ORIENTATION
  , pattern SDL_EVENT_DISPLAY_ADDED
  , pattern SDL_EVENT_DISPLAY_REMOVED
  , pattern SDL_EVENT_DISPLAY_MOVED
  , pattern SDL_EVENT_DISPLAY_DESKTOP_MODE_CHANGED
  , pattern SDL_EVENT_DISPLAY_CURRENT_MODE_CHANGED
  , pattern SDL_EVENT_DISPLAY_CONTENT_SCALE_CHANGED
  , pattern SDL_EVENT_WINDOW_SHOWN
  , pattern SDL_EVENT_WINDOW_HIDDEN
  , pattern SDL_EVENT_WINDOW_EXPOSED
  , pattern SDL_EVENT_WINDOW_MOVED
  , pattern SDL_EVENT_WINDOW_RESIZED
  , pattern SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED
  , pattern SDL_EVENT_WINDOW_MINIMIZED
  , pattern SDL_EVENT_WINDOW_MAXIMIZED
  , pattern SDL_EVENT_WINDOW_RESTORED
  , pattern SDL_EVENT_WINDOW_MOUSE_ENTER
  , pattern SDL_EVENT_WINDOW_MOUSE_LEAVE
  , pattern SDL_EVENT_WINDOW_FOCUS_GAINED
  , pattern SDL_EVENT_WINDOW_FOCUS_LOST
  , pattern SDL_EVENT_WINDOW_CLOSE_REQUESTED
  , pattern SDL_EVENT_WINDOW_DESTROYED
  , pattern SDL_EVENT_KEY_DOWN
  , pattern SDL_EVENT_KEY_UP
  , pattern SDL_EVENT_TEXT_EDITING
  , pattern SDL_EVENT_TEXT_INPUT
  , pattern SDL_EVENT_KEYMAP_CHANGED
  , pattern SDL_EVENT_KEYBOARD_ADDED
  , pattern SDL_EVENT_KEYBOARD_REMOVED
  , pattern SDL_EVENT_MOUSE_MOTION
  , pattern SDL_EVENT_MOUSE_BUTTON_DOWN
  , pattern SDL_EVENT_MOUSE_BUTTON_UP
  , pattern SDL_EVENT_MOUSE_WHEEL
  , pattern SDL_EVENT_MOUSE_ADDED
  , pattern SDL_EVENT_MOUSE_REMOVED
  , pattern SDL_EVENT_JOYSTICK_AXIS_MOTION
  , pattern SDL_EVENT_JOYSTICK_BUTTON_DOWN
  , pattern SDL_EVENT_JOYSTICK_BUTTON_UP
  , pattern SDL_EVENT_JOYSTICK_ADDED
  , pattern SDL_EVENT_JOYSTICK_REMOVED
  , pattern SDL_EVENT_CLIPBOARD_UPDATE
  , pattern SDL_EVENT_DROP_FILE
  , pattern SDL_EVENT_DROP_TEXT
  , pattern SDL_EVENT_DROP_BEGIN
  , pattern SDL_EVENT_DROP_COMPLETE
  , pattern SDL_EVENT_USER
  , SDLEvent(..)
  , SDLCommonEvent(..)
  , SDLDisplayEvent(..)
  , SDLWindowEvent(..)
  , SDLKeyboardEvent(..)
  , SDLTextInputEvent(..)
  , SDLMouseMotionEvent(..)
  , SDLMouseButtonEvent(..)
  , SDLMouseWheelEvent(..)
  , SDLQuitEvent(..)
  , SDLUserEvent(..)
  , SDLEventFilter

    -- * Event Functions
  , sdlPumpEvents
  , sdlPeepEvents
  , sdlHasEvent
  , sdlHasEvents
  , sdlFlushEvent
  , sdlFlushEvents
  , sdlPollEvent
  , sdlWaitEvent
  , sdlWaitEventTimeout
  , sdlPushEvent
  , sdlSetEventFilter
  , sdlGetEventFilter
  , sdlAddEventWatch
  , sdlRemoveEventWatch
  , sdlFilterEvents
  , sdlSetEventEnabled
  , sdlEventEnabled
  , sdlRegisterEvents
  , sdlGetWindowFromEvent
  ) where

#include <SDL3/SDL_events.h>

import Foreign (fromBool, toBool, FunPtr, with)
import Foreign.C.Types (CInt(..), CBool(..))
import Foreign.C.String (CString, peekCString)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (withArray)
import Foreign.Storable (Storable(..), peek)
import Data.Word
import Data.Int (Int32)
import SDL.Video (SDLWindow(..), SDLWindowID)
import SDL.Mouse (SDLMouseID, SDLMouseButtonFlags)
import SDL.Keyboard (SDLKeyboardID)
import SDL.Scancode (SDLScancode)
import SDL.Keycode (SDLKeycode, SDLKeymod)

-- | Event types as defined in SDL_EventType, represented as Word32 values
type SDLEventType = Word32

type SDLEventAction = CInt

-- Pattern synonyms for SDLEventType, mapping to C constants
pattern SDL_EVENT_QUIT                    :: SDLEventType
pattern SDL_EVENT_QUIT                    = (#const SDL_EVENT_QUIT)

pattern SDL_EVENT_TERMINATING             :: SDLEventType
pattern SDL_EVENT_TERMINATING             = (#const SDL_EVENT_TERMINATING)

pattern SDL_EVENT_LOW_MEMORY              :: SDLEventType
pattern SDL_EVENT_LOW_MEMORY              = (#const SDL_EVENT_LOW_MEMORY)

pattern SDL_EVENT_WILL_ENTER_BACKGROUND   :: SDLEventType
pattern SDL_EVENT_WILL_ENTER_BACKGROUND   = (#const SDL_EVENT_WILL_ENTER_BACKGROUND)

pattern SDL_EVENT_DID_ENTER_BACKGROUND    :: SDLEventType
pattern SDL_EVENT_DID_ENTER_BACKGROUND    = (#const SDL_EVENT_DID_ENTER_BACKGROUND)

pattern SDL_EVENT_WILL_ENTER_FOREGROUND   :: SDLEventType
pattern SDL_EVENT_WILL_ENTER_FOREGROUND   = (#const SDL_EVENT_WILL_ENTER_FOREGROUND)

pattern SDL_EVENT_DID_ENTER_FOREGROUND    :: SDLEventType
pattern SDL_EVENT_DID_ENTER_FOREGROUND    = (#const SDL_EVENT_DID_ENTER_FOREGROUND)

pattern SDL_EVENT_LOCALE_CHANGED          :: SDLEventType
pattern SDL_EVENT_LOCALE_CHANGED          = (#const SDL_EVENT_LOCALE_CHANGED)

pattern SDL_EVENT_SYSTEM_THEME_CHANGED    :: SDLEventType
pattern SDL_EVENT_SYSTEM_THEME_CHANGED    = (#const SDL_EVENT_SYSTEM_THEME_CHANGED)

pattern SDL_EVENT_DISPLAY_ORIENTATION     :: SDLEventType
pattern SDL_EVENT_DISPLAY_ORIENTATION     = (#const SDL_EVENT_DISPLAY_ORIENTATION)

pattern SDL_EVENT_DISPLAY_ADDED           :: SDLEventType
pattern SDL_EVENT_DISPLAY_ADDED           = (#const SDL_EVENT_DISPLAY_ADDED)

pattern SDL_EVENT_DISPLAY_REMOVED         :: SDLEventType
pattern SDL_EVENT_DISPLAY_REMOVED         = (#const SDL_EVENT_DISPLAY_REMOVED)

pattern SDL_EVENT_DISPLAY_MOVED           :: SDLEventType
pattern SDL_EVENT_DISPLAY_MOVED           = (#const SDL_EVENT_DISPLAY_MOVED)

pattern SDL_EVENT_DISPLAY_DESKTOP_MODE_CHANGED :: SDLEventType
pattern SDL_EVENT_DISPLAY_DESKTOP_MODE_CHANGED = (#const SDL_EVENT_DISPLAY_DESKTOP_MODE_CHANGED)

pattern SDL_EVENT_DISPLAY_CURRENT_MODE_CHANGED :: SDLEventType
pattern SDL_EVENT_DISPLAY_CURRENT_MODE_CHANGED = (#const SDL_EVENT_DISPLAY_CURRENT_MODE_CHANGED)

pattern SDL_EVENT_DISPLAY_CONTENT_SCALE_CHANGED :: SDLEventType
pattern SDL_EVENT_DISPLAY_CONTENT_SCALE_CHANGED = (#const SDL_EVENT_DISPLAY_CONTENT_SCALE_CHANGED)

pattern SDL_EVENT_WINDOW_SHOWN            :: SDLEventType
pattern SDL_EVENT_WINDOW_SHOWN            = (#const SDL_EVENT_WINDOW_SHOWN)

pattern SDL_EVENT_WINDOW_HIDDEN           :: SDLEventType
pattern SDL_EVENT_WINDOW_HIDDEN           = (#const SDL_EVENT_WINDOW_HIDDEN)

pattern SDL_EVENT_WINDOW_EXPOSED          :: SDLEventType
pattern SDL_EVENT_WINDOW_EXPOSED          = (#const SDL_EVENT_WINDOW_EXPOSED)

pattern SDL_EVENT_WINDOW_MOVED            :: SDLEventType
pattern SDL_EVENT_WINDOW_MOVED            = (#const SDL_EVENT_WINDOW_MOVED)

pattern SDL_EVENT_WINDOW_RESIZED          :: SDLEventType
pattern SDL_EVENT_WINDOW_RESIZED          = (#const SDL_EVENT_WINDOW_RESIZED)

pattern SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED :: SDLEventType
pattern SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED = (#const SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED)

pattern SDL_EVENT_WINDOW_MINIMIZED        :: SDLEventType
pattern SDL_EVENT_WINDOW_MINIMIZED        = (#const SDL_EVENT_WINDOW_MINIMIZED)

pattern SDL_EVENT_WINDOW_MAXIMIZED        :: SDLEventType
pattern SDL_EVENT_WINDOW_MAXIMIZED        = (#const SDL_EVENT_WINDOW_MAXIMIZED)

pattern SDL_EVENT_WINDOW_RESTORED         :: SDLEventType
pattern SDL_EVENT_WINDOW_RESTORED         = (#const SDL_EVENT_WINDOW_RESTORED)

pattern SDL_EVENT_WINDOW_MOUSE_ENTER      :: SDLEventType
pattern SDL_EVENT_WINDOW_MOUSE_ENTER      = (#const SDL_EVENT_WINDOW_MOUSE_ENTER)

pattern SDL_EVENT_WINDOW_MOUSE_LEAVE      :: SDLEventType
pattern SDL_EVENT_WINDOW_MOUSE_LEAVE      = (#const SDL_EVENT_WINDOW_MOUSE_LEAVE)

pattern SDL_EVENT_WINDOW_FOCUS_GAINED     :: SDLEventType
pattern SDL_EVENT_WINDOW_FOCUS_GAINED     = (#const SDL_EVENT_WINDOW_FOCUS_GAINED)

pattern SDL_EVENT_WINDOW_FOCUS_LOST       :: SDLEventType
pattern SDL_EVENT_WINDOW_FOCUS_LOST       = (#const SDL_EVENT_WINDOW_FOCUS_LOST)

pattern SDL_EVENT_WINDOW_CLOSE_REQUESTED  :: SDLEventType
pattern SDL_EVENT_WINDOW_CLOSE_REQUESTED  = (#const SDL_EVENT_WINDOW_CLOSE_REQUESTED)

pattern SDL_EVENT_WINDOW_DESTROYED        :: SDLEventType
pattern SDL_EVENT_WINDOW_DESTROYED        = (#const SDL_EVENT_WINDOW_DESTROYED)

pattern SDL_EVENT_KEY_DOWN                :: SDLEventType
pattern SDL_EVENT_KEY_DOWN                = (#const SDL_EVENT_KEY_DOWN)

pattern SDL_EVENT_KEY_UP                  :: SDLEventType
pattern SDL_EVENT_KEY_UP                  = (#const SDL_EVENT_KEY_UP)

pattern SDL_EVENT_TEXT_EDITING            :: SDLEventType
pattern SDL_EVENT_TEXT_EDITING            = (#const SDL_EVENT_TEXT_EDITING)

pattern SDL_EVENT_TEXT_INPUT              :: SDLEventType
pattern SDL_EVENT_TEXT_INPUT              = (#const SDL_EVENT_TEXT_INPUT)

pattern SDL_EVENT_KEYMAP_CHANGED          :: SDLEventType
pattern SDL_EVENT_KEYMAP_CHANGED          = (#const SDL_EVENT_KEYMAP_CHANGED)

pattern SDL_EVENT_KEYBOARD_ADDED          :: SDLEventType
pattern SDL_EVENT_KEYBOARD_ADDED          = (#const SDL_EVENT_KEYBOARD_ADDED)

pattern SDL_EVENT_KEYBOARD_REMOVED        :: SDLEventType
pattern SDL_EVENT_KEYBOARD_REMOVED        = (#const SDL_EVENT_KEYBOARD_REMOVED)

pattern SDL_EVENT_MOUSE_MOTION            :: SDLEventType
pattern SDL_EVENT_MOUSE_MOTION            = (#const SDL_EVENT_MOUSE_MOTION)

pattern SDL_EVENT_MOUSE_BUTTON_DOWN       :: SDLEventType
pattern SDL_EVENT_MOUSE_BUTTON_DOWN       = (#const SDL_EVENT_MOUSE_BUTTON_DOWN)

pattern SDL_EVENT_MOUSE_BUTTON_UP         :: SDLEventType
pattern SDL_EVENT_MOUSE_BUTTON_UP         = (#const SDL_EVENT_MOUSE_BUTTON_UP)

pattern SDL_EVENT_MOUSE_WHEEL             :: SDLEventType
pattern SDL_EVENT_MOUSE_WHEEL             = (#const SDL_EVENT_MOUSE_WHEEL)

pattern SDL_EVENT_MOUSE_ADDED             :: SDLEventType
pattern SDL_EVENT_MOUSE_ADDED             = (#const SDL_EVENT_MOUSE_ADDED)

pattern SDL_EVENT_MOUSE_REMOVED           :: SDLEventType
pattern SDL_EVENT_MOUSE_REMOVED           = (#const SDL_EVENT_MOUSE_REMOVED)

pattern SDL_EVENT_JOYSTICK_AXIS_MOTION    :: SDLEventType
pattern SDL_EVENT_JOYSTICK_AXIS_MOTION    = (#const SDL_EVENT_JOYSTICK_AXIS_MOTION)

pattern SDL_EVENT_JOYSTICK_BUTTON_DOWN    :: SDLEventType
pattern SDL_EVENT_JOYSTICK_BUTTON_DOWN    = (#const SDL_EVENT_JOYSTICK_BUTTON_DOWN)

pattern SDL_EVENT_JOYSTICK_BUTTON_UP      :: SDLEventType
pattern SDL_EVENT_JOYSTICK_BUTTON_UP      = (#const SDL_EVENT_JOYSTICK_BUTTON_UP)

pattern SDL_EVENT_JOYSTICK_ADDED          :: SDLEventType
pattern SDL_EVENT_JOYSTICK_ADDED          = (#const SDL_EVENT_JOYSTICK_ADDED)

pattern SDL_EVENT_JOYSTICK_REMOVED        :: SDLEventType
pattern SDL_EVENT_JOYSTICK_REMOVED        = (#const SDL_EVENT_JOYSTICK_REMOVED)

pattern SDL_EVENT_CLIPBOARD_UPDATE        :: SDLEventType
pattern SDL_EVENT_CLIPBOARD_UPDATE        = (#const SDL_EVENT_CLIPBOARD_UPDATE)

pattern SDL_EVENT_DROP_FILE               :: SDLEventType
pattern SDL_EVENT_DROP_FILE               = (#const SDL_EVENT_DROP_FILE)

pattern SDL_EVENT_DROP_TEXT               :: SDLEventType
pattern SDL_EVENT_DROP_TEXT               = (#const SDL_EVENT_DROP_TEXT)

pattern SDL_EVENT_DROP_BEGIN              :: SDLEventType
pattern SDL_EVENT_DROP_BEGIN              = (#const SDL_EVENT_DROP_BEGIN)

pattern SDL_EVENT_DROP_COMPLETE           :: SDLEventType
pattern SDL_EVENT_DROP_COMPLETE           = (#const SDL_EVENT_DROP_COMPLETE)

pattern SDL_EVENT_USER                    :: SDLEventType
pattern SDL_EVENT_USER                    = (#const SDL_EVENT_USER)

-- | Common event data structure
data SDLCommonEvent = SDLCommonEvent
  { sdlCommonType      :: Word32
  , sdlCommonTimestamp :: Word64
  } deriving (Eq, Show)

-- | Display event data structure
data SDLDisplayEvent = SDLDisplayEvent
  { sdlDisplayType      :: SDLEventType
  , sdlDisplayTimestamp :: Word64
  , sdlDisplayID        :: Word32
  , sdlDisplayData1     :: Int32
  , sdlDisplayData2     :: Int32
  } deriving (Eq, Show)

-- | Window event data structure
data SDLWindowEvent = SDLWindowEvent
  { sdlWindowType      :: SDLEventType
  , sdlWindowTimestamp :: Word64
  , sdlWindowID        :: SDLWindowID
  , sdlWindowData1     :: Int32
  , sdlWindowData2     :: Int32
  } deriving (Eq, Show)

-- | Keyboard event data structure
data SDLKeyboardEvent = SDLKeyboardEvent
  { sdlKeyboardType      :: SDLEventType  -- ^ SDL_EVENT_KEY_DOWN or SDL_EVENT_KEY_UP
  , sdlKeyboardTimestamp :: Word64        -- ^ In nanoseconds, populated using SDL_GetTicksNS()
  , sdlKeyboardWindowID  :: SDLWindowID   -- ^ The window with keyboard focus, if any
  , sdlKeyboardWhich     :: SDLKeyboardID -- ^ The keyboard instance id, or 0 if unknown or virtual
  , sdlKeyboardScancode  :: SDLScancode   -- ^ SDL physical key code
  , sdlKeyboardKey       :: SDLKeycode    -- ^ SDL virtual key code
  , sdlKeyboardMod       :: SDLKeymod     -- ^ Current key modifiers
  , sdlKeyboardRaw       :: Word16        -- ^ The platform dependent scancode for this event
  , sdlKeyboardDown      :: Bool          -- ^ True if the key is pressed
  , sdlKeyboardRepeat    :: Bool          -- ^ True if this is a key repeat
  } deriving (Eq, Show)

-- | Text input event data structure
data SDLTextInputEvent = SDLTextInputEvent
  { sdlTextInputType      :: SDLEventType
  , sdlTextInputTimestamp :: Word64
  , sdlTextInputWindowID  :: SDLWindowID
  , sdlTextInputText      :: String
  } deriving (Eq, Show)

-- | Mouse motion event data structure
data SDLMouseMotionEvent = SDLMouseMotionEvent
  { sdlMouseMotionType      :: SDLEventType
  , sdlMouseMotionTimestamp :: Word64
  , sdlMouseMotionWindowID  :: SDLWindowID
  , sdlMouseMotionWhich     :: SDLMouseID
  , sdlMouseMotionState     :: SDLMouseButtonFlags
  , sdlMouseMotionX         :: Float
  , sdlMouseMotionY         :: Float
  , sdlMouseMotionXrel      :: Float
  , sdlMouseMotionYrel      :: Float
  } deriving (Eq, Show)

-- | Mouse button event data structure
data SDLMouseButtonEvent = SDLMouseButtonEvent
  { sdlMouseButtonType      :: SDLEventType
  , sdlMouseButtonTimestamp :: Word64
  , sdlMouseButtonWindowID  :: SDLWindowID
  , sdlMouseButtonWhich     :: SDLMouseID
  , sdlMouseButtonButton    :: Word8
  , sdlMouseButtonDown      :: Bool
  , sdlMouseButtonClicks    :: Word8
  , sdlMouseButtonX         :: Float
  , sdlMouseButtonY         :: Float
  } deriving (Eq, Show)

-- | Mouse wheel event data structure
data SDLMouseWheelEvent = SDLMouseWheelEvent
  { sdlMouseWheelType      :: SDLEventType
  , sdlMouseWheelTimestamp :: Word64
  , sdlMouseWheelWindowID  :: SDLWindowID
  , sdlMouseWheelWhich     :: SDLMouseID
  , sdlMouseWheelX         :: Float
  , sdlMouseWheelY         :: Float
  , sdlMouseWheelMouseX    :: Float
  , sdlMouseWheelMouseY    :: Float
  } deriving (Eq, Show)

-- | Quit event data structure
data SDLQuitEvent = SDLQuitEvent
  { sdlQuitType      :: SDLEventType
  , sdlQuitTimestamp :: Word64
  } deriving (Eq, Show)

-- | User-defined event data structure
data SDLUserEvent = SDLUserEvent
  { sdlUserType      :: Word32
  , sdlUserTimestamp :: Word64
  , sdlUserWindowID  :: SDLWindowID
  , sdlUserCode      :: Int32
  , sdlUserData1     :: Ptr ()
  , sdlUserData2     :: Ptr ()
  } deriving (Eq, Show)

-- | Union of all SDL event types
data SDLEvent
  = SDLEventCommon SDLCommonEvent
  | SDLEventDisplay SDLDisplayEvent
  | SDLEventWindow SDLWindowEvent
  | SDLEventKeyboard SDLKeyboardEvent
  | SDLEventTextInput SDLTextInputEvent
  | SDLEventMouseMotion SDLMouseMotionEvent
  | SDLEventMouseButton SDLMouseButtonEvent
  | SDLEventMouseWheel SDLMouseWheelEvent
  | SDLEventQuit SDLQuitEvent
  | SDLEventUser SDLUserEvent
  deriving (Eq, Show)

-- Storable instance for SDLEvent (simplified for basic fields)
instance Storable SDLEvent where
  sizeOf _ = #size SDL_Event
  alignment _ = #alignment SDL_Event
  peek ptr = do
    eventType <- peek (castPtr ptr :: Ptr Word32)
    case eventType of
      t | t == SDL_EVENT_QUIT -> SDLEventQuit <$> peekQuitEvent ptr
      t | t >= SDL_EVENT_WINDOW_SHOWN && t <= SDL_EVENT_WINDOW_DESTROYED -> SDLEventWindow <$> peekWindowEvent ptr
      t | t == SDL_EVENT_KEY_DOWN || t == SDL_EVENT_KEY_UP -> SDLEventKeyboard <$> peekKeyboardEvent ptr
      t | t == SDL_EVENT_TEXT_INPUT -> SDLEventTextInput <$> peekTextInputEvent ptr
      t | t == SDL_EVENT_MOUSE_MOTION -> SDLEventMouseMotion <$> peekMouseMotionEvent ptr
      t | t == SDL_EVENT_MOUSE_BUTTON_DOWN || t == SDL_EVENT_MOUSE_BUTTON_UP -> SDLEventMouseButton <$> peekMouseButtonEvent ptr
      t | t == SDL_EVENT_MOUSE_WHEEL -> SDLEventMouseWheel <$> peekMouseWheelEvent ptr
      t | t >= SDL_EVENT_USER -> SDLEventUser <$> peekUserEvent ptr
      _ -> SDLEventCommon <$> peekCommonEvent ptr
  poke = error "poke not implemented for SDLEvent"

peekCommonEvent :: Ptr SDLEvent -> IO SDLCommonEvent
peekCommonEvent ptr = do
  t <- peek (castPtr ptr :: Ptr Word32)
  ts <- peekByteOff ptr 8
  return $ SDLCommonEvent t ts

peekWindowEvent :: Ptr SDLEvent -> IO SDLWindowEvent
peekWindowEvent ptr = do
  t <- peek (castPtr ptr :: Ptr Word32)
  ts <- peekByteOff ptr 8
  wid <- peekByteOff ptr 16
  d1 <- peekByteOff ptr 20
  d2 <- peekByteOff ptr 24
  return $ SDLWindowEvent t ts wid d1 d2

peekKeyboardEvent :: Ptr SDLEvent -> IO SDLKeyboardEvent
peekKeyboardEvent ptr = do
  t <- peek (castPtr ptr :: Ptr Word32)
  ts <- peekByteOff ptr 8
  wid <- peekByteOff ptr 16
  which <- peekByteOff ptr 20
  scancode <- peekByteOff ptr 24
  key <- peekByteOff ptr 28
  mod' <- peekByteOff ptr 32
  raw <- peekByteOff ptr 34
  down <- peekByteOff ptr 36
  repeat' <- peekByteOff ptr 37
  return $ SDLKeyboardEvent t ts wid which scancode key mod' raw (toBool (down :: CBool)) (toBool (repeat' :: CBool))

peekTextInputEvent :: Ptr SDLEvent -> IO SDLTextInputEvent
peekTextInputEvent ptr = do
  t <- peek (castPtr ptr :: Ptr Word32)
  ts <- peekByteOff ptr 8
  wid <- peekByteOff ptr 16
  textPtr <- peekByteOff ptr 24 :: IO CString
  text <- peekCString textPtr
  return $ SDLTextInputEvent t ts wid text

peekMouseMotionEvent :: Ptr SDLEvent -> IO SDLMouseMotionEvent
peekMouseMotionEvent ptr = do
  t <- peek (castPtr ptr :: Ptr Word32)
  ts <- peekByteOff ptr 8
  wid <- peekByteOff ptr 16
  which <- peekByteOff ptr 20
  state <- peekByteOff ptr 24
  x <- peekByteOff ptr 28
  y <- peekByteOff ptr 32
  xrel <- peekByteOff ptr 36
  yrel <- peekByteOff ptr 40
  return $ SDLMouseMotionEvent t ts wid which state x y xrel yrel

peekMouseButtonEvent :: Ptr SDLEvent -> IO SDLMouseButtonEvent
peekMouseButtonEvent ptr = do
  t <- peek (castPtr ptr :: Ptr Word32)
  ts <- peekByteOff ptr 8
  wid <- peekByteOff ptr 16
  which <- peekByteOff ptr 20
  button <- peekByteOff ptr 24
  down <- peekByteOff ptr 25
  clicks <- peekByteOff ptr 26
  x <- peekByteOff ptr 32
  y <- peekByteOff ptr 36
  return $ SDLMouseButtonEvent t ts wid which button (toBool (down :: CBool)) clicks x y

peekMouseWheelEvent :: Ptr SDLEvent -> IO SDLMouseWheelEvent
peekMouseWheelEvent ptr = do
  t <- peek (castPtr ptr :: Ptr Word32)
  ts <- peekByteOff ptr 8
  wid <- peekByteOff ptr 16
  which <- peekByteOff ptr 20
  x <- peekByteOff ptr 24
  y <- peekByteOff ptr 28
  mouseX <- peekByteOff ptr 36
  mouseY <- peekByteOff ptr 40
  return $ SDLMouseWheelEvent t ts wid which x y mouseX mouseY

peekQuitEvent :: Ptr SDLEvent -> IO SDLQuitEvent
peekQuitEvent ptr = do
  t <- peek (castPtr ptr :: Ptr Word32)
  ts <- peekByteOff ptr 8
  return $ SDLQuitEvent t ts

peekUserEvent :: Ptr SDLEvent -> IO SDLUserEvent
peekUserEvent ptr = do
  t <- peek (castPtr ptr :: Ptr Word32)
  ts <- peekByteOff ptr 8
  wid <- peekByteOff ptr 16
  code <- peekByteOff ptr 20
  data1 <- peekByteOff ptr 24
  data2 <- peekByteOff ptr 32
  return $ SDLUserEvent t ts wid code data1 data2

-- | Event filter callback type
type SDLEventFilter = Ptr () -> Ptr SDLEvent -> IO Bool

foreign import ccall "SDL_PumpEvents"
  sdlPumpEvents :: IO ()

foreign import ccall "SDL_PeepEvents"
  sdlPeepEventsRaw :: Ptr SDLEvent -> CInt -> SDLEventAction -> Word32 -> Word32 -> IO CInt

sdlPeepEvents :: [SDLEvent] -> Int -> SDLEventAction -> Word32 -> Word32 -> IO Int
sdlPeepEvents events num action minType maxType =
  withArray events $ \ptr -> do
    res <- sdlPeepEventsRaw ptr (fromIntegral num) action minType maxType
    return $ fromIntegral res

foreign import ccall "SDL_HasEvent"
  sdlHasEventRaw :: Word32 -> IO CBool

sdlHasEvent :: Word32 -> IO Bool
sdlHasEvent t = toBool <$> sdlHasEventRaw t

foreign import ccall "SDL_HasEvents"
  sdlHasEventsRaw :: Word32 -> Word32 -> IO CBool

sdlHasEvents :: Word32 -> Word32 -> IO Bool
sdlHasEvents minType maxType = toBool <$> sdlHasEventsRaw minType maxType

foreign import ccall "SDL_FlushEvent"
  sdlFlushEvent :: Word32 -> IO ()

foreign import ccall "SDL_FlushEvents"
  sdlFlushEvents :: Word32 -> Word32 -> IO ()

foreign import ccall "SDL_PollEvent"
  sdlPollEventRaw :: Ptr SDLEvent -> IO CBool

sdlPollEvent :: IO (Maybe SDLEvent)
sdlPollEvent = alloca $ \ptr -> do
  success <- toBool <$> sdlPollEventRaw ptr
  if success then Just <$> peek ptr else return Nothing

foreign import ccall "SDL_WaitEvent"
  sdlWaitEventRaw :: Ptr SDLEvent -> IO CBool

sdlWaitEvent :: IO (Maybe SDLEvent)
sdlWaitEvent = alloca $ \ptr -> do
  success <- toBool <$> sdlWaitEventRaw ptr
  if success then Just <$> peek ptr else return Nothing

foreign import ccall "SDL_WaitEventTimeout"
  sdlWaitEventTimeoutRaw :: Ptr SDLEvent -> Int32 -> IO CBool

sdlWaitEventTimeout :: Int -> IO (Maybe SDLEvent)
sdlWaitEventTimeout timeout = alloca $ \ptr -> do
  success <- toBool <$> sdlWaitEventTimeoutRaw ptr (fromIntegral timeout)
  if success then Just <$> peek ptr else return Nothing

foreign import ccall "SDL_PushEvent"
  sdlPushEventRaw :: Ptr SDLEvent -> IO CBool

sdlPushEvent :: SDLEvent -> IO Bool
sdlPushEvent event = with event $ \ptr -> toBool <$> sdlPushEventRaw ptr

foreign import ccall "SDL_SetEventFilter"
  sdlSetEventFilterRaw :: FunPtr SDLEventFilter -> Ptr () -> IO ()

sdlSetEventFilter :: SDLEventFilter -> Ptr () -> IO ()
sdlSetEventFilter filt userdata = do
  filtPtr <- wrapEventFilter filt
  sdlSetEventFilterRaw filtPtr userdata

foreign import ccall "dynamic"
  mkEventFilter :: FunPtr SDLEventFilter -> SDLEventFilter

foreign import ccall "SDL_GetEventFilter"
  sdlGetEventFilterRaw :: Ptr (FunPtr SDLEventFilter) -> Ptr (Ptr ()) -> IO CBool

sdlGetEventFilter :: IO (Maybe (SDLEventFilter, Ptr ()))
sdlGetEventFilter = alloca $ \filterPtr -> alloca $ \userdataPtr -> do
  success <- toBool <$> sdlGetEventFilterRaw filterPtr userdataPtr
  if success then do
    filt <- peek filterPtr
    userdata <- peek userdataPtr
    return $ Just (mkEventFilter filt, userdata)
  else return Nothing

foreign import ccall "SDL_AddEventWatch"
  sdlAddEventWatchRaw :: FunPtr SDLEventFilter -> Ptr () -> IO CBool

sdlAddEventWatch :: SDLEventFilter -> Ptr () -> IO Bool
sdlAddEventWatch filt userdata = do
  filterPtr <- wrapEventFilter filt
  toBool <$> sdlAddEventWatchRaw filterPtr userdata

foreign import ccall "SDL_RemoveEventWatch"
  sdlRemoveEventWatchRaw :: FunPtr SDLEventFilter -> Ptr () -> IO ()

sdlRemoveEventWatch :: SDLEventFilter -> Ptr () -> IO ()
sdlRemoveEventWatch filt userdata = do
  filterPtr <- wrapEventFilter filt
  sdlRemoveEventWatchRaw filterPtr userdata

foreign import ccall "SDL_FilterEvents"
  sdlFilterEventsRaw :: FunPtr SDLEventFilter -> Ptr () -> IO ()

sdlFilterEvents :: SDLEventFilter -> Ptr () -> IO ()
sdlFilterEvents filt userdata = do
  filterPtr <- wrapEventFilter filt
  sdlFilterEventsRaw filterPtr userdata

foreign import ccall "SDL_SetEventEnabled"
  sdlSetEventEnabledRaw :: Word32 -> CBool -> IO ()

sdlSetEventEnabled :: Word32 -> Bool -> IO ()
sdlSetEventEnabled t enabled = sdlSetEventEnabledRaw t (fromBool enabled)

foreign import ccall "SDL_EventEnabled"
  sdlEventEnabledRaw :: Word32 -> IO CBool

sdlEventEnabled :: Word32 -> IO Bool
sdlEventEnabled t = toBool <$> sdlEventEnabledRaw t

foreign import ccall "SDL_RegisterEvents"
  sdlRegisterEvents :: CInt -> IO Word32

foreign import ccall "SDL_GetWindowFromEvent"
  sdlGetWindowFromEventRaw :: Ptr SDLEvent -> IO (Ptr SDLWindow)

sdlGetWindowFromEvent :: SDLEvent -> IO (Maybe SDLWindow)
sdlGetWindowFromEvent event = with event $ \ptr -> do
  winPtr <- sdlGetWindowFromEventRaw ptr
  return $ if winPtr == nullPtr then Nothing else Just (SDLWindow winPtr)

-- Helper to wrap Haskell event filter into C-compatible FunPtr
foreign import ccall "wrapper"
 wrapEventFilter :: SDLEventFilter -> IO (FunPtr SDLEventFilter)

-- | Foreign import for SDL_GetEventDescription
foreign import ccall unsafe "SDL_GetEventDescription"
 c_sdlGetEventDescription :: Ptr SDLEvent -> CString -> CInt -> IO ()

-- | Get a human-readable description of an SDL_Event.
getEventDescription :: SDLEvent -> IO String
getEventDescription event =
 allocaBytes 256 $ \(buf :: CString) ->
   with event $ \eventPtr -> do
     c_sdlGetEventDescription eventPtr buf 256
     peekCString buf
