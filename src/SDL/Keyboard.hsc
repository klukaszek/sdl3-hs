{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : SDL.Keyboard
Description : SDL keyboard management
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

SDL keyboard management functions. This module provides functionality for
handling keyboard input, including querying connected keyboards, managing
keyboard focus, and handling text input.

Please refer to the Best Keyboard Practices document for details on how
best to accept keyboard input in various types of programs:
https://wiki.libsdl.org/SDL3/BestKeyboardPractices

Basic usage:
- Check for keyboard presence with 'sdlHasKeyboard'
- Get keyboard state with 'sdlGetKeyboardState'
- Manage text input with 'sdlStartTextInput' and 'sdlStopTextInput'
-}

module SDL.Keyboard
  ( -- * Types
    SDLKeyboardID(..)
  , SDLTextInputType(..)
  , SDLCapitalization(..)

    -- * Constants
  , pattern SDL_PROP_TEXTINPUT_TYPE_NUMBER
  , pattern SDL_PROP_TEXTINPUT_CAPITALIZATION_NUMBER
  , pattern SDL_PROP_TEXTINPUT_AUTOCORRECT_BOOLEAN
  , pattern SDL_PROP_TEXTINPUT_MULTILINE_BOOLEAN
  , pattern SDL_PROP_TEXTINPUT_ANDROID_INPUTTYPE_NUMBER

    -- * Keyboard Functions
  , sdlHasKeyboard
  , sdlGetKeyboards
  , sdlGetKeyboardNameForID
  , sdlGetKeyboardFocus
  , sdlGetKeyboardState
  , sdlResetKeyboard
  , sdlGetModState
  , sdlSetModState
  , sdlGetKeyFromScancode
  , sdlGetScancodeFromKey
  , sdlSetScancodeName
  , sdlGetScancodeName
  , sdlGetScancodeFromName
  , sdlGetKeyName
  , sdlGetKeyFromName
  , sdlStartTextInput
  , sdlStartTextInputWithProperties
  , sdlTextInputActive
  , sdlStopTextInput
  , sdlClearComposition
  , sdlSetTextInputArea
  , sdlGetTextInputArea
  , sdlHasScreenKeyboardSupport
  , sdlScreenKeyboardShown
  ) where

#include <SDL3/SDL_keyboard.h>

import Foreign (fromBool, toBool, with)
import Foreign.C.Types (CInt(..), CBool(..))
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (peek)
import Data.Word (Word32, Word16)
import SDL.Keycode (SDLKeycode(..), SDLKeymod(..))
import SDL.Properties (SDLPropertiesID(..))
import SDL.Rect (SDLRect(..))
import SDL.Scancode (SDLScancode(..))
import SDL.Video (SDLWindow(..))

-- | A unique ID for a keyboard, valid while connected to the system.
type SDLKeyboardID = Word32

-- | Text input type for 'sdlStartTextInputWithProperties'.
data SDLTextInputType
  = SDLTextInputTypeText
  | SDLTextInputTypeTextName
  | SDLTextInputTypeTextEmail
  | SDLTextInputTypeTextUsername
  | SDLTextInputTypeTextPasswordHidden
  | SDLTextInputTypeTextPasswordVisible
  | SDLTextInputTypeNumber
  | SDLTextInputTypeNumberPasswordHidden
  | SDLTextInputTypeNumberPasswordVisible
  deriving (Eq, Show, Enum)

-- | Auto capitalization type for 'sdlStartTextInputWithProperties'.
data SDLCapitalization
  = SDLCapitalizeNone
  | SDLCapitalizeSentences
  | SDLCapitalizeWords
  | SDLCapitalizeLetters
  deriving (Eq, Show, Enum)

-- Constants
pattern SDL_PROP_TEXTINPUT_TYPE_NUMBER = (#const_str SDL_PROP_TEXTINPUT_TYPE_NUMBER):: String
pattern SDL_PROP_TEXTINPUT_CAPITALIZATION_NUMBER = (#const_str SDL_PROP_TEXTINPUT_CAPITALIZATION_NUMBER) :: String
pattern SDL_PROP_TEXTINPUT_AUTOCORRECT_BOOLEAN = (#const_str SDL_PROP_TEXTINPUT_AUTOCORRECT_BOOLEAN) :: String
pattern SDL_PROP_TEXTINPUT_MULTILINE_BOOLEAN = (#const_str SDL_PROP_TEXTINPUT_MULTILINE_BOOLEAN) :: String
pattern SDL_PROP_TEXTINPUT_ANDROID_INPUTTYPE_NUMBER = (#const_str SDL_PROP_TEXTINPUT_ANDROID_INPUTTYPE_NUMBER) :: String

-- | Check if a keyboard is currently connected.
foreign import ccall "SDL_HasKeyboard"
  sdlHasKeyboard_ :: IO CBool

sdlHasKeyboard :: IO Bool
sdlHasKeyboard = toBool <$> sdlHasKeyboard_

-- | Get a list of currently connected keyboards.
foreign import ccall "SDL_GetKeyboards"
  sdlGetKeyboardsRaw :: Ptr CInt -> IO (Ptr SDLKeyboardID)

sdlGetKeyboards :: IO [SDLKeyboardID]
sdlGetKeyboards = alloca $ \countPtr -> do
  ptr <- sdlGetKeyboardsRaw countPtr
  count <- peek countPtr
  if ptr == nullPtr
    then return []
    else do
      keyboards <- peekArray (fromIntegral count) ptr
      free ptr
      return keyboards

-- | Get the name of a keyboard.
foreign import ccall "SDL_GetKeyboardNameForID"
  sdlGetKeyboardNameForIDRaw :: SDLKeyboardID -> IO CString

sdlGetKeyboardNameForID :: SDLKeyboardID -> IO String
sdlGetKeyboardNameForID kid = do
  cstr <- sdlGetKeyboardNameForIDRaw kid
  if cstr == nullPtr
    then return ""
    else peekCString cstr

-- | Query the window with current keyboard focus.
foreign import ccall "SDL_GetKeyboardFocus"
  sdlGetKeyboardFocusRaw :: IO (Ptr SDLWindow)

sdlGetKeyboardFocus :: IO (Maybe SDLWindow)
sdlGetKeyboardFocus = do
  ptr <- sdlGetKeyboardFocusRaw
  return $ if ptr == nullPtr then Nothing else Just (SDLWindow ptr)

-- | Get a snapshot of the current keyboard state.
foreign import ccall "SDL_GetKeyboardState"
  sdlGetKeyboardStateRaw :: Ptr CInt -> IO (Ptr CBool)

sdlGetKeyboardState :: IO [Bool]
sdlGetKeyboardState = alloca $ \numkeysPtr -> do
  ptr <- sdlGetKeyboardStateRaw numkeysPtr
  numkeys <- peek numkeysPtr
  map toBool <$> peekArray (fromIntegral numkeys) ptr

-- | Clear the state of the keyboard.
foreign import ccall "SDL_ResetKeyboard"
  sdlResetKeyboard :: IO ()

-- | Get the current key modifier state.
foreign import ccall "SDL_GetModState"
  sdlGetModState :: IO SDLKeymod

-- | Set the current key modifier state.
foreign import ccall "SDL_SetModState"
  sdlSetModState :: SDLKeymod -> IO ()

-- | Get key code from scancode with current keyboard layout.
foreign import ccall "SDL_GetKeyFromScancode"
  sdlGetKeyFromScancodeRaw :: SDLScancode -> SDLKeymod -> CBool -> IO SDLKeycode

sdlGetKeyFromScancode :: SDLScancode -> SDLKeymod -> Bool -> IO SDLKeycode
sdlGetKeyFromScancode scancode modstate keyEvent =
  sdlGetKeyFromScancodeRaw scancode modstate (fromBool keyEvent)

-- | Get scancode from key code with current keyboard layout.
foreign import ccall "SDL_GetScancodeFromKey"
  sdlGetScancodeFromKeyRaw :: Word32 -> Ptr Word16 -> IO CInt

sdlGetScancodeFromKey :: SDLKeycode -> IO (SDLScancode, Maybe SDLKeymod)
sdlGetScancodeFromKey key = alloca $ \modstatePtr -> do
  scancode <- sdlGetScancodeFromKeyRaw key modstatePtr
  modstate <- peek modstatePtr
  return (toEnum $ fromIntegral scancode, if modstate == 0 then Nothing else Just modstate)

-- | Set a human-readable name for a scancode.
foreign import ccall "SDL_SetScancodeName"
  sdlSetScancodeNameRaw :: Word32 -> CString -> IO CBool

sdlSetScancodeName :: SDLScancode -> String -> IO Bool
sdlSetScancodeName scancode name = withCString name $ \cstr ->
  toBool <$> sdlSetScancodeNameRaw scancode cstr

-- | Get a human-readable name for a scancode.
foreign import ccall "SDL_GetScancodeName"
  sdlGetScancodeNameRaw :: Word32 -> IO CString

sdlGetScancodeName :: SDLScancode -> IO String
sdlGetScancodeName scancode = do
  cstr <- sdlGetScancodeNameRaw scancode
  if cstr == nullPtr then return "" else peekCString cstr

-- | Get a scancode from a human-readable name.
foreign import ccall "SDL_GetScancodeFromName"
  sdlGetScancodeFromNameRaw :: CString -> IO CInt

sdlGetScancodeFromName :: String -> IO SDLScancode
sdlGetScancodeFromName name = withCString name $ \cstr ->
  toEnum . fromIntegral <$> sdlGetScancodeFromNameRaw cstr

-- | Get a human-readable name for a key.
foreign import ccall "SDL_GetKeyName"
  sdlGetKeyNameRaw :: Word32 -> IO CString

sdlGetKeyName :: SDLKeycode -> IO String
sdlGetKeyName key = do
  cstr <- sdlGetKeyNameRaw key
  if cstr == nullPtr then return "" else peekCString cstr

-- | Get a key code from a human-readable name.
foreign import ccall "SDL_GetKeyFromName"
  sdlGetKeyFromNameRaw :: CString -> IO Word32

sdlGetKeyFromName :: String -> IO SDLKeycode
sdlGetKeyFromName name = withCString name $ \cstr ->
  sdlGetKeyFromNameRaw cstr

-- | Start accepting Unicode text input events in a window.
foreign import ccall "SDL_StartTextInput"
  sdlStartTextInputRaw :: Ptr SDLWindow -> IO CBool

sdlStartTextInput :: SDLWindow -> IO Bool
sdlStartTextInput (SDLWindow window) = toBool <$> sdlStartTextInputRaw window

-- | Start text input with properties.
foreign import ccall "SDL_StartTextInputWithProperties"
  sdlStartTextInputWithPropertiesRaw :: Ptr SDLWindow -> SDLPropertiesID -> IO CBool

sdlStartTextInputWithProperties :: SDLWindow -> SDLPropertiesID -> IO Bool
sdlStartTextInputWithProperties (SDLWindow window) props =
  toBool <$> sdlStartTextInputWithPropertiesRaw window props

-- | Check if text input events are enabled for a window.
foreign import ccall "SDL_TextInputActive"
  sdlTextInputActiveRaw :: Ptr SDLWindow -> IO CBool

sdlTextInputActive :: SDLWindow -> IO Bool
sdlTextInputActive (SDLWindow window) = toBool <$> sdlTextInputActiveRaw window

-- | Stop receiving text input events in a window.
foreign import ccall "SDL_StopTextInput"
  sdlStopTextInputRaw :: Ptr SDLWindow -> IO CBool

sdlStopTextInput :: SDLWindow -> IO Bool
sdlStopTextInput (SDLWindow window) = toBool <$> sdlStopTextInputRaw window

-- | Dismiss the composition window/IME.
foreign import ccall "SDL_ClearComposition"
  sdlClearCompositionRaw :: Ptr SDLWindow -> IO CBool

sdlClearComposition :: SDLWindow -> IO Bool
sdlClearComposition (SDLWindow window) = toBool <$> sdlClearCompositionRaw window

-- | Set the area used to type Unicode text input.
foreign import ccall "SDL_SetTextInputArea"
  sdlSetTextInputAreaRaw :: Ptr SDLWindow -> Ptr SDLRect -> CInt -> IO CBool

sdlSetTextInputArea :: SDLWindow -> Maybe SDLRect -> Int -> IO Bool
sdlSetTextInputArea (SDLWindow window) mrect cursor = case mrect of
  Nothing -> toBool <$> sdlSetTextInputAreaRaw window nullPtr (fromIntegral cursor)
  Just rect -> with rect $ \rectPtr ->
    toBool <$> sdlSetTextInputAreaRaw window rectPtr (fromIntegral cursor)

-- | Get the area used to type Unicode text input.
foreign import ccall "SDL_GetTextInputArea"
  sdlGetTextInputAreaRaw :: Ptr SDLWindow -> Ptr SDLRect -> Ptr CInt -> IO CBool

sdlGetTextInputArea :: SDLWindow -> IO (Maybe SDLRect, Maybe Int)
sdlGetTextInputArea (SDLWindow window) = alloca $ \rectPtr -> alloca $ \cursorPtr -> do
  success <- toBool <$> sdlGetTextInputAreaRaw window rectPtr cursorPtr
  if not success
    then return (Nothing, Nothing)
    else do
      rect <- peek rectPtr
      cursor <- peek cursorPtr
      return (Just rect, Just $ fromIntegral cursor)

-- | Check if the platform has screen keyboard support.
foreign import ccall "SDL_HasScreenKeyboardSupport"
  sdlHasScreenKeyboardSupport_ :: IO CBool

sdlHasScreenKeyboardSupport :: IO Bool
sdlHasScreenKeyboardSupport = toBool <$> sdlHasScreenKeyboardSupport_

-- | Check if the screen keyboard is shown for a window.
foreign import ccall "SDL_ScreenKeyboardShown"
  sdlScreenKeyboardShownRaw :: Ptr SDLWindow -> IO CBool

sdlScreenKeyboardShown :: SDLWindow -> IO Bool
sdlScreenKeyboardShown (SDLWindow window) = toBool <$> sdlScreenKeyboardShownRaw window
