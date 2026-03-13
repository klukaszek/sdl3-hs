{-# LANGUAGE PatternSynonyms #-}

#include <SDL3/SDL_keyboard.h>

module SDL3.Raw.Keyboard
  ( SDLKeyboardID
  , SDLTextInputType(..)
  , SDLCapitalization(..)
  , pattern SDL_PROP_TEXTINPUT_TYPE_NUMBER
  , pattern SDL_PROP_TEXTINPUT_CAPITALIZATION_NUMBER
  , pattern SDL_PROP_TEXTINPUT_AUTOCORRECT_BOOLEAN
  , pattern SDL_PROP_TEXTINPUT_MULTILINE_BOOLEAN
  , pattern SDL_PROP_TEXTINPUT_ANDROID_INPUTTYPE_NUMBER
  , sdlHasKeyboardRaw
  , sdlGetKeyboardsRaw
  , sdlGetKeyboardNameForIDRaw
  , sdlGetKeyboardFocusRaw
  , sdlGetKeyboardStateRaw
  , sdlResetKeyboardRaw
  , sdlGetModStateRaw
  , sdlSetModStateRaw
  , sdlGetKeyFromScancodeRaw
  , sdlGetScancodeFromKeyRaw
  , sdlSetScancodeNameRaw
  , sdlGetScancodeNameRaw
  , sdlGetScancodeFromNameRaw
  , sdlGetKeyNameRaw
  , sdlGetKeyFromNameRaw
  , sdlStartTextInputRaw
  , sdlStartTextInputWithPropertiesRaw
  , sdlTextInputActiveRaw
  , sdlStopTextInputRaw
  , sdlClearCompositionRaw
  , sdlSetTextInputAreaRaw
  , sdlGetTextInputAreaRaw
  , sdlHasScreenKeyboardSupportRaw
  , sdlScreenKeyboardShownRaw
  ) where

import Data.Word (Word16, Word32)
import Foreign.C.String (CString)
import Foreign.C.Types (CBool(..), CInt(..))
import Foreign.Ptr (Ptr)
import SDL3.Raw.Keycode (SDLKeycode, SDLKeymod)
import SDL3.Raw.Properties (SDLPropertiesID)
import SDL3.Raw.Rect (SDLRect(..))
import SDL3.Raw.Scancode (SDLScancode)
import SDL3.Raw.Video (SDLWindow(..))

type SDLKeyboardID = Word32

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

data SDLCapitalization
  = SDLCapitalizeNone
  | SDLCapitalizeSentences
  | SDLCapitalizeWords
  | SDLCapitalizeLetters
  deriving (Eq, Show, Enum)

pattern SDL_PROP_TEXTINPUT_TYPE_NUMBER :: String
pattern SDL_PROP_TEXTINPUT_TYPE_NUMBER = #const_str SDL_PROP_TEXTINPUT_TYPE_NUMBER

pattern SDL_PROP_TEXTINPUT_CAPITALIZATION_NUMBER :: String
pattern SDL_PROP_TEXTINPUT_CAPITALIZATION_NUMBER = #const_str SDL_PROP_TEXTINPUT_CAPITALIZATION_NUMBER

pattern SDL_PROP_TEXTINPUT_AUTOCORRECT_BOOLEAN :: String
pattern SDL_PROP_TEXTINPUT_AUTOCORRECT_BOOLEAN = #const_str SDL_PROP_TEXTINPUT_AUTOCORRECT_BOOLEAN

pattern SDL_PROP_TEXTINPUT_MULTILINE_BOOLEAN :: String
pattern SDL_PROP_TEXTINPUT_MULTILINE_BOOLEAN = #const_str SDL_PROP_TEXTINPUT_MULTILINE_BOOLEAN

pattern SDL_PROP_TEXTINPUT_ANDROID_INPUTTYPE_NUMBER :: String
pattern SDL_PROP_TEXTINPUT_ANDROID_INPUTTYPE_NUMBER = #const_str SDL_PROP_TEXTINPUT_ANDROID_INPUTTYPE_NUMBER

foreign import ccall "SDL_HasKeyboard"
  sdlHasKeyboardRaw :: IO CBool

foreign import ccall "SDL_GetKeyboards"
  sdlGetKeyboardsRaw :: Ptr CInt -> IO (Ptr SDLKeyboardID)

foreign import ccall "SDL_GetKeyboardNameForID"
  sdlGetKeyboardNameForIDRaw :: SDLKeyboardID -> IO CString

foreign import ccall "SDL_GetKeyboardFocus"
  sdlGetKeyboardFocusRaw :: IO (Ptr SDLWindow)

foreign import ccall "SDL_GetKeyboardState"
  sdlGetKeyboardStateRaw :: Ptr CInt -> IO (Ptr CBool)

foreign import ccall "SDL_ResetKeyboard"
  sdlResetKeyboardRaw :: IO ()

foreign import ccall "SDL_GetModState"
  sdlGetModStateRaw :: IO SDLKeymod

foreign import ccall "SDL_SetModState"
  sdlSetModStateRaw :: SDLKeymod -> IO ()

foreign import ccall "SDL_GetKeyFromScancode"
  sdlGetKeyFromScancodeRaw :: SDLScancode -> SDLKeymod -> CBool -> IO SDLKeycode

foreign import ccall "SDL_GetScancodeFromKey"
  sdlGetScancodeFromKeyRaw :: Word32 -> Ptr Word16 -> IO CInt

foreign import ccall "SDL_SetScancodeName"
  sdlSetScancodeNameRaw :: Word32 -> CString -> IO CBool

foreign import ccall "SDL_GetScancodeName"
  sdlGetScancodeNameRaw :: Word32 -> IO CString

foreign import ccall "SDL_GetScancodeFromName"
  sdlGetScancodeFromNameRaw :: CString -> IO CInt

foreign import ccall "SDL_GetKeyName"
  sdlGetKeyNameRaw :: Word32 -> IO CString

foreign import ccall "SDL_GetKeyFromName"
  sdlGetKeyFromNameRaw :: CString -> IO Word32

foreign import ccall "SDL_StartTextInput"
  sdlStartTextInputRaw :: Ptr SDLWindow -> IO CBool

foreign import ccall "SDL_StartTextInputWithProperties"
  sdlStartTextInputWithPropertiesRaw :: Ptr SDLWindow -> SDLPropertiesID -> IO CBool

foreign import ccall "SDL_TextInputActive"
  sdlTextInputActiveRaw :: Ptr SDLWindow -> IO CBool

foreign import ccall "SDL_StopTextInput"
  sdlStopTextInputRaw :: Ptr SDLWindow -> IO CBool

foreign import ccall "SDL_ClearComposition"
  sdlClearCompositionRaw :: Ptr SDLWindow -> IO CBool

foreign import ccall "SDL_SetTextInputArea"
  sdlSetTextInputAreaRaw :: Ptr SDLWindow -> Ptr SDLRect -> CInt -> IO CBool

foreign import ccall "SDL_GetTextInputArea"
  sdlGetTextInputAreaRaw :: Ptr SDLWindow -> Ptr SDLRect -> Ptr CInt -> IO CBool

foreign import ccall "SDL_HasScreenKeyboardSupport"
  sdlHasScreenKeyboardSupportRaw :: IO CBool

foreign import ccall "SDL_ScreenKeyboardShown"
  sdlScreenKeyboardShownRaw :: Ptr SDLWindow -> IO CBool
