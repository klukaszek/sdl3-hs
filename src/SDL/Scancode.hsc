{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : SDL.Scancode
Description : SDL keyboard scancode definitions
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Defines keyboard scancodes for SDL. An SDL scancode is the physical representation
of a key on the keyboard, independent of language and keyboard mapping.

Please refer to the Best Keyboard Practices document for details on what
this information means and how best to use it:
https://wiki.libsdl.org/SDL3/BestKeyboardPractices

The values in this module are based on the USB usage page standard:
https://usb.org/sites/default/files/hut1_5.pdf
-}

module SDL.Scancode
  ( -- * Types
    SDLScancode
  , pattern SDL_SCANCODE_UNKNOWN
  , pattern SDL_SCANCODE_A
  , pattern SDL_SCANCODE_B
  , pattern SDL_SCANCODE_C
  , pattern SDL_SCANCODE_D
  , pattern SDL_SCANCODE_E
  , pattern SDL_SCANCODE_F
  , pattern SDL_SCANCODE_G
  , pattern SDL_SCANCODE_H
  , pattern SDL_SCANCODE_I
  , pattern SDL_SCANCODE_J
  , pattern SDL_SCANCODE_K
  , pattern SDL_SCANCODE_L
  , pattern SDL_SCANCODE_M
  , pattern SDL_SCANCODE_N
  , pattern SDL_SCANCODE_O
  , pattern SDL_SCANCODE_P
  , pattern SDL_SCANCODE_Q
  , pattern SDL_SCANCODE_R
  , pattern SDL_SCANCODE_S
  , pattern SDL_SCANCODE_T
  , pattern SDL_SCANCODE_U
  , pattern SDL_SCANCODE_V
  , pattern SDL_SCANCODE_W
  , pattern SDL_SCANCODE_X
  , pattern SDL_SCANCODE_Y
  , pattern SDL_SCANCODE_Z
  , pattern SDL_SCANCODE_1
  , pattern SDL_SCANCODE_2
  , pattern SDL_SCANCODE_3
  , pattern SDL_SCANCODE_4
  , pattern SDL_SCANCODE_5
  , pattern SDL_SCANCODE_6
  , pattern SDL_SCANCODE_7
  , pattern SDL_SCANCODE_8
  , pattern SDL_SCANCODE_9
  , pattern SDL_SCANCODE_0
  , pattern SDL_SCANCODE_RETURN
  , pattern SDL_SCANCODE_ESCAPE
  , pattern SDL_SCANCODE_BACKSPACE
  , pattern SDL_SCANCODE_TAB
  , pattern SDL_SCANCODE_SPACE
  , pattern SDL_SCANCODE_MINUS
  , pattern SDL_SCANCODE_EQUALS
  , pattern SDL_SCANCODE_LEFTBRACKET
  , pattern SDL_SCANCODE_RIGHTBRACKET
  , pattern SDL_SCANCODE_BACKSLASH
  , pattern SDL_SCANCODE_NONUSHASH
  , pattern SDL_SCANCODE_SEMICOLON
  , pattern SDL_SCANCODE_APOSTROPHE
  , pattern SDL_SCANCODE_GRAVE
  , pattern SDL_SCANCODE_COMMA
  , pattern SDL_SCANCODE_PERIOD
  , pattern SDL_SCANCODE_SLASH
  , pattern SDL_SCANCODE_CAPSLOCK
  , pattern SDL_SCANCODE_F1
  , pattern SDL_SCANCODE_F2
  , pattern SDL_SCANCODE_F3
  , pattern SDL_SCANCODE_F4
  , pattern SDL_SCANCODE_F5
  , pattern SDL_SCANCODE_F6
  , pattern SDL_SCANCODE_F7
  , pattern SDL_SCANCODE_F8
  , pattern SDL_SCANCODE_F9
  , pattern SDL_SCANCODE_F10
  , pattern SDL_SCANCODE_F11
  , pattern SDL_SCANCODE_F12
  , pattern SDL_SCANCODE_PRINTSCREEN
  , pattern SDL_SCANCODE_SCROLLLOCK
  , pattern SDL_SCANCODE_PAUSE
  , pattern SDL_SCANCODE_INSERT
  , pattern SDL_SCANCODE_HOME
  , pattern SDL_SCANCODE_PAGEUP
  , pattern SDL_SCANCODE_DELETE
  , pattern SDL_SCANCODE_END
  , pattern SDL_SCANCODE_PAGEDOWN
  , pattern SDL_SCANCODE_RIGHT
  , pattern SDL_SCANCODE_LEFT
  , pattern SDL_SCANCODE_DOWN
  , pattern SDL_SCANCODE_UP
  , pattern SDL_SCANCODE_NUMLOCKCLEAR
  , pattern SDL_SCANCODE_KP_DIVIDE
  , pattern SDL_SCANCODE_KP_MULTIPLY
  , pattern SDL_SCANCODE_KP_MINUS
  , pattern SDL_SCANCODE_KP_PLUS
  , pattern SDL_SCANCODE_KP_ENTER
  , pattern SDL_SCANCODE_KP_1
  , pattern SDL_SCANCODE_KP_2
  , pattern SDL_SCANCODE_KP_3
  , pattern SDL_SCANCODE_KP_4
  , pattern SDL_SCANCODE_KP_5
  , pattern SDL_SCANCODE_KP_6
  , pattern SDL_SCANCODE_KP_7
  , pattern SDL_SCANCODE_KP_8
  , pattern SDL_SCANCODE_KP_9
  , pattern SDL_SCANCODE_KP_0
  , pattern SDL_SCANCODE_KP_PERIOD
  , pattern SDL_SCANCODE_NONUSBACKSLASH
  , pattern SDL_SCANCODE_APPLICATION
  , pattern SDL_SCANCODE_POWER
  , pattern SDL_SCANCODE_KP_EQUALS
  , pattern SDL_SCANCODE_F13
  , pattern SDL_SCANCODE_F14
  , pattern SDL_SCANCODE_F15
  , pattern SDL_SCANCODE_F16
  , pattern SDL_SCANCODE_F17
  , pattern SDL_SCANCODE_F18
  , pattern SDL_SCANCODE_F19
  , pattern SDL_SCANCODE_F20
  , pattern SDL_SCANCODE_F21
  , pattern SDL_SCANCODE_F22
  , pattern SDL_SCANCODE_F23
  , pattern SDL_SCANCODE_F24
  , pattern SDL_SCANCODE_EXECUTE
  , pattern SDL_SCANCODE_HELP
  , pattern SDL_SCANCODE_MENU
  , pattern SDL_SCANCODE_SELECT
  , pattern SDL_SCANCODE_STOP
  , pattern SDL_SCANCODE_AGAIN
  , pattern SDL_SCANCODE_UNDO
  , pattern SDL_SCANCODE_CUT
  , pattern SDL_SCANCODE_COPY
  , pattern SDL_SCANCODE_PASTE
  , pattern SDL_SCANCODE_FIND
  , pattern SDL_SCANCODE_MUTE
  , pattern SDL_SCANCODE_VOLUMEUP
  , pattern SDL_SCANCODE_VOLUMEDOWN
  , pattern SDL_SCANCODE_KP_COMMA
  , pattern SDL_SCANCODE_KP_EQUALSAS400
  , pattern SDL_SCANCODE_INTERNATIONAL1
  , pattern SDL_SCANCODE_INTERNATIONAL2
  , pattern SDL_SCANCODE_INTERNATIONAL3
  , pattern SDL_SCANCODE_INTERNATIONAL4
  , pattern SDL_SCANCODE_INTERNATIONAL5
  , pattern SDL_SCANCODE_INTERNATIONAL6
  , pattern SDL_SCANCODE_INTERNATIONAL7
  , pattern SDL_SCANCODE_INTERNATIONAL8
  , pattern SDL_SCANCODE_INTERNATIONAL9
  , pattern SDL_SCANCODE_LANG1
  , pattern SDL_SCANCODE_LANG2
  , pattern SDL_SCANCODE_LANG3
  , pattern SDL_SCANCODE_LANG4
  , pattern SDL_SCANCODE_LANG5
  , pattern SDL_SCANCODE_LANG6
  , pattern SDL_SCANCODE_LANG7
  , pattern SDL_SCANCODE_LANG8
  , pattern SDL_SCANCODE_LANG9
  , pattern SDL_SCANCODE_ALTERASE
  , pattern SDL_SCANCODE_SYSREQ
  , pattern SDL_SCANCODE_CANCEL
  , pattern SDL_SCANCODE_CLEAR
  , pattern SDL_SCANCODE_PRIOR
  , pattern SDL_SCANCODE_RETURN2
  , pattern SDL_SCANCODE_SEPARATOR
  , pattern SDL_SCANCODE_OUT
  , pattern SDL_SCANCODE_OPER
  , pattern SDL_SCANCODE_CLEARAGAIN
  , pattern SDL_SCANCODE_CRSEL
  , pattern SDL_SCANCODE_EXSEL
  , pattern SDL_SCANCODE_KP_00
  , pattern SDL_SCANCODE_KP_000
  , pattern SDL_SCANCODE_THOUSANDSSEPARATOR
  , pattern SDL_SCANCODE_DECIMALSEPARATOR
  , pattern SDL_SCANCODE_CURRENCYUNIT
  , pattern SDL_SCANCODE_CURRENCYSUBUNIT
  , pattern SDL_SCANCODE_KP_LEFTPAREN
  , pattern SDL_SCANCODE_KP_RIGHTPAREN
  , pattern SDL_SCANCODE_KP_LEFTBRACE
  , pattern SDL_SCANCODE_KP_RIGHTBRACE
  , pattern SDL_SCANCODE_KP_TAB
  , pattern SDL_SCANCODE_KP_BACKSPACE
  , pattern SDL_SCANCODE_KP_A
  , pattern SDL_SCANCODE_KP_B
  , pattern SDL_SCANCODE_KP_C
  , pattern SDL_SCANCODE_KP_D
  , pattern SDL_SCANCODE_KP_E
  , pattern SDL_SCANCODE_KP_F
  , pattern SDL_SCANCODE_KP_XOR
  , pattern SDL_SCANCODE_KP_POWER
  , pattern SDL_SCANCODE_KP_PERCENT
  , pattern SDL_SCANCODE_KP_LESS
  , pattern SDL_SCANCODE_KP_GREATER
  , pattern SDL_SCANCODE_KP_AMPERSAND
  , pattern SDL_SCANCODE_KP_DBLAMPERSAND
  , pattern SDL_SCANCODE_KP_VERTICALBAR
  , pattern SDL_SCANCODE_KP_DBLVERTICALBAR
  , pattern SDL_SCANCODE_KP_COLON
  , pattern SDL_SCANCODE_KP_HASH
  , pattern SDL_SCANCODE_KP_SPACE
  , pattern SDL_SCANCODE_KP_AT
  , pattern SDL_SCANCODE_KP_EXCLAM
  , pattern SDL_SCANCODE_KP_MEMSTORE
  , pattern SDL_SCANCODE_KP_MEMRECALL
  , pattern SDL_SCANCODE_KP_MEMCLEAR
  , pattern SDL_SCANCODE_KP_MEMADD
  , pattern SDL_SCANCODE_KP_MEMSUBTRACT
  , pattern SDL_SCANCODE_KP_MEMMULTIPLY
  , pattern SDL_SCANCODE_KP_MEMDIVIDE
  , pattern SDL_SCANCODE_KP_PLUSMINUS
  , pattern SDL_SCANCODE_KP_CLEAR
  , pattern SDL_SCANCODE_KP_CLEARENTRY
  , pattern SDL_SCANCODE_KP_BINARY
  , pattern SDL_SCANCODE_KP_OCTAL
  , pattern SDL_SCANCODE_KP_DECIMAL
  , pattern SDL_SCANCODE_KP_HEXADECIMAL
  , pattern SDL_SCANCODE_LCTRL
  , pattern SDL_SCANCODE_LSHIFT
  , pattern SDL_SCANCODE_LALT
  , pattern SDL_SCANCODE_LGUI
  , pattern SDL_SCANCODE_RCTRL
  , pattern SDL_SCANCODE_RSHIFT
  , pattern SDL_SCANCODE_RALT
  , pattern SDL_SCANCODE_RGUI
  , pattern SDL_SCANCODE_MODE
  , pattern SDL_SCANCODE_SLEEP
  , pattern SDL_SCANCODE_WAKE
  , pattern SDL_SCANCODE_CHANNEL_INCREMENT
  , pattern SDL_SCANCODE_CHANNEL_DECREMENT
  , pattern SDL_SCANCODE_MEDIA_PLAY
  , pattern SDL_SCANCODE_MEDIA_PAUSE
  , pattern SDL_SCANCODE_MEDIA_RECORD
  , pattern SDL_SCANCODE_MEDIA_FAST_FORWARD
  , pattern SDL_SCANCODE_MEDIA_REWIND
  , pattern SDL_SCANCODE_MEDIA_NEXT_TRACK
  , pattern SDL_SCANCODE_MEDIA_PREVIOUS_TRACK
  , pattern SDL_SCANCODE_MEDIA_STOP
  , pattern SDL_SCANCODE_MEDIA_EJECT
  , pattern SDL_SCANCODE_MEDIA_PLAY_PAUSE
  , pattern SDL_SCANCODE_MEDIA_SELECT
  , pattern SDL_SCANCODE_AC_NEW
  , pattern SDL_SCANCODE_AC_OPEN
  , pattern SDL_SCANCODE_AC_CLOSE
  , pattern SDL_SCANCODE_AC_EXIT
  , pattern SDL_SCANCODE_AC_SAVE
  , pattern SDL_SCANCODE_AC_PRINT
  , pattern SDL_SCANCODE_AC_PROPERTIES
  , pattern SDL_SCANCODE_AC_SEARCH
  , pattern SDL_SCANCODE_AC_HOME
  , pattern SDL_SCANCODE_AC_BACK
  , pattern SDL_SCANCODE_AC_FORWARD
  , pattern SDL_SCANCODE_AC_STOP
  , pattern SDL_SCANCODE_AC_REFRESH
  , pattern SDL_SCANCODE_AC_BOOKMARKS
  , pattern SDL_SCANCODE_SOFTLEFT
  , pattern SDL_SCANCODE_SOFTRIGHT
  , pattern SDL_SCANCODE_CALL
  , pattern SDL_SCANCODE_ENDCALL
  , pattern SDL_SCANCODE_RESERVED
  , pattern SDL_SCANCODE_COUNT
  ) where

#include <SDL3/SDL_keyboard.h>
#include <SDL3/SDL_scancode.h>

import Foreign (fromBool, toBool, with)
import Foreign.C.Types (CInt(..), CBool(..))
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (peek)
import Data.Word (Word32, Word16)
import SDL.Properties (SDLPropertiesID(..))
import SDL.Rect (SDLRect(..))
import SDL.Video (SDLWindow(..))

-- | A unique ID for a keyboard, valid while connected to the system.
type SDLKeyboardID = Word32

-- | Scancode values as defined in SDL_scancode.h, represented as Word32
type SDLScancode = Word32

-- Pattern synonyms for SDLScancode, mapping to C constants

pattern SDL_SCANCODE_UNKNOWN = (#const SDL_SCANCODE_UNKNOWN) :: SDLScancode
pattern SDL_SCANCODE_A = (#const SDL_SCANCODE_A) :: SDLScancode
pattern SDL_SCANCODE_B = (#const SDL_SCANCODE_B) :: SDLScancode
pattern SDL_SCANCODE_C = (#const SDL_SCANCODE_C) :: SDLScancode
pattern SDL_SCANCODE_D = (#const SDL_SCANCODE_D) :: SDLScancode
pattern SDL_SCANCODE_E = (#const SDL_SCANCODE_E) :: SDLScancode
pattern SDL_SCANCODE_F = (#const SDL_SCANCODE_F) :: SDLScancode
pattern SDL_SCANCODE_G = (#const SDL_SCANCODE_G) :: SDLScancode
pattern SDL_SCANCODE_H = (#const SDL_SCANCODE_H) :: SDLScancode
pattern SDL_SCANCODE_I = (#const SDL_SCANCODE_I) :: SDLScancode
pattern SDL_SCANCODE_J = (#const SDL_SCANCODE_J) :: SDLScancode
pattern SDL_SCANCODE_K = (#const SDL_SCANCODE_K) :: SDLScancode
pattern SDL_SCANCODE_L = (#const SDL_SCANCODE_L) :: SDLScancode
pattern SDL_SCANCODE_M = (#const SDL_SCANCODE_M) :: SDLScancode
pattern SDL_SCANCODE_N = (#const SDL_SCANCODE_N) :: SDLScancode
pattern SDL_SCANCODE_O = (#const SDL_SCANCODE_O) :: SDLScancode
pattern SDL_SCANCODE_P = (#const SDL_SCANCODE_P) :: SDLScancode
pattern SDL_SCANCODE_Q = (#const SDL_SCANCODE_Q) :: SDLScancode
pattern SDL_SCANCODE_R = (#const SDL_SCANCODE_R) :: SDLScancode
pattern SDL_SCANCODE_S = (#const SDL_SCANCODE_S) :: SDLScancode
pattern SDL_SCANCODE_T = (#const SDL_SCANCODE_T) :: SDLScancode
pattern SDL_SCANCODE_U = (#const SDL_SCANCODE_U) :: SDLScancode
pattern SDL_SCANCODE_V = (#const SDL_SCANCODE_V) :: SDLScancode
pattern SDL_SCANCODE_W = (#const SDL_SCANCODE_W) :: SDLScancode
pattern SDL_SCANCODE_X = (#const SDL_SCANCODE_X) :: SDLScancode
pattern SDL_SCANCODE_Y = (#const SDL_SCANCODE_Y) :: SDLScancode
pattern SDL_SCANCODE_Z = (#const SDL_SCANCODE_Z) :: SDLScancode
pattern SDL_SCANCODE_1 = (#const SDL_SCANCODE_1) :: SDLScancode
pattern SDL_SCANCODE_2 = (#const SDL_SCANCODE_2) :: SDLScancode
pattern SDL_SCANCODE_3 = (#const SDL_SCANCODE_3) :: SDLScancode
pattern SDL_SCANCODE_4 = (#const SDL_SCANCODE_4) :: SDLScancode
pattern SDL_SCANCODE_5 = (#const SDL_SCANCODE_5) :: SDLScancode
pattern SDL_SCANCODE_6 = (#const SDL_SCANCODE_6) :: SDLScancode
pattern SDL_SCANCODE_7 = (#const SDL_SCANCODE_7) :: SDLScancode
pattern SDL_SCANCODE_8 = (#const SDL_SCANCODE_8) :: SDLScancode
pattern SDL_SCANCODE_9 = (#const SDL_SCANCODE_9) :: SDLScancode
pattern SDL_SCANCODE_0 = (#const SDL_SCANCODE_0) :: SDLScancode
pattern SDL_SCANCODE_RETURN = (#const SDL_SCANCODE_RETURN) :: SDLScancode
pattern SDL_SCANCODE_ESCAPE = (#const SDL_SCANCODE_ESCAPE) :: SDLScancode
pattern SDL_SCANCODE_BACKSPACE = (#const SDL_SCANCODE_BACKSPACE) :: SDLScancode
pattern SDL_SCANCODE_TAB = (#const SDL_SCANCODE_TAB) :: SDLScancode
pattern SDL_SCANCODE_SPACE = (#const SDL_SCANCODE_SPACE) :: SDLScancode
pattern SDL_SCANCODE_MINUS = (#const SDL_SCANCODE_MINUS) :: SDLScancode
pattern SDL_SCANCODE_EQUALS = (#const SDL_SCANCODE_EQUALS) :: SDLScancode
pattern SDL_SCANCODE_LEFTBRACKET = (#const SDL_SCANCODE_LEFTBRACKET) :: SDLScancode
pattern SDL_SCANCODE_RIGHTBRACKET = (#const SDL_SCANCODE_RIGHTBRACKET) :: SDLScancode
pattern SDL_SCANCODE_BACKSLASH = (#const SDL_SCANCODE_BACKSLASH) :: SDLScancode
pattern SDL_SCANCODE_NONUSHASH = (#const SDL_SCANCODE_NONUSHASH) :: SDLScancode
pattern SDL_SCANCODE_SEMICOLON = (#const SDL_SCANCODE_SEMICOLON) :: SDLScancode
pattern SDL_SCANCODE_APOSTROPHE = (#const SDL_SCANCODE_APOSTROPHE) :: SDLScancode
pattern SDL_SCANCODE_GRAVE = (#const SDL_SCANCODE_GRAVE) :: SDLScancode
pattern SDL_SCANCODE_COMMA = (#const SDL_SCANCODE_COMMA) :: SDLScancode
pattern SDL_SCANCODE_PERIOD = (#const SDL_SCANCODE_PERIOD) :: SDLScancode
pattern SDL_SCANCODE_SLASH = (#const SDL_SCANCODE_SLASH) :: SDLScancode
pattern SDL_SCANCODE_CAPSLOCK = (#const SDL_SCANCODE_CAPSLOCK) :: SDLScancode
pattern SDL_SCANCODE_F1 = (#const SDL_SCANCODE_F1) :: SDLScancode
pattern SDL_SCANCODE_F2 = (#const SDL_SCANCODE_F2) :: SDLScancode
pattern SDL_SCANCODE_F3 = (#const SDL_SCANCODE_F3) :: SDLScancode
pattern SDL_SCANCODE_F4 = (#const SDL_SCANCODE_F4) :: SDLScancode
pattern SDL_SCANCODE_F5 = (#const SDL_SCANCODE_F5) :: SDLScancode
pattern SDL_SCANCODE_F6 = (#const SDL_SCANCODE_F6) :: SDLScancode
pattern SDL_SCANCODE_F7 = (#const SDL_SCANCODE_F7) :: SDLScancode
pattern SDL_SCANCODE_F8 = (#const SDL_SCANCODE_F8) :: SDLScancode
pattern SDL_SCANCODE_F9 = (#const SDL_SCANCODE_F9) :: SDLScancode
pattern SDL_SCANCODE_F10 = (#const SDL_SCANCODE_F10) :: SDLScancode
pattern SDL_SCANCODE_F11 = (#const SDL_SCANCODE_F11) :: SDLScancode
pattern SDL_SCANCODE_F12 = (#const SDL_SCANCODE_F12) :: SDLScancode
pattern SDL_SCANCODE_PRINTSCREEN = (#const SDL_SCANCODE_PRINTSCREEN) :: SDLScancode
pattern SDL_SCANCODE_SCROLLLOCK = (#const SDL_SCANCODE_SCROLLLOCK) :: SDLScancode
pattern SDL_SCANCODE_PAUSE = (#const SDL_SCANCODE_PAUSE) :: SDLScancode
pattern SDL_SCANCODE_INSERT = (#const SDL_SCANCODE_INSERT) :: SDLScancode
pattern SDL_SCANCODE_HOME = (#const SDL_SCANCODE_HOME) :: SDLScancode
pattern SDL_SCANCODE_PAGEUP = (#const SDL_SCANCODE_PAGEUP) :: SDLScancode
pattern SDL_SCANCODE_DELETE = (#const SDL_SCANCODE_DELETE) :: SDLScancode
pattern SDL_SCANCODE_END = (#const SDL_SCANCODE_END) :: SDLScancode
pattern SDL_SCANCODE_PAGEDOWN = (#const SDL_SCANCODE_PAGEDOWN) :: SDLScancode
pattern SDL_SCANCODE_RIGHT = (#const SDL_SCANCODE_RIGHT) :: SDLScancode
pattern SDL_SCANCODE_LEFT = (#const SDL_SCANCODE_LEFT) :: SDLScancode
pattern SDL_SCANCODE_DOWN = (#const SDL_SCANCODE_DOWN) :: SDLScancode
pattern SDL_SCANCODE_UP = (#const SDL_SCANCODE_UP) :: SDLScancode
pattern SDL_SCANCODE_NUMLOCKCLEAR = (#const SDL_SCANCODE_NUMLOCKCLEAR) :: SDLScancode
pattern SDL_SCANCODE_KP_DIVIDE = (#const SDL_SCANCODE_KP_DIVIDE) :: SDLScancode
pattern SDL_SCANCODE_KP_MULTIPLY = (#const SDL_SCANCODE_KP_MULTIPLY) :: SDLScancode
pattern SDL_SCANCODE_KP_MINUS = (#const SDL_SCANCODE_KP_MINUS) :: SDLScancode
pattern SDL_SCANCODE_KP_PLUS = (#const SDL_SCANCODE_KP_PLUS) :: SDLScancode
pattern SDL_SCANCODE_KP_ENTER = (#const SDL_SCANCODE_KP_ENTER) :: SDLScancode
pattern SDL_SCANCODE_KP_1 = (#const SDL_SCANCODE_KP_1) :: SDLScancode
pattern SDL_SCANCODE_KP_2 = (#const SDL_SCANCODE_KP_2) :: SDLScancode
pattern SDL_SCANCODE_KP_3 = (#const SDL_SCANCODE_KP_3) :: SDLScancode
pattern SDL_SCANCODE_KP_4 = (#const SDL_SCANCODE_KP_4) :: SDLScancode
pattern SDL_SCANCODE_KP_5 = (#const SDL_SCANCODE_KP_5) :: SDLScancode
pattern SDL_SCANCODE_KP_6 = (#const SDL_SCANCODE_KP_6) :: SDLScancode
pattern SDL_SCANCODE_KP_7 = (#const SDL_SCANCODE_KP_7) :: SDLScancode
pattern SDL_SCANCODE_KP_8 = (#const SDL_SCANCODE_KP_8) :: SDLScancode
pattern SDL_SCANCODE_KP_9 = (#const SDL_SCANCODE_KP_9) :: SDLScancode
pattern SDL_SCANCODE_KP_0 = (#const SDL_SCANCODE_KP_0) :: SDLScancode
pattern SDL_SCANCODE_KP_PERIOD = (#const SDL_SCANCODE_KP_PERIOD) :: SDLScancode
pattern SDL_SCANCODE_NONUSBACKSLASH = (#const SDL_SCANCODE_NONUSBACKSLASH) :: SDLScancode
pattern SDL_SCANCODE_APPLICATION = (#const SDL_SCANCODE_APPLICATION) :: SDLScancode
pattern SDL_SCANCODE_POWER = (#const SDL_SCANCODE_POWER) :: SDLScancode
pattern SDL_SCANCODE_KP_EQUALS = (#const SDL_SCANCODE_KP_EQUALS) :: SDLScancode
pattern SDL_SCANCODE_F13 = (#const SDL_SCANCODE_F13) :: SDLScancode
pattern SDL_SCANCODE_F14 = (#const SDL_SCANCODE_F14) :: SDLScancode
pattern SDL_SCANCODE_F15 = (#const SDL_SCANCODE_F15) :: SDLScancode
pattern SDL_SCANCODE_F16 = (#const SDL_SCANCODE_F16) :: SDLScancode
pattern SDL_SCANCODE_F17 = (#const SDL_SCANCODE_F17) :: SDLScancode
pattern SDL_SCANCODE_F18 = (#const SDL_SCANCODE_F18) :: SDLScancode
pattern SDL_SCANCODE_F19 = (#const SDL_SCANCODE_F19) :: SDLScancode
pattern SDL_SCANCODE_F20 = (#const SDL_SCANCODE_F20) :: SDLScancode
pattern SDL_SCANCODE_F21 = (#const SDL_SCANCODE_F21) :: SDLScancode
pattern SDL_SCANCODE_F22 = (#const SDL_SCANCODE_F22) :: SDLScancode
pattern SDL_SCANCODE_F23 = (#const SDL_SCANCODE_F23) :: SDLScancode
pattern SDL_SCANCODE_F24 = (#const SDL_SCANCODE_F24) :: SDLScancode
pattern SDL_SCANCODE_EXECUTE = (#const SDL_SCANCODE_EXECUTE) :: SDLScancode
pattern SDL_SCANCODE_HELP = (#const SDL_SCANCODE_HELP) :: SDLScancode
pattern SDL_SCANCODE_MENU = (#const SDL_SCANCODE_MENU) :: SDLScancode
pattern SDL_SCANCODE_SELECT = (#const SDL_SCANCODE_SELECT) :: SDLScancode
pattern SDL_SCANCODE_STOP = (#const SDL_SCANCODE_STOP) :: SDLScancode
pattern SDL_SCANCODE_AGAIN = (#const SDL_SCANCODE_AGAIN) :: SDLScancode
pattern SDL_SCANCODE_UNDO = (#const SDL_SCANCODE_UNDO) :: SDLScancode
pattern SDL_SCANCODE_CUT = (#const SDL_SCANCODE_CUT) :: SDLScancode
pattern SDL_SCANCODE_COPY = (#const SDL_SCANCODE_COPY) :: SDLScancode
pattern SDL_SCANCODE_PASTE = (#const SDL_SCANCODE_PASTE) :: SDLScancode
pattern SDL_SCANCODE_FIND = (#const SDL_SCANCODE_FIND) :: SDLScancode
pattern SDL_SCANCODE_MUTE = (#const SDL_SCANCODE_MUTE) :: SDLScancode
pattern SDL_SCANCODE_VOLUMEUP = (#const SDL_SCANCODE_VOLUMEUP) :: SDLScancode
pattern SDL_SCANCODE_VOLUMEDOWN = (#const SDL_SCANCODE_VOLUMEDOWN) :: SDLScancode
pattern SDL_SCANCODE_KP_COMMA = (#const SDL_SCANCODE_KP_COMMA) :: SDLScancode
pattern SDL_SCANCODE_KP_EQUALSAS400 = (#const SDL_SCANCODE_KP_EQUALSAS400) :: SDLScancode
pattern SDL_SCANCODE_INTERNATIONAL1 = (#const SDL_SCANCODE_INTERNATIONAL1) :: SDLScancode
pattern SDL_SCANCODE_INTERNATIONAL2 = (#const SDL_SCANCODE_INTERNATIONAL2) :: SDLScancode
pattern SDL_SCANCODE_INTERNATIONAL3 = (#const SDL_SCANCODE_INTERNATIONAL3) :: SDLScancode
pattern SDL_SCANCODE_INTERNATIONAL4 = (#const SDL_SCANCODE_INTERNATIONAL4) :: SDLScancode
pattern SDL_SCANCODE_INTERNATIONAL5 = (#const SDL_SCANCODE_INTERNATIONAL5) :: SDLScancode
pattern SDL_SCANCODE_INTERNATIONAL6 = (#const SDL_SCANCODE_INTERNATIONAL6) :: SDLScancode
pattern SDL_SCANCODE_INTERNATIONAL7 = (#const SDL_SCANCODE_INTERNATIONAL7) :: SDLScancode
pattern SDL_SCANCODE_INTERNATIONAL8 = (#const SDL_SCANCODE_INTERNATIONAL8) :: SDLScancode
pattern SDL_SCANCODE_INTERNATIONAL9 = (#const SDL_SCANCODE_INTERNATIONAL9) :: SDLScancode
pattern SDL_SCANCODE_LANG1 = (#const SDL_SCANCODE_LANG1) :: SDLScancode
pattern SDL_SCANCODE_LANG2 = (#const SDL_SCANCODE_LANG2) :: SDLScancode
pattern SDL_SCANCODE_LANG3 = (#const SDL_SCANCODE_LANG3) :: SDLScancode
pattern SDL_SCANCODE_LANG4 = (#const SDL_SCANCODE_LANG4) :: SDLScancode
pattern SDL_SCANCODE_LANG5 = (#const SDL_SCANCODE_LANG5) :: SDLScancode
pattern SDL_SCANCODE_LANG6 = (#const SDL_SCANCODE_LANG6) :: SDLScancode
pattern SDL_SCANCODE_LANG7 = (#const SDL_SCANCODE_LANG7) :: SDLScancode
pattern SDL_SCANCODE_LANG8 = (#const SDL_SCANCODE_LANG8) :: SDLScancode
pattern SDL_SCANCODE_LANG9 = (#const SDL_SCANCODE_LANG9) :: SDLScancode
pattern SDL_SCANCODE_ALTERASE = (#const SDL_SCANCODE_ALTERASE) :: SDLScancode
pattern SDL_SCANCODE_SYSREQ = (#const SDL_SCANCODE_SYSREQ) :: SDLScancode
pattern SDL_SCANCODE_CANCEL = (#const SDL_SCANCODE_CANCEL) :: SDLScancode
pattern SDL_SCANCODE_CLEAR = (#const SDL_SCANCODE_CLEAR) :: SDLScancode
pattern SDL_SCANCODE_PRIOR = (#const SDL_SCANCODE_PRIOR) :: SDLScancode
pattern SDL_SCANCODE_RETURN2 = (#const SDL_SCANCODE_RETURN2) :: SDLScancode
pattern SDL_SCANCODE_SEPARATOR = (#const SDL_SCANCODE_SEPARATOR) :: SDLScancode
pattern SDL_SCANCODE_OUT = (#const SDL_SCANCODE_OUT) :: SDLScancode
pattern SDL_SCANCODE_OPER = (#const SDL_SCANCODE_OPER) :: SDLScancode
pattern SDL_SCANCODE_CLEARAGAIN = (#const SDL_SCANCODE_CLEARAGAIN) :: SDLScancode
pattern SDL_SCANCODE_CRSEL = (#const SDL_SCANCODE_CRSEL) :: SDLScancode
pattern SDL_SCANCODE_EXSEL = (#const SDL_SCANCODE_EXSEL) :: SDLScancode
pattern SDL_SCANCODE_KP_00 = (#const SDL_SCANCODE_KP_00) :: SDLScancode
pattern SDL_SCANCODE_KP_000 = (#const SDL_SCANCODE_KP_000) :: SDLScancode
pattern SDL_SCANCODE_THOUSANDSSEPARATOR = (#const SDL_SCANCODE_THOUSANDSSEPARATOR) :: SDLScancode
pattern SDL_SCANCODE_DECIMALSEPARATOR = (#const SDL_SCANCODE_DECIMALSEPARATOR) :: SDLScancode
pattern SDL_SCANCODE_CURRENCYUNIT = (#const SDL_SCANCODE_CURRENCYUNIT) :: SDLScancode
pattern SDL_SCANCODE_CURRENCYSUBUNIT = (#const SDL_SCANCODE_CURRENCYSUBUNIT) :: SDLScancode
pattern SDL_SCANCODE_KP_LEFTPAREN = (#const SDL_SCANCODE_KP_LEFTPAREN) :: SDLScancode
pattern SDL_SCANCODE_KP_RIGHTPAREN = (#const SDL_SCANCODE_KP_RIGHTPAREN) :: SDLScancode
pattern SDL_SCANCODE_KP_LEFTBRACE = (#const SDL_SCANCODE_KP_LEFTBRACE) :: SDLScancode
pattern SDL_SCANCODE_KP_RIGHTBRACE = (#const SDL_SCANCODE_KP_RIGHTBRACE) :: SDLScancode
pattern SDL_SCANCODE_KP_TAB = (#const SDL_SCANCODE_KP_TAB) :: SDLScancode
pattern SDL_SCANCODE_KP_BACKSPACE = (#const SDL_SCANCODE_KP_BACKSPACE) :: SDLScancode
pattern SDL_SCANCODE_KP_A = (#const SDL_SCANCODE_KP_A) :: SDLScancode
pattern SDL_SCANCODE_KP_B = (#const SDL_SCANCODE_KP_B) :: SDLScancode
pattern SDL_SCANCODE_KP_C = (#const SDL_SCANCODE_KP_C) :: SDLScancode
pattern SDL_SCANCODE_KP_D = (#const SDL_SCANCODE_KP_D) :: SDLScancode
pattern SDL_SCANCODE_KP_E = (#const SDL_SCANCODE_KP_E) :: SDLScancode
pattern SDL_SCANCODE_KP_F = (#const SDL_SCANCODE_KP_F) :: SDLScancode
pattern SDL_SCANCODE_KP_XOR = (#const SDL_SCANCODE_KP_XOR) :: SDLScancode
pattern SDL_SCANCODE_KP_POWER = (#const SDL_SCANCODE_KP_POWER) :: SDLScancode
pattern SDL_SCANCODE_KP_PERCENT = (#const SDL_SCANCODE_KP_PERCENT) :: SDLScancode
pattern SDL_SCANCODE_KP_LESS = (#const SDL_SCANCODE_KP_LESS) :: SDLScancode
pattern SDL_SCANCODE_KP_GREATER = (#const SDL_SCANCODE_KP_GREATER) :: SDLScancode
pattern SDL_SCANCODE_KP_AMPERSAND = (#const SDL_SCANCODE_KP_AMPERSAND) :: SDLScancode
pattern SDL_SCANCODE_KP_DBLAMPERSAND = (#const SDL_SCANCODE_KP_DBLAMPERSAND) :: SDLScancode
pattern SDL_SCANCODE_KP_VERTICALBAR = (#const SDL_SCANCODE_KP_VERTICALBAR) :: SDLScancode
pattern SDL_SCANCODE_KP_DBLVERTICALBAR = (#const SDL_SCANCODE_KP_DBLVERTICALBAR) :: SDLScancode
pattern SDL_SCANCODE_KP_COLON = (#const SDL_SCANCODE_KP_COLON) :: SDLScancode
pattern SDL_SCANCODE_KP_HASH = (#const SDL_SCANCODE_KP_HASH) :: SDLScancode
pattern SDL_SCANCODE_KP_SPACE = (#const SDL_SCANCODE_KP_SPACE) :: SDLScancode
pattern SDL_SCANCODE_KP_AT = (#const SDL_SCANCODE_KP_AT) :: SDLScancode
pattern SDL_SCANCODE_KP_EXCLAM = (#const SDL_SCANCODE_KP_EXCLAM) :: SDLScancode
pattern SDL_SCANCODE_KP_MEMSTORE = (#const SDL_SCANCODE_KP_MEMSTORE) :: SDLScancode
pattern SDL_SCANCODE_KP_MEMRECALL = (#const SDL_SCANCODE_KP_MEMRECALL) :: SDLScancode
pattern SDL_SCANCODE_KP_MEMCLEAR = (#const SDL_SCANCODE_KP_MEMCLEAR) :: SDLScancode
pattern SDL_SCANCODE_KP_MEMADD = (#const SDL_SCANCODE_KP_MEMADD) :: SDLScancode
pattern SDL_SCANCODE_KP_MEMSUBTRACT = (#const SDL_SCANCODE_KP_MEMSUBTRACT) :: SDLScancode
pattern SDL_SCANCODE_KP_MEMMULTIPLY = (#const SDL_SCANCODE_KP_MEMMULTIPLY) :: SDLScancode
pattern SDL_SCANCODE_KP_MEMDIVIDE = (#const SDL_SCANCODE_KP_MEMDIVIDE) :: SDLScancode
pattern SDL_SCANCODE_KP_PLUSMINUS = (#const SDL_SCANCODE_KP_PLUSMINUS) :: SDLScancode
pattern SDL_SCANCODE_KP_CLEAR = (#const SDL_SCANCODE_KP_CLEAR) :: SDLScancode
pattern SDL_SCANCODE_KP_CLEARENTRY = (#const SDL_SCANCODE_KP_CLEARENTRY) :: SDLScancode
pattern SDL_SCANCODE_KP_BINARY = (#const SDL_SCANCODE_KP_BINARY) :: SDLScancode
pattern SDL_SCANCODE_KP_OCTAL = (#const SDL_SCANCODE_KP_OCTAL) :: SDLScancode
pattern SDL_SCANCODE_KP_DECIMAL = (#const SDL_SCANCODE_KP_DECIMAL) :: SDLScancode
pattern SDL_SCANCODE_KP_HEXADECIMAL = (#const SDL_SCANCODE_KP_HEXADECIMAL) :: SDLScancode
pattern SDL_SCANCODE_LCTRL = (#const SDL_SCANCODE_LCTRL) :: SDLScancode
pattern SDL_SCANCODE_LSHIFT = (#const SDL_SCANCODE_LSHIFT) :: SDLScancode
pattern SDL_SCANCODE_LALT = (#const SDL_SCANCODE_LALT) :: SDLScancode
pattern SDL_SCANCODE_LGUI = (#const SDL_SCANCODE_LGUI) :: SDLScancode
pattern SDL_SCANCODE_RCTRL = (#const SDL_SCANCODE_RCTRL) :: SDLScancode
pattern SDL_SCANCODE_RSHIFT = (#const SDL_SCANCODE_RSHIFT) :: SDLScancode
pattern SDL_SCANCODE_RALT = (#const SDL_SCANCODE_RALT) :: SDLScancode
pattern SDL_SCANCODE_RGUI = (#const SDL_SCANCODE_RGUI) :: SDLScancode
pattern SDL_SCANCODE_MODE = (#const SDL_SCANCODE_MODE) :: SDLScancode
pattern SDL_SCANCODE_SLEEP = (#const SDL_SCANCODE_SLEEP) :: SDLScancode
pattern SDL_SCANCODE_WAKE = (#const SDL_SCANCODE_WAKE) :: SDLScancode
pattern SDL_SCANCODE_CHANNEL_INCREMENT = (#const SDL_SCANCODE_CHANNEL_INCREMENT) :: SDLScancode
pattern SDL_SCANCODE_CHANNEL_DECREMENT = (#const SDL_SCANCODE_CHANNEL_DECREMENT) :: SDLScancode
pattern SDL_SCANCODE_MEDIA_PLAY = (#const SDL_SCANCODE_MEDIA_PLAY) :: SDLScancode
pattern SDL_SCANCODE_MEDIA_PAUSE = (#const SDL_SCANCODE_MEDIA_PAUSE) :: SDLScancode
pattern SDL_SCANCODE_MEDIA_RECORD = (#const SDL_SCANCODE_MEDIA_RECORD) :: SDLScancode
pattern SDL_SCANCODE_MEDIA_FAST_FORWARD = (#const SDL_SCANCODE_MEDIA_FAST_FORWARD) :: SDLScancode
pattern SDL_SCANCODE_MEDIA_REWIND = (#const SDL_SCANCODE_MEDIA_REWIND) :: SDLScancode
pattern SDL_SCANCODE_MEDIA_NEXT_TRACK = (#const SDL_SCANCODE_MEDIA_NEXT_TRACK) :: SDLScancode
pattern SDL_SCANCODE_MEDIA_PREVIOUS_TRACK = (#const SDL_SCANCODE_MEDIA_PREVIOUS_TRACK) :: SDLScancode
pattern SDL_SCANCODE_MEDIA_STOP = (#const SDL_SCANCODE_MEDIA_STOP) :: SDLScancode
pattern SDL_SCANCODE_MEDIA_EJECT = (#const SDL_SCANCODE_MEDIA_EJECT) :: SDLScancode
pattern SDL_SCANCODE_MEDIA_PLAY_PAUSE = (#const SDL_SCANCODE_MEDIA_PLAY_PAUSE) :: SDLScancode
pattern SDL_SCANCODE_MEDIA_SELECT = (#const SDL_SCANCODE_MEDIA_SELECT) :: SDLScancode
pattern SDL_SCANCODE_AC_NEW = (#const SDL_SCANCODE_AC_NEW) :: SDLScancode
pattern SDL_SCANCODE_AC_OPEN = (#const SDL_SCANCODE_AC_OPEN) :: SDLScancode
pattern SDL_SCANCODE_AC_CLOSE = (#const SDL_SCANCODE_AC_CLOSE) :: SDLScancode
pattern SDL_SCANCODE_AC_EXIT = (#const SDL_SCANCODE_AC_EXIT) :: SDLScancode
pattern SDL_SCANCODE_AC_SAVE = (#const SDL_SCANCODE_AC_SAVE) :: SDLScancode
pattern SDL_SCANCODE_AC_PRINT = (#const SDL_SCANCODE_AC_PRINT) :: SDLScancode
pattern SDL_SCANCODE_AC_PROPERTIES = (#const SDL_SCANCODE_AC_PROPERTIES) :: SDLScancode
pattern SDL_SCANCODE_AC_SEARCH = (#const SDL_SCANCODE_AC_SEARCH) :: SDLScancode
pattern SDL_SCANCODE_AC_HOME = (#const SDL_SCANCODE_AC_HOME) :: SDLScancode
pattern SDL_SCANCODE_AC_BACK = (#const SDL_SCANCODE_AC_BACK) :: SDLScancode
pattern SDL_SCANCODE_AC_FORWARD = (#const SDL_SCANCODE_AC_FORWARD) :: SDLScancode
pattern SDL_SCANCODE_AC_STOP = (#const SDL_SCANCODE_AC_STOP) :: SDLScancode
pattern SDL_SCANCODE_AC_REFRESH = (#const SDL_SCANCODE_AC_REFRESH) :: SDLScancode
pattern SDL_SCANCODE_AC_BOOKMARKS = (#const SDL_SCANCODE_AC_BOOKMARKS) :: SDLScancode
pattern SDL_SCANCODE_SOFTLEFT = (#const SDL_SCANCODE_SOFTLEFT) :: SDLScancode
pattern SDL_SCANCODE_SOFTRIGHT = (#const SDL_SCANCODE_SOFTRIGHT) :: SDLScancode
pattern SDL_SCANCODE_CALL = (#const SDL_SCANCODE_CALL) :: SDLScancode
pattern SDL_SCANCODE_ENDCALL = (#const SDL_SCANCODE_ENDCALL) :: SDLScancode
pattern SDL_SCANCODE_RESERVED = (#const SDL_SCANCODE_RESERVED) :: SDLScancode
pattern SDL_SCANCODE_COUNT = (#const SDL_SCANCODE_COUNT) :: SDLScancode
