{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : SDL.Keycode
Description : SDL keyboard keycode definitions
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Defines constants which identify keyboard keys and modifiers. This module provides
the SDL virtual key representation (SDLKeycode) and key modifier flags (SDLKeymod).

Please refer to the Best Keyboard Practices document for details on what
this information means and how best to use it:
https://wiki.libsdl.org/SDL3/BestKeyboardPractices
-}

module SDL.Keycode
  ( -- * Types
    SDLKeycode(..)
  , SDLKeymod(..)

    -- * Constants
  , sdlExtendedMask
  , sdlScancodeMask
  , sdlScancodeToKeycode

   -- * Keycode constants
  , pattern SDLKUnknown            
  , pattern SDLKReturn             
  , pattern SDLKEscape             
  , pattern SDLKBackspace          
  , pattern SDLKTab                
  , pattern SDLKSpace              
  , pattern SDLKExclaim            
  , pattern SDLKDoubleApostrophe   
  , pattern SDLKHash               
  , pattern SDLKDollar             
  , pattern SDLKPercent            
  , pattern SDLAmpersand           
  , pattern SDLKApostrophe         
  , pattern SDLKLeftparen          
  , pattern SDLKRightparen         
  , pattern SDLKAsterisk           
  , pattern SDLKPlus               
  , pattern SDLKComma              
  , pattern SDLKMinus              
  , pattern SDLKPeriod             
  , pattern SDLKSlash              
  , pattern SDLK0                  
  , pattern SDLK1                  
  , pattern SDLK2                  
  , pattern SDLK3                  
  , pattern SDLK4                  
  , pattern SDLK5                  
  , pattern SDLK6                  
  , pattern SDLK7                  
  , pattern SDLK8                  
  , pattern SDLK9                  
  , pattern SDLKColon              
  , pattern SDKSemicolon           
  , pattern SDLKLess               
  , pattern SDLKEequals            
  , pattern SDLKGreater            
  , pattern SDLKQuestion           
  , pattern SDLKAt                 
  , pattern SDLKLeftbracket        
  , pattern SDLKBackslash          
  , pattern SDLKRightbracket       
  , pattern SDLKCaret              
  , pattern SDLKUnderscore         
  , pattern SDLGrave               
  , pattern SDLKA                  
  , pattern SDLKB                  
  , pattern SDLKC                  
  , pattern SDLKD                  
  , pattern SDLKE                  
  , pattern SDLKF                  
  , pattern SDLKG                  
  , pattern SDLKH                  
  , pattern SDLKI                  
  , pattern SDLKJ                  
  , pattern SDLKK                  
  , pattern SDLKL                  
  , pattern SDLKM                  
  , pattern SDLKN                  
  , pattern SDLKO                  
  , pattern SDLKP                  
  , pattern SDLKQ                  
  , pattern SDLKR                  
  , pattern SDLKS                  
  , pattern SDLKT                  
  , pattern SDLKU                  
  , pattern SDLKV                  
  , pattern SDLKW                  
  , pattern SDLKX                  
  , pattern SDLKY                  
  , pattern SDLKZ                  
  , pattern SDLKLeftbrace          
  , pattern SDLKPipe               
  , pattern SDLKRightbrace         
  , pattern SDLKTilde              
  , pattern SDLKDelete             
  , pattern SDLKPlusminus          
  , pattern SDLKCapslock           
  , pattern SDLKF1                 
  , pattern SDLKF2                 
  , pattern SDLKF3                 
  , pattern SDLKF4                 
  , pattern SDLKF5                 
  , pattern SDLKF6                 
  , pattern SDLKF7                 
  , pattern SDLKF8                 
  , pattern SDLKF9                 
  , pattern SDLKF10                
  , pattern SDLKF11                
  , pattern SDLKF12                
  , pattern SDLKPrintscreen        
  , pattern SDLKScrolllock         
  , pattern SDLKPause              
  , pattern SDLKInsert             
  , pattern SDLKHome               
  , pattern SDLKPageup             
  , pattern SDLKEnd                
  , pattern SDLPagedown            
  , pattern SDLKRight              
  , pattern SDLKLeft               
  , pattern SDLKDown               
  , pattern SDLKUp                 
  , pattern SDLKNumlockclear       
  , pattern SDLKKPDivide           
  , pattern SDLKKPMultiply         
  , pattern SDLKKPMinus            
  , pattern SDLKKPPlus             
  , pattern SDLKKPEnter            
  , pattern SDLKKP1                
  , pattern SDLKKP2                
  , pattern SDLKKP3                
  , pattern SDLKKP4                
  , pattern SDLKKP5                
  , pattern SDLKKP6                
  , pattern SDLKKP7                
  , pattern SDLKKP8                
  , pattern SDLKKP9                
  , pattern SDLKKP0                
  , pattern SDLKKPPeriod           
  , pattern SDLKApplication        
  , pattern SDLKPower              
  , pattern SDLKKPEquals           
  , pattern SDLKF13                
  , pattern SDLKF14                
  , pattern SDLKF15                
  , pattern SDLKF16                
  , pattern SDLKF17                
  , pattern SDLKF18                
  , pattern SDLKF19                
  , pattern SDLKF20                
  , pattern SDLKF21                
  , pattern SDLKF22                
  , pattern SDLKF23                
  , pattern SDLKF24                
  , pattern SDLKExecute            
  , pattern SDLKHelp               
  , pattern SDLKMenu               
  , pattern SDLKSelect             
  , pattern SDLKStop               
  , pattern SDLAgain               
  , pattern SDLKUndo               
  , pattern SDLKCut                
  , pattern SDLKCopy               
  , pattern SDLPaste               
  , pattern SDLKFind               
  , pattern SDLMute                
  , pattern SDLKVolumeup           
  , pattern SDLKVolumedown         
  , pattern SDLKKPComma            
  , pattern SDLKKPEqualsAS400      
  , pattern SDLKAlterase           
  , pattern SDLKSysreq             
  , pattern SDLKCancel             
  , pattern SDLKClear              
  , pattern SDLKPrior              
  , pattern SDLKReturn2            
  , pattern SDLKSeparator          
  , pattern SDLKOut                
  , pattern SDLKOoper              
  , pattern SDLKClearagain         
  , pattern SDLKCrsel              
  , pattern SDLKExsel              
  , pattern SDLKKP00               
  , pattern SDLKKP000              
  , pattern SDLKThousandsseparator 
  , pattern SDLKDecimalseparator   
  , pattern SDLKCurrencyunit       
  , pattern SDLKCurrencysubunit    
  , pattern SDLKKPLeftparen        
  , pattern SDLKKPRightparen       
  , pattern SDLKKPLeftbrace        
  , pattern SDLKKPRightbrace       
  , pattern SDLKKPTab              
  , pattern SDLKKPBackspace        
  , pattern SDLKKPA                
  , pattern SDLKKPB                
  , pattern SDLKKPC                
  , pattern SDLKKPD                
  , pattern SDLKKPE                
  , pattern SDLKKPF                
  , pattern SDLKKPXor              
  , pattern SDLKKPPower            
  , pattern SDLKKPPercent          
  , pattern SDLKKPLess             
  , pattern SDLKKPGreater          
  , pattern SDLKKPAmpersand        
  , pattern SDLKKPDblampersand     
  , pattern SDLKKPVerticalbar      
  , pattern SDLKKPDblverticalbar   
  , pattern SDLKKPColon            
  , pattern SDLKKPHash             
  , pattern SDLKKPSpace            
  , pattern SDLKKPAt               
  , pattern SDLKKPExclam           
  , pattern SDLKKPMemstore         
  , pattern SDLKKPMemrecall        
  , pattern SDLKKPMemclear         
  , pattern SDLKKPMemadd           
  , pattern SDLKKPMemsubtract      
  , pattern SDLKKPMemmultiply      
  , pattern SDLKKPMemdivide        
  , pattern SDLKKPPlusminus        
  , pattern SDLKKPClear            
  , pattern SDLKKPClearentry       
  , pattern SDLKKPBinary           
  , pattern SDLKKPOctal            
  , pattern SDLKKPDecimal          
  , pattern SDLKKPHexadecimal      
  , pattern SDLKLctrl              
  , pattern SDLKLshift             
  , pattern SDLKLalt               
  , pattern SDLKLgui               
  , pattern SDLKRctrl              
  , pattern SDLKRshift             
  , pattern SDLKRalt               
  , pattern SDLKRgui               
  , pattern SDLKMode               
  , pattern SDLKSleep              
  , pattern SDLKWake               
  , pattern SDLKChannelIncrement   
  , pattern SDLKChannelDecrement   
  , pattern SDLKMediaPlay          
  , pattern SDLKMediaPause         
  , pattern SDLKMediaRecord        
  , pattern SDLKMediaFastForward   
  , pattern SDLKMediaRewind        
  , pattern SDLKMediaNextTrack     
  , pattern SDLKMediaPreviousTrack 
  , pattern SDLKMediaStop          
  , pattern SDLKMediaEject         
  , pattern SDLKMediaPlayPause     
  , pattern SDLKMediaSelect        
  , pattern SDLKACNew              
  , pattern SDLKACOpen             
  , pattern SDLKACClose            
  , pattern SDLKACExit             
  , pattern SDLKACSave             
  , pattern SDLKACPrint            
  , pattern SDLKACProperties       
  , pattern SDLKACSearch           
  , pattern SDLKACHome             
  , pattern SDLKACBack             
  , pattern SDLKACForward          
  , pattern SDLKACStop             
  , pattern SDLKACRefresh          
  , pattern SDLKACBookmarks        
  , pattern SDLKSoftleft           
  , pattern SDLKSoftright          
  , pattern SDLKCall               
  , pattern SDLKEndcall            
  , pattern SDLKLeftTab            
  , pattern SDLKLevel5Shift        
  , pattern SDLKMultiKeyCompose    
  , pattern SDLKLmeta              
  , pattern SDLKRmeta              
  , pattern SDLKLhyper             
  , pattern SDLKRhyper             

-- Keymod constants
  , pattern SDLKMODNone   
  , pattern SDLKMODLshift 
  , pattern SDLKMODRshift 
  , pattern SDLKMODLevel5 
  , pattern SDLKMODLctrl  
  , pattern SDLKMODRctrl  
  , pattern SDLKMODLalt   
  , pattern SDLKMODRalt   
  , pattern SDLKMODLgui   
  , pattern SDLKMODRgui   
  , pattern SDLKMODNum    
  , pattern SDLKMODCaps   
  , pattern SDLKMODMode   
  , pattern SDLKMODScroll 
  , pattern SDLKMODCtrl   
  , pattern SDLKMODShift  
  , pattern SDLKMODAlt    
  , pattern SDLKMODGui    
  ) where

#include <SDL3/SDL_keycode.h>

import Data.Word (Word32, Word16)
import SDL.Scancode (SDLScancode(..))
import Data.Bits ((.|.))

-- | The SDL virtual key representation.
--
-- Values of this type are used to represent keyboard keys using the current
-- layout of the keyboard. These values include Unicode values representing
-- the unmodified character that would be generated by pressing the key, or an
-- `SDLKeycode` constant for those keys that do not generate characters.
--
-- A special exception is the number keys at the top of the keyboard which map
-- to SDLK_0...SDLK_9 on AZERTY layouts.
--
-- Keys with the `sdlExtendedMask` bit set do not map to a scancode or
-- unicode code point.
type SDLKeycode = Word32
 
-- | Valid key modifiers (possibly OR'd together).
type SDLKeymod =  Word16
 

-- | Bit mask for extended keys
sdlExtendedMask :: Word32
sdlExtendedMask = #const SDLK_EXTENDED_MASK

-- | Bit mask for scancode-derived keycodes
sdlScancodeMask :: Word32
sdlScancodeMask = #const SDLK_SCANCODE_MASK

-- | Convert a scancode to a keycode
sdlScancodeToKeycode :: SDLScancode -> SDLKeycode
sdlScancodeToKeycode scancode = fromIntegral (fromEnum scancode) .|. sdlScancodeMask

-- Keycode constants
pattern SDLKUnknown            = (#const SDLK_UNKNOWN) :: SDLKeycode
pattern SDLKReturn             = (#const SDLK_RETURN) :: SDLKeycode
pattern SDLKEscape             = (#const SDLK_ESCAPE) :: SDLKeycode
pattern SDLKBackspace          = (#const SDLK_BACKSPACE) :: SDLKeycode
pattern SDLKTab                = (#const SDLK_TAB) :: SDLKeycode
pattern SDLKSpace              = (#const SDLK_SPACE) :: SDLKeycode
pattern SDLKExclaim            = (#const SDLK_EXCLAIM) :: SDLKeycode
pattern SDLKDoubleApostrophe   = (#const SDLK_DBLAPOSTROPHE) :: SDLKeycode
pattern SDLKHash               = (#const SDLK_HASH) :: SDLKeycode
pattern SDLKDollar             = (#const SDLK_DOLLAR) :: SDLKeycode
pattern SDLKPercent            = (#const SDLK_PERCENT) :: SDLKeycode
pattern SDLAmpersand           = (#const SDLK_AMPERSAND) :: SDLKeycode
pattern SDLKApostrophe         = (#const SDLK_APOSTROPHE) :: SDLKeycode
pattern SDLKLeftparen          = (#const SDLK_LEFTPAREN) :: SDLKeycode
pattern SDLKRightparen         = (#const SDLK_RIGHTPAREN) :: SDLKeycode
pattern SDLKAsterisk           = (#const SDLK_ASTERISK) :: SDLKeycode
pattern SDLKPlus               = (#const SDLK_PLUS) :: SDLKeycode
pattern SDLKComma              = (#const SDLK_COMMA) :: SDLKeycode
pattern SDLKMinus              = (#const SDLK_MINUS) :: SDLKeycode
pattern SDLKPeriod             = (#const SDLK_PERIOD) :: SDLKeycode
pattern SDLKSlash              = (#const SDLK_SLASH) :: SDLKeycode
pattern SDLK0                  = (#const SDLK_0) :: SDLKeycode
pattern SDLK1                  = (#const SDLK_1) :: SDLKeycode
pattern SDLK2                  = (#const SDLK_2) :: SDLKeycode
pattern SDLK3                  = (#const SDLK_3) :: SDLKeycode
pattern SDLK4                  = (#const SDLK_4) :: SDLKeycode
pattern SDLK5                  = (#const SDLK_5) :: SDLKeycode
pattern SDLK6                  = (#const SDLK_6) :: SDLKeycode
pattern SDLK7                  = (#const SDLK_7) :: SDLKeycode
pattern SDLK8                  = (#const SDLK_8) :: SDLKeycode
pattern SDLK9                  = (#const SDLK_9) :: SDLKeycode
pattern SDLKColon              = (#const SDLK_COLON) :: SDLKeycode
pattern SDKSemicolon           = (#const SDLK_SEMICOLON) :: SDLKeycode
pattern SDLKLess               = (#const SDLK_LESS) :: SDLKeycode
pattern SDLKEequals            = (#const SDLK_EQUALS) :: SDLKeycode
pattern SDLKGreater            = (#const SDLK_GREATER) :: SDLKeycode
pattern SDLKQuestion           = (#const SDLK_QUESTION) :: SDLKeycode
pattern SDLKAt                 = (#const SDLK_AT) :: SDLKeycode
pattern SDLKLeftbracket        = (#const SDLK_LEFTBRACKET) :: SDLKeycode
pattern SDLKBackslash          = (#const SDLK_BACKSLASH) :: SDLKeycode
pattern SDLKRightbracket       = (#const SDLK_RIGHTBRACKET) :: SDLKeycode
pattern SDLKCaret              = (#const SDLK_CARET) :: SDLKeycode
pattern SDLKUnderscore         = (#const SDLK_UNDERSCORE) :: SDLKeycode
pattern SDLGrave               = (#const SDLK_GRAVE) :: SDLKeycode
pattern SDLKA                  = (#const SDLK_A) :: SDLKeycode
pattern SDLKB                  = (#const SDLK_B) :: SDLKeycode
pattern SDLKC                  = (#const SDLK_C) :: SDLKeycode
pattern SDLKD                  = (#const SDLK_D) :: SDLKeycode
pattern SDLKE                  = (#const SDLK_E) :: SDLKeycode
pattern SDLKF                  = (#const SDLK_F) :: SDLKeycode
pattern SDLKG                  = (#const SDLK_G) :: SDLKeycode
pattern SDLKH                  = (#const SDLK_H) :: SDLKeycode
pattern SDLKI                  = (#const SDLK_I) :: SDLKeycode
pattern SDLKJ                  = (#const SDLK_J) :: SDLKeycode
pattern SDLKK                  = (#const SDLK_K) :: SDLKeycode
pattern SDLKL                  = (#const SDLK_L) :: SDLKeycode
pattern SDLKM                  = (#const SDLK_M) :: SDLKeycode
pattern SDLKN                  = (#const SDLK_N) :: SDLKeycode
pattern SDLKO                  = (#const SDLK_O) :: SDLKeycode
pattern SDLKP                  = (#const SDLK_P) :: SDLKeycode
pattern SDLKQ                  = (#const SDLK_Q) :: SDLKeycode
pattern SDLKR                  = (#const SDLK_R) :: SDLKeycode
pattern SDLKS                  = (#const SDLK_S) :: SDLKeycode
pattern SDLKT                  = (#const SDLK_T) :: SDLKeycode
pattern SDLKU                  = (#const SDLK_U) :: SDLKeycode
pattern SDLKV                  = (#const SDLK_V) :: SDLKeycode
pattern SDLKW                  = (#const SDLK_W) :: SDLKeycode
pattern SDLKX                  = (#const SDLK_X) :: SDLKeycode
pattern SDLKY                  = (#const SDLK_Y) :: SDLKeycode
pattern SDLKZ                  = (#const SDLK_Z) :: SDLKeycode
pattern SDLKLeftbrace          = (#const SDLK_LEFTBRACE) :: SDLKeycode
pattern SDLKPipe               = (#const SDLK_PIPE) :: SDLKeycode
pattern SDLKRightbrace         = (#const SDLK_RIGHTBRACE) :: SDLKeycode
pattern SDLKTilde              = (#const SDLK_TILDE) :: SDLKeycode
pattern SDLKDelete             = (#const SDLK_DELETE) :: SDLKeycode
pattern SDLKPlusminus          = (#const SDLK_PLUSMINUS) :: SDLKeycode
pattern SDLKCapslock           = (#const SDLK_CAPSLOCK) :: SDLKeycode
pattern SDLKF1                 = (#const SDLK_F1) :: SDLKeycode
pattern SDLKF2                 = (#const SDLK_F2) :: SDLKeycode
pattern SDLKF3                 = (#const SDLK_F3) :: SDLKeycode
pattern SDLKF4                 = (#const SDLK_F4) :: SDLKeycode
pattern SDLKF5                 = (#const SDLK_F5) :: SDLKeycode
pattern SDLKF6                 = (#const SDLK_F6) :: SDLKeycode
pattern SDLKF7                 = (#const SDLK_F7) :: SDLKeycode
pattern SDLKF8                 = (#const SDLK_F8) :: SDLKeycode
pattern SDLKF9                 = (#const SDLK_F9) :: SDLKeycode
pattern SDLKF10                = (#const SDLK_F10) :: SDLKeycode
pattern SDLKF11                = (#const SDLK_F11) :: SDLKeycode
pattern SDLKF12                = (#const SDLK_F12) :: SDLKeycode
pattern SDLKPrintscreen        = (#const SDLK_PRINTSCREEN) :: SDLKeycode
pattern SDLKScrolllock         = (#const SDLK_SCROLLLOCK) :: SDLKeycode
pattern SDLKPause              = (#const SDLK_PAUSE) :: SDLKeycode
pattern SDLKInsert             = (#const SDLK_INSERT) :: SDLKeycode
pattern SDLKHome               = (#const SDLK_HOME) :: SDLKeycode
pattern SDLKPageup             = (#const SDLK_PAGEUP) :: SDLKeycode
pattern SDLKEnd                = (#const SDLK_END) :: SDLKeycode
pattern SDLPagedown            = (#const SDLK_PAGEDOWN) :: SDLKeycode
pattern SDLKRight              = (#const SDLK_RIGHT) :: SDLKeycode
pattern SDLKLeft               = (#const SDLK_LEFT) :: SDLKeycode
pattern SDLKDown               = (#const SDLK_DOWN) :: SDLKeycode
pattern SDLKUp                 = (#const SDLK_UP) :: SDLKeycode
pattern SDLKNumlockclear       = (#const SDLK_NUMLOCKCLEAR) :: SDLKeycode
pattern SDLKKPDivide           = (#const SDLK_KP_DIVIDE) :: SDLKeycode
pattern SDLKKPMultiply         = (#const SDLK_KP_MULTIPLY) :: SDLKeycode
pattern SDLKKPMinus            = (#const SDLK_KP_MINUS) :: SDLKeycode
pattern SDLKKPPlus             = (#const SDLK_KP_PLUS) :: SDLKeycode
pattern SDLKKPEnter            = (#const SDLK_KP_ENTER) :: SDLKeycode
pattern SDLKKP1                = (#const SDLK_KP_1) :: SDLKeycode
pattern SDLKKP2                = (#const SDLK_KP_2) :: SDLKeycode
pattern SDLKKP3                = (#const SDLK_KP_3) :: SDLKeycode
pattern SDLKKP4                = (#const SDLK_KP_4) :: SDLKeycode
pattern SDLKKP5                = (#const SDLK_KP_5) :: SDLKeycode
pattern SDLKKP6                = (#const SDLK_KP_6) :: SDLKeycode
pattern SDLKKP7                = (#const SDLK_KP_7) :: SDLKeycode
pattern SDLKKP8                = (#const SDLK_KP_8) :: SDLKeycode
pattern SDLKKP9                = (#const SDLK_KP_9) :: SDLKeycode
pattern SDLKKP0                = (#const SDLK_KP_0) :: SDLKeycode
pattern SDLKKPPeriod           = (#const SDLK_KP_PERIOD) :: SDLKeycode
pattern SDLKApplication        = (#const SDLK_APPLICATION) :: SDLKeycode
pattern SDLKPower              = (#const SDLK_POWER) :: SDLKeycode
pattern SDLKKPEquals           = (#const SDLK_KP_EQUALS) :: SDLKeycode
pattern SDLKF13                = (#const SDLK_F13) :: SDLKeycode
pattern SDLKF14                = (#const SDLK_F14) :: SDLKeycode
pattern SDLKF15                = (#const SDLK_F15) :: SDLKeycode
pattern SDLKF16                = (#const SDLK_F16) :: SDLKeycode
pattern SDLKF17                = (#const SDLK_F17) :: SDLKeycode
pattern SDLKF18                = (#const SDLK_F18) :: SDLKeycode
pattern SDLKF19                = (#const SDLK_F19) :: SDLKeycode
pattern SDLKF20                = (#const SDLK_F20) :: SDLKeycode
pattern SDLKF21                = (#const SDLK_F21) :: SDLKeycode
pattern SDLKF22                = (#const SDLK_F22) :: SDLKeycode
pattern SDLKF23                = (#const SDLK_F23) :: SDLKeycode
pattern SDLKF24                = (#const SDLK_F24) :: SDLKeycode
pattern SDLKExecute            = (#const SDLK_EXECUTE) :: SDLKeycode
pattern SDLKHelp               = (#const SDLK_HELP) :: SDLKeycode
pattern SDLKMenu               = (#const SDLK_MENU) :: SDLKeycode
pattern SDLKSelect             = (#const SDLK_SELECT) :: SDLKeycode
pattern SDLKStop               = (#const SDLK_STOP) :: SDLKeycode
pattern SDLAgain               = (#const SDLK_AGAIN) :: SDLKeycode
pattern SDLKUndo               = (#const SDLK_UNDO) :: SDLKeycode
pattern SDLKCut                = (#const SDLK_CUT) :: SDLKeycode
pattern SDLKCopy               = (#const SDLK_COPY) :: SDLKeycode
pattern SDLPaste               = (#const SDLK_PASTE) :: SDLKeycode
pattern SDLKFind               = (#const SDLK_FIND) :: SDLKeycode
pattern SDLMute                = (#const SDLK_MUTE) :: SDLKeycode
pattern SDLKVolumeup           = (#const SDLK_VOLUMEUP) :: SDLKeycode
pattern SDLKVolumedown         = (#const SDLK_VOLUMEDOWN) :: SDLKeycode
pattern SDLKKPComma            = (#const SDLK_KP_COMMA) :: SDLKeycode
pattern SDLKKPEqualsAS400      = (#const SDLK_KP_EQUALSAS400) :: SDLKeycode
pattern SDLKAlterase           = (#const SDLK_ALTERASE) :: SDLKeycode
pattern SDLKSysreq             = (#const SDLK_SYSREQ) :: SDLKeycode
pattern SDLKCancel             = (#const SDLK_CANCEL) :: SDLKeycode
pattern SDLKClear              = (#const SDLK_CLEAR) :: SDLKeycode
pattern SDLKPrior              = (#const SDLK_PRIOR) :: SDLKeycode
pattern SDLKReturn2            = (#const SDLK_RETURN2) :: SDLKeycode
pattern SDLKSeparator          = (#const SDLK_SEPARATOR) :: SDLKeycode
pattern SDLKOut                = (#const SDLK_OUT) :: SDLKeycode
pattern SDLKOoper              = (#const SDLK_OPER) :: SDLKeycode
pattern SDLKClearagain         = (#const SDLK_CLEARAGAIN) :: SDLKeycode
pattern SDLKCrsel              = (#const SDLK_CRSEL) :: SDLKeycode
pattern SDLKExsel              = (#const SDLK_EXSEL) :: SDLKeycode
pattern SDLKKP00               = (#const SDLK_KP_00) :: SDLKeycode
pattern SDLKKP000              = (#const SDLK_KP_000) :: SDLKeycode
pattern SDLKThousandsseparator = (#const SDLK_THOUSANDSSEPARATOR) :: SDLKeycode
pattern SDLKDecimalseparator   = (#const SDLK_DECIMALSEPARATOR) :: SDLKeycode
pattern SDLKCurrencyunit       = (#const SDLK_CURRENCYUNIT) :: SDLKeycode
pattern SDLKCurrencysubunit    = (#const SDLK_CURRENCYSUBUNIT) :: SDLKeycode
pattern SDLKKPLeftparen        = (#const SDLK_KP_LEFTPAREN) :: SDLKeycode
pattern SDLKKPRightparen       = (#const SDLK_KP_RIGHTPAREN) :: SDLKeycode
pattern SDLKKPLeftbrace        = (#const SDLK_KP_LEFTBRACE) :: SDLKeycode
pattern SDLKKPRightbrace       = (#const SDLK_KP_RIGHTBRACE) :: SDLKeycode
pattern SDLKKPTab              = (#const SDLK_KP_TAB) :: SDLKeycode
pattern SDLKKPBackspace        = (#const SDLK_KP_BACKSPACE) :: SDLKeycode
pattern SDLKKPA                = (#const SDLK_KP_A) :: SDLKeycode
pattern SDLKKPB                = (#const SDLK_KP_B) :: SDLKeycode
pattern SDLKKPC                = (#const SDLK_KP_C) :: SDLKeycode
pattern SDLKKPD                = (#const SDLK_KP_D) :: SDLKeycode
pattern SDLKKPE                = (#const SDLK_KP_E) :: SDLKeycode
pattern SDLKKPF                = (#const SDLK_KP_F) :: SDLKeycode
pattern SDLKKPXor              = (#const SDLK_KP_XOR) :: SDLKeycode
pattern SDLKKPPower            = (#const SDLK_KP_POWER) :: SDLKeycode
pattern SDLKKPPercent          = (#const SDLK_KP_PERCENT) :: SDLKeycode
pattern SDLKKPLess             = (#const SDLK_KP_LESS) :: SDLKeycode
pattern SDLKKPGreater          = (#const SDLK_KP_GREATER) :: SDLKeycode
pattern SDLKKPAmpersand        = (#const SDLK_KP_AMPERSAND) :: SDLKeycode
pattern SDLKKPDblampersand     = (#const SDLK_KP_DBLAMPERSAND) :: SDLKeycode
pattern SDLKKPVerticalbar      = (#const SDLK_KP_VERTICALBAR) :: SDLKeycode
pattern SDLKKPDblverticalbar   = (#const SDLK_KP_DBLVERTICALBAR) :: SDLKeycode
pattern SDLKKPColon            = (#const SDLK_KP_COLON) :: SDLKeycode
pattern SDLKKPHash             = (#const SDLK_KP_HASH) :: SDLKeycode
pattern SDLKKPSpace            = (#const SDLK_KP_SPACE) :: SDLKeycode
pattern SDLKKPAt               = (#const SDLK_KP_AT) :: SDLKeycode
pattern SDLKKPExclam           = (#const SDLK_KP_EXCLAM) :: SDLKeycode
pattern SDLKKPMemstore         = (#const SDLK_KP_MEMSTORE) :: SDLKeycode
pattern SDLKKPMemrecall        = (#const SDLK_KP_MEMRECALL) :: SDLKeycode
pattern SDLKKPMemclear         = (#const SDLK_KP_MEMCLEAR) :: SDLKeycode
pattern SDLKKPMemadd           = (#const SDLK_KP_MEMADD) :: SDLKeycode
pattern SDLKKPMemsubtract      = (#const SDLK_KP_MEMSUBTRACT) :: SDLKeycode
pattern SDLKKPMemmultiply      = (#const SDLK_KP_MEMMULTIPLY) :: SDLKeycode
pattern SDLKKPMemdivide        = (#const SDLK_KP_MEMDIVIDE) :: SDLKeycode
pattern SDLKKPPlusminus        = (#const SDLK_KP_PLUSMINUS) :: SDLKeycode
pattern SDLKKPClear            = (#const SDLK_KP_CLEAR) :: SDLKeycode
pattern SDLKKPClearentry       = (#const SDLK_KP_CLEARENTRY) :: SDLKeycode
pattern SDLKKPBinary           = (#const SDLK_KP_BINARY) :: SDLKeycode
pattern SDLKKPOctal            = (#const SDLK_KP_OCTAL) :: SDLKeycode
pattern SDLKKPDecimal          = (#const SDLK_KP_DECIMAL) :: SDLKeycode
pattern SDLKKPHexadecimal      = (#const SDLK_KP_HEXADECIMAL) :: SDLKeycode
pattern SDLKLctrl              = (#const SDLK_LCTRL) :: SDLKeycode
pattern SDLKLshift             = (#const SDLK_LSHIFT) :: SDLKeycode
pattern SDLKLalt               = (#const SDLK_LALT) :: SDLKeycode
pattern SDLKLgui               = (#const SDLK_LGUI) :: SDLKeycode
pattern SDLKRctrl              = (#const SDLK_RCTRL) :: SDLKeycode
pattern SDLKRshift             = (#const SDLK_RSHIFT) :: SDLKeycode
pattern SDLKRalt               = (#const SDLK_RALT) :: SDLKeycode
pattern SDLKRgui               = (#const SDLK_RGUI) :: SDLKeycode
pattern SDLKMode               = (#const SDLK_MODE) :: SDLKeycode
pattern SDLKSleep              = (#const SDLK_SLEEP) :: SDLKeycode
pattern SDLKWake               = (#const SDLK_WAKE) :: SDLKeycode
pattern SDLKChannelIncrement   = (#const SDLK_CHANNEL_INCREMENT) :: SDLKeycode
pattern SDLKChannelDecrement   = (#const SDLK_CHANNEL_DECREMENT) :: SDLKeycode
pattern SDLKMediaPlay          = (#const SDLK_MEDIA_PLAY) :: SDLKeycode
pattern SDLKMediaPause         = (#const SDLK_MEDIA_PAUSE) :: SDLKeycode
pattern SDLKMediaRecord        = (#const SDLK_MEDIA_RECORD) :: SDLKeycode
pattern SDLKMediaFastForward   = (#const SDLK_MEDIA_FAST_FORWARD) :: SDLKeycode
pattern SDLKMediaRewind        = (#const SDLK_MEDIA_REWIND) :: SDLKeycode
pattern SDLKMediaNextTrack     = (#const SDLK_MEDIA_NEXT_TRACK) :: SDLKeycode
pattern SDLKMediaPreviousTrack = (#const SDLK_MEDIA_PREVIOUS_TRACK) :: SDLKeycode
pattern SDLKMediaStop          = (#const SDLK_MEDIA_STOP) :: SDLKeycode
pattern SDLKMediaEject         = (#const SDLK_MEDIA_EJECT) :: SDLKeycode
pattern SDLKMediaPlayPause     = (#const SDLK_MEDIA_PLAY_PAUSE) :: SDLKeycode
pattern SDLKMediaSelect        = (#const SDLK_MEDIA_SELECT) :: SDLKeycode
pattern SDLKACNew              = (#const SDLK_AC_NEW) :: SDLKeycode
pattern SDLKACOpen             = (#const SDLK_AC_OPEN) :: SDLKeycode
pattern SDLKACClose            = (#const SDLK_AC_CLOSE) :: SDLKeycode
pattern SDLKACExit             = (#const SDLK_AC_EXIT) :: SDLKeycode
pattern SDLKACSave             = (#const SDLK_AC_SAVE) :: SDLKeycode
pattern SDLKACPrint            = (#const SDLK_AC_PRINT) :: SDLKeycode
pattern SDLKACProperties       = (#const SDLK_AC_PROPERTIES) :: SDLKeycode
pattern SDLKACSearch           = (#const SDLK_AC_SEARCH) :: SDLKeycode
pattern SDLKACHome             = (#const SDLK_AC_HOME) :: SDLKeycode
pattern SDLKACBack             = (#const SDLK_AC_BACK) :: SDLKeycode
pattern SDLKACForward          = (#const SDLK_AC_FORWARD) :: SDLKeycode
pattern SDLKACStop             = (#const SDLK_AC_STOP) :: SDLKeycode
pattern SDLKACRefresh          = (#const SDLK_AC_REFRESH) :: SDLKeycode
pattern SDLKACBookmarks        = (#const SDLK_AC_BOOKMARKS) :: SDLKeycode
pattern SDLKSoftleft           = (#const SDLK_SOFTLEFT) :: SDLKeycode
pattern SDLKSoftright          = (#const SDLK_SOFTRIGHT) :: SDLKeycode
pattern SDLKCall               = (#const SDLK_CALL) :: SDLKeycode
pattern SDLKEndcall            = (#const SDLK_ENDCALL) :: SDLKeycode
pattern SDLKLeftTab            = (#const SDLK_LEFT_TAB) :: SDLKeycode
pattern SDLKLevel5Shift        = (#const SDLK_LEVEL5_SHIFT) :: SDLKeycode
pattern SDLKMultiKeyCompose    = (#const SDLK_MULTI_KEY_COMPOSE) :: SDLKeycode
pattern SDLKLmeta              = (#const SDLK_LMETA) :: SDLKeycode
pattern SDLKRmeta              = (#const SDLK_RMETA) :: SDLKeycode
pattern SDLKLhyper             = (#const SDLK_LHYPER) :: SDLKeycode
pattern SDLKRhyper             = (#const SDLK_RHYPER) :: SDLKeycode

-- Keymod constants
pattern SDLKMODNone   = (#const SDL_KMOD_NONE) :: SDLKeymod
pattern SDLKMODLshift = (#const SDL_KMOD_LSHIFT) :: SDLKeymod
pattern SDLKMODRshift = (#const SDL_KMOD_RSHIFT) :: SDLKeymod
pattern SDLKMODLevel5 = (#const SDL_KMOD_LEVEL5) :: SDLKeymod
pattern SDLKMODLctrl  = (#const SDL_KMOD_LCTRL) :: SDLKeymod
pattern SDLKMODRctrl  = (#const SDL_KMOD_RCTRL) :: SDLKeymod
pattern SDLKMODLalt   = (#const SDL_KMOD_LALT) :: SDLKeymod
pattern SDLKMODRalt   = (#const SDL_KMOD_RALT) :: SDLKeymod
pattern SDLKMODLgui   = (#const SDL_KMOD_LGUI) :: SDLKeymod
pattern SDLKMODRgui   = (#const SDL_KMOD_RGUI) :: SDLKeymod
pattern SDLKMODNum    = (#const SDL_KMOD_NUM) :: SDLKeymod
pattern SDLKMODCaps   = (#const SDL_KMOD_CAPS) :: SDLKeymod
pattern SDLKMODMode   = (#const SDL_KMOD_MODE) :: SDLKeymod
pattern SDLKMODScroll = (#const SDL_KMOD_SCROLL) :: SDLKeymod
pattern SDLKMODCtrl   = (#const SDL_KMOD_CTRL) :: SDLKeymod
pattern SDLKMODShift  = (#const SDL_KMOD_SHIFT) :: SDLKeymod
pattern SDLKMODAlt    = (#const SDL_KMOD_ALT) :: SDLKeymod
pattern SDLKMODGui    = (#const SDL_KMOD_GUI) :: SDLKeymod
