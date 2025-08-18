{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

{-|
Module      : SDL.Messagebox
Description : SDL message box management
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

SDL offers a simple message box API, which is useful for simple alerts,
such as informing the user when something fatal happens at startup without
the need to build a UI for it (or informing the user before your UI is ready).

These message boxes are native system dialogs where possible. This module provides
functions to create both simple message boxes ('sdlShowSimpleMessageBox') and
customizable message boxes ('sdlShowMessageBox') with various options and button choices.
-}

module SDL.MessageBox
  ( -- * Types
    SDLMessageBoxFlags(..)
  , pattern SDL_MESSAGEBOX_ERROR
  , pattern SDL_MESSAGEBOX_WARNING
  , pattern SDL_MESSAGEBOX_INFORMATION
  , pattern SDL_MESSAGEBOX_LEFT_TO_RIGHT
  , pattern SDL_MESSAGEBOX_BUTTONS_RIGHT_TO_LEFT

  , SDLMessageBoxButtonFlags(..)
  , pattern SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT
  , pattern SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT

  , SDLMessageBoxButtonData(..)
  , SDLMessageBoxColor(..)
  , SDLMessageBoxColorType(..)
  , SDLMessageBoxColorScheme(..)
  , SDLMessageBoxData(..)

    -- * Message Box Functions
  , sdlShowMessageBox
  , sdlShowSimpleMessageBox
  ) where

#include <SDL3/SDL_messagebox.h>

import Foreign (new)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.C.String (CString, peekCString, newCString)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (newArray, peekArray, pokeArray)
import Data.Word (Word32)
import SDL.Pixels
import SDL.Video
import Data.Bits

unSDLWindow :: SDLWindow -> Ptr SDLWindow
unSDLWindow (SDLWindow ptr) = ptr

-- | Flags for message box display properties
newtype SDLMessageBoxFlags = SDLMessageBoxFlags Word32
  deriving (Show, Num, Bits, Storable, Eq)

pattern SDL_MESSAGEBOX_ERROR = (#const SDL_MESSAGEBOX_ERROR) :: SDLMessageBoxFlags
pattern SDL_MESSAGEBOX_WARNING            = (#const SDL_MESSAGEBOX_WARNING) :: SDLMessageBoxFlags
pattern SDL_MESSAGEBOX_INFORMATION        = (#const SDL_MESSAGEBOX_INFORMATION) :: SDLMessageBoxFlags
pattern SDL_MESSAGEBOX_LEFT_TO_RIGHT = (#const SDL_MESSAGEBOX_BUTTONS_LEFT_TO_RIGHT) :: SDLMessageBoxFlags
pattern SDL_MESSAGEBOX_BUTTONS_RIGHT_TO_LEFT = (#const SDL_MESSAGEBOX_BUTTONS_RIGHT_TO_LEFT) :: SDLMessageBoxFlags

-- | Flags for message box button properties
newtype SDLMessageBoxButtonFlags = SDLMessageBoxButtonFlags Word32
  deriving (Eq, Bits, Enum, Num, Show, Storable, Bounded)

pattern SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT = (#const SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT) :: SDLMessageBoxButtonFlags
pattern SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT = (#const SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT) :: SDLMessageBoxButtonFlags

-- | Individual button data
data SDLMessageBoxButtonData = SDLMessageBoxButtonData
  { buttonFlags :: SDLMessageBoxButtonFlags
  , buttonID    :: CInt
  , buttonText  :: String
  } deriving (Eq, Show)

instance Storable SDLMessageBoxButtonData where
  sizeOf _ = #{size SDL_MessageBoxButtonData}
  alignment _ = #{alignment SDL_MessageBoxButtonData}
  peek ptr = do
    -- Peek the flags directly; Storable instance handles the newtype
    flags    <- #{peek SDL_MessageBoxButtonData, flags} ptr
    bid <- #{peek SDL_MessageBoxButtonData, buttonID} ptr
    textPtr  <- #{peek SDL_MessageBoxButtonData, text} ptr
    -- Convert the text CString
    text     <- peekCString textPtr
    -- Construct the Haskell record
    pure $ SDLMessageBoxButtonData flags bid text

  poke ptr (SDLMessageBoxButtonData (SDLMessageBoxButtonFlags flags) bid text) = do
    -- Use pattern matching to get the Word32 out of the flags newtype
    #{poke SDL_MessageBoxButtonData, flags} ptr flags
    #{poke SDL_MessageBoxButtonData, buttonID} ptr bid
    -- Allocate a CString for the text (caller must ensure it lives long enough or use 'with')
    -- WARNING: This poke is still potentially unsafe if 'ptr' isn't temporary
    cText <- newCString text
    #{poke SDL_MessageBoxButtonData, text} ptr cText

-- | RGB color value for message box scheme
newtype SDLMessageBoxColor = SDLMessageBoxColor SDLColor
  deriving (Eq, Show)

instance Storable SDLMessageBoxColor where
  sizeOf _ = sizeOf (undefined :: SDLColor)
  alignment _ = alignment (undefined :: SDLColor)

  peek ptr = SDLMessageBoxColor <$> peek (castPtr ptr)

  poke ptr (SDLMessageBoxColor color) = poke (castPtr ptr) color

-- | Color type enumeration for message box scheme
-- | C Header declaration treats these as standard enums, can derive Enum
data SDLMessageBoxColorType
  = SDLMessageBoxColorBackground
  | SDLMessageBoxColorText
  | SDLMessageBoxColorButtonBorder
  | SDLMessageBoxColorButtonBackground
  | SDLMessageBoxColorButtonSelected
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Color scheme for message box
data SDLMessageBoxColorScheme = SDLMessageBoxColorScheme
  { colors :: [SDLMessageBoxColor]  -- ^ Array of colors matching SDLMessageBoxColorType order
  } deriving (Eq, Show)

instance Storable SDLMessageBoxColorScheme where
  sizeOf _ = #size SDL_MessageBoxColorScheme
  alignment _ = #alignment SDL_MessageBoxColorScheme
  peek ptr = SDLMessageBoxColorScheme
    <$> peekArray (#const SDL_MESSAGEBOX_COLOR_COUNT) (castPtr ptr)
  poke ptr (SDLMessageBoxColorScheme cols) =
    pokeArray (castPtr ptr) (take (#const SDL_MESSAGEBOX_COLOR_COUNT) cols)

-- | Message box data structure
data SDLMessageBoxData = SDLMessageBoxData
  { messageBoxFlags      :: [SDLMessageBoxFlags]
  , messageBoxWindow     :: Maybe SDLWindow
  , messageBoxTitle      :: String
  , messageBoxMessage    :: String
  , messageBoxButtons    :: [SDLMessageBoxButtonData]
  , messageBoxColorScheme :: Maybe SDLMessageBoxColorScheme
  } deriving (Eq, Show)

instance Storable SDLMessageBoxData where
  sizeOf _ = #{size SDL_MessageBoxData}
  alignment _ = #alignment SDL_MessageBoxData
  peek ptr = error "SDLMessageBoxData peek not fully implemented due to pointer complexity"
  poke ptr (SDLMessageBoxData flagsList win title msg buttons cs) = do
    -- Combine the list of flags into a single CUInt bitmask
    let combinedFlags = foldr ((.|.) . extractFlag) zeroBits flagsList
          where extractFlag (SDLMessageBoxFlags f) = f -- Helper to get CUInt

    #{poke SDL_MessageBoxData, flags} ptr combinedFlags -- Poke the combined CUInt
    -- Use pattern matching for window pointer
    #{poke SDL_MessageBoxData, window} ptr (maybe nullPtr (\(SDLWindow p) -> p) win)
    -- Use temporary CStrings (assuming 'ptr' is temporary via 'new'/'with')
    -- WARNING: Still potentially unsafe if 'ptr' isn't temporary
    cTitle <- newCString title
    cMsg   <- newCString msg
    #{poke SDL_MessageBoxData, title} ptr cTitle
    #{poke SDL_MessageBoxData, message} ptr cMsg
    #{poke SDL_MessageBoxData, numbuttons} ptr (fromIntegral $ length buttons :: CInt)
    -- Use newArray (pointer needs freeing later if not temporary)
    -- WARNING: Still potentially unsafe if 'ptr' isn't temporary
    buttonsPtr <- newArray buttons
    #{poke SDL_MessageBoxData, buttons} ptr buttonsPtr
    -- Use maybe (return nullPtr) new for color scheme
    -- WARNING: Still potentially unsafe if 'ptr' isn't temporary
    csPtr <- maybe (return nullPtr) new cs
    #{poke SDL_MessageBoxData, colorScheme} ptr csPtr

-- | Create a modal message box
foreign import ccall "SDL_ShowMessageBox"
  sdlShowMessageBoxRaw :: Ptr SDLMessageBoxData -> Ptr CInt -> IO Bool

-- | Haskell wrapper for SDL_ShowMessageBox
sdlShowMessageBox :: SDLMessageBoxData -> IO (Maybe Int)
sdlShowMessageBox msgData = alloca $ \buttonPtr -> do
    msgPtr <- new msgData
    res <- sdlShowMessageBoxRaw msgPtr buttonPtr
    if res
        then Just . fromIntegral <$> peek buttonPtr
        else return Nothing

-- | Display a simple modal message box
foreign import ccall "SDL_ShowSimpleMessageBox"
  sdlShowSimpleMessageBoxRaw :: Word32 -> CString -> CString -> Ptr SDLWindow -> IO Bool

-- | Haskell wrapper for SDL_ShowSimpleMessageBox
sdlShowSimpleMessageBox :: SDLMessageBoxFlags -> String -> String -> Maybe SDLWindow -> IO Bool
sdlShowSimpleMessageBox (SDLMessageBoxFlags flags) title msg win = do
  titleC <- newCString title
  msgC <- newCString msg
  sdlShowSimpleMessageBoxRaw flags titleC msgC (maybe nullPtr unSDLWindow win)
