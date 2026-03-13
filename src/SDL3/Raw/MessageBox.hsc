{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

module SDL3.Raw.MessageBox
  ( SDLMessageBoxFlags(..)
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
  , sdlShowMessageBox
  , sdlShowSimpleMessageBox
  ) where

#include <SDL3/SDL_messagebox.h>

import Data.Bits (Bits)
import Data.Word (Word32)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable(..))
import SDL3.Raw.Pixels (SDLColor(..))
import SDL3.Raw.Video (SDLWindow)

newtype SDLMessageBoxFlags = SDLMessageBoxFlags Word32
  deriving newtype (Bits, Eq, Num, Show, Storable)

pattern SDL_MESSAGEBOX_ERROR = (#const SDL_MESSAGEBOX_ERROR) :: SDLMessageBoxFlags
pattern SDL_MESSAGEBOX_WARNING = (#const SDL_MESSAGEBOX_WARNING) :: SDLMessageBoxFlags
pattern SDL_MESSAGEBOX_INFORMATION = (#const SDL_MESSAGEBOX_INFORMATION) :: SDLMessageBoxFlags
pattern SDL_MESSAGEBOX_LEFT_TO_RIGHT = (#const SDL_MESSAGEBOX_BUTTONS_LEFT_TO_RIGHT) :: SDLMessageBoxFlags
pattern SDL_MESSAGEBOX_BUTTONS_RIGHT_TO_LEFT = (#const SDL_MESSAGEBOX_BUTTONS_RIGHT_TO_LEFT) :: SDLMessageBoxFlags

newtype SDLMessageBoxButtonFlags = SDLMessageBoxButtonFlags Word32
  deriving newtype (Bits, Bounded, Enum, Eq, Num, Show, Storable)

pattern SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT = (#const SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT) :: SDLMessageBoxButtonFlags
pattern SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT = (#const SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT) :: SDLMessageBoxButtonFlags

data SDLMessageBoxButtonData = SDLMessageBoxButtonData
  { buttonFlags :: SDLMessageBoxButtonFlags
  , buttonID :: CInt
  , buttonText :: CString
  } deriving (Eq, Show)

instance Storable SDLMessageBoxButtonData where
  sizeOf _ = #{size SDL_MessageBoxButtonData}
  alignment _ = #{alignment SDL_MessageBoxButtonData}

  peek ptr =
    SDLMessageBoxButtonData
      <$> #{peek SDL_MessageBoxButtonData, flags} ptr
      <*> #{peek SDL_MessageBoxButtonData, buttonID} ptr
      <*> #{peek SDL_MessageBoxButtonData, text} ptr

  poke ptr SDLMessageBoxButtonData{..} = do
    #{poke SDL_MessageBoxButtonData, flags} ptr buttonFlags
    #{poke SDL_MessageBoxButtonData, buttonID} ptr buttonID
    #{poke SDL_MessageBoxButtonData, text} ptr buttonText

newtype SDLMessageBoxColor = SDLMessageBoxColor SDLColor
  deriving (Eq, Show)

instance Storable SDLMessageBoxColor where
  sizeOf _ = sizeOf (undefined :: SDLColor)
  alignment _ = alignment (undefined :: SDLColor)

  peek ptr = SDLMessageBoxColor <$> peek (castPtr ptr)
  poke ptr (SDLMessageBoxColor color) = poke (castPtr ptr) color

data SDLMessageBoxColorType
  = SDLMessageBoxColorBackground
  | SDLMessageBoxColorText
  | SDLMessageBoxColorButtonBorder
  | SDLMessageBoxColorButtonBackground
  | SDLMessageBoxColorButtonSelected
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data SDLMessageBoxColorScheme = SDLMessageBoxColorScheme
  { colors :: [SDLMessageBoxColor]
  } deriving (Eq, Show)

instance Storable SDLMessageBoxColorScheme where
  sizeOf _ = #{size SDL_MessageBoxColorScheme}
  alignment _ = #{alignment SDL_MessageBoxColorScheme}

  peek ptr =
    SDLMessageBoxColorScheme
      <$> peekArray (#const SDL_MESSAGEBOX_COLOR_COUNT) (castPtr ptr)

  poke ptr (SDLMessageBoxColorScheme cols) =
    pokeArray (castPtr ptr) (take (#const SDL_MESSAGEBOX_COLOR_COUNT) cols)

data SDLMessageBoxData = SDLMessageBoxData
  { messageBoxFlags :: SDLMessageBoxFlags
  , messageBoxWindow :: Ptr SDLWindow
  , messageBoxTitle :: CString
  , messageBoxMessage :: CString
  , messageBoxNumButtons :: CInt
  , messageBoxButtons :: Ptr SDLMessageBoxButtonData
  , messageBoxColorScheme :: Ptr SDLMessageBoxColorScheme
  } deriving (Eq, Show)

instance Storable SDLMessageBoxData where
  sizeOf _ = #{size SDL_MessageBoxData}
  alignment _ = #{alignment SDL_MessageBoxData}

  peek ptr =
    SDLMessageBoxData
      <$> #{peek SDL_MessageBoxData, flags} ptr
      <*> #{peek SDL_MessageBoxData, window} ptr
      <*> #{peek SDL_MessageBoxData, title} ptr
      <*> #{peek SDL_MessageBoxData, message} ptr
      <*> #{peek SDL_MessageBoxData, numbuttons} ptr
      <*> #{peek SDL_MessageBoxData, buttons} ptr
      <*> #{peek SDL_MessageBoxData, colorScheme} ptr

  poke ptr SDLMessageBoxData{..} = do
    #{poke SDL_MessageBoxData, flags} ptr messageBoxFlags
    #{poke SDL_MessageBoxData, window} ptr messageBoxWindow
    #{poke SDL_MessageBoxData, title} ptr messageBoxTitle
    #{poke SDL_MessageBoxData, message} ptr messageBoxMessage
    #{poke SDL_MessageBoxData, numbuttons} ptr messageBoxNumButtons
    #{poke SDL_MessageBoxData, buttons} ptr messageBoxButtons
    #{poke SDL_MessageBoxData, colorScheme} ptr messageBoxColorScheme

foreign import ccall "SDL_ShowMessageBox"
  sdlShowMessageBox :: Ptr SDLMessageBoxData -> Ptr CInt -> IO Bool

foreign import ccall "SDL_ShowSimpleMessageBox"
  sdlShowSimpleMessageBox :: SDLMessageBoxFlags -> CString -> CString -> Ptr SDLWindow -> IO Bool
