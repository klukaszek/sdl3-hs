{-# LANGUAGE ForeignFunctionInterface #-}
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
  , SDLMessageBoxButtonFlags(..)
  , SDLMessageBoxButtonData(..)
  , SDLMessageBoxColor(..)
  , SDLMessageBoxColorType(..)
  , SDLMessageBoxColorScheme(..)
  , SDLMessageBoxData(..)

    -- * Constants
  , sdlMessageBoxError
  , sdlMessageBoxWarning
  , sdlMessageBoxInformation
  , sdlMessageBoxButtonsLeftToRight
  , sdlMessageBoxButtonsRightToLeft
  , sdlMessageBoxButtonReturnKeyDefault
  , sdlMessageBoxButtonEscapeKeyDefault

    -- * Message Box Functions
  , sdlShowMessageBox
  , sdlShowSimpleMessageBox
  ) where

#include <SDL3/SDL_messagebox.h>

import Foreign (new)
import Foreign.C.Types (CBool, CChar, CInt)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.C.String (CString, peekCString, newCString)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (newArray, peekArray, pokeArray)
import Data.Word (Word32, Word8)
import SDL.Pixels
import SDL.Video
import Data.Bits

unSDLWindow :: SDLWindow -> Ptr SDLWindow
unSDLWindow (SDLWindow ptr) = ptr

-- | Flags for message box display properties
data SDLMessageBoxFlags
  = SDLMessageBoxError                    -- ^ error dialog
  | SDLMessageBoxWarning                  -- ^ warning dialog
  | SDLMessageBoxInformation             -- ^ informational dialog
  | SDLMessageBoxButtonsLeftToRight      -- ^ buttons placed left to right
  | SDLMessageBoxButtonsRightToLeft      -- ^ buttons placed right to left
  deriving (Eq, Ord, Show, Read)

-- | Convert flags to Word32 bitmask
toSDLMessageBoxFlags :: [SDLMessageBoxFlags] -> Word32
toSDLMessageBoxFlags flags = foldr (.|.) 0 $ map toFlag flags
  where
    toFlag SDLMessageBoxError              = #const SDL_MESSAGEBOX_ERROR
    toFlag SDLMessageBoxWarning            = #const SDL_MESSAGEBOX_WARNING
    toFlag SDLMessageBoxInformation        = #const SDL_MESSAGEBOX_INFORMATION
    toFlag SDLMessageBoxButtonsLeftToRight = #const SDL_MESSAGEBOX_BUTTONS_LEFT_TO_RIGHT
    toFlag SDLMessageBoxButtonsRightToLeft = #const SDL_MESSAGEBOX_BUTTONS_RIGHT_TO_LEFT

-- | Flags for message box button properties
data SDLMessageBoxButtonFlags
  = SDLMessageBoxButtonReturnKeyDefault  -- ^ default button when return is hit
  | SDLMessageBoxButtonEscapeKeyDefault  -- ^ default button when escape is hit
  deriving (Eq, Ord, Show, Read)

-- | Convert button flags to Word32 bitmask
toSDLMessageBoxButtonFlags :: [SDLMessageBoxButtonFlags] -> Word32
toSDLMessageBoxButtonFlags flags = foldr (.|.) 0 $ map toFlag flags
  where
    toFlag SDLMessageBoxButtonReturnKeyDefault = #const SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT
    toFlag SDLMessageBoxButtonEscapeKeyDefault = #const SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT

-- | Individual button data
data SDLMessageBoxButtonData = SDLMessageBoxButtonData
  { buttonFlags :: [SDLMessageBoxButtonFlags]
  , buttonID    :: CInt
  , buttonText  :: String
  } deriving (Eq, Show)

instance Storable SDLMessageBoxButtonData where
  sizeOf _ = #size SDL_MessageBoxButtonData
  alignment _ = #alignment SDL_MessageBoxButtonData
  peek ptr = SDLMessageBoxButtonData
    <$> (toFlags <$> (#peek SDL_MessageBoxButtonData, flags) ptr)
    <*> (fromIntegral <$> ((#peek SDL_MessageBoxButtonData, buttonID) ptr :: IO CInt))
    <*> ((#peek SDL_MessageBoxButtonData, text) ptr >>= peekCString)
    where
      toFlags flags =
        filter (`elem` [SDLMessageBoxButtonReturnKeyDefault, SDLMessageBoxButtonEscapeKeyDefault])
          [f | f <- [minBound..maxBound], flags .&. toSDLMessageBoxButtonFlags [f] /= 0]

  poke ptr (SDLMessageBoxButtonData flags bid text) = do
    (#poke SDL_MessageBoxButtonData, flags) ptr (toSDLMessageBoxButtonFlags flags)
    (#poke SDL_MessageBoxButtonData, buttonID) ptr (fromIntegral bid :: CInt)
    (#poke SDL_MessageBoxButtonData, text) ptr =<< newCString text

instance Enum SDLMessageBoxButtonFlags where
  toEnum 0 = SDLMessageBoxButtonReturnKeyDefault
  toEnum 1 = SDLMessageBoxButtonEscapeKeyDefault
  toEnum _ = error "Invalid SDLMessageBoxButtonFlags value"

  fromEnum SDLMessageBoxButtonReturnKeyDefault = 0
  fromEnum SDLMessageBoxButtonEscapeKeyDefault = 1

instance Bounded SDLMessageBoxButtonFlags where
  minBound = SDLMessageBoxButtonReturnKeyDefault
  maxBound = SDLMessageBoxButtonEscapeKeyDefault

-- | RGB color value for message box scheme
newtype SDLMessageBoxColor = SDLMessageBoxColor { unMessageBoxColor :: SDLColor }
  deriving (Eq, Show)

instance Storable SDLMessageBoxColor where
  sizeOf _ = sizeOf (undefined :: SDLColor)
  alignment _ = alignment (undefined :: SDLColor)

  peek ptr = SDLMessageBoxColor <$> peek (castPtr ptr)

  poke ptr (SDLMessageBoxColor color) = poke (castPtr ptr) color

-- | Color type enumeration for message box scheme
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
  sizeOf _ = #size SDL_MessageBoxData
  alignment _ = #alignment SDL_MessageBoxData
  peek ptr = error "SDLMessageBoxData peek not implemented"  -- Full implementation would require complex pointer handling
  poke ptr (SDLMessageBoxData flags win title msg buttons cs) = do
    (#poke SDL_MessageBoxData, flags) ptr (toSDLMessageBoxFlags flags)
    (#poke SDL_MessageBoxData, window) ptr (maybe nullPtr unSDLWindow win)
    (#poke SDL_MessageBoxData, title) ptr =<< newCString title
    (#poke SDL_MessageBoxData, message) ptr =<< newCString msg
    (#poke SDL_MessageBoxData, numbuttons) ptr (fromIntegral $ length buttons :: CInt)
    (#poke SDL_MessageBoxData, buttons) ptr =<< newArray buttons
    (#poke SDL_MessageBoxData, colorScheme) ptr =<< maybe (return nullPtr) new cs

-- | Constants
sdlMessageBoxError :: SDLMessageBoxFlags
sdlMessageBoxError = SDLMessageBoxError

sdlMessageBoxWarning :: SDLMessageBoxFlags
sdlMessageBoxWarning = SDLMessageBoxWarning

sdlMessageBoxInformation :: SDLMessageBoxFlags
sdlMessageBoxInformation = SDLMessageBoxInformation

sdlMessageBoxButtonsLeftToRight :: SDLMessageBoxFlags
sdlMessageBoxButtonsLeftToRight = SDLMessageBoxButtonsLeftToRight

sdlMessageBoxButtonsRightToLeft :: SDLMessageBoxFlags
sdlMessageBoxButtonsRightToLeft = SDLMessageBoxButtonsRightToLeft

sdlMessageBoxButtonReturnKeyDefault :: SDLMessageBoxButtonFlags
sdlMessageBoxButtonReturnKeyDefault = SDLMessageBoxButtonReturnKeyDefault

sdlMessageBoxButtonEscapeKeyDefault :: SDLMessageBoxButtonFlags
sdlMessageBoxButtonEscapeKeyDefault = SDLMessageBoxButtonEscapeKeyDefault

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
sdlShowSimpleMessageBox :: [SDLMessageBoxFlags] -> String -> String -> Maybe SDLWindow -> IO Bool
sdlShowSimpleMessageBox flags title msg win = do
  titleC <- newCString title
  msgC <- newCString msg
  sdlShowSimpleMessageBoxRaw (toSDLMessageBoxFlags flags) titleC msgC (maybe nullPtr unSDLWindow win)
