{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module SDL3.Wrapped.MessageBox
  ( SDLMessageBoxFlags(..)
  , pattern SDL_MESSAGEBOX_ERROR
  , pattern SDL_MESSAGEBOX_WARNING
  , pattern SDL_MESSAGEBOX_INFORMATION
  , pattern SDL_MESSAGEBOX_LEFT_TO_RIGHT
  , pattern SDL_MESSAGEBOX_BUTTONS_RIGHT_TO_LEFT
  , SDLMessageBoxButtonFlags(..)
  , pattern SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT
  , pattern SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT
  , SDLMessageBoxColor(..)
  , SDLMessageBoxColorType(..)
  , SDLMessageBoxColorScheme(..)
  , SDLMessageBoxButtonData(..)
  , SDLMessageBoxData(..)
  , sdlShowMessageBox
  , sdlShowSimpleMessageBox
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits (Bits((.|.)), zeroBits)
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (with, withMany)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import SDL3.Raw.MessageBox
  ( SDLMessageBoxFlags(..)
  , pattern SDL_MESSAGEBOX_ERROR
  , pattern SDL_MESSAGEBOX_WARNING
  , pattern SDL_MESSAGEBOX_INFORMATION
  , pattern SDL_MESSAGEBOX_LEFT_TO_RIGHT
  , pattern SDL_MESSAGEBOX_BUTTONS_RIGHT_TO_LEFT
  , SDLMessageBoxButtonFlags(..)
  , pattern SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT
  , pattern SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT
  , SDLMessageBoxColor(..)
  , SDLMessageBoxColorType(..)
  , SDLMessageBoxColorScheme(..)
  )
import qualified SDL3.Raw.MessageBox as Raw
import SDL3.Video (SDLWindow(..))

data SDLMessageBoxButtonData = SDLMessageBoxButtonData
  { buttonFlags :: SDLMessageBoxButtonFlags
  , buttonID :: CInt
  , buttonText :: String
  } deriving (Eq, Show)

data SDLMessageBoxData = SDLMessageBoxData
  { messageBoxFlags :: [SDLMessageBoxFlags]
  , messageBoxWindow :: Maybe SDLWindow
  , messageBoxTitle :: String
  , messageBoxMessage :: String
  , messageBoxButtons :: [SDLMessageBoxButtonData]
  , messageBoxColorScheme :: Maybe SDLMessageBoxColorScheme
  } deriving (Eq, Show)

combineMessageBoxFlags :: [SDLMessageBoxFlags] -> SDLMessageBoxFlags
combineMessageBoxFlags = foldr (.|.) zeroBits

withRawButton :: SDLMessageBoxButtonData -> (Raw.SDLMessageBoxButtonData -> IO a) -> IO a
withRawButton SDLMessageBoxButtonData{..} f =
  withCString buttonText $ \buttonTextPtr ->
    f Raw.SDLMessageBoxButtonData
      { Raw.buttonFlags = buttonFlags
      , Raw.buttonID = buttonID
      , Raw.buttonText = buttonTextPtr
      }

withRawButtons :: [SDLMessageBoxButtonData] -> (Ptr Raw.SDLMessageBoxButtonData -> CInt -> IO a) -> IO a
withRawButtons buttons f =
  withMany withRawButton buttons $ \rawButtons ->
    withArrayLen rawButtons $ \buttonCount buttonPtr ->
      f buttonPtr (fromIntegral buttonCount)

withRawMessageBoxData :: SDLMessageBoxData -> (Ptr Raw.SDLMessageBoxData -> IO a) -> IO a
withRawMessageBoxData SDLMessageBoxData{..} f =
  withCString messageBoxTitle $ \titlePtr ->
  withCString messageBoxMessage $ \messagePtr ->
  withRawButtons messageBoxButtons $ \buttonsPtr buttonCount ->
    let windowPtr = maybe nullPtr (\(SDLWindow ptr) -> ptr) messageBoxWindow
    in case messageBoxColorScheme of
      Nothing ->
        with Raw.SDLMessageBoxData
          { Raw.messageBoxFlags = combineMessageBoxFlags messageBoxFlags
          , Raw.messageBoxWindow = windowPtr
          , Raw.messageBoxTitle = titlePtr
          , Raw.messageBoxMessage = messagePtr
          , Raw.messageBoxNumButtons = buttonCount
          , Raw.messageBoxButtons = buttonsPtr
          , Raw.messageBoxColorScheme = nullPtr
          } f
      Just colorScheme ->
        with colorScheme $ \colorSchemePtr ->
          with Raw.SDLMessageBoxData
            { Raw.messageBoxFlags = combineMessageBoxFlags messageBoxFlags
            , Raw.messageBoxWindow = windowPtr
            , Raw.messageBoxTitle = titlePtr
            , Raw.messageBoxMessage = messagePtr
            , Raw.messageBoxNumButtons = buttonCount
            , Raw.messageBoxButtons = buttonsPtr
            , Raw.messageBoxColorScheme = colorSchemePtr
            } f

sdlShowMessageBox :: MonadIO m => SDLMessageBoxData -> m (Maybe Int)
sdlShowMessageBox messageBoxData =
  liftIO $
    alloca $ \buttonPtr ->
      withRawMessageBoxData messageBoxData $ \messageBoxPtr -> do
        success <- Raw.sdlShowMessageBox messageBoxPtr buttonPtr
        if success
          then Just . fromIntegral <$> peek buttonPtr
          else pure Nothing

sdlShowSimpleMessageBox :: MonadIO m => SDLMessageBoxFlags -> String -> String -> Maybe SDLWindow -> m Bool
sdlShowSimpleMessageBox flags title message window =
  liftIO $
    withCString title $ \titlePtr ->
      withCString message $ \messagePtr ->
        Raw.sdlShowSimpleMessageBox flags titlePtr messagePtr (maybe nullPtr (\(SDLWindow ptr) -> ptr) window)
