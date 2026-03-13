{-# LANGUAGE PatternSynonyms #-}

module SDL3.Wrapped.Keyboard
  ( SDLKeyboardID
  , SDLTextInputType(..)
  , SDLCapitalization(..)
  , pattern SDL_PROP_TEXTINPUT_TYPE_NUMBER
  , pattern SDL_PROP_TEXTINPUT_CAPITALIZATION_NUMBER
  , pattern SDL_PROP_TEXTINPUT_AUTOCORRECT_BOOLEAN
  , pattern SDL_PROP_TEXTINPUT_MULTILINE_BOOLEAN
  , pattern SDL_PROP_TEXTINPUT_ANDROID_INPUTTYPE_NUMBER
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

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign (fromBool, toBool, with)
import Foreign.C.String (peekCString, withCString)
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import SDL3.Keycode (SDLKeycode, SDLKeymod)
import SDL3.Raw.Keyboard
  ( SDLCapitalization(..)
  , SDLKeyboardID
  , SDLTextInputType(..)
  , pattern SDL_PROP_TEXTINPUT_ANDROID_INPUTTYPE_NUMBER
  , pattern SDL_PROP_TEXTINPUT_AUTOCORRECT_BOOLEAN
  , pattern SDL_PROP_TEXTINPUT_CAPITALIZATION_NUMBER
  , pattern SDL_PROP_TEXTINPUT_MULTILINE_BOOLEAN
  , pattern SDL_PROP_TEXTINPUT_TYPE_NUMBER
  )
import SDL3.Raw.Properties (SDLPropertiesID)
import qualified SDL3.Raw.Keyboard as Raw
import SDL3.Rect (SDLRect(..))
import SDL3.Scancode (SDLScancode)
import SDL3.Video (SDLWindow(..))

sdlHasKeyboard :: MonadIO m => m Bool
sdlHasKeyboard = liftIO $ toBool <$> Raw.sdlHasKeyboardRaw

sdlGetKeyboards :: MonadIO m => m [SDLKeyboardID]
sdlGetKeyboards = liftIO $ alloca $ \countPtr -> do
  ptr <- Raw.sdlGetKeyboardsRaw countPtr
  count <- peek countPtr
  if ptr == nullPtr
    then return []
    else do
      keyboards <- peekArray (fromIntegral count) ptr
      free ptr
      return keyboards

sdlGetKeyboardNameForID :: MonadIO m => SDLKeyboardID -> m String
sdlGetKeyboardNameForID kid = liftIO $ do
  cstr <- Raw.sdlGetKeyboardNameForIDRaw kid
  if cstr == nullPtr then return "" else peekCString cstr

sdlGetKeyboardFocus :: MonadIO m => m (Maybe SDLWindow)
sdlGetKeyboardFocus = liftIO $ do
  ptr <- Raw.sdlGetKeyboardFocusRaw
  return $ if ptr == nullPtr then Nothing else Just (SDLWindow ptr)

sdlGetKeyboardState :: MonadIO m => m [Bool]
sdlGetKeyboardState = liftIO $ alloca $ \numkeysPtr -> do
  ptr <- Raw.sdlGetKeyboardStateRaw numkeysPtr
  numkeys <- peek numkeysPtr
  map toBool <$> peekArray (fromIntegral numkeys) ptr

sdlResetKeyboard :: MonadIO m => m ()
sdlResetKeyboard = liftIO Raw.sdlResetKeyboardRaw

sdlGetModState :: MonadIO m => m SDLKeymod
sdlGetModState = liftIO Raw.sdlGetModStateRaw

sdlSetModState :: MonadIO m => SDLKeymod -> m ()
sdlSetModState = liftIO . Raw.sdlSetModStateRaw

sdlGetKeyFromScancode :: MonadIO m => SDLScancode -> SDLKeymod -> Bool -> m SDLKeycode
sdlGetKeyFromScancode scancode modstate keyEvent =
  liftIO $ Raw.sdlGetKeyFromScancodeRaw scancode modstate (fromBool keyEvent)

sdlGetScancodeFromKey :: MonadIO m => SDLKeycode -> m (SDLScancode, Maybe SDLKeymod)
sdlGetScancodeFromKey key = liftIO $ alloca $ \modstatePtr -> do
  scancode <- Raw.sdlGetScancodeFromKeyRaw key modstatePtr
  modstate <- peek modstatePtr
  return (toEnum $ fromIntegral scancode, if modstate == 0 then Nothing else Just modstate)

sdlSetScancodeName :: MonadIO m => SDLScancode -> String -> m Bool
sdlSetScancodeName scancode name = liftIO $
  withCString name $ \cstr ->
    toBool <$> Raw.sdlSetScancodeNameRaw scancode cstr

sdlGetScancodeName :: MonadIO m => SDLScancode -> m String
sdlGetScancodeName scancode = liftIO $ do
  cstr <- Raw.sdlGetScancodeNameRaw scancode
  if cstr == nullPtr then return "" else peekCString cstr

sdlGetScancodeFromName :: MonadIO m => String -> m SDLScancode
sdlGetScancodeFromName name = liftIO $
  withCString name $ \cstr ->
    toEnum . fromIntegral <$> Raw.sdlGetScancodeFromNameRaw cstr

sdlGetKeyName :: MonadIO m => SDLKeycode -> m String
sdlGetKeyName key = liftIO $ do
  cstr <- Raw.sdlGetKeyNameRaw key
  if cstr == nullPtr then return "" else peekCString cstr

sdlGetKeyFromName :: MonadIO m => String -> m SDLKeycode
sdlGetKeyFromName name = liftIO $
  withCString name Raw.sdlGetKeyFromNameRaw

sdlStartTextInput :: MonadIO m => SDLWindow -> m Bool
sdlStartTextInput (SDLWindow window) = liftIO $ toBool <$> Raw.sdlStartTextInputRaw window

sdlStartTextInputWithProperties :: MonadIO m => SDLWindow -> SDLPropertiesID -> m Bool
sdlStartTextInputWithProperties (SDLWindow window) props =
  liftIO $ toBool <$> Raw.sdlStartTextInputWithPropertiesRaw window props

sdlTextInputActive :: MonadIO m => SDLWindow -> m Bool
sdlTextInputActive (SDLWindow window) = liftIO $ toBool <$> Raw.sdlTextInputActiveRaw window

sdlStopTextInput :: MonadIO m => SDLWindow -> m Bool
sdlStopTextInput (SDLWindow window) = liftIO $ toBool <$> Raw.sdlStopTextInputRaw window

sdlClearComposition :: MonadIO m => SDLWindow -> m Bool
sdlClearComposition (SDLWindow window) = liftIO $ toBool <$> Raw.sdlClearCompositionRaw window

sdlSetTextInputArea :: MonadIO m => SDLWindow -> Maybe SDLRect -> Int -> m Bool
sdlSetTextInputArea (SDLWindow window) mrect cursor = liftIO $
  case mrect of
    Nothing -> toBool <$> Raw.sdlSetTextInputAreaRaw window nullPtr (fromIntegral cursor)
    Just rect -> with rect $ \rectPtr ->
      toBool <$> Raw.sdlSetTextInputAreaRaw window rectPtr (fromIntegral cursor)

sdlGetTextInputArea :: MonadIO m => SDLWindow -> m (Maybe SDLRect, Maybe Int)
sdlGetTextInputArea (SDLWindow window) = liftIO $
  alloca $ \rectPtr -> alloca $ \cursorPtr -> do
    success <- toBool <$> Raw.sdlGetTextInputAreaRaw window rectPtr cursorPtr
    if not success
      then return (Nothing, Nothing)
      else do
        rect <- peek rectPtr
        cursor <- peek cursorPtr
        return (Just rect, Just $ fromIntegral cursor)

sdlHasScreenKeyboardSupport :: MonadIO m => m Bool
sdlHasScreenKeyboardSupport = liftIO $ toBool <$> Raw.sdlHasScreenKeyboardSupportRaw

sdlScreenKeyboardShown :: MonadIO m => SDLWindow -> m Bool
sdlScreenKeyboardShown (SDLWindow window) = liftIO $ toBool <$> Raw.sdlScreenKeyboardShownRaw window
