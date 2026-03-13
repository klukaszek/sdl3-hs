{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

#include <SDL3/SDL_dialog.h>

module SDL3.Raw.Dialog
  ( SDLDialogFileFilter(..)
  , SDLDialogFileCallback
  , SDLFileDialogType(..)
  , pattern SDL_FILEDIALOG_OPENFILE
  , pattern SDL_FILEDIALOG_SAVEFILE
  , pattern SDL_FILEDIALOG_OPENFOLDER
  , sdlShowOpenFileDialog
  , sdlShowSaveFileDialog
  , sdlShowOpenFolderDialog
  , sdlShowFileDialogWithProperties
  , sdlPropFileDialogFiltersPointer
  , sdlPropFileDialogNfiltersNumber
  , sdlPropFileDialogWindowPointer
  , sdlPropFileDialogLocationString
  , sdlPropFileDialogManyBoolean
  , sdlPropFileDialogTitleString
  , sdlPropFileDialogAcceptString
  , sdlPropFileDialogCancelString
  ) where

import Foreign.C.String (CString)
import Foreign.C.Types (CBool(..), CInt(..))
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Storable (Storable(..))
import SDL3.Raw.Properties (SDLPropertiesID)
import SDL3.Raw.Video (SDLWindow)

data SDLDialogFileFilter = SDLDialogFileFilter
  { filterName :: CString
  , filterPattern :: CString
  } deriving (Eq, Show)

instance Storable SDLDialogFileFilter where
  sizeOf _ = #{size SDL_DialogFileFilter}
  alignment _ = #{alignment SDL_DialogFileFilter}

  peek ptr =
    SDLDialogFileFilter
      <$> #{peek SDL_DialogFileFilter, name} ptr
      <*> #{peek SDL_DialogFileFilter, pattern} ptr

  poke ptr SDLDialogFileFilter{..} = do
    #{poke SDL_DialogFileFilter, name} ptr filterName
    #{poke SDL_DialogFileFilter, pattern} ptr filterPattern

type SDLDialogFileCallback =
  Ptr () ->
  Ptr CString ->
  CInt ->
  IO ()

newtype SDLFileDialogType = SDLFileDialogType CInt
  deriving newtype (Enum, Eq, Ord, Show, Storable)

pattern SDL_FILEDIALOG_OPENFILE :: SDLFileDialogType
pattern SDL_FILEDIALOG_OPENFILE = SDLFileDialogType #{const SDL_FILEDIALOG_OPENFILE}

pattern SDL_FILEDIALOG_SAVEFILE :: SDLFileDialogType
pattern SDL_FILEDIALOG_SAVEFILE = SDLFileDialogType #{const SDL_FILEDIALOG_SAVEFILE}

pattern SDL_FILEDIALOG_OPENFOLDER :: SDLFileDialogType
pattern SDL_FILEDIALOG_OPENFOLDER = SDLFileDialogType #{const SDL_FILEDIALOG_OPENFOLDER}

foreign import ccall safe "SDL_ShowOpenFileDialog"
  sdlShowOpenFileDialog ::
    FunPtr SDLDialogFileCallback ->
    Ptr () ->
    Ptr SDLWindow ->
    Ptr SDLDialogFileFilter ->
    CInt ->
    CString ->
    CBool ->
    IO ()

foreign import ccall safe "SDL_ShowSaveFileDialog"
  sdlShowSaveFileDialog ::
    FunPtr SDLDialogFileCallback ->
    Ptr () ->
    Ptr SDLWindow ->
    Ptr SDLDialogFileFilter ->
    CInt ->
    CString ->
    IO ()

foreign import ccall safe "SDL_ShowOpenFolderDialog"
  sdlShowOpenFolderDialog ::
    FunPtr SDLDialogFileCallback ->
    Ptr () ->
    Ptr SDLWindow ->
    CString ->
    CBool ->
    IO ()

foreign import ccall safe "SDL_ShowFileDialogWithProperties"
  c_sdlShowFileDialogWithProperties ::
    CInt ->
    FunPtr SDLDialogFileCallback ->
    Ptr () ->
    SDLPropertiesID ->
    IO ()

sdlShowFileDialogWithProperties ::
  SDLFileDialogType ->
  FunPtr SDLDialogFileCallback ->
  Ptr () ->
  SDLPropertiesID ->
  IO ()
sdlShowFileDialogWithProperties dialogType callback userdata props =
  c_sdlShowFileDialogWithProperties
    (fromIntegral (fromEnum dialogType))
    callback
    userdata
    props

sdlPropFileDialogFiltersPointer :: String
sdlPropFileDialogFiltersPointer = #{const_str SDL_PROP_FILE_DIALOG_FILTERS_POINTER}

sdlPropFileDialogNfiltersNumber :: String
sdlPropFileDialogNfiltersNumber = #{const_str SDL_PROP_FILE_DIALOG_NFILTERS_NUMBER}

sdlPropFileDialogWindowPointer :: String
sdlPropFileDialogWindowPointer = #{const_str SDL_PROP_FILE_DIALOG_WINDOW_POINTER}

sdlPropFileDialogLocationString :: String
sdlPropFileDialogLocationString = #{const_str SDL_PROP_FILE_DIALOG_LOCATION_STRING}

sdlPropFileDialogManyBoolean :: String
sdlPropFileDialogManyBoolean = #{const_str SDL_PROP_FILE_DIALOG_MANY_BOOLEAN}

sdlPropFileDialogTitleString :: String
sdlPropFileDialogTitleString = #{const_str SDL_PROP_FILE_DIALOG_TITLE_STRING}

sdlPropFileDialogAcceptString :: String
sdlPropFileDialogAcceptString = #{const_str SDL_PROP_FILE_DIALOG_ACCEPT_STRING}

sdlPropFileDialogCancelString :: String
sdlPropFileDialogCancelString = #{const_str SDL_PROP_FILE_DIALOG_CANCEL_STRING}
