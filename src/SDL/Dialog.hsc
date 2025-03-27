{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

#include <SDL3/SDL_dialog.h>

{-|
Module      : SDL.Dialog
Description : File dialog support for SDL
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

This module provides bindings to SDL's file dialog functionality, allowing applications
to present native GUI file selection dialogs to users. Supports open dialogs, save dialogs,
and folder selection dialogs with various customization options.

Note that launching file dialogs is non-blocking; control returns to the application
immediately and callbacks are used to handle user selections.
-}

module SDL.Dialog
  ( -- * Types
    SDLDialogFileFilter(..)
  , SDLDialogFileCallback
  , SDLFileDialogType(..)
    
    -- * Dialog Functions
  , sdlShowOpenFileDialog
  , sdlShowSaveFileDialog
  , sdlShowOpenFolderDialog
  , sdlShowFileDialogWithProperties
    
    -- * Property Constants
  , sdlPropFileDialogFiltersPointer
  , sdlPropFileDialogNfiltersNumber
  , sdlPropFileDialogWindowPointer
  , sdlPropFileDialogLocationString
  , sdlPropFileDialogManyBoolean
  , sdlPropFileDialogTitleString
  , sdlPropFileDialogAcceptString
  , sdlPropFileDialogCancelString
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import SDL.Video (SDLWindow)
import SDL.Properties (SDLPropertiesID)

-- | File filter entry for dialog windows
data SDLDialogFileFilter = SDLDialogFileFilter
  { filterName    :: String  -- ^ User-readable label for the filter
  , filterPattern :: String  -- ^ Semicolon-separated list of extensions
  }

instance Storable SDLDialogFileFilter where
  sizeOf _ = #{size SDL_DialogFileFilter}
  alignment _ = #{alignment SDL_DialogFileFilter}
  peek ptr = SDLDialogFileFilter
    <$> (peekCString =<< #{peek SDL_DialogFileFilter, name} ptr)
    <*> (peekCString =<< #{peek SDL_DialogFileFilter, pattern} ptr)
  poke ptr (SDLDialogFileFilter name pattern) = do
    withCString name $ \cname ->
      #{poke SDL_DialogFileFilter, name} ptr cname
    withCString pattern $ \cpattern ->
      #{poke SDL_DialogFileFilter, pattern} ptr cpattern

-- | Callback type for file dialog results
type SDLDialogFileCallback = 
  Ptr () ->              -- ^ userdata
  Ptr CString ->         -- ^ filelist (null-terminated array of strings)
  CInt ->                -- ^ selected filter index
  IO ()

-- | Types of file dialogs available
data SDLFileDialogType
  = SDL_FILEDIALOG_OPENFILE    -- ^ Open file dialog
  | SDL_FILEDIALOG_SAVEFILE    -- ^ Save file dialog
  | SDL_FILEDIALOG_OPENFOLDER  -- ^ Open folder dialog
  deriving (Eq, Show, Enum)

-- | Show an open file dialog
foreign import ccall "SDL_ShowOpenFileDialog"
  sdlShowOpenFileDialog :: 
    FunPtr SDLDialogFileCallback ->     -- ^ callback
    Ptr () ->                           -- ^ userdata
    Ptr SDLWindow ->                    -- ^ window
    Ptr SDLDialogFileFilter ->          -- ^ filters
    CInt ->                             -- ^ nfilters
    CString ->                          -- ^ default_location
    CBool ->                            -- ^ allow_many
    IO ()

-- | Show a save file dialog
foreign import ccall "SDL_ShowSaveFileDialog"
  sdlShowSaveFileDialog ::
    FunPtr SDLDialogFileCallback ->     -- ^ callback
    Ptr () ->                           -- ^ userdata
    Ptr SDLWindow ->                    -- ^ window
    Ptr SDLDialogFileFilter ->          -- ^ filters
    CInt ->                             -- ^ nfilters
    CString ->                          -- ^ default_location
    IO ()

-- | Show an open folder dialog
foreign import ccall "SDL_ShowOpenFolderDialog"
  sdlShowOpenFolderDialog ::
    FunPtr SDLDialogFileCallback ->     -- ^ callback
    Ptr () ->                           -- ^ userdata
    Ptr SDLWindow ->                    -- ^ window
    CString ->                          -- ^ default_location
    CBool ->                            -- ^ allow_many
    IO ()

-- | Show a file dialog with custom properties
foreign import ccall "SDL_ShowFileDialogWithProperties"
  sdlShowFileDialogWithProperties ::
    CInt ->                -- ^ type
    FunPtr SDLDialogFileCallback ->     -- ^ callback
    Ptr () ->                           -- ^ userdata
    SDLPropertiesID ->                  -- ^ props
    IO ()

-- Property constants
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
