-- SDL/Dialog.hsc
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}

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

module SDL3.Dialog
  ( -- * Types
    SDLDialogFileFilter(..)
  , SDLDialogFileCallback
  , SDLFileDialogType(..)
  , pattern SDL_FILEDIALOG_OPENFILE
  , pattern SDL_FILEDIALOG_SAVEFILE
  , pattern SDL_FILEDIALOG_OPENFOLDER

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
import SDL3.Video (SDLWindow(..)) -- Import the newtype
import SDL3.Properties (SDLPropertiesID)

-- | File filter entry for dialog windows
data SDLDialogFileFilter = SDLDialogFileFilter
  { filterName    :: String  -- ^ User-readable label for the filter
  , filterPattern :: String  -- ^ Semicolon-separated list of extensions
  }

instance Storable SDLDialogFileFilter where
  sizeOf _ = #{size SDL_DialogFileFilter}
  alignment _ = #{alignment SDL_DialogFileFilter}
  peek ptr = do
    namePtr <- #{peek SDL_DialogFileFilter, name} ptr
    patternPtr <- #{peek SDL_DialogFileFilter, pattern} ptr
    SDLDialogFileFilter
      <$> peekCString namePtr
      <*> peekCString patternPtr
  -- Use a different variable name for the pattern field's value
  poke ptr (SDLDialogFileFilter name pattern_) = do -- Renamed pattern to pattern_
    -- WARNING: This assumes ptr points to memory managed by 'with' or similar
    cname <- newCString name
    cpattern <- newCString pattern_ -- Use the renamed variable
    #{poke SDL_DialogFileFilter, name} ptr cname
    #{poke SDL_DialogFileFilter, pattern} ptr cpattern
    -- Note: If this struct is used long-term, the CStrings allocated here
    -- would need to be freed manually later, which poke doesn't handle.

-- | Callback type for file dialog results
type SDLDialogFileCallback =
  Ptr () ->              -- ^ userdata
  Ptr CString ->         -- ^ filelist (null-terminated array of strings)
  CInt ->                -- ^ selected filter index
  IO ()

-- | Types of file dialogs available
newtype SDLFileDialogType = SDLFileDialogType CInt
  deriving newtype (Show, Eq, Ord, Storable, Enum)

pattern SDL_FILEDIALOG_OPENFILE :: SDLFileDialogType
pattern SDL_FILEDIALOG_OPENFILE   = SDLFileDialogType #{const SDL_FILEDIALOG_OPENFILE}
pattern SDL_FILEDIALOG_SAVEFILE :: SDLFileDialogType
pattern SDL_FILEDIALOG_SAVEFILE   = SDLFileDialogType #{const SDL_FILEDIALOG_SAVEFILE}
pattern SDL_FILEDIALOG_OPENFOLDER :: SDLFileDialogType
pattern SDL_FILEDIALOG_OPENFOLDER = SDLFileDialogType #{const SDL_FILEDIALOG_OPENFOLDER}

-- | FFI import for SDL_ShowOpenFileDialog
foreign import ccall safe "SDL_ShowOpenFileDialog"
  c_sdlShowOpenFileDialog ::
    FunPtr SDLDialogFileCallback ->     -- ^ callback
    Ptr () ->                           -- ^ userdata
    Ptr SDLWindow ->                   -- ^ window (using opaque C type)
    Ptr SDLDialogFileFilter ->          -- ^ filters
    CInt ->                             -- ^ nfilters
    CString ->                          -- ^ default_location
    CBool ->                            -- ^ allow_many
    IO ()

-- | Haskell wrapper for showing an open file dialog
sdlShowOpenFileDialog ::
    FunPtr SDLDialogFileCallback ->     -- ^ callback FunPtr
    Ptr () ->                           -- ^ userdata
    Maybe SDLWindow ->                  -- ^ Optional window
    Ptr SDLDialogFileFilter ->          -- ^ filters pointer (usually from withArrayLen)
    Int ->                              -- ^ nfilters
    Maybe String ->                     -- ^ Optional default location
    Bool ->                             -- ^ allow_many
    IO ()
sdlShowOpenFileDialog callback userdata mWindow filtersPtr nfilters mDefaultLoc allowMany =
  let windowPtr = maybe nullPtr (\(SDLWindow p) -> p) mWindow -- Unpack Maybe SDLWindow
  in maybeWith withCString mDefaultLoc $ \cDefaultLoc -> -- Handle Maybe String
       c_sdlShowOpenFileDialog
         callback
         userdata
         windowPtr -- Pass the raw Ptr SDL_Window
         filtersPtr
         (fromIntegral nfilters)
         cDefaultLoc
         (fromBool allowMany) -- Convert Bool to CBool

-- | FFI import for SDL_ShowSaveFileDialog
foreign import ccall safe "SDL_ShowSaveFileDialog"
  c_sdlShowSaveFileDialog ::
    FunPtr SDLDialogFileCallback ->     -- ^ callback
    Ptr () ->                           -- ^ userdata
    Ptr SDLWindow ->                   -- ^ window (using opaque C type)
    Ptr SDLDialogFileFilter ->          -- ^ filters
    CInt ->                             -- ^ nfilters
    CString ->                          -- ^ default_location
    IO ()

-- | Haskell wrapper for showing a save file dialog
sdlShowSaveFileDialog ::
    FunPtr SDLDialogFileCallback ->     -- ^ callback FunPtr
    Ptr () ->                           -- ^ userdata
    Maybe SDLWindow ->                  -- ^ Optional window
    Ptr SDLDialogFileFilter ->          -- ^ filters pointer (usually from withArrayLen)
    Int ->                              -- ^ nfilters
    Maybe String ->                     -- ^ Optional default location
    IO ()
sdlShowSaveFileDialog callback userdata mWindow filtersPtr nfilters mDefaultLoc =
  let windowPtr = maybe nullPtr (\(SDLWindow p) -> p) mWindow -- Unpack Maybe SDLWindow
  in maybeWith withCString mDefaultLoc $ \cDefaultLoc -> -- Handle Maybe String
       c_sdlShowSaveFileDialog
         callback
         userdata
         windowPtr -- Pass the raw Ptr SDL_Window
         filtersPtr
         (fromIntegral nfilters)
         cDefaultLoc

-- | FFI import for SDL_ShowOpenFolderDialog
foreign import ccall safe "SDL_ShowOpenFolderDialog"
  c_sdlShowOpenFolderDialog ::
    FunPtr SDLDialogFileCallback ->     -- ^ callback
    Ptr () ->                           -- ^ userdata
    Ptr SDLWindow ->                   -- ^ window (using opaque C type)
    CString ->                          -- ^ default_location
    CBool ->                            -- ^ allow_many (Added based on example, check SDL header)
    IO ()

-- | Haskell wrapper for showing an open folder dialog
sdlShowOpenFolderDialog ::
    FunPtr SDLDialogFileCallback ->     -- ^ callback FunPtr
    Ptr () ->                           -- ^ userdata
    Maybe SDLWindow ->                  -- ^ Optional window
    Maybe String ->                     -- ^ Optional default location
    Bool ->                             -- ^ allow_many
    IO ()
sdlShowOpenFolderDialog callback userdata mWindow mDefaultLoc allowMany =
  let windowPtr = maybe nullPtr (\(SDLWindow p) -> p) mWindow -- Unpack Maybe SDLWindow
  in maybeWith withCString mDefaultLoc $ \cDefaultLoc -> -- Handle Maybe String
       c_sdlShowOpenFolderDialog
         callback
         userdata
         windowPtr -- Pass the raw Ptr SDL_Window
         cDefaultLoc
         (fromBool allowMany) -- Convert Bool to CBool

-- | FFI import for SDL_ShowFileDialogWithProperties
foreign import ccall safe "SDL_ShowFileDialogWithProperties"
  c_sdlShowFileDialogWithProperties ::
    CInt ->                             -- ^ type
    FunPtr SDLDialogFileCallback ->     -- ^ callback
    Ptr () ->                           -- ^ userdata
    SDLPropertiesID ->                  -- ^ props
    IO ()

-- | Haskell wrapper for showing a file dialog using properties
sdlShowFileDialogWithProperties ::
    SDLFileDialogType ->                -- ^ Dialog type
    FunPtr SDLDialogFileCallback ->     -- ^ callback FunPtr
    Ptr () ->                           -- ^ userdata
    SDLPropertiesID ->                  -- ^ Properties ID
    IO ()
sdlShowFileDialogWithProperties dialogType callback userdata props =
  c_sdlShowFileDialogWithProperties
    (fromIntegral $ fromEnum dialogType) -- Convert SDLFileDialogType to CInt
    callback
    userdata
    props

-- Property constants (using const_str for safety)
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
