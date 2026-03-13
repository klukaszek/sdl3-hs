{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module SDL3.Wrapped.Dialog
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

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign (fromBool)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import SDL3.Properties (SDLPropertiesID)
import SDL3.Raw.Dialog
  ( SDLDialogFileCallback
  , SDLFileDialogType(..)
  , pattern SDL_FILEDIALOG_OPENFILE
  , pattern SDL_FILEDIALOG_SAVEFILE
  , pattern SDL_FILEDIALOG_OPENFOLDER
  , sdlPropFileDialogFiltersPointer
  , sdlPropFileDialogNfiltersNumber
  , sdlPropFileDialogWindowPointer
  , sdlPropFileDialogLocationString
  , sdlPropFileDialogManyBoolean
  , sdlPropFileDialogTitleString
  , sdlPropFileDialogAcceptString
  , sdlPropFileDialogCancelString
  )
import qualified SDL3.Raw.Dialog as Raw
import SDL3.Video (SDLWindow(..))

data SDLDialogFileFilter = SDLDialogFileFilter
  { filterName :: String
  , filterPattern :: String
  } deriving (Eq, Show)

withRawDialogFileFilter :: SDLDialogFileFilter -> (Raw.SDLDialogFileFilter -> IO a) -> IO a
withRawDialogFileFilter SDLDialogFileFilter{..} f =
  withCString filterName $ \filterNamePtr ->
    withCString filterPattern $ \filterPatternPtr ->
      f Raw.SDLDialogFileFilter
        { Raw.filterName = filterNamePtr
        , Raw.filterPattern = filterPatternPtr
        }

withRawDialogFilters :: [SDLDialogFileFilter] -> (Ptr Raw.SDLDialogFileFilter -> CInt -> IO a) -> IO a
withRawDialogFilters filters f =
  withMany withRawDialogFileFilter filters $ \rawFilters ->
    withArrayLen rawFilters $ \filterCount filterPtr ->
      f filterPtr (fromIntegral filterCount)

withMaybeCString :: Maybe String -> (CString -> IO a) -> IO a
withMaybeCString Nothing f = f nullPtr
withMaybeCString (Just str) f = withCString str f

windowPtr :: Maybe SDLWindow -> Ptr SDLWindow
windowPtr = maybe nullPtr (\(SDLWindow ptr) -> ptr)

sdlShowOpenFileDialog ::
  MonadIO m =>
  FunPtr SDLDialogFileCallback ->
  Ptr () ->
  Maybe SDLWindow ->
  [SDLDialogFileFilter] ->
  Maybe String ->
  Bool ->
  m ()
sdlShowOpenFileDialog callback userdata window filters defaultLocation allowMany =
  liftIO $
    withRawDialogFilters filters $ \filtersPtr filterCount ->
      withMaybeCString defaultLocation $ \defaultLocationPtr ->
        Raw.sdlShowOpenFileDialog
          callback
          userdata
          (windowPtr window)
          filtersPtr
          filterCount
          defaultLocationPtr
          (fromBool allowMany)

sdlShowSaveFileDialog ::
  MonadIO m =>
  FunPtr SDLDialogFileCallback ->
  Ptr () ->
  Maybe SDLWindow ->
  [SDLDialogFileFilter] ->
  Maybe String ->
  m ()
sdlShowSaveFileDialog callback userdata window filters defaultLocation =
  liftIO $
    withRawDialogFilters filters $ \filtersPtr filterCount ->
      withMaybeCString defaultLocation $ \defaultLocationPtr ->
        Raw.sdlShowSaveFileDialog
          callback
          userdata
          (windowPtr window)
          filtersPtr
          filterCount
          defaultLocationPtr

sdlShowOpenFolderDialog ::
  MonadIO m =>
  FunPtr SDLDialogFileCallback ->
  Ptr () ->
  Maybe SDLWindow ->
  Maybe String ->
  Bool ->
  m ()
sdlShowOpenFolderDialog callback userdata window defaultLocation allowMany =
  liftIO $
    withMaybeCString defaultLocation $ \defaultLocationPtr ->
      Raw.sdlShowOpenFolderDialog
        callback
        userdata
        (windowPtr window)
        defaultLocationPtr
        (fromBool allowMany)

sdlShowFileDialogWithProperties ::
  MonadIO m =>
  SDLFileDialogType ->
  FunPtr SDLDialogFileCallback ->
  Ptr () ->
  SDLPropertiesID ->
  m ()
sdlShowFileDialogWithProperties dialogType callback userdata props =
  liftIO $ Raw.sdlShowFileDialogWithProperties dialogType callback userdata props
