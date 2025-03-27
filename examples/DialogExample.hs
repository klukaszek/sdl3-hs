module Main where

import Control.Monad
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CBool(..), CInt(..))
import Foreign.Marshal.Array (peekArray0, withArrayLen)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import Foreign.StablePtr (freeStablePtr, newStablePtr)
import SDL
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  -- Initialize SDL
  initSuccess <- sdlInit [InitVideo]
  unless initSuccess $ do
    putStrLn "Failed to initialize SDL!"
    exitFailure

  -- Create a window for our dialog
  maybeWindow <- sdlCreateWindow      
    "SDL Dialog Example"
    800 
    600 
    [sdlWindowResizable]

  case maybeWindow of
    Nothing -> do
      putStrLn "Failed to create window!"
      sdlQuit
      exitFailure
    Just window -> do
      -- Define some file filters
      let filters = 
            [ SDLDialogFileFilter 
                { filterName = "Text Files"
                , filterPattern = "txt;doc;md"
                }
            , SDLDialogFileFilter
                { filterName = "All Files"
                , filterPattern = "*"
                }
            ]

      -- Create our callback function
      let dialogCallback :: Ptr () -> Ptr CString -> CInt -> IO ()
          dialogCallback userdata filelist filterIndex = do
            if filelist == nullPtr 
              then putStrLn "Dialog was cancelled or error occurred"
              else do
                -- Get the list of files
                files <- peekArray0 nullPtr filelist
                fileNames <- mapM peekCString files
                putStrLn $ "Selected files (filter index " ++ show filterIndex ++ "):"
                mapM_ (putStrLn . ("  - " ++)) fileNames

      -- Convert callback to stable pointer for C
      callbackPtr <- wrapDialogCallback dialogCallback

      -- Show different types of dialogs
      putStrLn "\nShowing Open File Dialog..."
      withFilters filters $ \filtersPtr nfilters ->
        sdlShowOpenFileDialog 
          callbackPtr
          nullPtr       
          (unSDLWindow window)
          filtersPtr    
          nfilters      
          nullPtr       
          1            -- allow multiple selection (1 for True)

      putStrLn "\nShowing Save File Dialog..."
      withFilters filters $ \filtersPtr nfilters ->
        sdlShowSaveFileDialog
          callbackPtr
          nullPtr       
          (unSDLWindow window)
          filtersPtr    
          nfilters      
          nullPtr       

      putStrLn "\nShowing Folder Dialog..."
      sdlShowOpenFolderDialog
        callbackPtr
        nullPtr        
        (unSDLWindow window)
        nullPtr        
        0             -- single selection only (0 for False)

      -- Show a dialog with custom properties
      putStrLn "\nShowing Custom Dialog..."
      props <- sdlCreateProperties
      
      -- Set some custom properties
      sdlSetStringProperty props sdlPropFileDialogTitleString "Select a File"
      sdlSetStringProperty props sdlPropFileDialogAcceptString "Choose"
      sdlSetStringProperty props sdlPropFileDialogCancelString "Never Mind"

      sdlShowFileDialogWithProperties 
        (fromIntegral $ fromEnum SDL_FILEDIALOG_OPENFILE)
        callbackPtr
        nullPtr
        props

      -- Cleanup
      sdlDestroyProperties props
      sdlDestroyWindow window
      sdlQuit
      exitSuccess

-- Helper function to manage filter array lifecycle
withFilters ::
  [SDLDialogFileFilter] ->
  (Ptr SDLDialogFileFilter -> CInt -> IO a) ->
  IO a
withFilters filters action =
  withArrayLen filters $ \len filtersPtr ->
    action filtersPtr (fromIntegral len)

-- | Type for the dialog callback function
type DialogCallback = Ptr () -> Ptr CString -> CInt -> IO ()

-- | Foreign wrapper for the callback
foreign import ccall "wrapper"
  wrapDialogCallback :: DialogCallback -> IO (FunPtr DialogCallback)
