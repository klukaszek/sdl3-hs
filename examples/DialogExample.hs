module Main where

import Control.Monad
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Array (peekArray0)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import SDL3
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  -- Initialize SDL
  initSuccess <- sdlInit [SDL_INIT_VIDEO]
  unless initSuccess $ do
    sdlLog "Failed to initialize SDL!"
    exitFailure

  -- Create a window for our dialog
  maybeWindow <-
    sdlCreateWindow
      "SDL Dialog Example"
      800
      600
      [SDL_WINDOW_RESIZABLE]

  case maybeWindow of
    Nothing -> do
      sdlLog "Failed to create window!"
      sdlQuit
      exitFailure
    Just window -> do
      -- Define some file filters
      let filters =
            [ SDLDialogFileFilter
                { filterName = "Text Files",
                  filterPattern = "txt;doc;md"
                },
              SDLDialogFileFilter
                { filterName = "All Files",
                  filterPattern = "*"
                }
            ]

      -- Create our callback function
      let dialogCallback :: Ptr () -> Ptr CString -> CInt -> IO ()
          dialogCallback _ filelist filterIndex = do
            if filelist == nullPtr
              then sdlLog "Dialog was cancelled or error occurred"
              else do
                -- Get the list of files
                files <- peekArray0 nullPtr filelist
                fileNames <- mapM peekCString files
                sdlLog $ "Selected files (filter index " ++ show filterIndex ++ "):"
                mapM_ (sdlLog . ("  - " ++)) fileNames

      -- Convert callback to stable pointer for C
      callbackPtr <- wrapDialogCallback dialogCallback

      -- Show different types of dialogs
      sdlLog "\nShowing Open File Dialog..."
      sdlShowOpenFileDialog
        callbackPtr
        nullPtr
        (Just window)
        filters
        Nothing
        True

      sdlLog "\nShowing Save File Dialog..."
      sdlShowSaveFileDialog
        callbackPtr
        nullPtr
        (Just window)
        filters
        Nothing

      sdlLog "\nShowing Folder Dialog..."
      sdlShowOpenFolderDialog
        callbackPtr
        nullPtr
        (Just window)
        Nothing
        False

      -- Show a dialog with custom properties
      sdlLog "\nShowing Custom Dialog..."
      props <- sdlCreateProperties

      -- Set some custom properties
      _ <- sdlSetStringProperty props sdlPropFileDialogTitleString "Select a File"
      _ <- sdlSetStringProperty props sdlPropFileDialogAcceptString "Choose"
      _ <- sdlSetStringProperty props sdlPropFileDialogCancelString "Never Mind"

      sdlShowFileDialogWithProperties
        SDL_FILEDIALOG_OPENFILE
        callbackPtr
        nullPtr
        props

      -- -- Event loop to keep program alive
      -- let eventLoop = do
      --       sdlPumpEvents  -- Process events
      --       threadDelay 100000  -- Small delay to prevent CPU spinning
      --       -- You might want to add a way to exit the loop
      --       -- For example, checking for a quit event or after all dialogs are done
      --       eventLoop

      -- -- Run the event loop
      -- sdlLog "Running event loop... (Press Ctrl+C to exit)"
      -- eventLoop `finally` do
      --   -- Cleanup
      sdlDestroyProperties props
      sdlDestroyWindow window
      sdlQuit
      exitSuccess

-- | Type for the dialog callback function
type DialogCallback = Ptr () -> Ptr CString -> CInt -> IO ()

-- | Foreign wrapper for the callback
foreign import ccall "wrapper"
  wrapDialogCallback :: DialogCallback -> IO (FunPtr DialogCallback)
