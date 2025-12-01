-- examples/TrayExample.hs

import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Data.IORef
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import SDL3
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  -- Check compiled version
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  when (sdlVersionAtLeast 3 3 0) $ sdlLog "Compiled with at least SDL 3.3.0"

  -- Get linked version
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  -- Initialize SDL with video and events subsystems
  initSuccess <- sdlInit [SDL_INIT_VIDEO, SDL_INIT_EVENTS]
  unless initSuccess $ do
    err <- sdlGetError
    sdlLog $ "Failed to initialize SDL: " ++ err
    exitFailure

  -- Check what subsystems are initialized
  initializedSystems <- sdlWasInit []
  sdlLog "Initialized subsystems:"
  mapM_ printSubsystem initializedSystems

  -- Load the tray icon
  mSurfacePtr <- sdlLoadBMP "examples/Content/Images/ravioli.bmp"
  case mSurfacePtr of
    Nothing -> do
      err <- sdlGetError
      sdlLog $ "Failed to load ravioli.bmp: " ++ err
      sdlQuit
      exitFailure
    Just surfacePtr -> do
      -- Inspect the surface
      surface <- peek surfacePtr
      sdlLog $ "Surface details: " ++ show surface

      -- Create the tray
      mTray <- sdlCreateTray (Just surfacePtr) (Just "SDL3 Haskell Tray Example")
      case mTray of
        Nothing -> do
          err <- sdlGetError
          sdlLog $ "Failed to create tray: " ++ err
          sdlDestroySurface surfacePtr
          sdlQuit
          exitFailure
        Just tray -> do
          sdlSetTrayIcon tray (Just surfacePtr)
          sdlSetTrayTooltip tray (Just "SDL3 Haskell Tray Example - Use menu to quit")

          -- Create a quit flag that can be shared between callbacks and main loop
          quitFlag <- newIORef False

          -- Create a simple menu for the tray
          mMenu <- sdlCreateTrayMenu tray
          case mMenu of
            Nothing -> do
              err <- sdlGetError
              sdlLog $ "Failed to create tray menu: " ++ err
            Just menu -> do
              -- Add menu entries
              mEntry1 <- sdlInsertTrayEntryAt menu 0 (Just "Hello from Haskell!") SDL_TRAYENTRY_BUTTON
              case mEntry1 of
                Nothing -> do
                  err <- sdlGetError
                  sdlLog $ "Failed to create menu entry 1: " ++ err
                Just entry1 -> do
                  sdlLog "Menu entry 1 created successfully"
                  sdlSetTrayEntryCallback entry1 (Just (helloCallback, nullPtr))

              -- Add a separator and quit entry
              mEntry2 <- sdlInsertTrayEntryAt menu 1 (Just "Quit Application") SDL_TRAYENTRY_BUTTON
              case mEntry2 of
                Nothing -> do
                  err <- sdlGetError
                  sdlLog $ "Failed to create menu entry 2: " ++ err
                Just entry2 -> do
                  sdlLog "Menu entry 2 created successfully"
                  -- Pass the quit flag to the callback
                  sdlSetTrayEntryCallback entry2 (Just (quitCallback quitFlag, nullPtr))

          -- Update the trays to ensure changes are reflected
          sdlUpdateTrays

          sdlLog "Tray created with menu. Try right-clicking the tray icon to see the menu."
          sdlLog "Use the 'Quit Application' menu item to exit."

          -- Start simple event loop
          eventLoop tray quitFlag

          -- Cleanup
          sdlDestroyTray tray
          sdlDestroySurface surfacePtr
          sdlLog "Tray destroyed."

  sdlLog "Shutting down SDL..."
  sdlQuit
  sdlLog "Application terminated successfully"
  exitSuccess

-- | Simple event loop that checks for quit flag and SDL quit events
eventLoop :: SDLTray -> IORef Bool -> IO ()
eventLoop tray quitFlag = do
  -- Check if quit was requested from tray menu
  shouldQuit <- readIORef quitFlag

  if shouldQuit
    then do
      sdlLog "Quit requested from tray menu."
      return ()
    else do
      -- Check for SDL quit events (like Ctrl+C)
      sdlPumpEvents
      maybeEvent <- sdlPollEvent
      case maybeEvent of
        Just (SDLEventQuit _) -> do
          sdlLog "SDL quit event received."
          return ()
        _ -> do
          -- Sleep for a short time to avoid busy waiting
          threadDelay 100000 -- 100ms
          eventLoop tray quitFlag

-- Simple callback for menu entry clicks
helloCallback :: SDLTrayCallback
helloCallback _ _ = do
  sdlLog "Hello menu entry clicked!"

-- Quit callback for menu entry clicks
quitCallback :: IORef Bool -> SDLTrayCallback
quitCallback quitFlag _ _ = do
  sdlLog "Quit menu entry clicked - quitting application!"
  writeIORef quitFlag True

-- Helper function to print subsystem names
printSubsystem :: SDLInitFlags -> IO ()
printSubsystem flag =
  sdlLog $
    "  - " ++ case flag of
      SDL_INIT_AUDIO -> "Audio"
      SDL_INIT_VIDEO -> "Video"
      SDL_INIT_JOYSTICK -> "Joystick"
      SDL_INIT_HAPTIC -> "Haptic"
      SDL_INIT_GAMEPAD -> "Gamepad"
      SDL_INIT_EVENTS -> "Events"
      SDL_INIT_SENSOR -> "Sensor"
      SDL_INIT_CAMERA -> "Camera"
      _ -> "Unknown subsystem"
