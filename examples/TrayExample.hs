-- examples/TrayExample.hs
import SDL
import Control.Monad
import System.Exit (exitFailure, exitSuccess)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)

main :: IO ()
main = do
  -- Initialize SDL with video and events subsystems
  initSuccess <- sdlInit [InitVideo]
  
  unless initSuccess $ do
    err <- sdlGetError
    sdlLog $ "Failed to initialize SDL: " ++ err
    exitFailure
  
  -- Check what subsystems are initialized
  initializedSystems <- sdlWasInit []
  sdlLog "Initialized subsystems:"
  mapM_ printSubsystem initializedSystems

  mSurfacePtr <- sdlLoadBMP "assets/ravioli.bmp"
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
      
      mTray <- sdlCreateTray (Just surfacePtr) (Just "My Tray Icon")
      case mTray of
        Nothing -> do
          err <- sdlGetError
          sdlLog $ "Failed to create tray: " ++ err
          sdlDestroySurface surfacePtr
          sdlQuit
          exitFailure
        Just tray -> do
          sdlSetTrayIcon tray (Just surfacePtr)
          -- Keep the program running to observe the tray
          sdlLog "Tray created. Press Enter to exit..."
          _ <- getLine
          sdlDestroyTray tray
          sdlDestroySurface surfacePtr
          sdlQuit
          exitSuccess


-- Helper function to print subsystem names
printSubsystem :: InitFlag -> IO ()
printSubsystem flag = sdlLog $ "  - " ++ case flag of
  InitAudio    -> "Audio"
  InitVideo    -> "Video"
  InitJoystick -> "Joystick"
  InitHaptic   -> "Haptic"
  InitGamepad  -> "Gamepad"
  InitEvents   -> "Events"
  InitSensor   -> "Sensor"
  InitCamera   -> "Camera"
  _            -> "Unknown subsystem"
