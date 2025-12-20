import SDL3
import Control.Monad (unless)
import System.Exit (exitFailure)

main :: IO ()
main = do
  -- Initialize SDL
  initSuccess <- sdlInit [SDL_INIT_VIDEO]
  unless initSuccess $ do
    sdlLog "Failed to initialize SDL!"
    exitFailure

#ifdef SDL_PLATFORM_ANDROID
  -- Check if running on a Chromebook
  isChromebook <- sdlIsChromebook
  sdlLog $ "Running on Chromebook: " ++ show isChromebook

  -- Show a toast notification
  success <- sdlShowAndroidToast "Hello from Haskell!" 1 (-1) 0 0
  sdlLog $ "Toast shown: " ++ show success
#endif

  -- Check device type
  isTablet <- sdlIsTablet
  isTV <- sdlIsTV
  sdlLog $ "Device type - Tablet: " ++ show isTablet ++ ", TV: " ++ show isTV

  -- Check sandbox environment
  sandbox <- sdlGetSandbox
  sdlLog $ "Sandbox environment: " ++ show sandbox

  case sandbox of
    SDLSandboxNone -> sdlLog "  → Running without sandbox (normal for development)"
    SDLSandboxMacOS -> sdlLog "  → Running in macOS App Sandbox (Mac App Store app)"
    SDLSandboxFlatpak -> sdlLog "  → Running in Flatpak sandbox"
    SDLSandboxSnap -> sdlLog "  → Running in Snap package sandbox"
    SDLSandboxUnknownContainer -> sdlLog "  → Running in unknown container"

  -- Clean up
  sdlQuit
  sdlLog "System example completed successfully"
