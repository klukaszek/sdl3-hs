import SDL

main :: IO ()
main = do
#ifdef SDL_PLATFORM_ANDROID
  -- Check if running on a Chromebook
  isChromebook <- sdlIsChromebook
  sdlLog $ "Running on Chromebook: " ++ show isChromebook

  -- Show a toast notification
  success <- sdlShowAndroidToast "Hello from Haskell!" 1 (-1) 0 0
  sdlLog $ "Toast shown: " ++ show success
#endif

  -- Check sandbox environment
  sandbox <- sdlGetSandbox
  sdlLog $ "Sandbox: " ++ show sandbox
