module Main where

import Control.Monad (unless)
import SDL

main :: IO ()
main = do
  -- Initialize SDL
  initSuccess <- sdlInit [SDL_INIT_VIDEO]
  unless initSuccess $ do
    sdlLog "Failed to initialize SDL"
    return ()

  -- Get preferred locales
  locales <- sdlGetPreferredLocales

  -- Print the results
  sdlLog "User's preferred locales:"
  mapM_ printLocale locales

  -- Clean up
  sdlQuit

-- Helper function to print a locale in a readable format
printLocale :: SDLLocale -> IO ()
printLocale (SDLLocale lang countryVal) = do
  let countryStr = maybe "" (", " ++) countryVal
  sdlLog $ "Language: " ++ lang ++ countryStr
