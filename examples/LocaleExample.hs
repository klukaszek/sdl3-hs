module Main where

import SDL
import Foreign.C.Types (CInt(..))
import Control.Monad (unless)

main :: IO ()
main = do
    -- Initialize SDL
    initSuccess <- sdlInit [InitVideo]
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
printLocale (SDLLocale lang country) = do
    let countryStr = maybe "" (", " ++) country
    sdlLog $ "Language: " ++ lang ++ countryStr
