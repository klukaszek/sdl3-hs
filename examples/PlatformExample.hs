module Main where

import SDL
import Control.Monad (when)

main :: IO ()
main = do
  platform <- sdlGetPlatform
  putStrLn $ "Running on platform: " ++ platform
  
  -- Check compile-time platform constants
  when sdlPlatformWindows $
    putStrLn "Compiled for Windows"
  
  when sdlPlatformLinux $
    putStrLn "Compiled for Linux"
  
  when sdlPlatformMacOS $
    putStrLn "Compiled for macOS"
