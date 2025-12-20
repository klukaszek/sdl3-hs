module Main where

import Control.Monad (when)
import SDL3

main :: IO ()
main = do
  platform <- sdlGetPlatform
  sdlLog $ "Running on platform: " ++ platform

  -- Check compile-time platform constants
  when sdlPlatformWindows $
    sdlLog "Compiled for Windows"

  when sdlPlatformLinux $
    sdlLog "Compiled for Linux"

  when sdlPlatformMacOS $
    sdlLog "Compiled for macOS"
