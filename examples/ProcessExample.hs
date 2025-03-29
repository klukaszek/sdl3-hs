module Main where

import SDL
import Control.Monad (unless)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  -- Initialize SDL
  initSuccess <- sdlInit []
  unless initSuccess $ do
    sdlLog "Failed to initialize SDL!"
    exitFailure

  -- Create a process to run 'ls' (or 'dir' on Windows)
  let args = ["ls", "-l"]  -- Use "dir" on Windows
  process <- sdlCreateProcess args True  -- Pipe stdio
  case process of
    Nothing -> do
      sdlLog "Failed to create process!"
      sdlQuit
      exitFailure
    Just proc -> do
      -- Read process output
      output <- sdlReadProcess proc
      case output of
        Nothing -> sdlLog "Failed to read process output!"
        Just (out, exitcode) -> do
          sdlLog $ "Process output:\n" ++ out
          sdlLog $ "Exit code: " ++ show exitcode

      -- Clean up
      sdlDestroyProcess proc

  -- Shutdown SDL
  sdlQuit
  sdlLog "Application terminated successfully"
  exitSuccess
