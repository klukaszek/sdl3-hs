module Main where

import SDL.Init
import SDL.Filesystem
import Control.Monad (unless, when)
import System.Exit (exitFailure, exitSuccess)
import Foreign.Ptr (nullPtr)

main :: IO ()
main = do
  -- Initialize SDL with basic subsystems
  initSuccess <- sdlInit [InitVideo, InitEvents]
  unless initSuccess $ do
    putStrLn "Failed to initialize SDL!"
    exitFailure

  -- Set basic app metadata
  appMetadataSuccess <- sdlSetAppMetadata 
    "SDL Filesystem Demo"
    "1.0.0"
    "com.example.filesystemdemo"
  unless appMetadataSuccess $ do
    putStrLn "Failed to set app metadata!"

  -- Get and print the base path
  basePath <- sdlGetBasePath
  putStrLn $ "Base Path: " ++ maybe "Not available" id basePath

  -- Get and print the preferences path
  prefPath <- sdlGetPrefPath "ExampleOrg" "FilesystemDemo"
  case prefPath of
    Nothing -> putStrLn "Failed to get preferences path!"
    Just path -> do
      putStrLn $ "Preferences Path: " ++ path

      -- Create a test directory in the preferences path
      let testDir = path ++ "test_dir"
      dirCreated <- sdlCreateDirectory testDir
      putStrLn $ "Created test directory: " ++ show dirCreated

      -- List contents of the preferences path
      dirContents <- sdlGlobDirectory path Nothing (SDLGlobFlags 0)
      putStrLn "Contents of preferences path:"
      case dirContents of
        Nothing -> putStrLn "  Failed to list directory contents!"
        Just contents -> mapM_ (putStrLn . ("  - " ++)) contents

  -- Get and print the user's Documents folder
  docsPath <- sdlGetUserFolder sdlFolderDOCUMENTS
  putStrLn $ "Documents Path: " ++ maybe "Not available" id docsPath

  -- Clean up and quit
  sdlQuit
  putStrLn "Application terminated successfully"
  exitSuccess
