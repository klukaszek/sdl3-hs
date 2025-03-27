module Main where

import SDL
import Control.Monad
import System.Exit (exitFailure, exitSuccess)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.C.Types
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.Marshal.Alloc (alloca, mallocBytes)
import Foreign.Marshal.Array (copyArray)
import Foreign.Storable (poke, peek)
import Data.Word (Word64)

-- Import SDL_free explicitly
foreign import ccall unsafe "SDL_free" sdlFree :: Ptr a -> IO ()

main :: IO ()
main = do
  -- Check compiled version
  putStrLn $ "Compiled SDL Version: " ++ show sdlVersion
  when (sdlVersionAtLeast 3 2 0) $
    putStrLn "Compiled with at least SDL 3.2.0"

  -- Get linked version
  linkedVersion <- sdlGetVersion
  putStrLn $ "Linked SDL Version: " ++ show linkedVersion

  -- Get revision
  revision <- sdlGetRevision
  putStrLn $ "SDL Revision: " ++ revision

  -- Initialize SDL with video and events subsystems
  initSuccess <- sdlInit [InitVideo]
  unless initSuccess $ do
    putStrLn "Failed to initialize SDL!"
    exitFailure

  -- Check initialized subsystems
  initializedSystems <- sdlWasInit []
  putStrLn "Initialized subsystems:"
  mapM_ printSubsystem initializedSystems

  -- Open user storage
  putStrLn "Opening user storage..."
  mStorage <- sdlOpenUserStorage "example.org" "StorageDemo" 0
  case mStorage of
    Nothing -> do
      putStrLn "Failed to open user storage!"
      sdlQuit
      exitFailure
    Just storage -> do
      putStrLn "User storage opened successfully."

      -- Wait for storage to be ready
      waitForStorageReady storage

      -- Write a test file
      writeTestFile storage

      -- Read the test file back
      readTestFile storage

      -- Enumerate directory contents
      listDirectory storage

      -- Close storage
      putStrLn "Closing storage..."
      success <- sdlCloseStorage storage
      unless success $ putStrLn "Failed to close storage cleanly!"

  -- Clean up and quit
  putStrLn "Shutting down SDL..."
  sdlQuit

  putStrLn "Application terminated successfully"
  exitSuccess

-- Helper to wait for storage to be ready
waitForStorageReady :: Ptr SDLStorage -> IO ()
waitForStorageReady storage = do
  putStrLn "Waiting for storage to be ready..."
  let checkReady = do
        ready <- sdlStorageReady storage
        unless ready $ do
          sdlDelay 1
          checkReady
  checkReady
  putStrLn "Storage is ready."

-- Write a test file
writeTestFile :: Ptr SDLStorage -> IO ()
writeTestFile storage = do
  putStrLn "Writing test file..."
  let content = "Hello, SDL Storage!"
      len = fromIntegral (length content) :: Word64
  contentPtr <- mallocBytes (fromIntegral len)
  withCString content $ \cstr -> do
    copyArray contentPtr cstr (fromIntegral len)
    success <- sdlWriteStorageFile storage "test.txt" (castPtr contentPtr) len
    free contentPtr  -- Use Haskell's free for mallocBytes
    if success
      then putStrLn "Successfully wrote test.txt"
      else do
        putStrLn "Failed to write test.txt!"
        err <- sdlGetError
        putStrLn $ "Error: " ++ err

-- Read the test file back
readTestFile :: Ptr SDLStorage -> IO ()
readTestFile storage = do
  putStrLn "Reading test file..."
  mSize <- sdlGetStorageFileSize storage "test.txt"
  case mSize of
    Nothing -> do
      putStrLn "Failed to get size of test.txt!"
      err <- sdlGetError
      putStrLn $ "Error: " ++ err
    Just size -> do
      buffer <- mallocBytes (fromIntegral size)
      success <- sdlReadStorageFile storage "test.txt" buffer size
      if success
        then do
          content <- peekCString (castPtr buffer)
          putStrLn $ "Read from test.txt: " ++ content
        else do
          putStrLn "Failed to read test.txt!"
          err <- sdlGetError
          putStrLn $ "Error: " ++ err
      free buffer  -- Use Haskell's free for mallocBytes

-- List directory contents
listDirectory :: Ptr SDLStorage -> IO ()
listDirectory storage = do
  putStrLn "Listing directory contents..."
  let callback :: Ptr () -> CString -> CString -> IO SDLEnumerationResult
      callback _ path name = do
        pathStr <- peekCString path
        nameStr <- peekCString name
        putStrLn $ " - " ++ pathStr ++ "/" ++ nameStr
        return sdlEnumSUCCESS
  success <- sdlEnumerateStorageDirectory storage Nothing callback nullPtr
  unless success $ do
    putStrLn "Failed to enumerate directory!"
    err <- sdlGetError
    putStrLn $ "Error: " ++ err

-- Helper function to print subsystem names
printSubsystem :: InitFlag -> IO ()
printSubsystem flag = putStrLn $ "  - " ++ case flag of
  InitAudio    -> "Audio"
  InitVideo    -> "Video"
  InitJoystick -> "Joystick"
  InitHaptic   -> "Haptic"
  InitGamepad  -> "Gamepad"
  InitEvents   -> "Events"
  InitSensor   -> "Sensor"
  InitCamera   -> "Camera"
  _            -> "Unknown subsystem"
