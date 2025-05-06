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

main :: IO ()
main = do
  -- Check compiled version
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  when (sdlVersionAtLeast 3 3 0) $
    sdlLog "Compiled with at least SDL 3.3.0"

  -- Get linked version
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion

  -- Get revision
  revision <- sdlGetRevision
  sdlLog $ "SDL Revision: " ++ revision

  -- Initialize SDL with video and events subsystems
  initSuccess <- sdlInit [SDL_INIT_VIDEO]
  unless initSuccess $ do
    sdlLog "Failed to initialize SDL!"
    exitFailure

  -- Check initialized subsystems
  initializedSystems <- sdlWasInit []
  sdlLog "Initialized subsystems:"
  mapM_ printSubsystem initializedSystems

  -- Open user storage
  sdlLog "Opening user storage..."
  mStorage <- sdlOpenUserStorage "example.org" "StorageDemo" 0
  case mStorage of
    Nothing -> do
      sdlLog "Failed to open user storage!"
      sdlQuit
      exitFailure
    Just storage -> do
      sdlLog "User storage opened successfully."

      -- Wait for storage to be ready
      waitForStorageReady storage

      -- Write a test file
      writeTestFile storage

      -- Read the test file back
      readTestFile storage

      -- Enumerate directory contents
      listDirectory storage

      -- Close storage
      sdlLog "Closing storage..."
      success <- sdlCloseStorage storage
      unless success $ sdlLog "Failed to close storage cleanly!"

  -- Clean up and quit
  sdlLog "Shutting down SDL..."
  sdlQuit

  sdlLog "Application terminated successfully"
  exitSuccess

-- Helper to wait for storage to be ready
waitForStorageReady :: Ptr SDLStorage -> IO ()
waitForStorageReady storage = do
  sdlLog "Waiting for storage to be ready..."
  let checkReady = do
        ready <- sdlStorageReady storage
        unless ready $ do
          sdlDelay 1
          checkReady
  checkReady
  sdlLog "Storage is ready."

-- Write a test file
writeTestFile :: Ptr SDLStorage -> IO ()
writeTestFile storage = do
  sdlLog "Writing test file..."
  let content = "Hello, SDL Storage!"
      len = fromIntegral (length content) :: Word64
  contentPtr <- mallocBytes (fromIntegral len)
  withCString content $ \cstr -> do
    copyArray contentPtr cstr (fromIntegral len)
    success <- sdlWriteStorageFile storage "test.txt" (castPtr contentPtr) len
    free contentPtr  -- Use Haskell's free for mallocBytes
    if success
      then sdlLog "Successfully wrote test.txt"
      else do
        sdlLog "Failed to write test.txt!"
        err <- sdlGetError
        sdlLog $ "Error: " ++ err

-- Read the test file back
readTestFile :: Ptr SDLStorage -> IO ()
readTestFile storage = do
  sdlLog "Reading test file..."
  mSize <- sdlGetStorageFileSize storage "test.txt"
  case mSize of
    Nothing -> do
      sdlLog "Failed to get size of test.txt!"
      err <- sdlGetError
      sdlLog $ "Error: " ++ err
    Just size -> do
      buffer <- mallocBytes (fromIntegral size)
      success <- sdlReadStorageFile storage "test.txt" buffer size
      if success
        then do
          content <- peekCString (castPtr buffer)
          sdlLog $ "Read from test.txt: " ++ content
        else do
          sdlLog "Failed to read test.txt!"
          err <- sdlGetError
          sdlLog $ "Error: " ++ err
      free buffer  -- Use Haskell's free for mallocBytes

-- List directory contents
listDirectory :: Ptr SDLStorage -> IO ()
listDirectory storage = do
  sdlLog "Listing directory contents..."
  let callback :: Ptr () -> CString -> CString -> IO SDLEnumerationResult
      callback _ path name = do
        pathStr <- peekCString path
        nameStr <- peekCString name
        sdlLog $ " - " ++ pathStr ++ "/" ++ nameStr
        return SDL_ENUM_SUCCESS
  success <- sdlEnumerateStorageDirectory storage Nothing callback nullPtr
  unless success $ do
    sdlLog "Failed to enumerate directory!"
    err <- sdlGetError
    sdlLog $ "Error: " ++ err

-- Helper function to print subsystem names
printSubsystem :: SDLInitFlags -> IO ()
printSubsystem flag = sdlLog $ "  - " ++ case flag of
  SDL_INIT_AUDIO    -> "Audio"
  SDL_INIT_VIDEO    -> "Video"
  SDL_INIT_JOYSTICK -> "Joystick"
  SDL_INIT_HAPTIC   -> "Haptic"
  SDL_INIT_GAMEPAD  -> "Gamepad"
  SDL_INIT_EVENTS   -> "Events"
  SDL_INIT_SENSOR   -> "Sensor"
  SDL_INIT_CAMERA   -> "Camera"
  _            -> "Unknown subsystem"
