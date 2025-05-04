{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SDL/Init.hs
{-|
Module      : SDL.Init
Description : Initialization and shutdown functions for SDL3.
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

This module provides Haskell bindings to the SDL stdinc functionality.
-}

#include <SDL3/SDL_init.h>

module SDL.Init
  ( 
  -- * Initialization
  -- ** Initialization Flags
    SDLInitFlags
  , pattern SDL_INIT_AUDIO
  , pattern SDL_INIT_VIDEO
  , pattern SDL_INIT_JOYSTICK
  , pattern SDL_INIT_HAPTIC
  , pattern SDL_INIT_GAMEPAD
  , pattern SDL_INIT_EVENTS
  , pattern SDL_INIT_SENSOR
  , pattern SDL_INIT_CAMERA
  
  -- ** Basic Initialization Functions
  , sdlInit
  , sdlInitSubSystem
  , sdlQuitSubSystem
  , sdlWasInit
  , sdlQuit
  
  -- ** Thread Functions
  , sdlIsMainThread
  , MainThreadCallback
  , sdlRunOnMainThread
  
  -- ** App Result Types
  , SDLAppResult(..)
  , SDLAppInitFunc
  , SDLAppIterateFunc
  , SDLAppEventFunc
  , SDLAppQuitFunc
  
  -- ** App Metadata
  , sdlSetAppMetadata
  , sdlSetAppMetadataProperty
  , sdlGetAppMetadataProperty
  
  -- *** App Metadata Properties
  , propAppMetadataName
  , propAppMetadataVersion
  , propAppMetadataIdentifier
  , propAppMetadataCreator
  , propAppMetadataCopyright
  , propAppMetadataUrl
  , propAppMetadataType
  ) where

import Prelude hiding (init)
import Foreign
import Foreign.C

-- | Flags for SDL initialization
type SDLInitFlags = Word32

-- | Use the audio subsystem
pattern SDL_INIT_AUDIO = (#const SDL_INIT_AUDIO) :: SDLInitFlags

-- | Use the video subsystem
pattern SDL_INIT_VIDEO = (#const SDL_INIT_VIDEO) :: SDLInitFlags

-- | Use the joystick subsystem
pattern SDL_INIT_JOYSTICK = (#const SDL_INIT_JOYSTICK) :: SDLInitFlags

-- | Use the haptic (force feedback) subsystem
pattern SDL_INIT_HAPTIC = (#const SDL_INIT_HAPTIC) :: SDLInitFlags

-- | Use the gamepad subsystem
pattern SDL_INIT_GAMEPAD = (#const SDL_INIT_GAMEPAD) :: SDLInitFlags

-- | Use the events subsystem
pattern SDL_INIT_EVENTS = (#const SDL_INIT_EVENTS) :: SDLInitFlags

-- | Use the sensor subsystem
pattern SDL_INIT_SENSOR = (#const SDL_INIT_SENSOR) :: SDLInitFlags

-- | Use the camera subsystem
pattern SDL_INIT_CAMERA = (#const SDL_INIT_CAMERA) :: SDLInitFlags

-- | Result for app callbacks
data SDLAppResult = 
    SDL_APP_CONTINUE    -- ^ Continue app execution
  | SDL_APP_SUCCESS     -- ^ Terminate with success
  | SDL_APP_FAILURE     -- ^ Terminate with error
  deriving (Eq, Show, Enum)

-- | Type for app initialization callback
type SDLAppInitFunc = Ptr (Ptr ()) -> CInt -> Ptr (Ptr CChar) -> IO SDLAppResult

-- | Create a C function pointer from a Haskell AppInitFunc
foreign import ccall "wrapper"
  makeAppInitCallback :: (Ptr (Ptr ()) -> CInt -> Ptr (Ptr CChar) -> IO CInt) -> IO (FunPtr (Ptr (Ptr ()) -> CInt -> Ptr (Ptr CChar) -> IO CInt))

-- | Wrap a Haskell AppInitFunc to convert the result
wrapAppInitFunc :: SDLAppInitFunc -> (Ptr (Ptr ()) -> CInt -> Ptr (Ptr CChar) -> IO CInt)
wrapAppInitFunc f appstate argc argv = do
  result <- f appstate argc argv
  return (fromIntegral (fromEnum result))

-- | Type for app iteration callback
type SDLAppIterateFunc = Ptr () -> IO SDLAppResult

-- | Create a C function pointer from a Haskell AppIterateFunc
foreign import ccall "wrapper"
  makeAppIterateCallback :: (Ptr () -> IO CInt) -> IO (FunPtr (Ptr () -> IO CInt))

-- | Wrap a Haskell AppIterateFunc to convert the result
wrapAppIterateFunc :: SDLAppIterateFunc -> (Ptr () -> IO CInt)
wrapAppIterateFunc f appstate = do
  result <- f appstate
  return (fromIntegral (fromEnum result))

-- | Type for app event callback
type SDLAppEventFunc = Ptr () -> Ptr () -> IO SDLAppResult  -- Second Ptr () should be Ptr SDL_Event

-- | Create a C function pointer from a Haskell AppEventFunc
foreign import ccall "wrapper"
  makeAppEventCallback :: (Ptr () -> Ptr () -> IO CInt) -> IO (FunPtr (Ptr () -> Ptr () -> IO CInt))

-- | Wrap a Haskell AppEventFunc to convert the result
wrapAppEventFunc :: SDLAppEventFunc -> (Ptr () -> Ptr () -> IO CInt)
wrapAppEventFunc f appstate event = do
  result <- f appstate event
  return (fromIntegral (fromEnum result))

-- | Type for app quit callback
type SDLAppQuitFunc = Ptr () -> SDLAppResult -> IO ()

-- | Create a C function pointer from a Haskell AppQuitFunc
foreign import ccall "wrapper"
  makeAppQuitCallback :: (Ptr () -> CInt -> IO ()) -> IO (FunPtr (Ptr () -> CInt -> IO ()))

-- | Wrap a Haskell AppQuitFunc to convert the input argument
wrapAppQuitFunc :: SDLAppQuitFunc -> (Ptr () -> CInt -> IO ())
wrapAppQuitFunc f appstate result = f appstate (toEnum (fromIntegral result))

-- | Initialize the SDL library.
foreign import ccall unsafe "SDL_Init" sdlInit_ :: Word32 -> IO Bool

-- | Initialize the SDL library.
sdlInit :: [SDLInitFlags] -> IO Bool
sdlInit flags = sdlInit_ (foldr (.|.) 0 flags)

-- | Initialize specific SDL subsystems.
foreign import ccall unsafe "SDL_InitSubSystem" sdlInitSubSystem_ :: Word32 -> IO Bool

-- | Initialize specific SDL subsystems.
sdlInitSubSystem :: [SDLInitFlags] -> IO Bool
sdlInitSubSystem flags = sdlInitSubSystem_ (foldr (.|.) 0 flags)

-- | Shut down specific SDL subsystems.
foreign import ccall unsafe "SDL_QuitSubSystem" sdlQuitSubSystem_ :: Word32 -> IO ()

-- | Shut down specific SDL subsystems.
sdlQuitSubSystem :: [SDLInitFlags] -> IO ()
sdlQuitSubSystem flags = sdlQuitSubSystem_ (foldr (.|.) 0 flags)

-- | Get a mask of the specified subsystems which are currently initialized.
foreign import ccall unsafe "SDL_WasInit" sdlWasInit_ :: Word32 -> IO Word32

-- | Get a mask of the specified subsystems which are currently initialized.
sdlWasInit :: [SDLInitFlags] -> IO [SDLInitFlags]
sdlWasInit flags = do
  result <- sdlWasInit_ (foldr (.|.) 0 flags)
  -- Create list of initialized flags
  return $ filter (\flag -> (result .&. flag) /= 0) 
    [SDL_INIT_AUDIO, SDL_INIT_VIDEO, SDL_INIT_JOYSTICK, SDL_INIT_HAPTIC, SDL_INIT_GAMEPAD, SDL_INIT_EVENTS, SDL_INIT_SENSOR, SDL_INIT_CAMERA]

-- | Clean up all initialized subsystems.
foreign import ccall unsafe "SDL_Quit" sdlQuit_ :: IO ()

-- | Clean up all initialized subsystems.
sdlQuit :: IO ()
sdlQuit = sdlQuit_

-- | Return whether this is the main thread.
foreign import ccall unsafe "SDL_IsMainThread" sdlIsMainThread_ :: IO Bool

-- | Return whether this is the main thread.
sdlIsMainThread :: IO Bool
sdlIsMainThread = sdlIsMainThread_

-- | Type for main thread callback
type MainThreadCallback = Ptr () -> IO ()

-- | Create a C function pointer from a Haskell function
foreign import ccall "wrapper"
  makeMainThreadCallback :: MainThreadCallback -> IO (FunPtr MainThreadCallback)

-- | Call a function on the main thread during event processing.
foreign import ccall safe "SDL_RunOnMainThread" sdlRunOnMainThread_ :: FunPtr MainThreadCallback -> Ptr () -> Bool -> IO Bool

-- | Call a function on the main thread during event processing.
sdlRunOnMainThread :: MainThreadCallback -> Ptr () -> Bool -> IO Bool
sdlRunOnMainThread callback userdata waitComplete = do
  callbackPtr <- makeMainThreadCallback callback
  sdlRunOnMainThread_ callbackPtr userdata waitComplete
  -- Note: In a real implementation, we would need to free the callback pointer
  -- This is a simplification

-- | Specify basic metadata about your app.
foreign import ccall unsafe "SDL_SetAppMetadata" sdlSetAppMetadata_ :: CString -> CString -> CString -> IO Bool

-- | Set basic app metadata
sdlSetAppMetadata :: String -> String -> String -> IO Bool
sdlSetAppMetadata appname appversion appidentifier = 
  withCString appname $ \appnamePtr ->
  withCString appversion $ \appversionPtr ->
  withCString appidentifier $ \appidentifierPtr ->
    sdlSetAppMetadata_ appnamePtr appversionPtr appidentifierPtr

-- | Specify metadata about your app through a set of properties.
foreign import ccall unsafe "SDL_SetAppMetadataProperty" sdlSetAppMetadataProperty_ :: CString -> CString -> IO Bool

-- | Set an app metadata property
sdlSetAppMetadataProperty :: String -> String -> IO Bool
sdlSetAppMetadataProperty name value = 
  withCString name $ \namePtr ->
  withCString value $ \valuePtr ->
    sdlSetAppMetadataProperty_ namePtr valuePtr

-- | Get metadata about your app.
foreign import ccall unsafe "SDL_GetAppMetadataProperty" sdlGetAppMetadataProperty_ :: CString -> IO CString

-- | Get an app metadata property
sdlGetAppMetadataProperty :: String -> IO (Maybe String)
sdlGetAppMetadataProperty name =
  withCString name $ \namePtr -> do
    valuePtr <- sdlGetAppMetadataProperty_ namePtr
    if valuePtr == nullPtr
      then return Nothing
      else do
        value <- peekCString valuePtr
        return (Just value)

-- App metadata property constants
propAppMetadataName :: String
propAppMetadataName = "SDL.app.metadata.name"

propAppMetadataVersion :: String
propAppMetadataVersion = "SDL.app.metadata.version"

propAppMetadataIdentifier :: String
propAppMetadataIdentifier = "SDL.app.metadata.identifier"

propAppMetadataCreator :: String
propAppMetadataCreator = "SDL.app.metadata.creator"

propAppMetadataCopyright :: String
propAppMetadataCopyright = "SDL.app.metadata.copyright"

propAppMetadataUrl :: String
propAppMetadataUrl = "SDL.app.metadata.url"

propAppMetadataType :: String
propAppMetadataType = "SDL.app.metadata.type"
