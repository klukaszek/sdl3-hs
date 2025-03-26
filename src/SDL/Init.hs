{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}

-- SDL/Init.hs
{-|
Module      : SDL.Init
Description : Initialization and shutdown functions for SDL3.
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3
Maintainer  : kylelukaszek@gmail.com

This module provides Haskell bindings to the SDL stdinc functionality.
-}

module SDL.Init
  ( 
  -- * Initialization
  -- ** Initialization Flags
    InitFlag
  , pattern InitAudio
  , pattern InitVideo
  , pattern InitJoystick
  , pattern InitHaptic
  , pattern InitGamepad
  , pattern InitEvents
  , pattern InitSensor
  , pattern InitCamera
  
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
  , AppResult(..)
  , AppInitFunc
  , AppIterateFunc
  , AppEventFunc
  , AppQuitFunc
  
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
type InitFlag = Word32

-- | Use the audio subsystem
pattern InitAudio :: InitFlag
pattern InitAudio = 0x00000010

-- | Use the video subsystem
pattern InitVideo :: InitFlag
pattern InitVideo = 0x00000020

-- | Use the joystick subsystem
pattern InitJoystick :: InitFlag
pattern InitJoystick = 0x00000200

-- | Use the haptic (force feedback) subsystem
pattern InitHaptic :: InitFlag
pattern InitHaptic = 0x00001000

-- | Use the gamepad subsystem
pattern InitGamepad :: InitFlag
pattern InitGamepad = 0x00002000

-- | Use the events subsystem
pattern InitEvents :: InitFlag
pattern InitEvents = 0x00004000

-- | Use the sensor subsystem
pattern InitSensor :: InitFlag
pattern InitSensor = 0x00008000

-- | Use the camera subsystem
pattern InitCamera :: InitFlag
pattern InitCamera = 0x00010000

-- | Result for app callbacks
data AppResult = 
    AppContinue    -- ^ Continue app execution
  | AppSuccess     -- ^ Terminate with success
  | AppFailure     -- ^ Terminate with error
  deriving (Eq, Show)

-- | Convert Haskell AppResult to C value
appResultToC :: AppResult -> CInt
appResultToC AppContinue = 0
appResultToC AppSuccess = 1
appResultToC AppFailure = 2

-- | Convert C AppResult to Haskell value
appResultFromC :: CInt -> AppResult
appResultFromC 0 = AppContinue
appResultFromC 1 = AppSuccess
appResultFromC 2 = AppFailure
appResultFromC _ = AppFailure -- Default for unknown values

-- | Type for app initialization callback
type AppInitFunc = Ptr (Ptr ()) -> CInt -> Ptr (Ptr CChar) -> IO AppResult

-- | Create a C function pointer from a Haskell AppInitFunc
foreign import ccall "wrapper"
  makeAppInitCallback :: (Ptr (Ptr ()) -> CInt -> Ptr (Ptr CChar) -> IO CInt) -> IO (FunPtr (Ptr (Ptr ()) -> CInt -> Ptr (Ptr CChar) -> IO CInt))

-- | Wrap a Haskell AppInitFunc to convert the result
wrapAppInitFunc :: AppInitFunc -> (Ptr (Ptr ()) -> CInt -> Ptr (Ptr CChar) -> IO CInt)
wrapAppInitFunc f appstate argc argv = do
  result <- f appstate argc argv
  return (appResultToC result)

-- | Type for app iteration callback
type AppIterateFunc = Ptr () -> IO AppResult

-- | Create a C function pointer from a Haskell AppIterateFunc
foreign import ccall "wrapper"
  makeAppIterateCallback :: (Ptr () -> IO CInt) -> IO (FunPtr (Ptr () -> IO CInt))

-- | Wrap a Haskell AppIterateFunc to convert the result
wrapAppIterateFunc :: AppIterateFunc -> (Ptr () -> IO CInt)
wrapAppIterateFunc f appstate = do
  result <- f appstate
  return (appResultToC result)

-- | Type for app event callback
type AppEventFunc = Ptr () -> Ptr () -> IO AppResult  -- Second Ptr () should be Ptr SDL_Event

-- | Create a C function pointer from a Haskell AppEventFunc
foreign import ccall "wrapper"
  makeAppEventCallback :: (Ptr () -> Ptr () -> IO CInt) -> IO (FunPtr (Ptr () -> Ptr () -> IO CInt))

-- | Wrap a Haskell AppEventFunc to convert the result
wrapAppEventFunc :: AppEventFunc -> (Ptr () -> Ptr () -> IO CInt)
wrapAppEventFunc f appstate event = do
  result <- f appstate event
  return (appResultToC result)

-- | Type for app quit callback
type AppQuitFunc = Ptr () -> AppResult -> IO ()

-- | Create a C function pointer from a Haskell AppQuitFunc
foreign import ccall "wrapper"
  makeAppQuitCallback :: (Ptr () -> CInt -> IO ()) -> IO (FunPtr (Ptr () -> CInt -> IO ()))

-- | Wrap a Haskell AppQuitFunc to convert the input argument
wrapAppQuitFunc :: AppQuitFunc -> (Ptr () -> CInt -> IO ())
wrapAppQuitFunc f appstate result = f appstate (appResultFromC result)

-- | Initialize the SDL library.
foreign import ccall unsafe "SDL_Init" sdlInit_ :: Word32 -> IO Bool

-- | Initialize the SDL library.
sdlInit :: [InitFlag] -> IO Bool
sdlInit flags = sdlInit_ (foldr (.|.) 0 flags)

-- | Initialize specific SDL subsystems.
foreign import ccall unsafe "SDL_InitSubSystem" sdlInitSubSystem_ :: Word32 -> IO Bool

-- | Initialize specific SDL subsystems.
sdlInitSubSystem :: [InitFlag] -> IO Bool
sdlInitSubSystem flags = sdlInitSubSystem_ (foldr (.|.) 0 flags)

-- | Shut down specific SDL subsystems.
foreign import ccall unsafe "SDL_QuitSubSystem" sdlQuitSubSystem_ :: Word32 -> IO ()

-- | Shut down specific SDL subsystems.
sdlQuitSubSystem :: [InitFlag] -> IO ()
sdlQuitSubSystem flags = sdlQuitSubSystem_ (foldr (.|.) 0 flags)

-- | Get a mask of the specified subsystems which are currently initialized.
foreign import ccall unsafe "SDL_WasInit" sdlWasInit_ :: Word32 -> IO Word32

-- | Get a mask of the specified subsystems which are currently initialized.
sdlWasInit :: [InitFlag] -> IO [InitFlag]
sdlWasInit flags = do
  result <- sdlWasInit_ (foldr (.|.) 0 flags)
  -- Create list of initialized flags
  return $ filter (\flag -> (result .&. flag) /= 0) 
    [InitAudio, InitVideo, InitJoystick, InitHaptic, InitGamepad, InitEvents, InitSensor, InitCamera]

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
  result <- sdlRunOnMainThread_ callbackPtr userdata waitComplete
  -- Note: In a real implementation, we would need to free the callback pointer
  -- This is a simplification
  return result

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
