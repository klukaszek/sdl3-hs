{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds#-}

module SDL3.Raw.Init
  ( SDLInitFlags
  , pattern SDL_INIT_AUDIO
  , pattern SDL_INIT_VIDEO
  , pattern SDL_INIT_JOYSTICK
  , pattern SDL_INIT_HAPTIC
  , pattern SDL_INIT_GAMEPAD
  , pattern SDL_INIT_EVENTS
  , pattern SDL_INIT_SENSOR
  , pattern SDL_INIT_CAMERA
  , sdlInit
  , sdlInitSubSystem
  , sdlQuitSubSystem
  , sdlWasInit
  , sdlQuit
  , sdlIsMainThread
  , MainThreadCallback
  , makeMainThreadCallback
  , sdlRunOnMainThread
  , SDLAppResult(..)
  , SDLAppInitFunc
  , SDLAppIterateFunc
  , SDLAppEventFunc
  , SDLAppQuitFunc
  , makeAppInitCallback
  , wrapAppInitFunc
  , makeAppIterateCallback
  , wrapAppIterateFunc
  , makeAppEventCallback
  , wrapAppEventFunc
  , makeAppQuitCallback
  , wrapAppQuitFunc
  , sdlSetAppMetadata
  , sdlSetAppMetadataProperty
  , sdlGetAppMetadataProperty
  , propAppMetadataName
  , propAppMetadataVersion
  , propAppMetadataIdentifier
  , propAppMetadataCreator
  , propAppMetadataCopyright
  , propAppMetadataUrl
  , propAppMetadataType
  ) where

#include <SDL3/SDL_init.h>

import Foreign
import Foreign.C

type SDLInitFlags = Word32

pattern SDL_INIT_AUDIO = (#const SDL_INIT_AUDIO) :: SDLInitFlags
pattern SDL_INIT_VIDEO = (#const SDL_INIT_VIDEO) :: SDLInitFlags
pattern SDL_INIT_JOYSTICK = (#const SDL_INIT_JOYSTICK) :: SDLInitFlags
pattern SDL_INIT_HAPTIC = (#const SDL_INIT_HAPTIC) :: SDLInitFlags
pattern SDL_INIT_GAMEPAD = (#const SDL_INIT_GAMEPAD) :: SDLInitFlags
pattern SDL_INIT_EVENTS = (#const SDL_INIT_EVENTS) :: SDLInitFlags
pattern SDL_INIT_SENSOR = (#const SDL_INIT_SENSOR) :: SDLInitFlags
pattern SDL_INIT_CAMERA = (#const SDL_INIT_CAMERA) :: SDLInitFlags

data SDLAppResult =
    SDL_APP_CONTINUE
  | SDL_APP_SUCCESS
  | SDL_APP_FAILURE
  deriving (Eq, Show, Enum)

type SDLAppInitFunc = Ptr (Ptr ()) -> CInt -> Ptr (Ptr CChar) -> IO SDLAppResult

foreign import ccall "wrapper"
  makeAppInitCallback :: (Ptr (Ptr ()) -> CInt -> Ptr (Ptr CChar) -> IO CInt) -> IO (FunPtr (Ptr (Ptr ()) -> CInt -> Ptr (Ptr CChar) -> IO CInt))

wrapAppInitFunc :: SDLAppInitFunc -> (Ptr (Ptr ()) -> CInt -> Ptr (Ptr CChar) -> IO CInt)
wrapAppInitFunc f appstate argc argv = fromIntegral . fromEnum <$> f appstate argc argv

type SDLAppIterateFunc = Ptr () -> IO SDLAppResult

foreign import ccall "wrapper"
  makeAppIterateCallback :: (Ptr () -> IO CInt) -> IO (FunPtr (Ptr () -> IO CInt))

wrapAppIterateFunc :: SDLAppIterateFunc -> (Ptr () -> IO CInt)
wrapAppIterateFunc f appstate = fromIntegral . fromEnum <$> f appstate

type SDLAppEventFunc = Ptr () -> Ptr () -> IO SDLAppResult

foreign import ccall "wrapper"
  makeAppEventCallback :: (Ptr () -> Ptr () -> IO CInt) -> IO (FunPtr (Ptr () -> Ptr () -> IO CInt))

wrapAppEventFunc :: SDLAppEventFunc -> (Ptr () -> Ptr () -> IO CInt)
wrapAppEventFunc f appstate event = fromIntegral . fromEnum <$> f appstate event

type SDLAppQuitFunc = Ptr () -> SDLAppResult -> IO ()

foreign import ccall "wrapper"
  makeAppQuitCallback :: (Ptr () -> CInt -> IO ()) -> IO (FunPtr (Ptr () -> CInt -> IO ()))

wrapAppQuitFunc :: SDLAppQuitFunc -> (Ptr () -> CInt -> IO ())
wrapAppQuitFunc f appstate result = f appstate (toEnum (fromIntegral result))

foreign import ccall unsafe "SDL_Init" sdlInit :: Word32 -> IO Bool
foreign import ccall unsafe "SDL_InitSubSystem" sdlInitSubSystem :: Word32 -> IO Bool
foreign import ccall unsafe "SDL_QuitSubSystem" sdlQuitSubSystem :: Word32 -> IO ()
foreign import ccall unsafe "SDL_WasInit" sdlWasInit :: Word32 -> IO Word32
foreign import ccall unsafe "SDL_Quit" sdlQuit :: IO ()
foreign import ccall unsafe "SDL_IsMainThread" sdlIsMainThread :: IO Bool

type MainThreadCallback = Ptr () -> IO ()

foreign import ccall "wrapper"
  makeMainThreadCallback :: MainThreadCallback -> IO (FunPtr MainThreadCallback)

foreign import ccall safe "SDL_RunOnMainThread"
  sdlRunOnMainThread :: FunPtr MainThreadCallback -> Ptr () -> Bool -> IO Bool

foreign import ccall unsafe "SDL_SetAppMetadata"
  sdlSetAppMetadata :: CString -> CString -> CString -> IO Bool

foreign import ccall unsafe "SDL_SetAppMetadataProperty"
  sdlSetAppMetadataProperty :: CString -> CString -> IO Bool

foreign import ccall unsafe "SDL_GetAppMetadataProperty"
  sdlGetAppMetadataProperty :: CString -> IO CString

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
