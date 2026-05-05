module SDL3.Raw.Clipboard
  ( SDLClipboardDataCallback
  , SDLClipboardCleanupCallback
  , sdlSetClipboardText
  , sdlGetClipboardText
  , sdlHasClipboardText
  , sdlSetPrimarySelectionText
  , sdlGetPrimarySelectionText
  , sdlHasPrimarySelectionText
  , sdlSetClipboardData
  , sdlClearClipboardData
  , sdlGetClipboardData
  , sdlHasClipboardData
  , sdlGetClipboardMimeTypes
  , makeCallback
  , makeCleanup
  ) where

import Foreign.C.String (CString)
import Foreign.C.Types
import Foreign.Ptr

type SDLClipboardDataCallback =
  Ptr () ->
  CString ->
  Ptr CSize ->
  IO (Ptr ())

type SDLClipboardCleanupCallback =
  Ptr () ->
  IO ()

foreign import ccall "SDL_SetClipboardText"
  sdlSetClipboardText :: CString -> IO CBool

foreign import ccall "SDL_GetClipboardText"
  sdlGetClipboardText :: IO CString

foreign import ccall "SDL_HasClipboardText"
  sdlHasClipboardText :: IO CBool

foreign import ccall "SDL_SetPrimarySelectionText"
  sdlSetPrimarySelectionText :: CString -> IO CBool

foreign import ccall "SDL_GetPrimarySelectionText"
  sdlGetPrimarySelectionText :: IO CString

foreign import ccall "SDL_HasPrimarySelectionText"
  sdlHasPrimarySelectionText :: IO CBool

foreign import ccall "SDL_SetClipboardData"
  sdlSetClipboardData ::
    FunPtr SDLClipboardDataCallback ->
    FunPtr SDLClipboardCleanupCallback ->
    Ptr () ->
    Ptr CString ->
    CSize ->
    IO CBool

foreign import ccall "SDL_ClearClipboardData"
  sdlClearClipboardData :: IO CBool

foreign import ccall "SDL_GetClipboardData"
  sdlGetClipboardData ::
    CString ->
    Ptr CSize ->
    IO (Ptr ())

foreign import ccall "SDL_HasClipboardData"
  sdlHasClipboardData :: CString -> IO CBool

foreign import ccall "SDL_GetClipboardMimeTypes"
  sdlGetClipboardMimeTypes :: Ptr CSize -> IO (Ptr CString)

foreign import ccall "wrapper"
  makeCallback :: SDLClipboardDataCallback -> IO (FunPtr SDLClipboardDataCallback)

foreign import ccall "wrapper"
  makeCleanup :: SDLClipboardCleanupCallback -> IO (FunPtr SDLClipboardCleanupCallback)
