{-# LANGUAGE ForeignFunctionInterface #-}

module SDL3.Raw.LoadSO
  ( SDLSharedObject
  , SDLFunctionPointer
  , sdlLoadObjectRaw
  , sdlLoadFunctionRaw
  , sdlUnloadObjectRaw
  ) where

import Foreign.C.String (CString)
import Foreign.Ptr (FunPtr, Ptr)

data SDLSharedObject

type SDLFunctionPointer = FunPtr ()

foreign import ccall "SDL_LoadObject"
  sdlLoadObjectRaw :: CString -> IO (Ptr SDLSharedObject)

foreign import ccall "SDL_LoadFunction"
  sdlLoadFunctionRaw :: Ptr SDLSharedObject -> CString -> IO SDLFunctionPointer

foreign import ccall "SDL_UnloadObject"
  sdlUnloadObjectRaw :: Ptr SDLSharedObject -> IO ()
