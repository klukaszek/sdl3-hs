{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : SDL.LoadSO
Description : SDL shared object loading utilities
Copyright   : (c) Kyle Lukaszek, 2025
License     : BS3

System-dependent library loading routines for working with shared objects
(DLLs on Windows, shared libraries on Linux, etc.).
-}

module SDL.LoadSO
  ( -- * Types
    SDLSharedObject
  , SDLFunctionPointer

    -- * Functions
  , sdlLoadObject
  , sdlLoadFunction
  , sdlUnloadObject
  ) where

#include <SDL3/SDL_loadso.h>

import Foreign.Ptr (Ptr, nullPtr, FunPtr, nullFunPtr)
import Foreign.C.String (CString, withCString)

-- | Opaque type representing a loaded shared object
data SDLSharedObject

-- | Type alias for function pointers loaded from shared objects
type SDLFunctionPointer = FunPtr ()

-- | Dynamically load a shared object
foreign import ccall "SDL_LoadObject"
  sdlLoadObjectRaw :: CString -> IO (Ptr SDLSharedObject)

sdlLoadObject :: String -> IO (Maybe (Ptr SDLSharedObject))
sdlLoadObject sofile = withCString sofile $ \cstr -> do
  handle <- sdlLoadObjectRaw cstr
  return $ if handle == nullPtr
             then Nothing
             else Just handle

-- | Look up a function in a shared object
foreign import ccall "SDL_LoadFunction"
  sdlLoadFunctionRaw :: Ptr SDLSharedObject -> CString -> IO SDLFunctionPointer

sdlLoadFunction :: Ptr SDLSharedObject -> String -> IO (Maybe SDLFunctionPointer)
sdlLoadFunction handle name = withCString name $ \cstr -> do
  ptr <- sdlLoadFunctionRaw handle cstr
  return $ if ptr == nullFunPtr
             then Nothing
             else Just ptr

-- | Unload a shared object
foreign import ccall "SDL_UnloadObject"
  sdlUnloadObject :: Ptr SDLSharedObject -> IO ()
