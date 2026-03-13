module SDL3.Wrapped.LoadSO
  ( SDLSharedObject
  , SDLFunctionPointer
  , sdlLoadObject
  , sdlLoadFunction
  , sdlUnloadObject
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.String (withCString)
import Foreign.Ptr (Ptr, nullFunPtr, nullPtr)
import SDL3.Raw.LoadSO (SDLFunctionPointer, SDLSharedObject)
import qualified SDL3.Raw.LoadSO as Raw

sdlLoadObject :: MonadIO m => String -> m (Maybe (Ptr SDLSharedObject))
sdlLoadObject sofile = liftIO $
  withCString sofile $ \cstr -> do
    handle <- Raw.sdlLoadObjectRaw cstr
    return $
      if handle == nullPtr
        then Nothing
        else Just handle

sdlLoadFunction :: MonadIO m => Ptr SDLSharedObject -> String -> m (Maybe SDLFunctionPointer)
sdlLoadFunction handle name = liftIO $
  withCString name $ \cstr -> do
    ptr <- Raw.sdlLoadFunctionRaw handle cstr
    return $
      if ptr == nullFunPtr
        then Nothing
        else Just ptr

sdlUnloadObject :: MonadIO m => Ptr SDLSharedObject -> m ()
sdlUnloadObject handle = liftIO $ Raw.sdlUnloadObjectRaw handle
