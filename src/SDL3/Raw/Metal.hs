{-# LANGUAGE ForeignFunctionInterface #-}

module SDL3.Raw.Metal
  ( SDLMetalView(..)
  , sdlMetalCreateViewRaw
  , sdlMetalDestroyViewRaw
  , sdlMetalGetLayerRaw
  ) where

import Foreign
import SDL3.Raw.Video (SDLWindow(..))

newtype SDLMetalView = SDLMetalView (Ptr ())
  deriving (Show, Eq)

foreign import ccall unsafe "SDL_Metal_CreateView"
  sdlMetalCreateViewRaw :: Ptr SDLWindow -> IO (Ptr ())

foreign import ccall unsafe "SDL_Metal_DestroyView"
  sdlMetalDestroyViewRaw :: Ptr () -> IO ()

foreign import ccall unsafe "SDL_Metal_GetLayer"
  sdlMetalGetLayerRaw :: Ptr () -> IO (Ptr ())
