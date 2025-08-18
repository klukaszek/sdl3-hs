-- SDL/Metal.hsc
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- Keep if SDLMetalView needs it later

{-|
Module      : SDL.Metal
Description : SDL Metal layer and view creation functions
Copyright   : Kyle Lukaszek, 2025
License     : BSD3

This module provides bindings to the SDL3 Metal API, allowing Haskell applications
to create Metal layers and views on SDL windows for Apple platforms (macOS, iOS, tvOS).
These functions are useful for specific OS-level integration tasks with Metal.

Note: On macOS, SDL does not automatically associate a MTLDevice with the CAMetalLayer;
this must be handled in user code.
-}

module SDL.Metal
  ( -- * Types
    SDLMetalView(..)

    -- * Metal Support Functions
  , sdlMetalCreateView
  , sdlMetalDestroyView
  , sdlMetalGetLayer
  ) where

import Foreign hiding (void) -- Avoid hiding void from Prelude if Ptr () is okay
import SDL.Video (SDLWindow(..))

#include <SDL3/SDL_metal.h>

-- | A handle to a CAMetalLayer-backed NSView (macOS) or UIView (iOS/tvOS).
-- Using Ptr () as the C API returns void*.
newtype SDLMetalView = SDLMetalView (Ptr ())
  deriving (Show, Eq)

-- | Raw C binding for SDL_Metal_CreateView
foreign import ccall unsafe "SDL_Metal_CreateView"
  sdlMetalCreateViewRaw :: Ptr SDLWindow -> IO (Ptr ()) -- Use Ptr SDL_Window

-- | Create a CAMetalLayer-backed NSView/UIView and attach it to the specified window.
-- Returns Nothing if creation fails.
sdlMetalCreateView :: SDLWindow -> IO (Maybe SDLMetalView)
sdlMetalCreateView (SDLWindow windowPtr) = do -- Unpack SDLWindow
  view <- sdlMetalCreateViewRaw windowPtr      -- Pass unwrapped Ptr SDL_Window
  if view == nullPtr
    then return Nothing
    else return $ Just $ SDLMetalView view

-- | Raw C binding for SDL_Metal_DestroyView
foreign import ccall unsafe "SDL_Metal_DestroyView"
  sdlMetalDestroyViewRaw :: Ptr () -> IO ()

-- | Destroy an existing SDL_MetalView object.
-- Should be called before destroying the associated window if created after window creation.
sdlMetalDestroyView :: SDLMetalView -> IO ()
sdlMetalDestroyView (SDLMetalView view) = sdlMetalDestroyViewRaw view

-- | Raw C binding for SDL_Metal_GetLayer
foreign import ccall unsafe "SDL_Metal_GetLayer"
  sdlMetalGetLayerRaw :: Ptr () -> IO (Ptr ())

-- | Get a pointer to the backing CAMetalLayer for the given view.
-- Returns Nothing if the layer cannot be retrieved.
sdlMetalGetLayer :: SDLMetalView -> IO (Maybe (Ptr ()))
sdlMetalGetLayer (SDLMetalView view) = do
  layer <- sdlMetalGetLayerRaw view
  if layer == nullPtr
    then return Nothing
    else return $ Just layer
