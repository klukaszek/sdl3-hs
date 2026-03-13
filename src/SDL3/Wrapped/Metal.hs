module SDL3.Wrapped.Metal
  ( SDLMetalView(..)
  , sdlMetalCreateView
  , sdlMetalDestroyView
  , sdlMetalGetLayer
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign
import SDL3.Raw.Metal (SDLMetalView(..))
import qualified SDL3.Raw.Metal as Raw
import SDL3.Video (SDLWindow(..))

sdlMetalCreateView :: MonadIO m => SDLWindow -> m (Maybe SDLMetalView)
sdlMetalCreateView (SDLWindow windowPtr) = liftIO $ do
  view <- Raw.sdlMetalCreateViewRaw windowPtr
  if view == nullPtr
    then return Nothing
    else return $ Just $ SDLMetalView view

sdlMetalDestroyView :: MonadIO m => SDLMetalView -> m ()
sdlMetalDestroyView (SDLMetalView view) = liftIO $ Raw.sdlMetalDestroyViewRaw view

sdlMetalGetLayer :: MonadIO m => SDLMetalView -> m (Maybe (Ptr ()))
sdlMetalGetLayer (SDLMetalView view) = liftIO $ do
  layer <- Raw.sdlMetalGetLayerRaw view
  if layer == nullPtr
    then return Nothing
    else return $ Just layer
