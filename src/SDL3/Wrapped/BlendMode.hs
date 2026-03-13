module SDL3.Wrapped.BlendMode
  ( module SDL3.Raw.BlendMode
  , sdlComposeCustomBlendMode
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import SDL3.Raw.BlendMode hiding (sdlComposeCustomBlendMode)
import qualified SDL3.Raw.BlendMode as Raw

sdlComposeCustomBlendMode
  :: MonadIO m
  => SDLBlendFactor
  -> SDLBlendFactor
  -> SDLBlendOperation
  -> SDLBlendFactor
  -> SDLBlendFactor
  -> SDLBlendOperation
  -> m SDLBlendMode
sdlComposeCustomBlendMode srcColorFactor dstColorFactor colorOperation srcAlphaFactor dstAlphaFactor alphaOperation =
  liftIO $
    Raw.sdlComposeCustomBlendMode
      srcColorFactor
      dstColorFactor
      colorOperation
      srcAlphaFactor
      dstAlphaFactor
      alphaOperation
