module SDL3.Wrapped.Misc
  ( sdlOpenURL
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.String (withCString)
import qualified SDL3.Raw.Misc as Raw

sdlOpenURL :: MonadIO m => String -> m Bool
sdlOpenURL url = liftIO $ withCString url Raw.sdlOpenURLRaw
