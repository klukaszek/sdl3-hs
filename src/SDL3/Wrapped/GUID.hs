module SDL3.Wrapped.GUID
  ( SDLGUID(..)
  , sdlGUIDToString
  , sdlStringToGUID
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign
import Foreign.C.String
import SDL3.Raw.GUID (SDLGUID(..))
import qualified SDL3.Raw.GUID as Raw

sdlGUIDToString :: MonadIO m => SDLGUID -> m String
sdlGUIDToString (SDLGUID bytes) = liftIO $ do
  let guidBytes = take 16 $ bytes ++ repeat 0
  withArray guidBytes $ \guidPtr ->
    allocaArray 33 $ \strPtr -> do
      Raw.sdlGUIDToStringRaw guidPtr strPtr 33
      peekCString strPtr

sdlStringToGUID :: MonadIO m => String -> m SDLGUID
sdlStringToGUID str = liftIO $
  allocaArray 16 $ \guidPtr ->
    withCString str $ \strPtr -> do
      Raw.sdlStringToGUIDRaw strPtr guidPtr
      SDLGUID <$> peekArray 16 guidPtr
