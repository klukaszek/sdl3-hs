module SDL3.Wrapped.Error
  ( sdlSetError
  , sdlSetErrorV
  , sdlOutOfMemory
  , sdlGetError
  , sdlClearError
  , sdlUnsupported
  , sdlInvalidParamError
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.String (peekCString, withCString)
import Foreign.C.Types (CBool(..))
import qualified SDL3.Raw.Error as Raw

cboolToBool :: CBool -> Bool
cboolToBool (CBool 0) = False
cboolToBool _ = True

sdlSetError :: MonadIO m => String -> m Bool
sdlSetError message =
  liftIO $ withCString message (fmap cboolToBool . Raw.sdlSetError)

sdlSetErrorV :: MonadIO m => String -> m Bool
sdlSetErrorV message =
  liftIO $ withCString message (fmap cboolToBool . Raw.sdlSetErrorV)

sdlOutOfMemory :: MonadIO m => m Bool
sdlOutOfMemory = liftIO $ cboolToBool <$> Raw.sdlOutOfMemory

sdlGetError :: MonadIO m => m String
sdlGetError = liftIO $ Raw.sdlGetError >>= peekCString

sdlClearError :: MonadIO m => m Bool
sdlClearError = liftIO $ cboolToBool <$> Raw.sdlClearError

sdlUnsupported :: MonadIO m => m Bool
sdlUnsupported = sdlSetError "That operation is not supported"

sdlInvalidParamError :: MonadIO m => String -> m Bool
sdlInvalidParamError param = sdlSetError ("Parameter '" ++ param ++ "' is invalid")
