{-# LANGUAGE CPP #-}

module SDL3.Wrapped.Assert
  ( SDLAssertState(..)
  , SDLAssertData(..)
  , SDLAssertDataFFI
  , SDLAssertionHandler
  , sdlReportAssertion
  , sdlSetAssertionHandler
  , sdlGetDefaultAssertionHandler
  , sdlGetAssertionHandler
  , sdlGetAssertionReport
  , sdlResetAssertionReport
  , sdlAssert
  , sdlAssertRelease
  , sdlAssertParanoid
  , sdlAssertAlways
  , sdlTriggerBreakpoint
  , sdlAssertLevel
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import SDL3.Raw.Assert
  ( SDLAssertData(..)
  , SDLAssertDataFFI
  , SDLAssertState(..)
  , SDLAssertionHandler
  , sdlAssertLevel
  )
import qualified SDL3.Raw.Assert as Raw

sdlReportAssertion :: MonadIO m => Ptr SDLAssertDataFFI -> CString -> CString -> CInt -> m CInt
sdlReportAssertion a b c d = liftIO $ Raw.sdlReportAssertionRaw a b c d

sdlSetAssertionHandler :: MonadIO m => FunPtr SDLAssertionHandler -> Ptr () -> m ()
sdlSetAssertionHandler a b = liftIO $ Raw.sdlSetAssertionHandlerRaw a b

sdlGetDefaultAssertionHandler :: MonadIO m => m (FunPtr SDLAssertionHandler)
sdlGetDefaultAssertionHandler = liftIO Raw.sdlGetDefaultAssertionHandlerRaw

sdlGetAssertionHandler :: MonadIO m => Ptr (Ptr ()) -> m (FunPtr SDLAssertionHandler)
sdlGetAssertionHandler ptr = liftIO $ Raw.sdlGetAssertionHandlerRaw ptr

sdlGetAssertionReport :: MonadIO m => m (Ptr SDLAssertDataFFI)
sdlGetAssertionReport = liftIO Raw.sdlGetAssertionReportRaw

sdlResetAssertionReport :: MonadIO m => m ()
sdlResetAssertionReport = liftIO Raw.sdlResetAssertionReportRaw

sdlTriggerBreakpoint :: MonadIO m => m ()
sdlTriggerBreakpoint = liftIO Raw.sdlTriggerBreakpointRaw

sdlAssert :: MonadIO m => Bool -> m ()
#if defined(DEBUG) || (sdlAssertLevel >= 2)
sdlAssert condition = unless condition $ sdlAssertHelper "assertion failed"
#else
sdlAssert _ = return ()
#endif

sdlAssertRelease :: MonadIO m => Bool -> m ()
#if (sdlAssertLevel >= 1)
sdlAssertRelease condition = unless condition $ sdlAssertHelper "release assertion failed"
#else
sdlAssertRelease _ = return ()
#endif

sdlAssertParanoid :: MonadIO m => Bool -> m ()
#if (sdlAssertLevel >= 3)
sdlAssertParanoid condition = unless condition $ sdlAssertHelper "paranoid assertion failed"
#else
sdlAssertParanoid _ = return ()
#endif

sdlAssertAlways :: MonadIO m => Bool -> m ()
sdlAssertAlways cond = unless cond $ sdlAssertHelper "assertion failed"

sdlAssertHelper :: MonadIO m => String -> m ()
sdlAssertHelper msg = do
  liftIO $ putStrLn $ "SDL assertion: " ++ msg
  sdlTriggerBreakpoint
