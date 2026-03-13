{-# LANGUAGE CPP #-}

module SDL3.Raw.Assert
  ( SDLAssertState(..)
  , SDLAssertData(..)
  , SDLAssertDataFFI
  , SDLAssertionHandler
  , sdlReportAssertionRaw
  , sdlSetAssertionHandlerRaw
  , sdlGetDefaultAssertionHandlerRaw
  , sdlGetAssertionHandlerRaw
  , sdlGetAssertionReportRaw
  , sdlResetAssertionReportRaw
  , sdlTriggerBreakpointRaw
  , sdlAssertLevel
  ) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

data SDLAssertState
  = SDL_ASSERTION_RETRY
  | SDL_ASSERTION_BREAK
  | SDL_ASSERTION_ABORT
  | SDL_ASSERTION_IGNORE
  | SDL_ASSERTION_ALWAYS_IGNORE
  deriving (Eq, Show, Enum)

data SDLAssertData = SDLAssertData
  { alwaysIgnore :: Bool
  , triggerCount :: Word
  , condition :: String
  , filename :: String
  , linenum :: Int
  , function :: String
  , next :: Maybe SDLAssertData
  }

data SDLAssertDataFFI

type SDLAssertionHandler = Ptr SDLAssertDataFFI -> Ptr () -> IO CInt

sdlAssertLevel :: Int
#ifdef DEBUG
sdlAssertLevel = 2
#else
sdlAssertLevel = 1
#endif

foreign import ccall unsafe "SDL_ReportAssertion"
  sdlReportAssertionRaw :: Ptr SDLAssertDataFFI -> CString -> CString -> CInt -> IO CInt

foreign import ccall unsafe "SDL_SetAssertionHandler"
  sdlSetAssertionHandlerRaw :: FunPtr SDLAssertionHandler -> Ptr () -> IO ()

foreign import ccall unsafe "SDL_GetDefaultAssertionHandler"
  sdlGetDefaultAssertionHandlerRaw :: IO (FunPtr SDLAssertionHandler)

foreign import ccall unsafe "SDL_GetAssertionHandler"
  sdlGetAssertionHandlerRaw :: Ptr (Ptr ()) -> IO (FunPtr SDLAssertionHandler)

foreign import ccall unsafe "SDL_GetAssertionReport"
  sdlGetAssertionReportRaw :: IO (Ptr SDLAssertDataFFI)

foreign import ccall unsafe "SDL_ResetAssertionReport"
  sdlResetAssertionReportRaw :: IO ()

foreign import ccall unsafe "wrapper_SDL_TriggerBreakpoint"
  sdlTriggerBreakpointRaw :: IO ()
