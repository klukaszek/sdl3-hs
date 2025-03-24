{-|
Module      : SDL.Assert
Description : Bindings to SDL assertion functionality
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3
Maintainer  : your.email@example.com

This module provides Haskell bindings to the SDL assertion functionality.
SDL assertions operate like your usual assert macro, but with additional features.
-}

module SDL.Assert
  (
  -- * Types
    SDL_AssertState(..)
  , SDL_AssertData(..)
  , SDL_AssertionHandler
  
  -- * Functions
  , sdlReportAssertion
  , sdlSetAssertionHandler
  , sdlGetDefaultAssertionHandler
  , sdlGetAssertionHandler
  , sdlGetAssertionReport
  , sdlResetAssertionReport
  
  -- * Assertion macros (as functions in Haskell)
  , sdlAssert
  , sdlAssertRelease
  , sdlAssertParanoid
  , sdlAssertAlways
  , sdlTriggerBreakpoint
  
  -- * Constants
  , sdlAssertLevel
  ) where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Control.Monad
import SDL.Stdinc (SDLBool)

-- | Possible outcomes from a triggered assertion
data SDL_AssertState = 
    SDL_ASSERTION_RETRY         -- ^ Retry the assert immediately
  | SDL_ASSERTION_BREAK         -- ^ Make the debugger trigger a breakpoint
  | SDL_ASSERTION_ABORT         -- ^ Terminate the program
  | SDL_ASSERTION_IGNORE        -- ^ Ignore the assert
  | SDL_ASSERTION_ALWAYS_IGNORE -- ^ Ignore the assert from now on
  deriving (Eq, Show, Enum)

-- | Information about an assertion failure
data SDL_AssertData = SDL_AssertData {
    alwaysIgnore  :: Bool,             -- ^ true if app should always continue when assertion is triggered
    triggerCount  :: Word,             -- ^ Number of times this assertion has been triggered
    condition     :: String,           -- ^ A string of this assert's test code
    filename      :: String,           -- ^ The source file where this assert lives
    linenum       :: Int,              -- ^ The line in `filename` where this assert lives
    function      :: String,           -- ^ The name of the function where this assert lives
    next          :: Maybe SDL_AssertData -- ^ next item in the linked list
}

-- | Foreign representation of the SDL_AssertData structure
data SDL_AssertData_FFI

-- | Callback type for custom assertion handlers
type SDL_AssertionHandler = Ptr SDL_AssertData_FFI -> Ptr () -> IO CInt

-- | The level of assertion aggressiveness
sdlAssertLevel :: Int
#ifdef DEBUG
sdlAssertLevel = 2  -- Debug settings
#else
sdlAssertLevel = 1  -- Release settings
#endif

-- | Report an assertion failure
foreign import ccall unsafe "SDL_ReportAssertion"
  sdlReportAssertion :: Ptr SDL_AssertData_FFI -> CString -> CString -> CInt -> IO CInt

-- | Set an application-defined assertion handler
foreign import ccall unsafe "SDL_SetAssertionHandler"
  sdlSetAssertionHandler :: FunPtr SDL_AssertionHandler -> Ptr () -> IO ()

-- | Get the default assertion handler
foreign import ccall unsafe "SDL_GetDefaultAssertionHandler"
  sdlGetDefaultAssertionHandler :: IO (FunPtr SDL_AssertionHandler)

-- | Get the current assertion handler
foreign import ccall unsafe "SDL_GetAssertionHandler"
  sdlGetAssertionHandler :: Ptr (Ptr ()) -> IO (FunPtr SDL_AssertionHandler)

-- | Get a list of all assertion failures
foreign import ccall unsafe "SDL_GetAssertionReport"
  sdlGetAssertionReport :: IO (Ptr SDL_AssertData_FFI)

-- | Clear the list of all assertion failures
foreign import ccall unsafe "SDL_ResetAssertionReport"
  sdlResetAssertionReport :: IO ()

-- | Attempt to tell an attached debugger to pause
foreign import ccall unsafe "SDL_TriggerBreakpoint"
  sdlTriggerBreakpoint :: IO ()

-- | Implementation for SDL_assert as a Haskell function
--
-- The original C macro uses compile-time magic to enable/disable assertions,
-- but in Haskell we need to use runtime checking.
sdlAssert :: Bool -> IO ()
#if defined(DEBUG) || (sdlAssertLevel >= 2)
sdlAssert condition = unless condition $ sdlAssertHelper "assertion failed"
#else
sdlAssert _ = return ()
#endif

-- | Implementation for SDL_assert_release as a Haskell function
sdlAssertRelease :: Bool -> IO ()
#if (sdlAssertLevel >= 1)
sdlAssertRelease condition = unless condition $ sdlAssertHelper "release assertion failed"
#else
sdlAssertRelease _ = return ()
#endif

-- | Implementation for SDL_assert_paranoid as a Haskell function
sdlAssertParanoid :: Bool -> IO ()
#if (sdlAssertLevel >= 3)
sdlAssertParanoid condition = unless condition $ sdlAssertHelper "paranoid assertion failed"
#else
sdlAssertParanoid _ = return ()
#endif

-- | Implementation for SDL_assert_always as a Haskell function
sdlAssertAlways :: Bool -> IO ()
sdlAssertAlways condition = unless condition $ sdlAssertHelper "assertion failed"

-- | Helper function for assertion implementations
sdlAssertHelper :: String -> IO ()
sdlAssertHelper msg = do
  putStrLn $ "SDL assertion: " ++ msg
  sdlTriggerBreakpoint
