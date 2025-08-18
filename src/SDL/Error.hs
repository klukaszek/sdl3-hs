-- |
-- Module      : SDL.Error
-- Description : Simple error message routines for SDL
-- Copyright   : (c) Kyle Lukaszek
-- License     : BSD3
--
-- Simple error message routines for SDL.
--
-- Most apps will interface with these APIs in exactly one function: when almost any SDL function
-- call reports failure, you can get a human-readable string of the problem from 'sdlGetError'.
--
-- These strings are maintained per-thread, and apps are welcome to set their own errors, which is
-- popular when building libraries on top of SDL for other apps to consume. These strings are set
-- by calling 'sdlSetError'.
module SDL.Error
  ( -- * Error Handling Functions
    sdlSetError,
    sdlSetErrorV,
    sdlOutOfMemory,
    sdlGetError,
    sdlClearError,

    -- * Internal Error Macros
    sdlUnsupported,
    sdlInvalidParamError,
  )
where

import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CBool (..))

-- | Set the SDL error message for the current thread.
--
-- Calling this function will replace any previous error message that was set.
--
-- This function always returns False, since SDL frequently uses False to signify a failing result.
--
-- @since 3.2.0
foreign import ccall "SDL_SetError" sdlSetError :: CString -> IO CBool

-- | Set the SDL error message for the current thread.
--
-- Calling this function will replace any previous error message that was set.
--
-- @since 3.2.0
foreign import ccall "SDL_SetErrorV" sdlSetErrorV :: CString -> IO CBool

-- | Set an error indicating that memory allocation failed.
--
-- This function does not do any memory allocation.
--
-- @since 3.2.0
foreign import ccall "SDL_OutOfMemory" sdlOutOfMemory :: IO CBool

-- | Retrieve a message about the last error that occurred on the current thread.
--
-- It is possible for multiple errors to occur before calling 'sdlGetError'.
-- Only the last error is returned.
--
-- The message is only applicable when an SDL function has signaled an error.
-- You must check the return values of SDL function calls to determine when to
-- appropriately call 'sdlGetError'. You should not use the results of
-- 'sdlGetError' to decide if an error has occurred! Sometimes SDL will set
-- an error string even when reporting success.
--
-- SDL will not clear the error string for successful API calls. You must
-- check return values for failure cases before you can assume the error
-- string applies.
--
-- Error strings are set per-thread, so an error set in a different thread
-- will not interfere with the current thread's operation.
--
-- The returned value is a thread-local string which will remain valid until
-- the current thread's error string is changed. The caller should make a copy
-- if the value is needed after the next SDL API call.
--
-- @since 3.2.0
sdlGetError :: IO String
sdlGetError = do
  cstr <- sdlGetErrorRaw
  peekCString cstr

-- | Raw C function for SDL_GetError
foreign import ccall "SDL_GetError" sdlGetErrorRaw :: IO CString

-- | Clear any previous error message for this thread.
--
-- @since 3.2.0
sdlClearError :: IO Bool
sdlClearError = fmap cboolToBool sdlClearErrorRaw
  where
    cboolToBool :: CBool -> Bool
    cboolToBool (CBool 0) = False
    cboolToBool _ = True

-- | Raw C function for SDL_ClearError
foreign import ccall "SDL_ClearError" sdlClearErrorRaw :: IO CBool

-- | A standard error message for unsupported operations.
--
-- This is a convenience function that sets the error message to
-- "That operation is not supported".
--
-- @since 3.2.0
sdlUnsupported :: IO Bool
sdlUnsupported = withCString "That operation is not supported" $ \cstr -> do
  CBool val <- sdlSetError cstr
  return (val /= 0)

-- | A standard error message for invalid parameters.
--
-- This is a convenience function that sets the error message to
-- "Parameter 'param' is invalid", where param is the provided string.
--
-- @since 3.2.0
sdlInvalidParamError :: String -> IO Bool
sdlInvalidParamError param =
  withCString ("Parameter '" ++ param ++ "' is invalid") $ \cstr -> do
    CBool val <- sdlSetError cstr
    return (val /= 0)
