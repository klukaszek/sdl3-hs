{-# LANGUAGE ForeignFunctionInterface #-}

{-|
Module      : SDL.Guid
Description : SDL GUID functions for working with globally unique identifiers
Copyright   : Kyle Lukaszek, 2025
License     : BSD3

This module provides bindings to the SDL3 GUID functions, allowing Haskell applications
to work with 128-bit globally unique identifiers (GUIDs) used to identify input devices
across runs of SDL programs on the same platform.

The GUID functionality allows conversion between binary GUID structures and their
ASCII string representations.
-}

module SDL.Guid
  ( -- * Types
    SDLGUID(..)
    
    -- * GUID Operations
  , sdlGUIDToString
  , sdlStringToGUID
  ) where

import System.IO
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.Word
import Control.Monad

#include <SDL3/SDL_guid.h>

-- Define the proper structure for SDL_GUID to match C declaration
data SDL_GUID = SDL_GUID 
  { guidData :: [Word8] -- 16-byte array
  } deriving (Show, Eq)

-- Create a newtype for easier manipulation in Haskell
newtype SDLGUID = SDLGUID { unSDLGUID :: [Word8] }
  deriving (Show, Eq)

-- Define the storable instance for SDL_GUID to properly handle marshalling
instance Storable SDL_GUID where
  sizeOf _ = 16  -- Size of the SDL_GUID struct (16 bytes)
  alignment _ = alignment (undefined :: Word8)
  
  peek ptr = do
    bytes <- peekArray 16 (castPtr ptr)
    return $ SDL_GUID bytes
    
  poke ptr (SDL_GUID bytes) = do
    pokeArray (castPtr ptr) (take 16 $ bytes ++ repeat 0)

-- Foreign declarations around wrappers
foreign import ccall safe "wrapper_SDL_GUIDToString" sdlGUIDToStringRaw :: 
  Ptr Word8 -> CString -> CInt -> IO ()
foreign import ccall safe "wrapper_SDL_StringToGUID" sdlStringToGUIDRaw :: 
  CString -> Ptr Word8 -> IO ()

-- Convert our SDLGUID to string
sdlGUIDToString :: SDLGUID -> IO String
sdlGUIDToString (SDLGUID bytes) = do
  let guidBytes = take 16 $ bytes ++ repeat 0  -- Ensure we have exactly 16 bytes
  
  withArray guidBytes $ \guidPtr -> do
    -- Allocate a buffer for the string (33 bytes as specified in the SDL docs)
    allocaArray 33 $ \strPtr -> do
      -- Call the SDL wrapper function
      sdlGUIDToStringRaw guidPtr strPtr 33
      -- Convert the result to a Haskell String
      peekCString strPtr

-- Convert a string to our SDLGUID
sdlStringToGUID :: String -> IO SDLGUID
sdlStringToGUID str = do
  -- Allocate space for the GUID data (16 bytes)
  allocaArray 16 $ \guidPtr -> do
    -- Convert our string to C string and call SDL wrapper function
    withCString str $ \strPtr -> do
      sdlStringToGUIDRaw strPtr guidPtr
      -- Read the bytes back
      bytes <- peekArray 16 guidPtr
      return $ SDLGUID bytes

