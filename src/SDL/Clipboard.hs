{-|
Module      : SDL.Clipboard
Description : Clipboard management for SDL applications
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

This module provides bindings to the SDL3 clipboard API, allowing Haskell applications
to interact with system clipboards. It supports text and arbitrary data transfer
using mime types, including both standard clipboard and primary selection.

The clipboard API allows setting and retrieving text, checking clipboard contents,
and managing more complex clipboard interactions with multiple data formats.
-}

module SDL.Clipboard 
  ( -- * Text Clipboard Operations
    sdlSetClipboardText
  , sdlGetClipboardText
  , sdlHasClipboardText

    -- * Primary Selection Operations
  , sdlSetPrimarySelectionText
  , sdlGetPrimarySelectionText
  , sdlHasPrimarySelectionText

    -- * Clipboard Data Callbacks
  , SDLClipboardDataCallback
  , SDLClipboardCleanupCallback

    -- * Advanced Clipboard Data Management
  , sdlSetClipboardData
  , sdlClearClipboardData
  , sdlGetClipboardData
  , sdlHasClipboardData
  , sdlGetClipboardMimeTypes
  ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek, poke)
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.IO.Unsafe (unsafePerformIO)

-- | Callback function for providing clipboard data when requested
type SDLClipboardDataCallback = 
  Ptr () ->           -- userdata
  CString ->          -- mime_type
  Ptr CSize ->        -- size
  IO (Ptr ())         -- data pointer

-- | Callback function for cleaning up clipboard data
type SDLClipboardCleanupCallback = 
  Ptr () ->           -- userdata
  IO ()

-- | Set clipboard text 
--
-- Returns True on success, False on failure
--
-- @since 3.2.0
foreign import ccall "SDL_SetClipboardText"
  sdlSetClipboardTextRaw :: CString -> IO CBool

sdlSetClipboardText :: String -> IO Bool
sdlSetClipboardText text = do
  withCString text $ \cstr -> do
    result <- sdlSetClipboardTextRaw cstr
    return $ cbool result

-- | Get clipboard text
--
-- Returns the clipboard text or Nothing on failure
--
-- @since 3.2.0
foreign import ccall "SDL_GetClipboardText"
  sdlGetClipboardTextRaw :: IO CString

sdlGetClipboardText :: IO (Maybe String)
sdlGetClipboardText = do
  cstr <- sdlGetClipboardTextRaw
  if cstr == nullPtr
    then return Nothing
    else Just <$> peekCString cstr

-- | Check if clipboard contains text
--
-- @since 3.2.0
foreign import ccall "SDL_HasClipboardText"
  sdlHasClipboardText :: IO CBool

-- | Set primary selection text
--
-- Returns True on success, False on failure
--
-- @since 3.2.0
foreign import ccall "SDL_SetPrimarySelectionText"
  sdlSetPrimarySelectionTextRaw :: CString -> IO CBool

sdlSetPrimarySelectionText :: String -> IO Bool
sdlSetPrimarySelectionText text = do
  withCString text $ \cstr -> do
    result <- sdlSetPrimarySelectionTextRaw cstr
    return $ cbool result

-- | Get primary selection text
--
-- Returns the primary selection text or Nothing on failure
--
-- @since 3.2.0
foreign import ccall "SDL_GetPrimarySelectionText"
  sdlGetPrimarySelectionTextRaw :: IO CString

sdlGetPrimarySelectionText :: IO (Maybe String)
sdlGetPrimarySelectionText = do
  cstr <- sdlGetPrimarySelectionTextRaw
  if cstr == nullPtr
    then return Nothing
    else Just <$> peekCString cstr

-- | Check if primary selection contains text
--
-- @since 3.2.0
foreign import ccall "SDL_HasPrimarySelectionText"
  sdlHasPrimarySelectionText :: IO CBool

-- | Set clipboard data with mime types and callbacks
--
-- @since 3.2.0
foreign import ccall "SDL_SetClipboardData"
  sdlSetClipboardDataRaw :: 
    FunPtr SDLClipboardDataCallback ->    -- callback
    FunPtr SDLClipboardCleanupCallback -> -- cleanup
    Ptr () ->                             -- userdata
    Ptr CString ->                        -- mime_types
    CSize ->                              -- num_mime_types
    IO CBool

sdlSetClipboardData :: 
  SDLClipboardDataCallback -> 
  SDLClipboardCleanupCallback -> 
  Ptr () -> 
  [String] -> 
  IO Bool
sdlSetClipboardData callback cleanup userdata mimeTypes = do
  withArrayLen (map (unsafeToText . noNullPtr) mimeTypes) $ \len mimeArray -> do
    callbackPtr <- makeCallback callback
    cleanupPtr <- makeCleanup cleanup
    result <- sdlSetClipboardDataRaw 
      callbackPtr 
      cleanupPtr 
      userdata 
      mimeArray 
      (fromIntegral len)
    return $ cbool result
  where
    -- Ensure non-null text conversion
    unsafeToText s = unsafePerformIO $ newCString s
    noNullPtr "" = "null"
    noNullPtr s  = s

-- | Clear clipboard data
--
-- @since 3.2.0
foreign import ccall "SDL_ClearClipboardData"
  sdlClearClipboardData :: IO CBool

-- | Get clipboard data for a specific mime type
--
-- @since 3.2.0
foreign import ccall "SDL_GetClipboardData"
  sdlGetClipboardDataRaw :: 
    CString ->     -- mime_type
    Ptr CSize ->   -- size
    IO (Ptr ())    -- data

sdlGetClipboardData :: String -> IO (Maybe ByteString)
sdlGetClipboardData mimeType = do
  alloca $ \sizePtr -> do
    withCString mimeType $ \cstr -> do
      dataPtr <- sdlGetClipboardDataRaw cstr sizePtr
      if dataPtr == nullPtr
        then return Nothing
        else do
          size <- peek sizePtr
          bs <- BS.packCStringLen (castPtr dataPtr, fromIntegral size)
          return $ Just bs

-- | Check if clipboard has data for a specific mime type
--
-- @since 3.2.0
foreign import ccall "SDL_HasClipboardData"
  sdlHasClipboardDataRaw :: CString -> IO CBool

sdlHasClipboardData :: String -> IO Bool
sdlHasClipboardData mimeType = do
  withCString mimeType $ \cstr -> do
    result <- sdlHasClipboardDataRaw cstr
    return $ cbool result

-- | Get list of mime types in clipboard
--
-- @since 3.2.0
foreign import ccall "SDL_GetClipboardMimeTypes"
  sdlGetClipboardMimeTypesRaw :: Ptr CSize -> IO (Ptr CString)

sdlGetClipboardMimeTypes :: IO [String]
sdlGetClipboardMimeTypes = do
  alloca $ \countPtr -> do
    poke countPtr 0
    mimeTypesPtr <- sdlGetClipboardMimeTypesRaw countPtr
    if mimeTypesPtr == nullPtr
      then return []
      else do
        count <- peek countPtr
        mimeTypesCStr <- peekArray (fromIntegral count) mimeTypesPtr
        mapM peekCString mimeTypesCStr

-- | Convert CBool to Haskell Bool
cbool :: CBool -> Bool
cbool = (/= 0)

-- Foreign import helpers (these would typically be in a separate FFI module)
foreign import ccall "wrapper"
  makeCallback :: SDLClipboardDataCallback -> IO (FunPtr SDLClipboardDataCallback)

foreign import ccall "wrapper"
  makeCleanup :: SDLClipboardCleanupCallback -> IO (FunPtr SDLClipboardCleanupCallback)
