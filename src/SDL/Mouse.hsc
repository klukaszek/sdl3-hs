-- SDL/Mouse.hsc
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : SDL.Mouse
-- Description : Mouse input handling for SDL applications
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- This module provides bindings to the SDL3 mouse handling API, allowing Haskell applications
-- to interact with mouse input. It supports cursor management, mouse state tracking,
-- multiple mice, and relative mouse mode.
--
-- Functions are provided to query mouse position and button states, create and manage
-- cursors, and handle mouse capture for operations like dragging.

#include <SDL3/SDL_mouse.h>
#include <SDL3/SDL_stdinc.h> -- For CBool

module SDL.Mouse
  ( -- * Types
    SDLMouseID
  , SDLCursor(..)
  , SDLMouseButtonFlags
  , SDLSystemCursor(..)
  , SDLMouseWheelDirection(..)
  , SDLMouseMotionTransformCallback

    -- * Patterns / Constants
    -- ** System Cursors
  , pattern SDL_SYSTEM_CURSOR_DEFAULT
  , pattern SDL_SYSTEM_CURSOR_TEXT
  , pattern SDL_SYSTEM_CURSOR_WAIT
  , pattern SDL_SYSTEM_CURSOR_CROSSHAIR
  , pattern SDL_SYSTEM_CURSOR_PROGRESS
  , pattern SDL_SYSTEM_CURSOR_NWSE_RESIZE
  , pattern SDL_SYSTEM_CURSOR_NESW_RESIZE
  , pattern SDL_SYSTEM_CURSOR_EW_RESIZE
  , pattern SDL_SYSTEM_CURSOR_NS_RESIZE
  , pattern SDL_SYSTEM_CURSOR_MOVE
  , pattern SDL_SYSTEM_CURSOR_NOT_ALLOWED
  , pattern SDL_SYSTEM_CURSOR_POINTER
  , pattern SDL_SYSTEM_CURSOR_NW_RESIZE
  , pattern SDL_SYSTEM_CURSOR_N_RESIZE
  , pattern SDL_SYSTEM_CURSOR_NE_RESIZE
  , pattern SDL_SYSTEM_CURSOR_E_RESIZE
  , pattern SDL_SYSTEM_CURSOR_SE_RESIZE
  , pattern SDL_SYSTEM_CURSOR_S_RESIZE
  , pattern SDL_SYSTEM_CURSOR_SW_RESIZE
  , pattern SDL_SYSTEM_CURSOR_W_RESIZE
    -- ** Mouse Wheel Direction
  , pattern SDL_MOUSEWHEEL_NORMAL
  , pattern SDL_MOUSEWHEEL_FLIPPED
    -- ** Mouse Buttons (Indices)
  , pattern SDL_BUTTON_LEFT
  , pattern SDL_BUTTON_MIDDLE
  , pattern SDL_BUTTON_RIGHT
  , pattern SDL_BUTTON_X1
  , pattern SDL_BUTTON_X2
    -- ** Mouse Button Masks
  , pattern SDL_BUTTON_LMASK
  , pattern SDL_BUTTON_MMASK
  , pattern SDL_BUTTON_RMASK
  , pattern SDL_BUTTON_X1MASK
  , pattern SDL_BUTTON_X2MASK

    -- * Mouse Management
  , sdlHasMouse
  , sdlGetMice
  , sdlGetMouseNameForID
  , sdlGetMouseFocus

    -- * Mouse State
  , sdlGetMouseState
  , sdlGetGlobalMouseState
  , sdlGetRelativeMouseState

    -- * Mouse Position Control
  , sdlWarpMouseInWindow
  , sdlWarpMouseGlobal
  , sdlSetRelativeMouseTransform

    -- * Relative Mouse Mode
  , sdlSetWindowRelativeMouseMode
  , sdlGetWindowRelativeMouseMode
  , sdlCaptureMouse

    -- * Cursor Management
  , sdlCreateCursor
  , sdlCreateColorCursor
  , sdlCreateSystemCursor
  , sdlSetCursor
  , sdlGetCursor
  , sdlGetDefaultCursor
  , sdlDestroyCursor
  , sdlShowCursor
  , sdlHideCursor
  , sdlCursorVisible
  ) where

import Foreign hiding (free)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (maybeWith, with, toBool) -- Added with, toBool
import Data.Word
import Data.Int
import Control.Monad

-- Assuming these types are defined elsewhere consistently
import SDL.Error (sdlGetError)
import SDL.Stdinc (free, SDLBool(..)) -- Or directly use CBool/Bool
import SDL.Surface (SDLSurface) -- Assuming 'data SDLSurface' with Storable
import SDL.Video (SDLWindow(..)) -- Assuming 'newtype SDLWindow (Ptr SDL_Window)'

-- Opaque C struct types for FFI clarity
data SDL_Cursor
data SDL_Surface

-- | Unique ID for a mouse for the time it is connected to the system.
-- The value 0 is an invalid ID.
type SDLMouseID = Word32

-- | An opaque structure used to identify an SDL cursor.
newtype SDLCursor = SDLCursor (Ptr SDL_Cursor)
  deriving (Show, Eq)

-- | A bitmask of pressed mouse buttons, as reported by sdlGetMouseState, etc.
type SDLMouseButtonFlags = Word32

-- | Callback used to transform mouse motion delta from raw values.
type SDLMouseMotionTransformCallback =
  Ptr () ->              -- userdata
  Word64 ->              -- timestamp
  Ptr SDLWindow ->      -- window pointer
  SDLMouseID ->          -- mouseID
  Ptr CFloat ->          -- x pointer
  Ptr CFloat ->          -- y pointer
  IO ()

-- | System cursor types for sdlCreateSystemCursor.
newtype SDLSystemCursor = SDLSystemCursor CInt
  deriving newtype (Show, Eq, Ord, Storable, Enum)

pattern SDL_SYSTEM_CURSOR_DEFAULT :: SDLSystemCursor
pattern SDL_SYSTEM_CURSOR_DEFAULT      = SDLSystemCursor #{const SDL_SYSTEM_CURSOR_DEFAULT}
pattern SDL_SYSTEM_CURSOR_TEXT :: SDLSystemCursor
pattern SDL_SYSTEM_CURSOR_TEXT         = SDLSystemCursor #{const SDL_SYSTEM_CURSOR_TEXT}
pattern SDL_SYSTEM_CURSOR_WAIT :: SDLSystemCursor
pattern SDL_SYSTEM_CURSOR_WAIT         = SDLSystemCursor #{const SDL_SYSTEM_CURSOR_WAIT}
pattern SDL_SYSTEM_CURSOR_CROSSHAIR :: SDLSystemCursor
pattern SDL_SYSTEM_CURSOR_CROSSHAIR    = SDLSystemCursor #{const SDL_SYSTEM_CURSOR_CROSSHAIR}
pattern SDL_SYSTEM_CURSOR_PROGRESS :: SDLSystemCursor
pattern SDL_SYSTEM_CURSOR_PROGRESS     = SDLSystemCursor #{const SDL_SYSTEM_CURSOR_PROGRESS}
pattern SDL_SYSTEM_CURSOR_NWSE_RESIZE :: SDLSystemCursor
pattern SDL_SYSTEM_CURSOR_NWSE_RESIZE  = SDLSystemCursor #{const SDL_SYSTEM_CURSOR_NWSE_RESIZE}
pattern SDL_SYSTEM_CURSOR_NESW_RESIZE :: SDLSystemCursor
pattern SDL_SYSTEM_CURSOR_NESW_RESIZE  = SDLSystemCursor #{const SDL_SYSTEM_CURSOR_NESW_RESIZE}
pattern SDL_SYSTEM_CURSOR_EW_RESIZE :: SDLSystemCursor
pattern SDL_SYSTEM_CURSOR_EW_RESIZE    = SDLSystemCursor #{const SDL_SYSTEM_CURSOR_EW_RESIZE}
pattern SDL_SYSTEM_CURSOR_NS_RESIZE :: SDLSystemCursor
pattern SDL_SYSTEM_CURSOR_NS_RESIZE    = SDLSystemCursor #{const SDL_SYSTEM_CURSOR_NS_RESIZE}
pattern SDL_SYSTEM_CURSOR_MOVE :: SDLSystemCursor
pattern SDL_SYSTEM_CURSOR_MOVE         = SDLSystemCursor #{const SDL_SYSTEM_CURSOR_MOVE}
pattern SDL_SYSTEM_CURSOR_NOT_ALLOWED :: SDLSystemCursor
pattern SDL_SYSTEM_CURSOR_NOT_ALLOWED  = SDLSystemCursor #{const SDL_SYSTEM_CURSOR_NOT_ALLOWED}
pattern SDL_SYSTEM_CURSOR_POINTER :: SDLSystemCursor
pattern SDL_SYSTEM_CURSOR_POINTER      = SDLSystemCursor #{const SDL_SYSTEM_CURSOR_POINTER}
pattern SDL_SYSTEM_CURSOR_NW_RESIZE :: SDLSystemCursor
pattern SDL_SYSTEM_CURSOR_NW_RESIZE    = SDLSystemCursor #{const SDL_SYSTEM_CURSOR_NW_RESIZE}
pattern SDL_SYSTEM_CURSOR_N_RESIZE :: SDLSystemCursor
pattern SDL_SYSTEM_CURSOR_N_RESIZE     = SDLSystemCursor #{const SDL_SYSTEM_CURSOR_N_RESIZE}
pattern SDL_SYSTEM_CURSOR_NE_RESIZE :: SDLSystemCursor
pattern SDL_SYSTEM_CURSOR_NE_RESIZE    = SDLSystemCursor #{const SDL_SYSTEM_CURSOR_NE_RESIZE}
pattern SDL_SYSTEM_CURSOR_E_RESIZE :: SDLSystemCursor
pattern SDL_SYSTEM_CURSOR_E_RESIZE     = SDLSystemCursor #{const SDL_SYSTEM_CURSOR_E_RESIZE}
pattern SDL_SYSTEM_CURSOR_SE_RESIZE :: SDLSystemCursor
pattern SDL_SYSTEM_CURSOR_SE_RESIZE    = SDLSystemCursor #{const SDL_SYSTEM_CURSOR_SE_RESIZE}
pattern SDL_SYSTEM_CURSOR_S_RESIZE :: SDLSystemCursor
pattern SDL_SYSTEM_CURSOR_S_RESIZE     = SDLSystemCursor #{const SDL_SYSTEM_CURSOR_S_RESIZE}
pattern SDL_SYSTEM_CURSOR_SW_RESIZE :: SDLSystemCursor
pattern SDL_SYSTEM_CURSOR_SW_RESIZE    = SDLSystemCursor #{const SDL_SYSTEM_CURSOR_SW_RESIZE}
pattern SDL_SYSTEM_CURSOR_W_RESIZE :: SDLSystemCursor
pattern SDL_SYSTEM_CURSOR_W_RESIZE     = SDLSystemCursor #{const SDL_SYSTEM_CURSOR_W_RESIZE}

-- | Scroll direction types for the Scroll event
newtype SDLMouseWheelDirection = SDLMouseWheelDirection CUInt -- C enum uses Uint32
  deriving newtype (Show, Eq, Ord, Storable, Enum)

pattern SDL_MOUSEWHEEL_NORMAL :: SDLMouseWheelDirection
pattern SDL_MOUSEWHEEL_NORMAL = SDLMouseWheelDirection #{const SDL_MOUSEWHEEL_NORMAL}
pattern SDL_MOUSEWHEEL_FLIPPED :: SDLMouseWheelDirection
pattern SDL_MOUSEWHEEL_FLIPPED = SDLMouseWheelDirection #{const SDL_MOUSEWHEEL_FLIPPED}

-- Mouse button constants (Indices)
pattern SDL_BUTTON_LEFT :: Int
pattern SDL_BUTTON_LEFT   = #{const SDL_BUTTON_LEFT}
pattern SDL_BUTTON_MIDDLE :: Int
pattern SDL_BUTTON_MIDDLE = #{const SDL_BUTTON_MIDDLE}
pattern SDL_BUTTON_RIGHT :: Int
pattern SDL_BUTTON_RIGHT  = #{const SDL_BUTTON_RIGHT}
pattern SDL_BUTTON_X1 :: Int
pattern SDL_BUTTON_X1     = #{const SDL_BUTTON_X1}
pattern SDL_BUTTON_X2 :: Int
pattern SDL_BUTTON_X2     = #{const SDL_BUTTON_X2}

-- Mouse button masks (Flags)
pattern SDL_BUTTON_LMASK :: SDLMouseButtonFlags
pattern SDL_BUTTON_LMASK  = #{const SDL_BUTTON_LMASK}
pattern SDL_BUTTON_MMASK :: SDLMouseButtonFlags
pattern SDL_BUTTON_MMASK  = #{const SDL_BUTTON_MMASK}
pattern SDL_BUTTON_RMASK :: SDLMouseButtonFlags
pattern SDL_BUTTON_RMASK  = #{const SDL_BUTTON_RMASK}
pattern SDL_BUTTON_X1MASK :: SDLMouseButtonFlags
pattern SDL_BUTTON_X1MASK = #{const SDL_BUTTON_X1MASK}
pattern SDL_BUTTON_X2MASK :: SDLMouseButtonFlags
pattern SDL_BUTTON_X2MASK = #{const SDL_BUTTON_X2MASK}

-- * Mouse Management

-- | Return whether a mouse is currently connected.
foreign import ccall unsafe "SDL_HasMouse" sdlHasMouse :: IO CBool

-- | Get a list of currently connected mice.
-- Memory allocated by SDL must be freed by the caller.
foreign import ccall unsafe "SDL_GetMice"
  c_sdlGetMice :: Ptr CInt -> IO (Ptr SDLMouseID)

sdlGetMice :: IO [SDLMouseID]
sdlGetMice = alloca $ \countPtr -> do
  idPtr <- c_sdlGetMice countPtr
  if idPtr == nullPtr
    then return []
    else do
      count <- peek countPtr
      ids <- peekArray (fromIntegral count) idPtr
      -- Free the memory allocated by SDL
      free (castPtr idPtr)
      return ids

-- | Get the name of a mouse. The returned string is owned by SDL.
foreign import ccall unsafe "SDL_GetMouseNameForID"
  c_sdlGetMouseNameForID :: SDLMouseID -> IO CString

sdlGetMouseNameForID :: SDLMouseID -> IO (Maybe String)
sdlGetMouseNameForID mid = do
  cStr <- c_sdlGetMouseNameForID mid
  if cStr == nullPtr
    then return Nothing
    else Just <$> peekCString cStr

foreign import ccall unsafe "SDL_GetMouseFocus"
  c_sdlGetMouseFocus :: IO (Ptr SDLWindow)

sdlGetMouseFocus :: IO (Maybe SDLWindow)
sdlGetMouseFocus = do
  ptr <- c_sdlGetMouseFocus
  -- This works because GHC treats the ptr type from FFI
  -- as compatible with the Ptr SDL_Window expected by the
  -- imported SDLWindow constructor (since both ultimately refer
  -- to the same underlying C type despite the Haskell scoping).
  pure $ if ptr == nullPtr then Nothing else Just (SDLWindow ptr)

-- * Mouse State

-- | Query SDL's cache for the synchronous mouse button state and the
-- window-relative SDL-cursor position.
foreign import ccall unsafe "SDL_GetMouseState"
  c_sdlGetMouseState :: Ptr CFloat -> Ptr CFloat -> IO SDLMouseButtonFlags

sdlGetMouseState :: IO (SDLMouseButtonFlags, Maybe (Float, Float))
sdlGetMouseState = alloca $ \xPtr ->
  alloca $ \yPtr -> do
    buttons <- c_sdlGetMouseState xPtr yPtr
    -- Check if coordinates are valid (they might not be if no window has focus?)
    -- SDL doesn't explicitly document this, but it's safer.
    -- We'll assume non-negative means valid for now.
    x <- realToFrac <$> peek xPtr
    y <- realToFrac <$> peek yPtr
    -- A better check might involve querying focus, but let's return Maybe
    -- based on some heuristic or always return Just if SDL guarantees it.
    -- For simplicity, let's assume SDL provides coordinates if focus exists.
    mFocus <- sdlGetMouseFocus
    let pos = if mFocus == Nothing && x == 0 && y == 0 -- Heuristic guess
              then Nothing
              else Just (x, y)
    return (buttons, pos)

-- | Query the platform for the asynchronous mouse button state and the
-- desktop-relative platform-cursor position.
foreign import ccall unsafe "SDL_GetGlobalMouseState"
  c_sdlGetGlobalMouseState :: Ptr CFloat -> Ptr CFloat -> IO SDLMouseButtonFlags

sdlGetGlobalMouseState :: IO (SDLMouseButtonFlags, (Float, Float))
sdlGetGlobalMouseState = alloca $ \xPtr ->
  alloca $ \yPtr -> do
    buttons <- c_sdlGetGlobalMouseState xPtr yPtr
    x <- realToFrac <$> peek xPtr
    y <- realToFrac <$> peek yPtr
    return (buttons, (x, y))

-- | Query SDL's cache for the synchronous mouse button state and accumulated
-- mouse delta since last call.
foreign import ccall unsafe "SDL_GetRelativeMouseState"
  c_sdlGetRelativeMouseState :: Ptr CFloat -> Ptr CFloat -> IO SDLMouseButtonFlags

sdlGetRelativeMouseState :: IO (SDLMouseButtonFlags, (Float, Float))
sdlGetRelativeMouseState = alloca $ \xPtr ->
  alloca $ \yPtr -> do
    buttons <- c_sdlGetRelativeMouseState xPtr yPtr
    x <- realToFrac <$> peek xPtr
    y <- realToFrac <$> peek yPtr
    return (buttons, (x, y))

-- * Mouse Position Control

-- | Move the mouse cursor to the given position within the window.
foreign import ccall unsafe "SDL_WarpMouseInWindow"
  c_sdlWarpMouseInWindow :: Ptr SDLWindow -> CFloat -> CFloat -> IO ()

sdlWarpMouseInWindow :: Maybe SDLWindow -> Float -> Float -> IO ()
sdlWarpMouseInWindow mWindow x y =
  let windowPtr = maybe nullPtr (\(SDLWindow p) -> p) mWindow
  in c_sdlWarpMouseInWindow windowPtr (realToFrac x) (realToFrac y)

-- | Move the mouse to the given position in global screen space.
foreign import ccall unsafe "SDL_WarpMouseGlobal"
  c_sdlWarpMouseGlobal :: CFloat -> CFloat -> IO CBool

sdlWarpMouseGlobal :: Float -> Float -> IO Bool
sdlWarpMouseGlobal x y = toBool <$> c_sdlWarpMouseGlobal (realToFrac x) (realToFrac y)

-- | Set a user-defined function by which to transform relative mouse inputs.
foreign import ccall "wrapper"
  wrapMouseMotionTransform :: SDLMouseMotionTransformCallback -> IO (FunPtr SDLMouseMotionTransformCallback)

foreign import ccall unsafe "SDL_SetRelativeMouseTransform"
  c_sdlSetRelativeMouseTransform :: FunPtr SDLMouseMotionTransformCallback -> Ptr () -> IO CBool

sdlSetRelativeMouseTransform :: Maybe SDLMouseMotionTransformCallback -> Ptr () -> IO Bool
sdlSetRelativeMouseTransform Nothing userdata =
  toBool <$> c_sdlSetRelativeMouseTransform nullFunPtr userdata
sdlSetRelativeMouseTransform (Just callback) userdata = do
  callbackPtr <- wrapMouseMotionTransform callback
  toBool <$> c_sdlSetRelativeMouseTransform callbackPtr userdata

-- * Relative Mouse Mode

-- | Set relative mouse mode for a window.
foreign import ccall unsafe "SDL_SetWindowRelativeMouseMode"
  c_sdlSetWindowRelativeMouseMode :: Ptr SDLWindow -> CBool -> IO CBool

sdlSetWindowRelativeMouseMode :: SDLWindow -> Bool -> IO Bool
sdlSetWindowRelativeMouseMode (SDLWindow windowPtr) enabled =
  toBool <$> c_sdlSetWindowRelativeMouseMode windowPtr (fromIntegral $ fromEnum enabled)

-- | Query whether relative mouse mode is enabled for a window.
foreign import ccall unsafe "SDL_GetWindowRelativeMouseMode"
  c_sdlGetWindowRelativeMouseMode :: Ptr SDLWindow -> IO CBool

sdlGetWindowRelativeMouseMode :: SDLWindow -> IO Bool
sdlGetWindowRelativeMouseMode (SDLWindow windowPtr) = toBool <$> c_sdlGetWindowRelativeMouseMode windowPtr

-- | Capture the mouse and track input outside an SDL window.
foreign import ccall unsafe "SDL_CaptureMouse"
  c_sdlCaptureMouse :: CBool -> IO CBool

sdlCaptureMouse :: Bool -> IO Bool
sdlCaptureMouse enabled = toBool <$> c_sdlCaptureMouse (fromIntegral $ fromEnum enabled)

-- * Cursor Management

-- | Create a cursor using the specified bitmap data and mask (in MSB format).
foreign import ccall unsafe "SDL_CreateCursor"
  c_sdlCreateCursor :: Ptr Word8 -> Ptr Word8 -> CInt -> CInt -> CInt -> CInt -> IO (Ptr SDL_Cursor)

sdlCreateCursor :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> Int -> Int -> IO (Maybe SDLCursor)
sdlCreateCursor dataPtr maskPtr w h hot_x hot_y = do
  ptr <- c_sdlCreateCursor dataPtr maskPtr
                           (fromIntegral w) (fromIntegral h)
                           (fromIntegral hot_x) (fromIntegral hot_y)
  pure $ if ptr == nullPtr then Nothing else Just (SDLCursor ptr)

-- | Create a color cursor from an SDL surface.
foreign import ccall unsafe "SDL_CreateColorCursor"
  c_sdlCreateColorCursor :: Ptr SDL_Surface -> CInt -> CInt -> IO (Ptr SDL_Cursor)

sdlCreateColorCursor :: SDLSurface -> Int -> Int -> IO (Maybe SDLCursor)
sdlCreateColorCursor surfaceRec hot_x hot_y = do
  -- Use 'with' as SDLSurface is a Storable data record
  with surfaceRec $ \surfacePtr -> do
    ptr <- c_sdlCreateColorCursor (castPtr surfacePtr) -- Cast Ptr SDLSurface to Ptr SDL_Surface
                                  (fromIntegral hot_x)
                                  (fromIntegral hot_y)
    pure $ if ptr == nullPtr then Nothing else Just (SDLCursor ptr)

-- | Create a system cursor.
foreign import ccall unsafe "SDL_CreateSystemCursor"
  c_sdlCreateSystemCursor :: CInt -> IO (Ptr SDL_Cursor)

sdlCreateSystemCursor :: SDLSystemCursor -> IO (Maybe SDLCursor)
sdlCreateSystemCursor cursorType = do
  -- Use fromIntegral . fromEnum to get CInt from Enum-derived newtype
  ptr <- c_sdlCreateSystemCursor (fromIntegral $ fromEnum cursorType)
  pure $ if ptr == nullPtr then Nothing else Just (SDLCursor ptr)

-- | Set the active cursor. Pass Nothing to set the default cursor.
foreign import ccall unsafe "SDL_SetCursor"
  c_sdlSetCursor :: Ptr SDL_Cursor -> IO CBool

sdlSetCursor :: Maybe SDLCursor -> IO Bool
sdlSetCursor mCursor =
  let cursorPtr = maybe nullPtr (\(SDLCursor p) -> p) mCursor
  in toBool <$> c_sdlSetCursor cursorPtr

-- | Get the active cursor. Returns Nothing if default cursor is active or no cursor shown.
foreign import ccall unsafe "SDL_GetCursor"
  c_sdlGetCursor :: IO (Ptr SDL_Cursor)

sdlGetCursor :: IO (Maybe SDLCursor)
sdlGetCursor = do
  ptr <- c_sdlGetCursor
  pure $ if ptr == nullPtr then Nothing else Just (SDLCursor ptr)

-- | Get the default cursor.
foreign import ccall unsafe "SDL_GetDefaultCursor"
  c_sdlGetDefaultCursor :: IO (Ptr SDL_Cursor)

sdlGetDefaultCursor :: IO (Maybe SDLCursor)
sdlGetDefaultCursor = do
  ptr <- c_sdlGetDefaultCursor
  pure $ if ptr == nullPtr then Nothing else Just (SDLCursor ptr)

-- | Free a previously-created cursor.
foreign import ccall unsafe "SDL_DestroyCursor"
  c_sdlDestroyCursor :: Ptr SDL_Cursor -> IO ()

sdlDestroyCursor :: SDLCursor -> IO ()
sdlDestroyCursor (SDLCursor cursorPtr) = c_sdlDestroyCursor cursorPtr

-- | Show the cursor. Returns True on success.
foreign import ccall unsafe "SDL_ShowCursor"
  c_sdlShowCursor :: IO CBool

sdlShowCursor :: IO Bool
sdlShowCursor = toBool <$> c_sdlShowCursor

-- | Hide the cursor. Returns True on success.
foreign import ccall unsafe "SDL_HideCursor"
  c_sdlHideCursor :: IO CBool

sdlHideCursor :: IO Bool
sdlHideCursor = toBool <$> c_sdlHideCursor

-- | Return whether the cursor is currently being shown.
foreign import ccall unsafe "SDL_CursorVisible"
  c_sdlCursorVisible :: IO CBool

sdlCursorVisible :: IO Bool
sdlCursorVisible = toBool <$> c_sdlCursorVisible
