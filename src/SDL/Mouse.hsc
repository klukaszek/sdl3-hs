{-# LANGUAGE ForeignFunctionInterface #-}

{-|
Module      : SDL.Mouse
Description : Mouse input handling for SDL applications
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

This module provides bindings to the SDL3 mouse handling API, allowing Haskell applications
to interact with mouse input. It supports cursor management, mouse state tracking, 
multiple mice, and relative mouse mode.

Functions are provided to query mouse position and button states, create and manage
cursors, and handle mouse capture for operations like dragging.
-}

module SDL.Mouse
  ( -- * Mouse Management
    SDLMouseID(..)
  , sdlHasMouse
  , sdlGetMice
  , sdlGetMouseNameForID
  , sdlGetMouseFocus

    -- * Mouse State
  , SDLMouseButtonFlags
  , sdlGetMouseState
  , sdlGetGlobalMouseState
  , sdlGetRelativeMouseState
  
    -- * Mouse Position Control
  , sdlWarpMouseInWindow
  , sdlWarpMouseGlobal
  , sdlSetRelativeMouseTransform
  , SDLMouseMotionTransformCallback

    -- * Relative Mouse Mode
  , sdlSetWindowRelativeMouseMode
  , sdlGetWindowRelativeMouseMode
  , sdlCaptureMouse

    -- * Cursor Management
  , SDLCursor(..)
  , SDLSystemCursor(..)
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

    -- * Mouse Wheel Direction
  , SDLMouseWheelDirection(..)
  
    -- * Mouse Button Constants
  , sdlBUTTON_LEFT
  , sdlBUTTON_MIDDLE
  , sdlBUTTON_RIGHT
  , sdlBUTTON_X1
  , sdlBUTTON_X2
  , sdlBUTTON_LMASK
  , sdlBUTTON_MMASK
  , sdlBUTTON_RMASK
  , sdlBUTTON_X1MASK
  , sdlBUTTON_X2MASK
  ) where

import Foreign hiding (free)
import Foreign.C.Types
import Foreign.C.String
import Data.Word
import Data.Int
import Control.Monad

import SDL.Stdinc
import SDL.Error
import SDL.Surface
import SDL.Video

#include <SDL3/SDL_mouse.h>

-- | Unique ID for a mouse for the time it is connected to the system.
-- The value 0 is an invalid ID.
newtype SDLMouseID = SDLMouseID { unSDLMouseID :: Word32 }
  deriving (Show, Eq, Ord)

-- | An opaque structure used to identify an SDL cursor.
newtype SDLCursor = SDLCursor { unSDLCursor :: Ptr SDLCursor }
  deriving (Show, Eq)

-- | A bitmask of pressed mouse buttons, as reported by sdlGetMouseState, etc.
type SDLMouseButtonFlags = Word32

-- | Callback used to transform mouse motion delta from raw values.
type SDLMouseMotionTransformCallback = 
  Ptr () ->              -- userdata
  Word64 ->              -- timestamp
  Ptr SDLWindow ->       -- window
  SDLMouseID ->          -- mouseID
  Ptr CFloat ->          -- x pointer
  Ptr CFloat ->          -- y pointer
  IO ()

-- | System cursor types for sdlCreateSystemCursor.
data SDLSystemCursor
  = SDL_SYSTEM_CURSOR_DEFAULT      -- ^ Default cursor. Usually an arrow.
  | SDL_SYSTEM_CURSOR_TEXT         -- ^ Text selection. Usually an I-beam.
  | SDL_SYSTEM_CURSOR_WAIT         -- ^ Wait. Usually an hourglass or watch or spinning ball.
  | SDL_SYSTEM_CURSOR_CROSSHAIR    -- ^ Crosshair.
  | SDL_SYSTEM_CURSOR_PROGRESS     -- ^ Program is busy but still interactive. Usually it's WAIT with an arrow.
  | SDL_SYSTEM_CURSOR_NWSE_RESIZE  -- ^ Double arrow pointing northwest and southeast.
  | SDL_SYSTEM_CURSOR_NESW_RESIZE  -- ^ Double arrow pointing northeast and southwest.
  | SDL_SYSTEM_CURSOR_EW_RESIZE    -- ^ Double arrow pointing west and east.
  | SDL_SYSTEM_CURSOR_NS_RESIZE    -- ^ Double arrow pointing north and south.
  | SDL_SYSTEM_CURSOR_MOVE         -- ^ Four pointed arrow pointing north, south, east, and west.
  | SDL_SYSTEM_CURSOR_NOT_ALLOWED  -- ^ Not permitted. Usually a slashed circle or crossbones.
  | SDL_SYSTEM_CURSOR_POINTER      -- ^ Pointer that indicates a link. Usually a pointing hand.
  | SDL_SYSTEM_CURSOR_NW_RESIZE    -- ^ Window resize top-left. This may be a single arrow or a double arrow.
  | SDL_SYSTEM_CURSOR_N_RESIZE     -- ^ Window resize top. May be NS_RESIZE.
  | SDL_SYSTEM_CURSOR_NE_RESIZE    -- ^ Window resize top-right. May be NESW_RESIZE.
  | SDL_SYSTEM_CURSOR_E_RESIZE     -- ^ Window resize right. May be EW_RESIZE.
  | SDL_SYSTEM_CURSOR_SE_RESIZE    -- ^ Window resize bottom-right. May be NWSE_RESIZE.
  | SDL_SYSTEM_CURSOR_S_RESIZE     -- ^ Window resize bottom. May be NS_RESIZE.
  | SDL_SYSTEM_CURSOR_SW_RESIZE    -- ^ Window resize bottom-left. May be NESW_RESIZE.
  | SDL_SYSTEM_CURSOR_W_RESIZE     -- ^ Window resize left. May be EW_RESIZE.
  deriving (Show, Eq, Enum)

-- | Scroll direction types for the Scroll event
data SDLMouseWheelDirection
  = SDL_MOUSEWHEEL_NORMAL    -- ^ The scroll direction is normal
  | SDL_MOUSEWHEEL_FLIPPED   -- ^ The scroll direction is flipped / natural
  deriving (Show, Eq, Enum)

-- Mouse button constants
sdlBUTTON_LEFT, sdlBUTTON_MIDDLE, sdlBUTTON_RIGHT, sdlBUTTON_X1, sdlBUTTON_X2 :: Int
sdlBUTTON_LEFT   = #{const SDL_BUTTON_LEFT}
sdlBUTTON_MIDDLE = #{const SDL_BUTTON_MIDDLE}
sdlBUTTON_RIGHT  = #{const SDL_BUTTON_RIGHT}
sdlBUTTON_X1     = #{const SDL_BUTTON_X1}
sdlBUTTON_X2     = #{const SDL_BUTTON_X2}

-- Mouse button masks
sdlBUTTON_LMASK, sdlBUTTON_MMASK, sdlBUTTON_RMASK, sdlBUTTON_X1MASK, sdlBUTTON_X2MASK :: Word32
sdlBUTTON_LMASK  = #{const SDL_BUTTON_LMASK}
sdlBUTTON_MMASK  = #{const SDL_BUTTON_MMASK}
sdlBUTTON_RMASK  = #{const SDL_BUTTON_RMASK}
sdlBUTTON_X1MASK = #{const SDL_BUTTON_X1MASK}
sdlBUTTON_X2MASK = #{const SDL_BUTTON_X2MASK}

-- | Return whether a mouse is currently connected.
foreign import ccall "SDL_HasMouse" sdlHasMouse :: IO Bool

-- | Get a list of currently connected mice.
foreign import ccall "SDL_GetMice" sdlGetMiceRaw :: Ptr CInt -> IO (Ptr Word32)

sdlGetMice :: IO [SDLMouseID]
sdlGetMice = alloca $ \countPtr -> do
  pArr <- sdlGetMiceRaw countPtr
  if pArr == nullPtr
    then return []
    else do
      count <- peek countPtr
      arr <- peekArray (fromIntegral count) pArr
      free (castPtr pArr)
      return $ map SDLMouseID arr

-- | Get the name of a mouse.
foreign import ccall "SDL_GetMouseNameForID" sdlGetMouseNameForIDRaw :: Word32 -> IO CString

sdlGetMouseNameForID :: SDLMouseID -> IO (Maybe String)
sdlGetMouseNameForID (SDLMouseID id) = do
  cstr <- sdlGetMouseNameForIDRaw id
  if cstr == nullPtr
    then return Nothing
    else Just <$> peekCString cstr

-- | Get the window which currently has mouse focus.
foreign import ccall "SDL_GetMouseFocus" sdlGetMouseFocus :: IO (Ptr SDLWindow)

-- | Query SDL's cache for the synchronous mouse button state and the
-- window-relative SDL-cursor position.
foreign import ccall "SDL_GetMouseState" sdlGetMouseStateRaw :: Ptr CFloat -> Ptr CFloat -> IO Word32

sdlGetMouseState :: IO (SDLMouseButtonFlags, Maybe (Float, Float))
sdlGetMouseState = alloca $ \xPtr -> 
  alloca $ \yPtr -> do
    buttons <- sdlGetMouseStateRaw xPtr yPtr
    x <- peek xPtr
    y <- peek yPtr
    return (buttons, Just (realToFrac x, realToFrac y))

-- | Query the platform for the asynchronous mouse button state and the
-- desktop-relative platform-cursor position.
foreign import ccall "SDL_GetGlobalMouseState" sdlGetGlobalMouseStateRaw :: Ptr CFloat -> Ptr CFloat -> IO Word32

sdlGetGlobalMouseState :: IO (SDLMouseButtonFlags, Maybe (Float, Float))
sdlGetGlobalMouseState = alloca $ \xPtr -> 
  alloca $ \yPtr -> do
    buttons <- sdlGetGlobalMouseStateRaw xPtr yPtr
    x <- peek xPtr
    y <- peek yPtr
    return (buttons, Just (realToFrac x, realToFrac y))

-- | Query SDL's cache for the synchronous mouse button state and accumulated
-- mouse delta since last call.
foreign import ccall "SDL_GetRelativeMouseState" sdlGetRelativeMouseStateRaw :: Ptr CFloat -> Ptr CFloat -> IO Word32

sdlGetRelativeMouseState :: IO (SDLMouseButtonFlags, Maybe (Float, Float))
sdlGetRelativeMouseState = alloca $ \xPtr -> 
  alloca $ \yPtr -> do
    buttons <- sdlGetRelativeMouseStateRaw xPtr yPtr
    x <- peek xPtr
    y <- peek yPtr
    return (buttons, Just (realToFrac x, realToFrac y))

-- | Move the mouse cursor to the given position within the window.
foreign import ccall "SDL_WarpMouseInWindow" sdlWarpMouseInWindowRaw :: Ptr SDLWindow -> CFloat -> CFloat -> IO ()

sdlWarpMouseInWindow :: Maybe SDLWindow -> Float -> Float -> IO ()
sdlWarpMouseInWindow window x y = 
  sdlWarpMouseInWindowRaw windowPtr (realToFrac x) (realToFrac y)
  where
    windowPtr = maybe nullPtr unSDLWindow window

-- | Move the mouse to the given position in global screen space.
foreign import ccall "SDL_WarpMouseGlobal" sdlWarpMouseGlobalRaw :: CFloat -> CFloat -> IO Bool

sdlWarpMouseGlobal :: Float -> Float -> IO Bool
sdlWarpMouseGlobal x y = sdlWarpMouseGlobalRaw (realToFrac x) (realToFrac y)

-- | Set a user-defined function by which to transform relative mouse inputs.
foreign import ccall "wrapper" wrapMouseMotionTransform :: 
  SDLMouseMotionTransformCallback -> IO (FunPtr SDLMouseMotionTransformCallback)

foreign import ccall "SDL_SetRelativeMouseTransform" 
  sdlSetRelativeMouseTransformRaw :: FunPtr SDLMouseMotionTransformCallback -> Ptr () -> IO Bool

sdlSetRelativeMouseTransform :: Maybe SDLMouseMotionTransformCallback -> Ptr () -> IO Bool
sdlSetRelativeMouseTransform Nothing userdata = 
  sdlSetRelativeMouseTransformRaw nullFunPtr userdata
sdlSetRelativeMouseTransform (Just callback) userdata = do
  callbackPtr <- wrapMouseMotionTransform callback
  sdlSetRelativeMouseTransformRaw callbackPtr userdata

-- | Set relative mouse mode for a window.
foreign import ccall "SDL_SetWindowRelativeMouseMode" 
  sdlSetWindowRelativeMouseMode :: Ptr SDLWindow -> Bool -> IO Bool

-- | Query whether relative mouse mode is enabled for a window.
foreign import ccall "SDL_GetWindowRelativeMouseMode" 
  sdlGetWindowRelativeMouseMode :: Ptr SDLWindow -> IO Bool

-- | Capture the mouse and to track input outside an SDL window.
foreign import ccall "SDL_CaptureMouse" sdlCaptureMouse :: Bool -> IO Bool

-- | Create a cursor using the specified bitmap data and mask (in MSB format).
foreign import ccall "SDL_CreateCursor" sdlCreateCursorRaw :: 
  Ptr Word8 -> Ptr Word8 -> CInt -> CInt -> CInt -> CInt -> IO (Ptr SDLCursor)

sdlCreateCursor :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> Int -> Int -> IO SDLCursor
sdlCreateCursor data_ mask w h hot_x hot_y = do
  cursor <- sdlCreateCursorRaw data_ mask 
                             (fromIntegral w) 
                             (fromIntegral h) 
                             (fromIntegral hot_x) 
                             (fromIntegral hot_y)
  return $ SDLCursor cursor

-- | Create a color cursor.
foreign import ccall "SDL_CreateColorCursor" sdlCreateColorCursorRaw :: 
  Ptr SDLSurface -> CInt -> CInt -> IO (Ptr SDLCursor)

sdlCreateColorCursor :: SDLSurface -> Int -> Int -> IO SDLCursor
sdlCreateColorCursor surface hot_x hot_y = do
  -- Use the Storable instance to pass the surface to the C function
  with surface $ \surfacePtr -> do
    cursor <- sdlCreateColorCursorRaw 
                surfacePtr
                (fromIntegral hot_x) 
                (fromIntegral hot_y)
    return $ SDLCursor cursor

-- | Create a system cursor.
foreign import ccall "SDL_CreateSystemCursor" sdlCreateSystemCursorRaw :: 
  CInt -> IO (Ptr SDLCursor)

sdlCreateSystemCursor :: SDLSystemCursor -> IO SDLCursor
sdlCreateSystemCursor cursorType = do
  cursor <- sdlCreateSystemCursorRaw (fromIntegral $ fromEnum cursorType)
  return $ SDLCursor cursor

-- | Set the active cursor.
foreign import ccall "SDL_SetCursor" sdlSetCursorRaw :: Ptr SDLCursor -> IO Bool

sdlSetCursor :: SDLCursor -> IO Bool
sdlSetCursor (SDLCursor cursor) = sdlSetCursorRaw cursor

-- | Get the active cursor.
foreign import ccall "SDL_GetCursor" sdlGetCursorRaw :: IO (Ptr SDLCursor)

sdlGetCursor :: IO (Maybe SDLCursor)
sdlGetCursor = do
  cursor <- sdlGetCursorRaw
  if cursor == nullPtr
    then return Nothing
    else return $ Just $ SDLCursor cursor

-- | Get the default cursor.
foreign import ccall "SDL_GetDefaultCursor" sdlGetDefaultCursorRaw :: IO (Ptr SDLCursor)

sdlGetDefaultCursor :: IO (Maybe SDLCursor)
sdlGetDefaultCursor = do
  cursor <- sdlGetDefaultCursorRaw
  if cursor == nullPtr
    then return Nothing
    else return $ Just $ SDLCursor cursor

-- | Free a previously-created cursor.
foreign import ccall "SDL_DestroyCursor" sdlDestroyCursorRaw :: Ptr SDLCursor -> IO ()

sdlDestroyCursor :: SDLCursor -> IO ()
sdlDestroyCursor (SDLCursor cursor) = sdlDestroyCursorRaw cursor

-- | Show the cursor.
foreign import ccall "SDL_ShowCursor" sdlShowCursor :: IO Bool

-- | Hide the cursor.
foreign import ccall "SDL_HideCursor" sdlHideCursor :: IO Bool

-- | Return whether the cursor is currently being shown.
foreign import ccall "SDL_CursorVisible" sdlCursorVisible :: IO Bool
