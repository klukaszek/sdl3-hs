{-# LANGUAGE PatternSynonyms #-}

module SDL3.Wrapped.Mouse
  ( SDLMouseID
  , SDLCursor(..)
  , SDLMouseButtonFlags
  , SDLSystemCursor(..)
  , SDLMouseWheelDirection(..)
  , SDLMouseMotionTransformCallback
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
  , pattern SDL_MOUSEWHEEL_NORMAL
  , pattern SDL_MOUSEWHEEL_FLIPPED
  , pattern SDL_BUTTON_LEFT
  , pattern SDL_BUTTON_MIDDLE
  , pattern SDL_BUTTON_RIGHT
  , pattern SDL_BUTTON_X1
  , pattern SDL_BUTTON_X2
  , pattern SDL_BUTTON_LMASK
  , pattern SDL_BUTTON_MMASK
  , pattern SDL_BUTTON_RMASK
  , pattern SDL_BUTTON_X1MASK
  , pattern SDL_BUTTON_X2MASK
  , sdlHasMouse
  , sdlGetMice
  , sdlGetMouseNameForID
  , sdlGetMouseFocus
  , sdlGetMouseState
  , sdlGetGlobalMouseState
  , sdlGetRelativeMouseState
  , sdlWarpMouseInWindow
  , sdlWarpMouseGlobal
  , sdlSetRelativeMouseTransform
  , sdlSetWindowRelativeMouseMode
  , sdlGetWindowRelativeMouseMode
  , sdlCaptureMouse
  , sdlCreateCursor
  , sdlCreateColorCursor
  , sdlCreateSystemCursor
  , SDLCursorFrameInfo(..)
  , sdlCreateAnimatedCursor
  , sdlSetCursor
  , sdlGetCursor
  , sdlGetDefaultCursor
  , sdlDestroyCursor
  , sdlShowCursor
  , sdlHideCursor
  , sdlCursorVisible
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Word (Word8)
import Foreign.C.Types (CBool)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, nullFunPtr)
import System.IO.Unsafe (unsafePerformIO)
import SDL3.Surface (SDLSurface, withSurfacePtr)
import SDL3.Video (SDLWindow)
import SDL3.Raw.Mouse
  ( SDLCursor(..)
  , SDLCursorFrameInfo(..)
  , SDLMouseButtonFlags
  , SDLMouseID
  , SDLMouseMotionTransformCallback
  , SDLMouseWheelDirection(..)
  , SDLSystemCursor(..)
  , pattern SDL_BUTTON_LEFT
  , pattern SDL_BUTTON_LMASK
  , pattern SDL_BUTTON_MIDDLE
  , pattern SDL_BUTTON_MMASK
  , pattern SDL_BUTTON_RIGHT
  , pattern SDL_BUTTON_RMASK
  , pattern SDL_BUTTON_X1
  , pattern SDL_BUTTON_X1MASK
  , pattern SDL_BUTTON_X2
  , pattern SDL_BUTTON_X2MASK
  , pattern SDL_MOUSEWHEEL_FLIPPED
  , pattern SDL_MOUSEWHEEL_NORMAL
  , pattern SDL_SYSTEM_CURSOR_CROSSHAIR
  , pattern SDL_SYSTEM_CURSOR_DEFAULT
  , pattern SDL_SYSTEM_CURSOR_EW_RESIZE
  , pattern SDL_SYSTEM_CURSOR_E_RESIZE
  , pattern SDL_SYSTEM_CURSOR_MOVE
  , pattern SDL_SYSTEM_CURSOR_NESW_RESIZE
  , pattern SDL_SYSTEM_CURSOR_NS_RESIZE
  , pattern SDL_SYSTEM_CURSOR_NE_RESIZE
  , pattern SDL_SYSTEM_CURSOR_NOT_ALLOWED
  , pattern SDL_SYSTEM_CURSOR_NWSE_RESIZE
  , pattern SDL_SYSTEM_CURSOR_NW_RESIZE
  , pattern SDL_SYSTEM_CURSOR_N_RESIZE
  , pattern SDL_SYSTEM_CURSOR_POINTER
  , pattern SDL_SYSTEM_CURSOR_PROGRESS
  , pattern SDL_SYSTEM_CURSOR_SE_RESIZE
  , pattern SDL_SYSTEM_CURSOR_SW_RESIZE
  , pattern SDL_SYSTEM_CURSOR_S_RESIZE
  , pattern SDL_SYSTEM_CURSOR_TEXT
  , pattern SDL_SYSTEM_CURSOR_WAIT
  , pattern SDL_SYSTEM_CURSOR_W_RESIZE
  )
import qualified SDL3.Raw.Mouse as Raw

mouseMotionTransformRef :: IORef (Maybe (FunPtr Raw.SDLMouseMotionTransformCallback))
mouseMotionTransformRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE mouseMotionTransformRef #-}

cbool :: CBool -> Bool
cbool = (/= 0)

releaseMouseMotionTransform :: Maybe (FunPtr Raw.SDLMouseMotionTransformCallback) -> IO ()
releaseMouseMotionTransform Nothing = pure ()
releaseMouseMotionTransform (Just callbackPtr) = freeHaskellFunPtr callbackPtr

sdlHasMouse :: MonadIO m => m Bool
sdlHasMouse = liftIO $ cbool <$> Raw.sdlHasMouse

sdlGetMice :: MonadIO m => m [SDLMouseID]
sdlGetMice = liftIO Raw.sdlGetMice

sdlGetMouseNameForID :: MonadIO m => SDLMouseID -> m (Maybe String)
sdlGetMouseNameForID = liftIO . Raw.sdlGetMouseNameForID

sdlGetMouseFocus :: MonadIO m => m (Maybe SDLWindow)
sdlGetMouseFocus = liftIO Raw.sdlGetMouseFocus

sdlGetMouseState :: MonadIO m => m (SDLMouseButtonFlags, Maybe (Float, Float))
sdlGetMouseState = liftIO Raw.sdlGetMouseState

sdlGetGlobalMouseState :: MonadIO m => m (SDLMouseButtonFlags, (Float, Float))
sdlGetGlobalMouseState = liftIO Raw.sdlGetGlobalMouseState

sdlGetRelativeMouseState :: MonadIO m => m (SDLMouseButtonFlags, (Float, Float))
sdlGetRelativeMouseState = liftIO Raw.sdlGetRelativeMouseState

sdlWarpMouseInWindow :: MonadIO m => Maybe SDLWindow -> Float -> Float -> m ()
sdlWarpMouseInWindow window x y = liftIO $ Raw.sdlWarpMouseInWindow window x y

sdlWarpMouseGlobal :: MonadIO m => Float -> Float -> m Bool
sdlWarpMouseGlobal x y = liftIO $ Raw.sdlWarpMouseGlobal x y

sdlSetRelativeMouseTransform :: MonadIO m => Maybe SDLMouseMotionTransformCallback -> Ptr () -> m Bool
sdlSetRelativeMouseTransform Nothing userdata =
  liftIO $ do
    result <- Raw.sdlSetRelativeMouseTransform nullFunPtr userdata
    if cbool result
      then do
        previous <- atomicModifyIORef' mouseMotionTransformRef (\current -> (Nothing, current))
        releaseMouseMotionTransform previous
        pure True
      else pure False
sdlSetRelativeMouseTransform (Just callback) userdata =
  liftIO $ do
    callbackPtr <- Raw.wrapMouseMotionTransform callback
    result <- Raw.sdlSetRelativeMouseTransform callbackPtr userdata
    if cbool result
      then do
        previous <- atomicModifyIORef' mouseMotionTransformRef (\current -> (Just callbackPtr, current))
        releaseMouseMotionTransform previous
        pure True
      else do
        freeHaskellFunPtr callbackPtr
        pure False

sdlSetWindowRelativeMouseMode :: MonadIO m => SDLWindow -> Bool -> m Bool
sdlSetWindowRelativeMouseMode window enabled = liftIO $ Raw.sdlSetWindowRelativeMouseMode window enabled

sdlGetWindowRelativeMouseMode :: MonadIO m => SDLWindow -> m Bool
sdlGetWindowRelativeMouseMode = liftIO . Raw.sdlGetWindowRelativeMouseMode

sdlCaptureMouse :: MonadIO m => Bool -> m Bool
sdlCaptureMouse = liftIO . Raw.sdlCaptureMouse

sdlCreateCursor :: MonadIO m => Ptr Word8 -> Ptr Word8 -> Int -> Int -> Int -> Int -> m (Maybe SDLCursor)
sdlCreateCursor dataPtr maskPtr w h hotX hotY =
  liftIO $ Raw.sdlCreateCursor dataPtr maskPtr w h hotX hotY

sdlCreateColorCursor :: MonadIO m => SDLSurface -> Int -> Int -> m (Maybe SDLCursor)
sdlCreateColorCursor surface hotX hotY =
  withSurfacePtr surface $ \surfacePtr ->
    Raw.sdlCreateColorCursor surfacePtr hotX hotY

sdlCreateSystemCursor :: MonadIO m => SDLSystemCursor -> m (Maybe SDLCursor)
sdlCreateSystemCursor = liftIO . Raw.sdlCreateSystemCursor

sdlCreateAnimatedCursor :: MonadIO m => [SDLCursorFrameInfo] -> Int -> Int -> m (Maybe SDLCursor)
sdlCreateAnimatedCursor frames hotX hotY = liftIO $ Raw.sdlCreateAnimatedCursor frames hotX hotY

sdlSetCursor :: MonadIO m => Maybe SDLCursor -> m Bool
sdlSetCursor = liftIO . Raw.sdlSetCursor

sdlGetCursor :: MonadIO m => m (Maybe SDLCursor)
sdlGetCursor = liftIO Raw.sdlGetCursor

sdlGetDefaultCursor :: MonadIO m => m (Maybe SDLCursor)
sdlGetDefaultCursor = liftIO Raw.sdlGetDefaultCursor

sdlDestroyCursor :: MonadIO m => SDLCursor -> m ()
sdlDestroyCursor = liftIO . Raw.sdlDestroyCursor

sdlShowCursor :: MonadIO m => m Bool
sdlShowCursor = liftIO Raw.sdlShowCursor

sdlHideCursor :: MonadIO m => m Bool
sdlHideCursor = liftIO Raw.sdlHideCursor

sdlCursorVisible :: MonadIO m => m Bool
sdlCursorVisible = liftIO Raw.sdlCursorVisible
