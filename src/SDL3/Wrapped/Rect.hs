module SDL3.Wrapped.Rect
  ( SDLPoint(..)
  , SDLFPoint(..)
  , SDLRect(..)
  , SDLFRect(..)
  , sdlRectToFRect
  , sdlPointInRect
  , sdlRectEmpty
  , sdlRectsEqual
  , sdlHasRectIntersection
  , sdlGetRectIntersection
  , sdlGetRectUnion
  , sdlGetRectEnclosingPoints
  , sdlGetRectAndLineIntersection
  , sdlPointInRectFloat
  , sdlRectEmptyFloat
  , sdlRectsEqualEpsilon
  , sdlRectsEqualFloat
  , sdlHasRectIntersectionFloat
  , sdlGetRectIntersectionFloat
  , sdlGetRectUnionFloat
  , sdlGetRectEnclosingPointsFloat
  , sdlGetRectAndLineIntersectionFloat
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.Types (CFloat, CInt)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import SDL3.Raw.Rect
  ( SDLFPoint(..)
  , SDLFRect(..)
  , SDLPoint(..)
  , SDLRect(..)
  , sdlPointInRect
  , sdlPointInRectFloat
  , sdlRectEmpty
  , sdlRectEmptyFloat
  , sdlRectToFRect
  , sdlRectsEqual
  , sdlRectsEqualEpsilon
  , sdlRectsEqualFloat
  )
import qualified SDL3.Raw.Rect as Raw

sdlHasRectIntersection :: MonadIO m => Ptr SDLRect -> Ptr SDLRect -> m Bool
sdlHasRectIntersection a b = liftIO $ Raw.sdlHasRectIntersectionRaw a b

sdlGetRectIntersection :: MonadIO m => SDLRect -> SDLRect -> m (Maybe SDLRect)
sdlGetRectIntersection a b = liftIO $
  with a $ \aPtr ->
  with b $ \bPtr ->
  with (SDLRect 0 0 0 0) $ \resultPtr -> do
    success <- Raw.sdlGetRectIntersectionRaw aPtr bPtr resultPtr
    if success
      then Just <$> peek resultPtr
      else return Nothing

sdlGetRectUnion :: MonadIO m => SDLRect -> SDLRect -> m (Maybe SDLRect)
sdlGetRectUnion a b = liftIO $
  with a $ \aPtr ->
  with b $ \bPtr ->
  with (SDLRect 0 0 0 0) $ \resultPtr -> do
    success <- Raw.sdlGetRectUnionRaw aPtr bPtr resultPtr
    if success
      then Just <$> peek resultPtr
      else return Nothing

sdlGetRectEnclosingPoints :: MonadIO m => [SDLPoint] -> Maybe SDLRect -> m (Maybe SDLRect)
sdlGetRectEnclosingPoints points mClip = liftIO $
  withArray points $ \pointsPtr -> do
    let count = fromIntegral $ length points
    withMaybe mClip $ \clipPtr ->
      with (SDLRect 0 0 0 0) $ \resultPtr -> do
        success <- Raw.sdlGetRectEnclosingPointsRaw pointsPtr count clipPtr resultPtr
        if success
          then Just <$> peek resultPtr
          else return Nothing
  where
    withMaybe Nothing f = f nullPtr
    withMaybe (Just x) f = with x f

sdlGetRectAndLineIntersection :: MonadIO m => SDLRect -> (CInt, CInt) -> (CInt, CInt) -> m (Maybe ((CInt, CInt), (CInt, CInt)))
sdlGetRectAndLineIntersection rect (x1, y1) (x2, y2) = liftIO $
  with rect $ \rectPtr ->
  with x1 $ \x1Ptr ->
  with y1 $ \y1Ptr ->
  with x2 $ \x2Ptr ->
  with y2 $ \y2Ptr -> do
    success <- Raw.sdlGetRectAndLineIntersectionRaw rectPtr x1Ptr y1Ptr x2Ptr y2Ptr
    if success
      then do
        x1' <- peek x1Ptr
        y1' <- peek y1Ptr
        x2' <- peek x2Ptr
        y2' <- peek y2Ptr
        return $ Just ((x1', y1'), (x2', y2'))
      else return Nothing

sdlHasRectIntersectionFloat :: MonadIO m => Ptr SDLFRect -> Ptr SDLFRect -> m Bool
sdlHasRectIntersectionFloat a b = liftIO $ Raw.sdlHasRectIntersectionFloatRaw a b

sdlGetRectIntersectionFloat :: MonadIO m => SDLFRect -> SDLFRect -> m (Maybe SDLFRect)
sdlGetRectIntersectionFloat a b = liftIO $
  with a $ \aPtr ->
  with b $ \bPtr ->
  with (SDLFRect 0 0 0 0) $ \resultPtr -> do
    success <- Raw.sdlGetRectIntersectionFloatRaw aPtr bPtr resultPtr
    if success
      then Just <$> peek resultPtr
      else return Nothing

sdlGetRectUnionFloat :: MonadIO m => SDLFRect -> SDLFRect -> m (Maybe SDLFRect)
sdlGetRectUnionFloat a b = liftIO $
  with a $ \aPtr ->
  with b $ \bPtr ->
  with (SDLFRect 0 0 0 0) $ \resultPtr -> do
    success <- Raw.sdlGetRectUnionFloatRaw aPtr bPtr resultPtr
    if success
      then Just <$> peek resultPtr
      else return Nothing

sdlGetRectEnclosingPointsFloat :: MonadIO m => [SDLFPoint] -> Maybe SDLFRect -> m (Maybe SDLFRect)
sdlGetRectEnclosingPointsFloat points mClip = liftIO $
  withArray points $ \pointsPtr -> do
    let count = fromIntegral $ length points
    withMaybe mClip $ \clipPtr ->
      with (SDLFRect 0 0 0 0) $ \resultPtr -> do
        success <- Raw.sdlGetRectEnclosingPointsFloatRaw pointsPtr count clipPtr resultPtr
        if success
          then Just <$> peek resultPtr
          else return Nothing
  where
    withMaybe Nothing f = f nullPtr
    withMaybe (Just x) f = with x f

sdlGetRectAndLineIntersectionFloat :: MonadIO m => SDLFRect -> (CFloat, CFloat) -> (CFloat, CFloat) -> m (Maybe ((CFloat, CFloat), (CFloat, CFloat)))
sdlGetRectAndLineIntersectionFloat rect (x1, y1) (x2, y2) = liftIO $
  with rect $ \rectPtr ->
  with x1 $ \x1Ptr ->
  with y1 $ \y1Ptr ->
  with x2 $ \x2Ptr ->
  with y2 $ \y2Ptr -> do
    success <- Raw.sdlGetRectAndLineIntersectionFloatRaw rectPtr x1Ptr y1Ptr x2Ptr y2Ptr
    if success
      then do
        x1' <- peek x1Ptr
        y1' <- peek y1Ptr
        x2' <- peek x2Ptr
        y2' <- peek y2Ptr
        return $ Just ((x1', y1'), (x2', y2'))
      else return Nothing
