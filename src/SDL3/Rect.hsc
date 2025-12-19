{-|
Module      : SDL.Rect
Description : Helper functions for managing rectangles and 2D points
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

This module provides helper functions for managing rectangles and 2D points,
in both integer and floating-point versions. These utilities are useful for
tasks such as collision detection, bounding box calculations, and geometric
operations in 2D space.
-}
module SDL3.Rect
  ( -- * Data Types
    SDLPoint(..)
  , SDLFPoint(..)
  , SDLRect(..)
  , SDLFRect(..)

    -- * Rectangle Conversion
  , sdlRectToFRect

    -- * Integer Point and Rectangle Functions
  , sdlPointInRect
  , sdlRectEmpty
  , sdlRectsEqual
  , sdlHasRectIntersection
  , sdlGetRectIntersection
  , sdlGetRectUnion
  , sdlGetRectEnclosingPoints
  , sdlGetRectAndLineIntersection

    -- * Floating-Point Point and Rectangle Functions
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

#include <SDL3/SDL_rect.h>

import Foreign.C.Types
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (with)

-- | A structure that defines a point using integers (SDL_Point).
data SDLPoint = SDLPoint
  { pointX :: CInt  -- ^ x coordinate
  , pointY :: CInt  -- ^ y coordinate
  } deriving (Eq, Show)

instance Storable SDLPoint where
  sizeOf _ = #{size SDL_Point}
  alignment _ = #{alignment SDL_Point}
  peek ptr = do
    x <- #{peek SDL_Point, x} ptr
    y <- #{peek SDL_Point, y} ptr
    return $ SDLPoint x y
  poke ptr (SDLPoint x y) = do
    #{poke SDL_Point, x} ptr x
    #{poke SDL_Point, y} ptr y

-- | A structure that defines a point using floating-point values (SDL_FPoint).
data SDLFPoint = SDLFPoint
  { fPointX :: CFloat  -- ^ x coordinate
  , fPointY :: CFloat  -- ^ y coordinate
  } deriving (Eq, Show)

instance Storable SDLFPoint where
  sizeOf _ = #{size SDL_FPoint}
  alignment _ = #{alignment SDL_FPoint}
  peek ptr = do
    x <- #{peek SDL_FPoint, x} ptr
    y <- #{peek SDL_FPoint, y} ptr
    return $ SDLFPoint x y
  poke ptr (SDLFPoint x y) = do
    #{poke SDL_FPoint, x} ptr x
    #{poke SDL_FPoint, y} ptr y

-- | A rectangle with the origin at the upper left, using integers (SDL_Rect).
data SDLRect = SDLRect
  { rectX :: CInt  -- ^ x coordinate of top-left corner
  , rectY :: CInt  -- ^ y coordinate of top-left corner
  , rectW :: CInt  -- ^ width
  , rectH :: CInt  -- ^ height
  } deriving (Eq, Show)

instance Storable SDLRect where
  sizeOf _ = #{size SDL_Rect}
  alignment _ = #{alignment SDL_Rect}
  peek ptr = do
    x <- #{peek SDL_Rect, x} ptr
    y <- #{peek SDL_Rect, y} ptr
    w <- #{peek SDL_Rect, w} ptr
    h <- #{peek SDL_Rect, h} ptr
    return $ SDLRect x y w h
  poke ptr (SDLRect x y w h) = do
    #{poke SDL_Rect, x} ptr x
    #{poke SDL_Rect, y} ptr y
    #{poke SDL_Rect, w} ptr w
    #{poke SDL_Rect, h} ptr h

-- | A rectangle with the origin at the upper left, using floating-point values (SDL_FRect).
data SDLFRect = SDLFRect
  { fRectX :: CFloat  -- ^ x coordinate of top-left corner
  , fRectY :: CFloat  -- ^ y coordinate of top-left corner
  , fRectW :: CFloat  -- ^ width
  , fRectH :: CFloat  -- ^ height
  } deriving (Eq, Show)

instance Storable SDLFRect where
  sizeOf _ = #{size SDL_FRect}
  alignment _ = #{alignment SDL_FRect}
  peek ptr = do
    x <- #{peek SDL_FRect, x} ptr
    y <- #{peek SDL_FRect, y} ptr
    w <- #{peek SDL_FRect, w} ptr
    h <- #{peek SDL_FRect, h} ptr
    return $ SDLFRect x y w h
  poke ptr (SDLFRect x y w h) = do
    #{poke SDL_FRect, x} ptr x
    #{poke SDL_FRect, y} ptr y
    #{poke SDL_FRect, w} ptr w
    #{poke SDL_FRect, h} ptr h

-- | Convert an SDL_Rect to SDL_FRect (SDL_RectToFRect).
sdlRectToFRect :: SDLRect -> SDLFRect
sdlRectToFRect (SDLRect x y w h) =
  SDLFRect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

-- | Determine whether a point resides inside a rectangle (SDL_PointInRect).
sdlPointInRect :: SDLPoint -> SDLRect -> Bool
sdlPointInRect p r =
  pointX p >= rectX r &&
  pointX p < (rectX r + rectW r) &&
  pointY p >= rectY r &&
  pointY p < (rectY r + rectH r)

-- | Determine whether a rectangle has no area (SDL_RectEmpty).
sdlRectEmpty :: SDLRect -> Bool
sdlRectEmpty r = rectW r <= 0 || rectH r <= 0

-- | Determine whether two rectangles are equal (SDL_RectsEqual).
sdlRectsEqual :: SDLRect -> SDLRect -> Bool
sdlRectsEqual a b =
  rectX a == rectX b &&
  rectY a == rectY b &&
  rectW a == rectW b &&
  rectH a == rectH b

-- | Determine whether two rectangles intersect (SDL_HasRectIntersection).
foreign import ccall "SDL_HasRectIntersection"
  sdlHasRectIntersection :: Ptr SDLRect -> Ptr SDLRect -> IO Bool

-- | Calculate the intersection of two rectangles (SDL_GetRectIntersection).
foreign import ccall "SDL_GetRectIntersection"
  sdlGetRectIntersectionRaw :: Ptr SDLRect -> Ptr SDLRect -> Ptr SDLRect -> IO Bool

-- | Haskell wrapper for SDL_GetRectIntersection.
sdlGetRectIntersection :: SDLRect -> SDLRect -> IO (Maybe SDLRect)
sdlGetRectIntersection a b = with a $ \aPtr -> with b $ \bPtr -> with (SDLRect 0 0 0 0) $ \resultPtr -> do
  success <- sdlGetRectIntersectionRaw aPtr bPtr resultPtr
  if success
    then Just <$> peek resultPtr
    else return Nothing

-- | Calculate the union of two rectangles (SDL_GetRectUnion).
foreign import ccall "SDL_GetRectUnion"
  sdlGetRectUnionRaw :: Ptr SDLRect -> Ptr SDLRect -> Ptr SDLRect -> IO Bool

-- | Haskell wrapper for SDL_GetRectUnion.
sdlGetRectUnion :: SDLRect -> SDLRect -> IO (Maybe SDLRect)
sdlGetRectUnion a b = with a $ \aPtr -> with b $ \bPtr -> with (SDLRect 0 0 0 0) $ \resultPtr -> do
  success <- sdlGetRectUnionRaw aPtr bPtr resultPtr
  if success
    then Just <$> peek resultPtr
    else return Nothing

-- | Calculate a minimal rectangle enclosing a set of points (SDL_GetRectEnclosingPoints).
foreign import ccall "SDL_GetRectEnclosingPoints"
  sdlGetRectEnclosingPointsRaw :: Ptr SDLPoint -> CInt -> Ptr SDLRect -> Ptr SDLRect -> IO Bool

-- | Haskell wrapper for SDL_GetRectEnclosingPoints.
sdlGetRectEnclosingPoints :: [SDLPoint] -> Maybe SDLRect -> IO (Maybe SDLRect)
sdlGetRectEnclosingPoints points mClip = withArray points $ \pointsPtr -> do
  let count = fromIntegral $ length points
  withMaybe mClip $ \clipPtr -> with (SDLRect 0 0 0 0) $ \resultPtr -> do
    success <- sdlGetRectEnclosingPointsRaw pointsPtr count clipPtr resultPtr
    if success
      then Just <$> peek resultPtr
      else return Nothing
  where
    withMaybe Nothing f = f nullPtr
    withMaybe (Just x) f = with x f

-- | Calculate the intersection of a rectangle and line segment (SDL_GetRectAndLineIntersection).
foreign import ccall "SDL_GetRectAndLineIntersection"
  sdlGetRectAndLineIntersectionRaw :: Ptr SDLRect -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO Bool

-- | Haskell wrapper for SDL_GetRectAndLineIntersection.
sdlGetRectAndLineIntersection :: SDLRect -> (CInt, CInt) -> (CInt, CInt) -> IO (Maybe ((CInt, CInt), (CInt, CInt)))
sdlGetRectAndLineIntersection rect (x1, y1) (x2, y2) =
  with rect $ \rectPtr ->
  with x1 $ \x1Ptr ->
  with y1 $ \y1Ptr ->
  with x2 $ \x2Ptr ->
  with y2 $ \y2Ptr -> do
    success <- sdlGetRectAndLineIntersectionRaw rectPtr x1Ptr y1Ptr x2Ptr y2Ptr
    if success
      then do
        x1' <- peek x1Ptr
        y1' <- peek y1Ptr
        x2' <- peek x2Ptr
        y2' <- peek y2Ptr
        return $ Just ((x1', y1'), (x2', y2'))
      else return Nothing

-- | Determine whether a point resides inside a floating-point rectangle (SDL_PointInRectFloat).
sdlPointInRectFloat :: SDLFPoint -> SDLFRect -> Bool
sdlPointInRectFloat p r =
  fPointX p >= fRectX r &&
  fPointX p <= (fRectX r + fRectW r) &&
  fPointY p >= fRectY r &&
  fPointY p <= (fRectY r + fRectH r)

-- | Determine whether a floating-point rectangle can contain any point (SDL_RectEmptyFloat).
sdlRectEmptyFloat :: SDLFRect -> Bool
sdlRectEmptyFloat r = fRectW r < 0 || fRectH r < 0

-- | Determine whether two floating-point rectangles are equal within some epsilon (SDL_RectsEqualEpsilon).
sdlRectsEqualEpsilon :: SDLFRect -> SDLFRect -> CFloat -> Bool
sdlRectsEqualEpsilon a b epsilon =
  a == b ||
  (abs (fRectX a - fRectX b) <= epsilon &&
   abs (fRectY a - fRectY b) <= epsilon &&
   abs (fRectW a - fRectW b) <= epsilon &&
   abs (fRectH a - fRectH b) <= epsilon)

-- | Determine whether two floating-point rectangles are equal within SDL_FLT_EPSILON (SDL_RectsEqualFloat).
sdlRectsEqualFloat :: SDLFRect -> SDLFRect -> Bool
sdlRectsEqualFloat a b = sdlRectsEqualEpsilon a b (#const SDL_FLT_EPSILON)

-- | Determine whether two floating-point rectangles intersect (SDL_HasRectIntersectionFloat).
foreign import ccall "SDL_HasRectIntersectionFloat"
  sdlHasRectIntersectionFloat :: Ptr SDLFRect -> Ptr SDLFRect -> IO Bool

-- | Calculate the intersection of two floating-point rectangles (SDL_GetRectIntersectionFloat).
foreign import ccall "SDL_GetRectIntersectionFloat"
  sdlGetRectIntersectionFloatRaw :: Ptr SDLFRect -> Ptr SDLFRect -> Ptr SDLFRect -> IO Bool

-- | Haskell wrapper for SDL_GetRectIntersectionFloat.
sdlGetRectIntersectionFloat :: SDLFRect -> SDLFRect -> IO (Maybe SDLFRect)
sdlGetRectIntersectionFloat a b = with a $ \aPtr -> with b $ \bPtr -> with (SDLFRect 0 0 0 0) $ \resultPtr -> do
  success <- sdlGetRectIntersectionFloatRaw aPtr bPtr resultPtr
  if success
    then Just <$> peek resultPtr
    else return Nothing

-- | Calculate the union of two floating-point rectangles (SDL_GetRectUnionFloat).
foreign import ccall "SDL_GetRectUnionFloat"
  sdlGetRectUnionFloatRaw :: Ptr SDLFRect -> Ptr SDLFRect -> Ptr SDLFRect -> IO Bool

-- | Haskell wrapper for SDL_GetRectUnionFloat.
sdlGetRectUnionFloat :: SDLFRect -> SDLFRect -> IO (Maybe SDLFRect)
sdlGetRectUnionFloat a b = with a $ \aPtr -> with b $ \bPtr -> with (SDLFRect 0 0 0 0) $ \resultPtr -> do
  success <- sdlGetRectUnionFloatRaw aPtr bPtr resultPtr
  if success
    then Just <$> peek resultPtr
    else return Nothing

-- | Calculate a minimal rectangle enclosing a set of floating-point points (SDL_GetRectEnclosingPointsFloat).
foreign import ccall "SDL_GetRectEnclosingPointsFloat"
  sdlGetRectEnclosingPointsFloatRaw :: Ptr SDLFPoint -> CInt -> Ptr SDLFRect -> Ptr SDLFRect -> IO Bool

-- | Haskell wrapper for SDL_GetRectEnclosingPointsFloat.
sdlGetRectEnclosingPointsFloat :: [SDLFPoint] -> Maybe SDLFRect -> IO (Maybe SDLFRect)
sdlGetRectEnclosingPointsFloat points mClip = withArray points $ \pointsPtr -> do
  let count = fromIntegral $ length points
  withMaybe mClip $ \clipPtr -> with (SDLFRect 0 0 0 0) $ \resultPtr -> do
    success <- sdlGetRectEnclosingPointsFloatRaw pointsPtr count clipPtr resultPtr
    if success
      then Just <$> peek resultPtr
      else return Nothing
  where
    withMaybe Nothing f = f nullPtr
    withMaybe (Just x) f = with x f

-- | Calculate the intersection of a floating-point rectangle and line segment (SDL_GetRectAndLineIntersectionFloat).
foreign import ccall "SDL_GetRectAndLineIntersectionFloat"
  sdlGetRectAndLineIntersectionFloatRaw :: Ptr SDLFRect -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO Bool

-- | Haskell wrapper for SDL_GetRectAndLineIntersectionFloat.
sdlGetRectAndLineIntersectionFloat :: SDLFRect -> (CFloat, CFloat) -> (CFloat, CFloat) -> IO (Maybe ((CFloat, CFloat), (CFloat, CFloat)))
sdlGetRectAndLineIntersectionFloat rect (x1, y1) (x2, y2) =
  with rect $ \rectPtr ->
  with x1 $ \x1Ptr ->
  with y1 $ \y1Ptr ->
  with x2 $ \x2Ptr ->
  with y2 $ \y2Ptr -> do
    success <- sdlGetRectAndLineIntersectionFloatRaw rectPtr x1Ptr y1Ptr x2Ptr y2Ptr
    if success
      then do
        x1' <- peek x1Ptr
        y1' <- peek y1Ptr
        x2' <- peek x2Ptr
        y2' <- peek y2Ptr
        return $ Just ((x1', y1'), (x2', y2'))
      else return Nothing
