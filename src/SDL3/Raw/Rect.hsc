{-# LANGUAGE ForeignFunctionInterface #-}

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
module SDL3.Raw.Rect
  ( SDLPoint(..)
  , SDLFPoint(..)
  , SDLRect(..)
  , SDLFRect(..)
  , sdlRectToFRect
  , sdlPointInRect
  , sdlRectEmpty
  , sdlRectsEqual
  , sdlHasRectIntersectionRaw
  , sdlGetRectIntersectionRaw
  , sdlGetRectUnionRaw
  , sdlGetRectEnclosingPointsRaw
  , sdlGetRectAndLineIntersectionRaw
  , sdlPointInRectFloat
  , sdlRectEmptyFloat
  , sdlRectsEqualEpsilon
  , sdlRectsEqualFloat
  , sdlHasRectIntersectionFloatRaw
  , sdlGetRectIntersectionFloatRaw
  , sdlGetRectUnionFloatRaw
  , sdlGetRectEnclosingPointsFloatRaw
  , sdlGetRectAndLineIntersectionFloatRaw
  ) where

#include <SDL3/SDL_rect.h>

import Foreign.C.Types
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))

-- | A structure that defines a point using integers (SDL_Point).
data SDLPoint = SDLPoint
  { pointX :: CInt
  , pointY :: CInt
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
  { fPointX :: CFloat
  , fPointY :: CFloat
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
  { rectX :: CInt
  , rectY :: CInt
  , rectW :: CInt
  , rectH :: CInt
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
  { fRectX :: CFloat
  , fRectY :: CFloat
  , fRectW :: CFloat
  , fRectH :: CFloat
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

sdlRectToFRect :: SDLRect -> SDLFRect
sdlRectToFRect (SDLRect x y w h) =
  SDLFRect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

sdlPointInRect :: SDLPoint -> SDLRect -> Bool
sdlPointInRect p r =
  pointX p >= rectX r &&
  pointX p < (rectX r + rectW r) &&
  pointY p >= rectY r &&
  pointY p < (rectY r + rectH r)

sdlRectEmpty :: SDLRect -> Bool
sdlRectEmpty r = rectW r <= 0 || rectH r <= 0

sdlRectsEqual :: SDLRect -> SDLRect -> Bool
sdlRectsEqual a b =
  rectX a == rectX b &&
  rectY a == rectY b &&
  rectW a == rectW b &&
  rectH a == rectH b

foreign import ccall "SDL_HasRectIntersection"
  sdlHasRectIntersectionRaw :: Ptr SDLRect -> Ptr SDLRect -> IO Bool

foreign import ccall "SDL_GetRectIntersection"
  sdlGetRectIntersectionRaw :: Ptr SDLRect -> Ptr SDLRect -> Ptr SDLRect -> IO Bool

foreign import ccall "SDL_GetRectUnion"
  sdlGetRectUnionRaw :: Ptr SDLRect -> Ptr SDLRect -> Ptr SDLRect -> IO Bool

foreign import ccall "SDL_GetRectEnclosingPoints"
  sdlGetRectEnclosingPointsRaw :: Ptr SDLPoint -> CInt -> Ptr SDLRect -> Ptr SDLRect -> IO Bool

foreign import ccall "SDL_GetRectAndLineIntersection"
  sdlGetRectAndLineIntersectionRaw :: Ptr SDLRect -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO Bool

sdlPointInRectFloat :: SDLFPoint -> SDLFRect -> Bool
sdlPointInRectFloat p r =
  fPointX p >= fRectX r &&
  fPointX p <= (fRectX r + fRectW r) &&
  fPointY p >= fRectY r &&
  fPointY p <= (fRectY r + fRectH r)

sdlRectEmptyFloat :: SDLFRect -> Bool
sdlRectEmptyFloat r = fRectW r < 0 || fRectH r < 0

sdlRectsEqualEpsilon :: SDLFRect -> SDLFRect -> CFloat -> Bool
sdlRectsEqualEpsilon a b epsilon =
  a == b ||
  (abs (fRectX a - fRectX b) <= epsilon &&
   abs (fRectY a - fRectY b) <= epsilon &&
   abs (fRectW a - fRectW b) <= epsilon &&
   abs (fRectH a - fRectH b) <= epsilon)

sdlRectsEqualFloat :: SDLFRect -> SDLFRect -> Bool
sdlRectsEqualFloat a b = sdlRectsEqualEpsilon a b (#const SDL_FLT_EPSILON)

foreign import ccall "SDL_HasRectIntersectionFloat"
  sdlHasRectIntersectionFloatRaw :: Ptr SDLFRect -> Ptr SDLFRect -> IO Bool

foreign import ccall "SDL_GetRectIntersectionFloat"
  sdlGetRectIntersectionFloatRaw :: Ptr SDLFRect -> Ptr SDLFRect -> Ptr SDLFRect -> IO Bool

foreign import ccall "SDL_GetRectUnionFloat"
  sdlGetRectUnionFloatRaw :: Ptr SDLFRect -> Ptr SDLFRect -> Ptr SDLFRect -> IO Bool

foreign import ccall "SDL_GetRectEnclosingPointsFloat"
  sdlGetRectEnclosingPointsFloatRaw :: Ptr SDLFPoint -> CInt -> Ptr SDLFRect -> Ptr SDLFRect -> IO Bool

foreign import ccall "SDL_GetRectAndLineIntersectionFloat"
  sdlGetRectAndLineIntersectionFloatRaw :: Ptr SDLFRect -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO Bool
