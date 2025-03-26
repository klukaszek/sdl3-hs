{-|
Module      : SDL.Surface
Description : Bindings to SDL surfaces for pixel buffer management
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

This module provides bindings to SDL surfaces, which are buffers of pixels in system RAM.
Surfaces are useful for manipulating images not stored in GPU memory, supporting various
pixel formats and operations like blitting, scaling, and color manipulation.

SDL surfaces include tools for creating, modifying, and converting pixel data, as well
as loading and saving BMP files. For advanced image format support, consider using the
SDL_image library.
-}
module SDL.Surface
  ( -- * Data Types
    SDLSurface(..)
  , SDLSurfaceFlags(..)
  , SDLScaleMode(..)
  , SDLFlipMode(..)

    -- * Surface Creation and Destruction
  , sdlCreateSurface
  , sdlCreateSurfaceFrom
  , sdlDestroySurface

    -- * Surface Properties
  , sdlGetSurfaceProperties
  , SDLSurfaceProperty(..)

    -- * Surface Colorspace Management
  , sdlSetSurfaceColorspace
  , sdlGetSurfaceColorspace

    -- * Surface Palette Management
  , sdlCreateSurfacePalette
  , sdlSetSurfacePalette
  , sdlGetSurfacePalette

    -- * Surface Locking
  , sdlLockSurface
  , sdlUnlockSurface
  , sdlMustLock

    -- * BMP File Operations
  , sdlLoadBMP
  , sdlSaveBMP

    -- * Surface Blitting
  , sdlBlitSurface
  , sdlBlitSurfaceScaled

    -- * Surface Transformations
  , sdlFlipSurface
  , sdlScaleSurface
  , sdlDuplicateSurface

    -- * Pixel Operations
  , sdlMapSurfaceRGB
  , sdlMapSurfaceRGBA
  , sdlReadSurfacePixel
  , sdlWriteSurfacePixel
  ) where

#include <SDL3/SDL_surface.h>

import Foreign.C.Types
import Foreign.Ptr (Ptr, nullPtr, FunPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Utils (with)
import Foreign.C.String (CString, withCString)
import Control.Monad (when)
import Data.Word
import SDL.Pixels (SDLColorspace, SDLPixelFormat, SDLPalette, pixelFormatToWord32)
import SDL.Properties (SDLPropertiesID)
import SDL.Rect (SDLRect)
import SDL.Stdinc (SDLBool)

-- | Flags associated with an SDL surface, generally read-only.
newtype SDLSurfaceFlags = SDLSurfaceFlags Word32
  deriving (Eq, Show)

#{enum SDLSurfaceFlags, SDLSurfaceFlags
 , sdlSurfacePreallocated = SDL_SURFACE_PREALLOCATED
 , sdlSurfaceLockNeeded = SDL_SURFACE_LOCK_NEEDED
 , sdlSurfaceLocked = SDL_SURFACE_LOCKED
 , sdlSurfaceSimdAligned = SDL_SURFACE_SIMD_ALIGNED
 }

instance Enum SDLSurfaceFlags where
  fromEnum (SDLSurfaceFlags w) = fromIntegral w
  toEnum n = SDLSurfaceFlags (fromIntegral n)

-- | Scaling mode for surface operations.
data SDLScaleMode
  = SDLScaleModeNearest   -- ^ Nearest pixel sampling
  | SDLScaleModeLinear    -- ^ Linear filtering
  | SDLScaleModePixelArt  -- ^ Improved scaling for pixel art
  deriving (Eq, Show)

instance Enum SDLScaleMode where
  fromEnum SDLScaleModeNearest  = #{const SDL_SCALEMODE_NEAREST}
  fromEnum SDLScaleModeLinear   = #{const SDL_SCALEMODE_LINEAR}
  fromEnum SDLScaleModePixelArt = #{const SDL_SCALEMODE_PIXELART}
  toEnum n
    | n == #{const SDL_SCALEMODE_NEAREST}  = SDLScaleModeNearest
    | n == #{const SDL_SCALEMODE_LINEAR}   = SDLScaleModeLinear
    | n == #{const SDL_SCALEMODE_PIXELART} = SDLScaleModePixelArt
    | otherwise                            = error "Invalid SDLScaleMode"

-- | Flip mode for surface transformations.
data SDLFlipMode
  = SDLFlipNone        -- ^ Do not flip
  | SDLFlipHorizontal  -- ^ Flip horizontally
  | SDLFlipVertical    -- ^ Flip vertically
  deriving (Eq, Show)

instance Enum SDLFlipMode where
  fromEnum SDLFlipNone       = #{const SDL_FLIP_NONE}
  fromEnum SDLFlipHorizontal = #{const SDL_FLIP_HORIZONTAL}
  fromEnum SDLFlipVertical   = #{const SDL_FLIP_VERTICAL}
  toEnum n
    | n == #{const SDL_FLIP_NONE}       = SDLFlipNone
    | n == #{const SDL_FLIP_HORIZONTAL} = SDLFlipHorizontal
    | n == #{const SDL_FLIP_VERTICAL}   = SDLFlipVertical
    | otherwise                         = error "Invalid SDLFlipMode"

-- | A collection of pixels used in software blitting (SDL_Surface).
data SDLSurface = SDLSurface
  { sdlSurfaceFlags   :: SDLSurfaceFlags  -- ^ Surface flags, read-only
  , sdlSurfaceFormat  :: SDLPixelFormat   -- ^ Pixel format, read-only
  , sdlSurfaceWidth   :: CInt             -- ^ Width, read-only
  , sdlSurfaceHeight  :: CInt             -- ^ Height, read-only
  , sdlSurfacePitch   :: CInt             -- ^ Bytes between rows, read-only
  , sdlSurfacePixels  :: Ptr ()           -- ^ Pointer to pixel data
  , sdlSurfaceRefcount :: CInt            -- ^ Reference count
  } deriving (Eq, Show)

instance Storable SDLSurface where
  sizeOf _ = #{size SDL_Surface}
  alignment _ = #{alignment SDL_Surface}
  peek ptr = do
    flags    <- SDLSurfaceFlags <$> #{peek SDL_Surface, flags} ptr
    format   <- toEnum . fromIntegral <$> (#{peek SDL_Surface, format} ptr :: IO Word32)  -- Convert Word32 to SDLPixelFormat
    w        <- #{peek SDL_Surface, w} ptr
    h        <- #{peek SDL_Surface, h} ptr
    pitch    <- #{peek SDL_Surface, pitch} ptr
    pixels   <- #{peek SDL_Surface, pixels} ptr
    refcount <- #{peek SDL_Surface, refcount} ptr
    return $ SDLSurface flags format w h pitch pixels refcount
  poke ptr (SDLSurface flags format w h pitch pixels refcount) = do
    #{poke SDL_Surface, flags} ptr (fromIntegral (fromEnum flags) :: Word32)
    #{poke SDL_Surface, format} ptr (pixelFormatToWord32 format :: Word32)  -- Convert SDLPixelFormat to Word32
    #{poke SDL_Surface, w} ptr w
    #{poke SDL_Surface, h} ptr h
    #{poke SDL_Surface, pitch} ptr pitch
    #{poke SDL_Surface, pixels} ptr pixels
    #{poke SDL_Surface, refcount} ptr refcount
    
-- | Properties associated with a surface.
data SDLSurfaceProperty
  = SDLSurfaceSDRWhitePoint Float  -- ^ Value of 100% diffuse white
  | SDLSurfaceHDRHeadroom Float    -- ^ Maximum dynamic range
  | SDLSurfaceTonemapOperator String -- ^ Tone mapping operator
  | SDLSurfaceHotspotX CInt         -- ^ Hotspot x offset
  | SDLSurfaceHotspotY CInt         -- ^ Hotspot y offset
  deriving (Eq, Show)

-- | Check if a surface needs locking before pixel access.
sdlMustLock :: SDLSurface -> Bool
sdlMustLock surface = sdlSurfaceFlags surface == sdlSurfaceLockNeeded

-- | Create a new surface with a specific pixel format.
foreign import ccall "SDL_CreateSurface"
  sdlCreateSurfaceRaw :: CInt -> CInt -> Word32 -> IO (Ptr SDLSurface)

sdlCreateSurface :: CInt -> CInt -> Word32 -> IO (Maybe (Ptr SDLSurface))
sdlCreateSurface width height format = do
  ptr <- sdlCreateSurfaceRaw width height format
  return $ if ptr == nullPtr then Nothing else Just ptr

-- | Create a new surface from existing pixel data.
foreign import ccall "SDL_CreateSurfaceFrom"
  sdlCreateSurfaceFromRaw :: CInt -> CInt -> Word32 -> Ptr () -> CInt -> IO (Ptr SDLSurface)

sdlCreateSurfaceFrom :: CInt -> CInt -> Word32 -> Ptr () -> CInt -> IO (Maybe (Ptr SDLSurface))
sdlCreateSurfaceFrom width height format pixels pitch = do
  ptr <- sdlCreateSurfaceFromRaw width height format pixels pitch
  return $ if ptr == nullPtr then Nothing else Just ptr

-- | Free a surface.
foreign import ccall "SDL_DestroySurface"
  sdlDestroySurface :: Ptr SDLSurface -> IO ()

-- | Get the properties associated with a surface.
foreign import ccall "SDL_GetSurfaceProperties"
  sdlGetSurfaceProperties :: Ptr SDLSurface -> IO SDLPropertiesID

-- | Set the colorspace used by a surface.
foreign import ccall "SDL_SetSurfaceColorspace"
  sdlSetSurfaceColorspaceRaw :: Ptr SDLSurface -> Word32 -> IO SDLBool

sdlSetSurfaceColorspace :: Ptr SDLSurface -> Word32 -> IO Bool
sdlSetSurfaceColorspace surface colorspace = fromSDLBool <$> sdlSetSurfaceColorspaceRaw surface colorspace

-- | Get the colorspace used by a surface.
foreign import ccall "SDL_GetSurfaceColorspace"
  sdlGetSurfaceColorspaceRaw :: Ptr SDLSurface -> IO Word32
sdlGetSurfaceColorspace :: Ptr SDLSurface -> IO SDLColorspace
sdlGetSurfaceColorspace surface = toEnum . fromIntegral <$> sdlGetSurfaceColorspaceRaw surface

-- | Create a palette and associate it with a surface.
foreign import ccall "SDL_CreateSurfacePalette"
  sdlCreateSurfacePalette :: Ptr SDLSurface -> IO (Ptr SDLPalette)

-- | Set the palette used by a surface.
foreign import ccall "SDL_SetSurfacePalette"
  sdlSetSurfacePaletteRaw :: Ptr SDLSurface -> Ptr SDLPalette -> IO SDLBool

sdlSetSurfacePalette :: Ptr SDLSurface -> Ptr SDLPalette -> IO Bool
sdlSetSurfacePalette surface palette = fromSDLBool <$> sdlSetSurfacePaletteRaw surface palette

-- | Get the palette used by a surface.
foreign import ccall "SDL_GetSurfacePalette"
  sdlGetSurfacePalette :: Ptr SDLSurface -> IO (Ptr SDLPalette)

-- | Lock a surface for direct pixel access.
foreign import ccall "SDL_LockSurface"
  sdlLockSurfaceRaw :: Ptr SDLSurface -> IO SDLBool

sdlLockSurface :: Ptr SDLSurface -> IO Bool
sdlLockSurface surface = fromSDLBool <$> sdlLockSurfaceRaw surface

-- | Unlock a surface after direct pixel access.
foreign import ccall "SDL_UnlockSurface"
  sdlUnlockSurface :: Ptr SDLSurface -> IO ()

-- | Load a BMP image from a file.
foreign import ccall "SDL_LoadBMP"
  sdlLoadBMPRaw :: CString -> IO (Ptr SDLSurface)

sdlLoadBMP :: String -> IO (Maybe (Ptr SDLSurface))
sdlLoadBMP file = do
  ptr <- withCString file sdlLoadBMPRaw
  return $ if ptr == nullPtr then Nothing else Just ptr

-- | Save a surface to a BMP file.
foreign import ccall "SDL_SaveBMP"
  sdlSaveBMPRaw :: Ptr SDLSurface -> CString -> IO SDLBool

sdlSaveBMP :: Ptr SDLSurface -> String -> IO Bool
sdlSaveBMP surface file = withCString file $ \cstr -> fromSDLBool <$> sdlSaveBMPRaw surface cstr

-- | Perform a fast blit from source to destination surface.
foreign import ccall "SDL_BlitSurface"
  sdlBlitSurfaceRaw :: Ptr SDLSurface -> Ptr SDLRect -> Ptr SDLSurface -> Ptr SDLRect -> IO SDLBool

sdlBlitSurface :: Ptr SDLSurface -> Maybe SDLRect -> Ptr SDLSurface -> Maybe SDLRect -> IO Bool
sdlBlitSurface src mSrcRect dst mDstRect =
  withMaybe mSrcRect $ \srcPtr ->
  withMaybe mDstRect $ \dstPtr ->
    fromSDLBool <$> sdlBlitSurfaceRaw src srcPtr dst dstPtr
  where
    withMaybe Nothing f = f nullPtr
    withMaybe (Just x) f = with x f

-- | Perform a scaled blit to a destination surface.
foreign import ccall "SDL_BlitSurfaceScaled"
  sdlBlitSurfaceScaledRaw :: Ptr SDLSurface -> Ptr SDLRect -> Ptr SDLSurface -> Ptr SDLRect -> CInt -> IO SDLBool

sdlBlitSurfaceScaled :: Ptr SDLSurface -> Maybe SDLRect -> Ptr SDLSurface -> Maybe SDLRect -> SDLScaleMode -> IO Bool
sdlBlitSurfaceScaled src mSrcRect dst mDstRect scaleMode =
  withMaybe mSrcRect $ \srcPtr ->
  withMaybe mDstRect $ \dstPtr ->
    fromSDLBool <$> sdlBlitSurfaceScaledRaw src srcPtr dst dstPtr (fromIntegral $ fromEnum scaleMode)
  where
    withMaybe Nothing f = f nullPtr
    withMaybe (Just x) f = with x f

-- | Flip a surface vertically or horizontally.
foreign import ccall "SDL_FlipSurface"
  sdlFlipSurfaceRaw :: Ptr SDLSurface -> CInt -> IO SDLBool

sdlFlipSurface :: Ptr SDLSurface -> SDLFlipMode -> IO Bool
sdlFlipSurface surface flip = fromSDLBool <$> sdlFlipSurfaceRaw surface (fromIntegral $ fromEnum flip)

-- | Scale a surface to a new size.
foreign import ccall "SDL_ScaleSurface"
  sdlScaleSurfaceRaw :: Ptr SDLSurface -> CInt -> CInt -> CInt -> IO (Ptr SDLSurface)

sdlScaleSurface :: Ptr SDLSurface -> CInt -> CInt -> SDLScaleMode -> IO (Maybe (Ptr SDLSurface))
sdlScaleSurface surface width height scaleMode = do
  ptr <- sdlScaleSurfaceRaw surface width height (fromIntegral $ fromEnum scaleMode)
  return $ if ptr == nullPtr then Nothing else Just ptr

-- | Duplicate a surface.
foreign import ccall "SDL_DuplicateSurface"
  sdlDuplicateSurfaceRaw :: Ptr SDLSurface -> IO (Ptr SDLSurface)

sdlDuplicateSurface :: Ptr SDLSurface -> IO (Maybe (Ptr SDLSurface))
sdlDuplicateSurface surface = do
  ptr <- sdlDuplicateSurfaceRaw surface
  return $ if ptr == nullPtr then Nothing else Just ptr

-- | Map an RGB triple to a pixel value for a surface.
foreign import ccall "SDL_MapSurfaceRGB"
  sdlMapSurfaceRGB :: Ptr SDLSurface -> Word8 -> Word8 -> Word8 -> IO Word32

-- | Map an RGBA quadruple to a pixel value for a surface.
foreign import ccall "SDL_MapSurfaceRGBA"
  sdlMapSurfaceRGBA :: Ptr SDLSurface -> Word8 -> Word8 -> Word8 -> Word8 -> IO Word32

-- | Read a single pixel from a surface.
foreign import ccall "SDL_ReadSurfacePixel"
  sdlReadSurfacePixelRaw :: Ptr SDLSurface -> CInt -> CInt -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO SDLBool

sdlReadSurfacePixel :: Ptr SDLSurface -> CInt -> CInt -> IO (Maybe (Word8, Word8, Word8, Word8))
sdlReadSurfacePixel surface x y =
  with 0 $ \rPtr ->
  with 0 $ \gPtr ->
  with 0 $ \bPtr ->
  with 0 $ \aPtr -> do
    success <- sdlReadSurfacePixelRaw surface x y rPtr gPtr bPtr aPtr
    if fromSDLBool success
      then do
        r <- peek rPtr
        g <- peek gPtr
        b <- peek bPtr
        a <- peek aPtr
        return $ Just (r, g, b, a)
      else return Nothing

-- | Write a single pixel to a surface.
foreign import ccall "SDL_WriteSurfacePixel"
  sdlWriteSurfacePixelRaw :: Ptr SDLSurface -> CInt -> CInt -> Word8 -> Word8 -> Word8 -> Word8 -> IO SDLBool

sdlWriteSurfacePixel :: Ptr SDLSurface -> CInt -> CInt -> Word8 -> Word8 -> Word8 -> Word8 -> IO Bool
sdlWriteSurfacePixel surface x y r g b a =
  fromSDLBool <$> sdlWriteSurfacePixelRaw surface x y r g b a


-- Helpers

fromSDLBool :: SDLBool -> Bool
fromSDLBool 0 = False
fromSDLBool _ = True  -- Any non-zero value is considered True in SDL
