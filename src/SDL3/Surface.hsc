-- SDL/Surface.hsc
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
#include <SDL3/SDL_surface.h>

module SDL3.Surface
  ( -- * Types
    -- ** Enums
    SDLScaleMode(..)
  , SDLFlipMode(..)
    -- ** Bitmasks
  , SDLSurfaceFlags(..)
    -- ** Structs (Simple/Storable)
  , SDLSurface(..) -- Export the data constructor and fields
    -- * Enum Patterns
  , pattern SDL_PROP_SURFACE_SDR_WHITE_POINT_FLOAT
  , pattern SDL_PROP_SURFACE_HDR_HEADROOM_FLOAT
  , pattern SDL_PROP_SURFACE_TONEMAP_OPERATOR_STRING
  , pattern SDL_PROP_SURFACE_HOTSPOT_X_NUMBER
  , pattern SDL_PROP_SURFACE_HOTSPOT_Y_NUMBER

  , pattern SDL_SCALEMODE_NEAREST
  , pattern SDL_SCALEMODE_LINEAR
  , pattern SDL_SCALEMODE_PIXELART
  , pattern SDL_FLIP_NONE
  , pattern SDL_FLIP_HORIZONTAL
  , pattern SDL_FLIP_VERTICAL

    -- * Bitmask Patterns
  , pattern SDL_SURFACE_PREALLOCATED
  , pattern SDL_SURFACE_LOCK_NEEDED
  , pattern SDL_SURFACE_LOCKED
  , pattern SDL_SURFACE_SIMD_ALIGNED

    -- * Functions
    -- ** Surface Creation and Destruction
  , sdlCreateSurface
  , sdlCreateSurfaceFrom
  , sdlDestroySurface -- Takes Ptr SDLSurface

    -- ** Surface Properties
  , sdlGetSurfaceProperties -- Takes Ptr SDLSurface
    -- Currently no setters defined in C API via direct function
    -- Use SDL.Properties functions with SDL_PROP_SURFACE_* constants

    -- ** Surface Colorspace Management
  , sdlSetSurfaceColorspace -- Takes Ptr SDLSurface
  , sdlGetSurfaceColorspace -- Takes Ptr SDLSurface

    -- ** Surface Palette Management
  , sdlCreateSurfacePalette -- Takes Ptr SDLSurface
  , sdlSetSurfacePalette    -- Takes Ptr SDLSurface
  , sdlGetSurfacePalette    -- Takes Ptr SDLSurface

    -- ** Surface Locking
  , sdlLockSurface          -- Takes Ptr SDLSurface
  , sdlUnlockSurface        -- Takes Ptr SDLSurface
  , sdlMustLockSurface      -- Takes Ptr SDLSurface

    -- ** BMP File Operations
  , sdlLoadBMP
  , sdlSaveBMP              -- Takes Ptr SDLSurface

    -- ** Surface Blitting
  , sdlBlitSurface          -- Takes Ptr SDLSurface
  , sdlBlitSurfaceScaled    -- Takes Ptr SDLSurface

    -- ** Surface Transformations
  , sdlFlipSurface          -- Takes Ptr SDLSurface
  , sdlScaleSurface         -- Takes Ptr SDLSurface
  , sdlDuplicateSurface     -- Takes Ptr SDLSurface

    -- ** Pixel Operations
  , sdlMapSurfaceRGB        -- Takes Ptr SDLSurface
  , sdlMapSurfaceRGBA       -- Takes Ptr SDLSurface
  , sdlReadSurfacePixel     -- Takes Ptr SDLSurface
  , sdlWriteSurfacePixel    -- Takes Ptr SDLSurface
  , sdlConvertSurface       -- Takes Ptr SDLSurface
  -- ** SDL3.2+ Extended Surface API (STUBS, TODO: Implement)
  , sdlAddSurfaceAlternateImage
  , sdlSurfaceHasAlternateImages
  , sdlGetSurfaceImages
  , sdlRemoveSurfaceAlternateImages
  , sdlLoadBMPIo
  , sdlSaveBMPIo
  , sdlSetSurfaceRLE
  , sdlSurfaceHasRLE
  , sdlSetSurfaceColorKey
  , sdlSurfaceHasColorKey
  , sdlGetSurfaceColorKey
  , sdlSetSurfaceColorMod
  , sdlGetSurfaceColorMod
  , sdlSetSurfaceAlphaMod
  , sdlGetSurfaceAlphaMod
  , sdlSetSurfaceBlendMode
  , sdlGetSurfaceBlendMode
  , sdlSetSurfaceClipRect
  , sdlGetSurfaceClipRect
  , sdlConvertSurfaceAndColorspace
  , sdlConvertPixels
  , sdlConvertPixelsAndColorspace
  , sdlPremultiplyAlpha
  , sdlPremultiplySurfaceAlpha
  , sdlClearSurface
  , sdlFillSurfaceRect
  , sdlFillSurfaceRects
  , sdlBlitSurfaceUnchecked
  , sdlBlitSurfaceUncheckedScaled
  , sdlStretchSurface
  , sdlBlitSurfaceTiled
  , sdlBlitSurfaceTiledWithScale
  , sdlBlitSurface9Grid
  , sdlReadSurfacePixelFloat
  , sdlWriteSurfacePixelFloat
  ) where

import Foreign.C.Types
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Utils (with, maybeWith, toBool, fromBool)
import Foreign.Marshal.Alloc (alloca)
import Foreign.C.String (CString, withCString)
import Foreign.Marshal.Array (peekArray)
import Data.Word
import Data.Bits (Bits, (.&.))
import SDL3.IOStream (SDLIOStream(..))

-- Assuming these imports provide the necessary types
import SDL3.Pixels (SDLColorspace(..), SDLPixelFormat, SDLPalette, pixelFormatToCUInt, cUIntToPixelFormat, cUIntToColorspace)
import SDL3.Properties (SDLPropertiesID)
import SDL3.Rect (SDLRect)

-- Helper Functions
fromCBool :: CBool -> Bool
fromCBool = toBool

-- Enum Types

-- | Scaling mode for surface operations.
newtype SDLScaleMode = SDLScaleMode CInt
  deriving stock (Show, Eq)
  deriving newtype (Storable, Enum)

pattern SDL_SCALEMODE_NEAREST   :: SDLScaleMode
pattern SDL_SCALEMODE_NEAREST   = SDLScaleMode #{const SDL_SCALEMODE_NEAREST}
pattern SDL_SCALEMODE_LINEAR    :: SDLScaleMode
pattern SDL_SCALEMODE_LINEAR    = SDLScaleMode #{const SDL_SCALEMODE_LINEAR}
pattern SDL_SCALEMODE_PIXELART  :: SDLScaleMode
pattern SDL_SCALEMODE_PIXELART  = SDLScaleMode #{const SDL_SCALEMODE_PIXELART}

-- | Flip mode for surface transformations.
newtype SDLFlipMode = SDLFlipMode CInt
  deriving stock (Show, Eq)
  deriving newtype (Storable, Enum)

pattern SDL_FLIP_NONE        :: SDLFlipMode
pattern SDL_FLIP_NONE        = SDLFlipMode #{const SDL_FLIP_NONE}
pattern SDL_FLIP_HORIZONTAL  :: SDLFlipMode
pattern SDL_FLIP_HORIZONTAL  = SDLFlipMode #{const SDL_FLIP_HORIZONTAL}
pattern SDL_FLIP_VERTICAL    :: SDLFlipMode
pattern SDL_FLIP_VERTICAL    = SDLFlipMode #{const SDL_FLIP_VERTICAL}

-- Bitmask Types

-- | Flags associated with an SDL surface, generally read-only.
newtype SDLSurfaceFlags = SDLSurfaceFlags CUInt -- Use CUInt for flags
  deriving stock (Show, Eq)
  deriving newtype (Storable, Bits, Num, Enum) -- Add Bits, Num

pattern SDL_SURFACE_PREALLOCATED :: SDLSurfaceFlags
pattern SDL_SURFACE_PREALLOCATED = SDLSurfaceFlags #{const SDL_SURFACE_PREALLOCATED}
pattern SDL_SURFACE_LOCK_NEEDED   :: SDLSurfaceFlags
pattern SDL_SURFACE_LOCK_NEEDED   = SDLSurfaceFlags #{const SDL_SURFACE_LOCK_NEEDED}
pattern SDL_SURFACE_LOCKED       :: SDLSurfaceFlags
pattern SDL_SURFACE_LOCKED       = SDLSurfaceFlags #{const SDL_SURFACE_LOCKED}
pattern SDL_SURFACE_SIMD_ALIGNED :: SDLSurfaceFlags
pattern SDL_SURFACE_SIMD_ALIGNED = SDLSurfaceFlags #{const SDL_SURFACE_SIMD_ALIGNED}

-- Struct Types (Storable)

-- | A collection of pixels used in software blitting.
-- Most functions operate on `Ptr SDLSurface`. This `Storable` instance
-- allows peeking/poking the struct contents if needed (e.g., for sdlMustLockSurface),
-- but direct modification (especially of flags/format/refcount) is generally unsafe.
data SDLSurface = SDLSurface -- Use the original name
  { surfaceFlags   :: SDLSurfaceFlags  -- ^ Surface flags, read-only
  , surfaceFormat  :: SDLPixelFormat   -- ^ Pixel format, read-only
  , surfaceW       :: Int             -- ^ Width, read-only
  , surfaceH       :: Int             -- ^ Height, read-only
  , surfacePitch   :: Int             -- ^ Bytes between rows, read-only
  , surfacePixels  :: Ptr ()           -- ^ Pointer to pixel data
  , surfaceRefcount :: Int            -- ^ Reference count (internal use)
  } deriving (Show, Eq)

instance Storable SDLSurface where -- Use the original name
  sizeOf _ = #{size SDL_Surface}
  alignment _ = #{alignment SDL_Surface}
  peek ptr = do
    flagsWord <- #{peek SDL_Surface, flags} ptr :: IO CUInt
    formatWord <- #{peek SDL_Surface, format} ptr :: IO Word32
    w        <- #{peek SDL_Surface, w} ptr :: IO CInt
    h        <- #{peek SDL_Surface, h} ptr :: IO CInt
    pitch    <- #{peek SDL_Surface, pitch} ptr :: IO CInt
    pixels   <- #{peek SDL_Surface, pixels} ptr
    refcount <- #{peek SDL_Surface, refcount} ptr
    -- Convert from raw C types to Haskell types
    let flags = SDLSurfaceFlags flagsWord
    -- Assuming SDLPixelFormat has an Enum instance mapping from Word32
    return $ SDLSurface flags (cUIntToPixelFormat (fromIntegral formatWord)) (fromIntegral w) (fromIntegral h) (fromIntegral pitch) pixels refcount
  poke ptr (SDLSurface flags format w h pitch pixels refcount) = do
    -- Convert from Haskell types back to C types
    let (SDLSurfaceFlags flagsWord) = flags
    let formatWord = pixelFormatToCUInt format -- Use helper function
    #{poke SDL_Surface, flags} ptr flagsWord -- Poke as CUInt
    #{poke SDL_Surface, format} ptr formatWord -- Poke as Word32
    #{poke SDL_Surface, w} ptr w
    #{poke SDL_Surface, h} ptr h
    #{poke SDL_Surface, pitch} ptr pitch
    #{poke SDL_Surface, pixels} ptr pixels
    #{poke SDL_Surface, refcount} ptr refcount -- Poke refcount

-- Struct Types (Haskell Representation)

-- Property Key Patterns
-- Use pattern synonyms for the property name strings defined in C.
pattern SDL_PROP_SURFACE_SDR_WHITE_POINT_FLOAT :: String
pattern SDL_PROP_SURFACE_SDR_WHITE_POINT_FLOAT = #{const_str SDL_PROP_SURFACE_SDR_WHITE_POINT_FLOAT}

pattern SDL_PROP_SURFACE_HDR_HEADROOM_FLOAT :: String
pattern SDL_PROP_SURFACE_HDR_HEADROOM_FLOAT = #{const_str SDL_PROP_SURFACE_HDR_HEADROOM_FLOAT}

pattern SDL_PROP_SURFACE_TONEMAP_OPERATOR_STRING :: String
pattern SDL_PROP_SURFACE_TONEMAP_OPERATOR_STRING = #{const_str SDL_PROP_SURFACE_TONEMAP_OPERATOR_STRING}

pattern SDL_PROP_SURFACE_HOTSPOT_X_NUMBER :: String
pattern SDL_PROP_SURFACE_HOTSPOT_X_NUMBER = #{const_str SDL_PROP_SURFACE_HOTSPOT_X_NUMBER}

pattern SDL_PROP_SURFACE_HOTSPOT_Y_NUMBER :: String
pattern SDL_PROP_SURFACE_HOTSPOT_Y_NUMBER = #{const_str SDL_PROP_SURFACE_HOTSPOT_Y_NUMBER}
-- Functions

-- | Check if a surface needs locking before pixel access.
-- Requires peeking the surface struct using the Storable instance.
sdlMustLockSurface :: Ptr SDLSurface -> IO Bool
sdlMustLockSurface surfacePtr = do
  -- Peek the structure using the Storable SDLSurface instance
  surfaceData <- peek surfacePtr -- No cast needed now
  -- Check the specific flag using bitwise AND
  return $ (surfaceFlags surfaceData .&. SDL_SURFACE_LOCK_NEEDED) /= SDLSurfaceFlags 0

-- | Create a new surface with a specific pixel format.
foreign import ccall unsafe "SDL_CreateSurface"
  c_sdlCreateSurface :: CInt -> CInt -> CUInt -> IO (Ptr SDLSurface)

sdlCreateSurface :: CInt -> CInt -> SDLPixelFormat -> IO (Maybe (Ptr SDLSurface))
sdlCreateSurface width height formatEnum = do
  let format = pixelFormatToCUInt formatEnum
  ptr <- c_sdlCreateSurface width height format
  return $ if ptr == nullPtr then Nothing else Just ptr

-- | Create a new surface from existing pixel data.
foreign import ccall unsafe "SDL_CreateSurfaceFrom"
  c_sdlCreateSurfaceFrom :: CInt -> CInt -> CUInt -> Ptr () -> CInt -> IO (Ptr SDLSurface)

sdlCreateSurfaceFrom :: CInt -> CInt -> SDLPixelFormat -> Ptr () -> CInt -> IO (Maybe (Ptr SDLSurface))
sdlCreateSurfaceFrom width height formatEnum pixels pitch = do
  let format = pixelFormatToCUInt formatEnum
  ptr <- c_sdlCreateSurfaceFrom width height format pixels pitch
  return $ if ptr == nullPtr then Nothing else Just ptr

-- | Free a surface.
foreign import ccall unsafe "SDL_DestroySurface"
  sdlDestroySurface :: Ptr SDLSurface -> IO ()

-- | Get the properties associated with a surface.
foreign import ccall unsafe "SDL_GetSurfaceProperties"
  sdlGetSurfaceProperties :: Ptr SDLSurface -> IO SDLPropertiesID

-- | Set the colorspace used by a surface.
foreign import ccall unsafe "SDL_SetSurfaceColorspace"
  c_sdlSetSurfaceColorspace :: Ptr SDLSurface -> Word32 -> IO CBool -- Use SDLColorspace directly if it's an enum based on Uint32/int

sdlSetSurfaceColorspace :: Ptr SDLSurface -> SDLColorspace -> IO Bool
sdlSetSurfaceColorspace surfacePtr (SDLColorspace csVal) =
    fromCBool <$> c_sdlSetSurfaceColorspace surfacePtr (fromIntegral csVal)

-- | Get the colorspace used by a surface.
foreign import ccall unsafe "SDL_GetSurfaceColorspace"
  c_sdlGetSurfaceColorspace :: Ptr SDLSurface -> IO Word32

sdlGetSurfaceColorspace :: Ptr SDLSurface -> IO SDLColorspace
sdlGetSurfaceColorspace surfacePtr =
        cUIntToColorspace . fromIntegral <$> c_sdlGetSurfaceColorspace surfacePtr

-- | Create a palette and associate it with a surface.
foreign import ccall unsafe "SDL_CreateSurfacePalette"
  sdlCreateSurfacePalette :: Ptr SDLSurface -> IO (Ptr SDLPalette) -- Returns pointer, check for null

-- | Set the palette used by a surface.
foreign import ccall unsafe "SDL_SetSurfacePalette"
  c_sdlSetSurfacePalette :: Ptr SDLSurface -> Ptr SDLPalette -> IO CBool

sdlSetSurfacePalette :: Ptr SDLSurface -> Ptr SDLPalette -> IO Bool
sdlSetSurfacePalette surface palette = fromCBool <$> c_sdlSetSurfacePalette surface palette

-- | Get the palette used by a surface.
foreign import ccall unsafe "SDL_GetSurfacePalette"
  sdlGetSurfacePalette :: Ptr SDLSurface -> IO (Ptr SDLPalette) -- Returns pointer, check for null

-- | Lock a surface for direct pixel access.
foreign import ccall unsafe "SDL_LockSurface"
  c_sdlLockSurface :: Ptr SDLSurface -> IO CBool

sdlLockSurface :: Ptr SDLSurface -> IO Bool
sdlLockSurface surface = fromCBool <$> c_sdlLockSurface surface

-- | Unlock a surface after direct pixel access.
foreign import ccall unsafe "SDL_UnlockSurface"
  sdlUnlockSurface :: Ptr SDLSurface -> IO ()

-- | Load a BMP image from a file.
foreign import ccall unsafe "SDL_LoadBMP"
  c_sdlLoadBMP :: CString -> IO (Ptr SDLSurface)

sdlLoadBMP :: FilePath -> IO (Maybe (Ptr SDLSurface))
sdlLoadBMP file = do
  ptr <- withCString file c_sdlLoadBMP
  return $ if ptr == nullPtr then Nothing else Just ptr

-- | Save a surface to a BMP file.
foreign import ccall unsafe "SDL_SaveBMP"
  c_sdlSaveBMP :: Ptr SDLSurface -> CString -> IO CBool

sdlSaveBMP :: Ptr SDLSurface -> FilePath -> IO Bool
sdlSaveBMP surface file = withCString file $ \cstr -> fromCBool <$> c_sdlSaveBMP surface cstr

-- | Perform a fast blit from source to destination surface.
foreign import ccall unsafe "SDL_BlitSurface"
  c_sdlBlitSurface :: Ptr SDLSurface -> Ptr SDLRect -> Ptr SDLSurface -> Ptr SDLRect -> IO CBool

sdlBlitSurface :: Ptr SDLSurface -> Maybe SDLRect -> Ptr SDLSurface -> Maybe SDLRect -> IO Bool
sdlBlitSurface src mSrcRect dst mDstRect =
  maybeWith with mSrcRect $ \srcPtr ->
  maybeWith with mDstRect $ \dstPtr ->
    fromCBool <$> c_sdlBlitSurface src srcPtr dst dstPtr

-- | Perform a scaled blit to a destination surface.
foreign import ccall unsafe "SDL_BlitSurfaceScaled"
  c_sdlBlitSurfaceScaled :: Ptr SDLSurface -> Ptr SDLRect -> Ptr SDLSurface -> Ptr SDLRect -> SDLScaleMode -> IO CBool

sdlBlitSurfaceScaled :: Ptr SDLSurface -> Maybe SDLRect -> Ptr SDLSurface -> Maybe SDLRect -> SDLScaleMode -> IO Bool
sdlBlitSurfaceScaled src mSrcRect dst mDstRect scaleMode =
  maybeWith with mSrcRect $ \srcPtr ->
  maybeWith with mDstRect $ \dstPtr ->
    fromCBool <$> c_sdlBlitSurfaceScaled src srcPtr dst dstPtr scaleMode

-- | Flip a surface vertically or horizontally.
foreign import ccall unsafe "SDL_FlipSurface"
  c_sdlFlipSurface :: Ptr SDLSurface -> SDLFlipMode -> IO CBool

sdlFlipSurface :: Ptr SDLSurface -> SDLFlipMode -> IO Bool
sdlFlipSurface surface flipMode = fromCBool <$> c_sdlFlipSurface surface flipMode

-- | Scale a surface to a new size. Creates a *new* surface.
foreign import ccall unsafe "SDL_ScaleSurface"
  c_sdlScaleSurface :: Ptr SDLSurface -> CInt -> CInt -> SDLScaleMode -> IO (Ptr SDLSurface)

sdlScaleSurface :: Ptr SDLSurface -> CInt -> CInt -> SDLScaleMode -> IO (Maybe (Ptr SDLSurface))
sdlScaleSurface surface width height scaleMode = do
  ptr <- c_sdlScaleSurface surface width height scaleMode
  return $ if ptr == nullPtr then Nothing else Just ptr

-- | Duplicate a surface. Creates a *new* surface.
foreign import ccall unsafe "SDL_DuplicateSurface"
  c_sdlDuplicateSurface :: Ptr SDLSurface -> IO (Ptr SDLSurface)

sdlDuplicateSurface :: Ptr SDLSurface -> IO (Maybe (Ptr SDLSurface))
sdlDuplicateSurface surface = do
  ptr <- c_sdlDuplicateSurface surface
  return $ if ptr == nullPtr then Nothing else Just ptr

-- | Map an RGB triple to a pixel value for a surface.
foreign import ccall unsafe "SDL_MapSurfaceRGB"
  sdlMapSurfaceRGB :: Ptr SDLSurface -> Word8 -> Word8 -> Word8 -> IO Word32

-- | Map an RGBA quadruple to a pixel value for a surface.
foreign import ccall unsafe "SDL_MapSurfaceRGBA"
  sdlMapSurfaceRGBA :: Ptr SDLSurface -> Word8 -> Word8 -> Word8 -> Word8 -> IO Word32

-- | Read a single pixel from a surface.
foreign import ccall unsafe "SDL_ReadSurfacePixel"
  c_sdlReadSurfacePixel :: Ptr SDLSurface -> CInt -> CInt -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CBool

sdlReadSurfacePixel :: Ptr SDLSurface -> CInt -> CInt -> IO (Maybe (Word8, Word8, Word8, Word8))
sdlReadSurfacePixel surface x y =
  alloca $ \rPtr ->
  alloca $ \gPtr ->
  alloca $ \bPtr ->
  alloca $ \aPtr -> do
    success <- c_sdlReadSurfacePixel surface x y rPtr gPtr bPtr aPtr
    if fromCBool success
      then do
        r <- peek rPtr
        g <- peek gPtr
        b <- peek bPtr
        a <- peek aPtr
        return $ Just (r, g, b, a)
      else return Nothing

-- | Write a single pixel to a surface.
foreign import ccall unsafe "SDL_WriteSurfacePixel"
  c_sdlWriteSurfacePixel :: Ptr SDLSurface -> CInt -> CInt -> Word8 -> Word8 -> Word8 -> Word8 -> IO CBool

sdlWriteSurfacePixel :: Ptr SDLSurface -> CInt -> CInt -> Word8 -> Word8 -> Word8 -> Word8 -> IO Bool
sdlWriteSurfacePixel surface x y r g b a =
  fromCBool <$> c_sdlWriteSurfacePixel surface x y r g b a

-- | Convert a surface to a new pixel format. Creates a *new* surface.
foreign import ccall unsafe "SDL_ConvertSurface"
  c_sdlConvertSurface :: Ptr SDLSurface -> CUInt -> IO (Ptr SDLSurface)

sdlConvertSurface :: Ptr SDLSurface     -- ^ The source surface pointer.
                  -> SDLPixelFormat     -- ^ The desired pixel format enum.
                  -> IO (Maybe (Ptr SDLSurface)) -- ^ Returns pointer to the new surface or Nothing on failure.
sdlConvertSurface surfacePtr formatEnum = do
  let format = pixelFormatToCUInt formatEnum
  newSurfacePtr <- c_sdlConvertSurface surfacePtr format
  return $ if newSurfacePtr == nullPtr then Nothing else Just newSurfacePtr

--------------------------------------------------------------------------------
-- SDL3.2+ Extended Surface API (STUBS)
--------------------------------------------------------------------------------

-- | Add an alternate image to a surface.
foreign import ccall unsafe "SDL_AddSurfaceAlternateImage"
  c_sdlAddSurfaceAlternateImage :: Ptr SDLSurface -> Ptr SDLSurface -> IO CBool

sdlAddSurfaceAlternateImage :: Ptr SDLSurface -> Ptr SDLSurface -> IO Bool
sdlAddSurfaceAlternateImage surface image =
  fromCBool <$> c_sdlAddSurfaceAlternateImage surface image

-- | Query if a surface has alternate images.
foreign import ccall unsafe "SDL_SurfaceHasAlternateImages"
  c_sdlSurfaceHasAlternateImages :: Ptr SDLSurface -> IO CBool

sdlSurfaceHasAlternateImages :: Ptr SDLSurface -> IO Bool
sdlSurfaceHasAlternateImages surface =
  fromCBool <$> c_sdlSurfaceHasAlternateImages surface

-- | Get all alternate images for a surface.
foreign import ccall unsafe "SDL_GetSurfaceImages"
  c_sdlGetSurfaceImages :: Ptr SDLSurface -> Ptr CInt -> IO (Ptr (Ptr SDLSurface))



-- | Returns a list of alternate image surface pointers for the given surface.
--
-- This function marshals the C array of surface pointers into a Haskell list.
-- The returned pointers are not owned by Haskell and must not be freed.
sdlGetSurfaceImages :: Ptr SDLSurface -> IO [Ptr SDLSurface]
sdlGetSurfaceImages surface =
  alloca $ \countPtr -> do
    arrPtr <- c_sdlGetSurfaceImages surface countPtr
    count <- fromIntegral <$> peek countPtr
    if arrPtr == nullPtr || count == 0
      then return []
      else peekArray count arrPtr

-- | Remove all alternate images from a surface.
foreign import ccall unsafe "SDL_RemoveSurfaceAlternateImages"
  c_sdlRemoveSurfaceAlternateImages :: Ptr SDLSurface -> IO ()

sdlRemoveSurfaceAlternateImages :: Ptr SDLSurface -> IO ()
sdlRemoveSurfaceAlternateImages = c_sdlRemoveSurfaceAlternateImages

-- | Load a BMP from an SDL_IOStream.
--
-- The stream must be a valid SDLIOStream (see "SDL.IOStream"). If @closeio@ is True,
-- the stream will be closed after loading.
foreign import ccall unsafe "SDL_LoadBMP_IO"
  c_sdlLoadBMPIo :: Ptr SDLIOStream -> CBool -> IO (Ptr SDLSurface)

-- | Load a BMP from an SDLIOStream.
--
-- Returns @Nothing@ on failure. The stream is closed if @closeio@ is True.
sdlLoadBMPIo :: SDLIOStream -> Bool -> IO (Maybe (Ptr SDLSurface))
sdlLoadBMPIo (SDLIOStream streamPtr) closeio = do
  ptr <- c_sdlLoadBMPIo streamPtr (fromBool closeio)
  return $ if ptr == nullPtr then Nothing else Just ptr

-- | Save a surface to a BMP via SDLIOStream.
--
-- The stream must be a valid SDLIOStream (see "SDL.IOStream"). If @closeio@ is True,
-- the stream will be closed after saving.
foreign import ccall unsafe "SDL_SaveBMP_IO"
  c_sdlSaveBMPIo :: Ptr SDLSurface -> Ptr SDLIOStream -> CBool -> IO CBool

-- | Save a surface to a BMP via SDLIOStream.
--
-- Returns True on success. The stream is closed if @closeio@ is True.
sdlSaveBMPIo :: Ptr SDLSurface -> SDLIOStream -> Bool -> IO Bool
sdlSaveBMPIo surface (SDLIOStream dstPtr) closeio =
  fromCBool <$> c_sdlSaveBMPIo surface dstPtr (fromBool closeio)

-- | Enable or disable RLE acceleration for a surface.
foreign import ccall unsafe "SDL_SetSurfaceRLE"
  c_sdlSetSurfaceRLE :: Ptr SDLSurface -> CBool -> IO CBool

sdlSetSurfaceRLE :: Ptr SDLSurface -> Bool -> IO Bool
sdlSetSurfaceRLE surface enabled =
  fromCBool <$> c_sdlSetSurfaceRLE surface (fromBool enabled)

foreign import ccall unsafe "SDL_SurfaceHasRLE"
  c_sdlSurfaceHasRLE :: Ptr SDLSurface -> IO CBool

sdlSurfaceHasRLE :: Ptr SDLSurface -> IO Bool
sdlSurfaceHasRLE surface =
  fromCBool <$> c_sdlSurfaceHasRLE surface

foreign import ccall unsafe "SDL_SetSurfaceColorKey"
  c_sdlSetSurfaceColorKey :: Ptr SDLSurface -> CBool -> Word32 -> IO CBool

sdlSetSurfaceColorKey :: Ptr SDLSurface -> Bool -> Word32 -> IO Bool
sdlSetSurfaceColorKey surface enabled key =
  fromCBool <$> c_sdlSetSurfaceColorKey surface (fromBool enabled) key

foreign import ccall unsafe "SDL_SurfaceHasColorKey"
  c_sdlSurfaceHasColorKey :: Ptr SDLSurface -> IO CBool

sdlSurfaceHasColorKey :: Ptr SDLSurface -> IO Bool
sdlSurfaceHasColorKey surface =
  fromCBool <$> c_sdlSurfaceHasColorKey surface

foreign import ccall unsafe "SDL_GetSurfaceColorKey"
  c_sdlGetSurfaceColorKey :: Ptr SDLSurface -> Ptr Word32 -> IO CBool

sdlGetSurfaceColorKey :: Ptr SDLSurface -> IO (Maybe Word32)
sdlGetSurfaceColorKey surface =
  alloca $ \keyPtr -> do
    ok <- c_sdlGetSurfaceColorKey surface keyPtr
    if fromCBool ok
      then Just <$> peek keyPtr
      else return Nothing

foreign import ccall unsafe "SDL_SetSurfaceColorMod"
  c_sdlSetSurfaceColorMod :: Ptr SDLSurface -> Word8 -> Word8 -> Word8 -> IO CBool

sdlSetSurfaceColorMod :: Ptr SDLSurface -> Word8 -> Word8 -> Word8 -> IO Bool
sdlSetSurfaceColorMod surface r g b =
  fromCBool <$> c_sdlSetSurfaceColorMod surface r g b

foreign import ccall unsafe "SDL_GetSurfaceColorMod"
  c_sdlGetSurfaceColorMod :: Ptr SDLSurface -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CBool

sdlGetSurfaceColorMod :: Ptr SDLSurface -> IO (Maybe (Word8, Word8, Word8))
sdlGetSurfaceColorMod surface =
  alloca $ \rPtr ->
  alloca $ \gPtr ->
  alloca $ \bPtr -> do
    ok <- c_sdlGetSurfaceColorMod surface rPtr gPtr bPtr
    if fromCBool ok
      then do
        r <- peek rPtr
        g <- peek gPtr
        b <- peek bPtr
        return $ Just (r, g, b)
      else return Nothing

foreign import ccall unsafe "SDL_SetSurfaceAlphaMod"
  c_sdlSetSurfaceAlphaMod :: Ptr SDLSurface -> Word8 -> IO CBool

sdlSetSurfaceAlphaMod :: Ptr SDLSurface -> Word8 -> IO Bool
sdlSetSurfaceAlphaMod surface alpha =
  fromCBool <$> c_sdlSetSurfaceAlphaMod surface alpha

foreign import ccall unsafe "SDL_GetSurfaceAlphaMod"
  c_sdlGetSurfaceAlphaMod :: Ptr SDLSurface -> Ptr Word8 -> IO CBool

sdlGetSurfaceAlphaMod :: Ptr SDLSurface -> IO (Maybe Word8)
sdlGetSurfaceAlphaMod surface =
  alloca $ \aPtr -> do
    ok <- c_sdlGetSurfaceAlphaMod surface aPtr
    if fromCBool ok
      then Just <$> peek aPtr
      else return Nothing

foreign import ccall unsafe "SDL_SetSurfaceBlendMode"
  c_sdlSetSurfaceBlendMode :: Ptr SDLSurface -> CInt -> IO CBool

sdlSetSurfaceBlendMode :: Ptr SDLSurface -> CInt -> IO Bool
sdlSetSurfaceBlendMode surface blendMode =
  fromCBool <$> c_sdlSetSurfaceBlendMode surface blendMode

foreign import ccall unsafe "SDL_GetSurfaceBlendMode"
  c_sdlGetSurfaceBlendMode :: Ptr SDLSurface -> Ptr CInt -> IO CBool

sdlGetSurfaceBlendMode :: Ptr SDLSurface -> IO (Maybe CInt)
sdlGetSurfaceBlendMode surface =
  alloca $ \modePtr -> do
    ok <- c_sdlGetSurfaceBlendMode surface modePtr
    if fromCBool ok
      then Just <$> peek modePtr
      else return Nothing

foreign import ccall unsafe "SDL_SetSurfaceClipRect"
  c_sdlSetSurfaceClipRect :: Ptr SDLSurface -> Ptr SDLRect -> IO CBool

sdlSetSurfaceClipRect :: Ptr SDLSurface -> Ptr SDLRect -> IO Bool
sdlSetSurfaceClipRect surface rectPtr =
  fromCBool <$> c_sdlSetSurfaceClipRect surface rectPtr

foreign import ccall unsafe "SDL_GetSurfaceClipRect"
  c_sdlGetSurfaceClipRect :: Ptr SDLSurface -> Ptr SDLRect -> IO CBool

sdlGetSurfaceClipRect :: Ptr SDLSurface -> Ptr SDLRect -> IO Bool
sdlGetSurfaceClipRect surface rectPtr =
  fromCBool <$> c_sdlGetSurfaceClipRect surface rectPtr

-- | Convert a surface and colorspace.
-- | Convert a surface to a new format, palette, and colorspace.
--
-- This is a low-level wrapper. The @palette@ argument should be a pointer to an SDL_Palette,
-- @colorspace@ and @props@ are advanced options. Returns @Nothing@ on failure.
foreign import ccall unsafe "SDL_ConvertSurfaceAndColorspace"
  c_sdlConvertSurfaceAndColorspace :: Ptr SDLSurface -> CUInt -> Ptr () -> CInt -> CUInt -> IO (Ptr SDLSurface)

sdlConvertSurfaceAndColorspace :: Ptr SDLSurface -> CUInt -> Ptr () -> CInt -> CUInt -> IO (Maybe (Ptr SDLSurface))
sdlConvertSurfaceAndColorspace surface format palette colorspace props = do
  ptr <- c_sdlConvertSurfaceAndColorspace surface format palette colorspace props
  return $ if ptr == nullPtr then Nothing else Just ptr

-- | Convert pixels between formats.
foreign import ccall unsafe "SDL_ConvertPixels"
  c_sdlConvertPixels :: CInt -> CInt -> CUInt -> Ptr () -> CInt -> CUInt -> Ptr () -> CInt -> IO CBool

sdlConvertPixels :: CInt -> CInt -> CUInt -> Ptr () -> CInt -> CUInt -> Ptr () -> CInt -> IO Bool
sdlConvertPixels w h srcFmt src srcPitch dstFmt dst dstPitch =
  fromCBool <$> c_sdlConvertPixels w h srcFmt src srcPitch dstFmt dst dstPitch

-- | Convert pixels and colorspace.
-- | Convert pixels between formats and colorspaces.
--
-- This is a low-level wrapper. All pointers and enums must be correct for the underlying C API.
-- Returns True on success.
foreign import ccall unsafe "SDL_ConvertPixelsAndColorspace"
  c_sdlConvertPixelsAndColorspace :: CInt -> CInt -> CUInt -> CInt -> CUInt -> Ptr () -> CInt -> CUInt -> CInt -> CUInt -> Ptr () -> CInt -> IO CBool

sdlConvertPixelsAndColorspace :: CInt -> CInt -> CUInt -> CInt -> CUInt -> Ptr () -> CInt -> CUInt -> CInt -> CUInt -> Ptr () -> CInt -> IO Bool
sdlConvertPixelsAndColorspace a b c d e f g h i j k l =
  fromCBool <$> c_sdlConvertPixelsAndColorspace a b c d e f g h i j k l

-- | Premultiply alpha for pixel data.
foreign import ccall unsafe "SDL_PremultiplyAlpha"
  c_sdlPremultiplyAlpha :: CInt -> CInt -> CUInt -> Ptr () -> CInt -> CUInt -> Ptr () -> CInt -> CBool -> IO CBool

sdlPremultiplyAlpha :: CInt -> CInt -> CUInt -> Ptr () -> CInt -> CUInt -> Ptr () -> CInt -> Bool -> IO Bool
sdlPremultiplyAlpha w h srcFmt src srcPitch dstFmt dst dstPitch linear =
  fromCBool <$> c_sdlPremultiplyAlpha w h srcFmt src srcPitch dstFmt dst dstPitch (fromBool linear)

-- | Premultiply alpha for a surface.
foreign import ccall unsafe "SDL_PremultiplySurfaceAlpha"
  c_sdlPremultiplySurfaceAlpha :: Ptr SDLSurface -> CBool -> IO CBool

sdlPremultiplySurfaceAlpha :: Ptr SDLSurface -> Bool -> IO Bool
sdlPremultiplySurfaceAlpha surface linear =
  fromCBool <$> c_sdlPremultiplySurfaceAlpha surface (fromBool linear)

-- | Clear a surface to a color.
foreign import ccall unsafe "SDL_ClearSurface"
  c_sdlClearSurface :: Ptr SDLSurface -> CFloat -> CFloat -> CFloat -> CFloat -> IO CBool

sdlClearSurface :: Ptr SDLSurface -> Float -> Float -> Float -> Float -> IO Bool
sdlClearSurface surface r g b a =
  fromCBool <$> c_sdlClearSurface surface (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)

-- | Fill a rectangle on a surface.
foreign import ccall unsafe "SDL_FillSurfaceRect"
  c_sdlFillSurfaceRect :: Ptr SDLSurface -> Ptr SDLRect -> Word32 -> IO CBool

sdlFillSurfaceRect :: Ptr SDLSurface -> Ptr SDLRect -> Word32 -> IO Bool
sdlFillSurfaceRect surface rect color =
  fromCBool <$> c_sdlFillSurfaceRect surface rect color

-- | Fill multiple rectangles on a surface.
foreign import ccall unsafe "SDL_FillSurfaceRects"
  c_sdlFillSurfaceRects :: Ptr SDLSurface -> Ptr SDLRect -> CInt -> Word32 -> IO CBool

sdlFillSurfaceRects :: Ptr SDLSurface -> Ptr SDLRect -> CInt -> Word32 -> IO Bool
sdlFillSurfaceRects surface rects count color =
  fromCBool <$> c_sdlFillSurfaceRects surface rects count color

-- | Unchecked blit between surfaces.
foreign import ccall unsafe "SDL_BlitSurfaceUnchecked"
  c_sdlBlitSurfaceUnchecked :: Ptr SDLSurface -> Ptr SDLRect -> Ptr SDLSurface -> Ptr SDLRect -> IO CBool

sdlBlitSurfaceUnchecked :: Ptr SDLSurface -> Ptr SDLRect -> Ptr SDLSurface -> Ptr SDLRect -> IO Bool
sdlBlitSurfaceUnchecked src srcRect dst dstRect =
  fromCBool <$> c_sdlBlitSurfaceUnchecked src srcRect dst dstRect

-- | Unchecked scaled blit between surfaces.
foreign import ccall unsafe "SDL_BlitSurfaceUncheckedScaled"
  c_sdlBlitSurfaceUncheckedScaled :: Ptr SDLSurface -> Ptr SDLRect -> Ptr SDLSurface -> Ptr SDLRect -> CInt -> IO CBool

sdlBlitSurfaceUncheckedScaled :: Ptr SDLSurface -> Ptr SDLRect -> Ptr SDLSurface -> Ptr SDLRect -> CInt -> IO Bool
sdlBlitSurfaceUncheckedScaled src srcRect dst dstRect scaleMode =
  fromCBool <$> c_sdlBlitSurfaceUncheckedScaled src srcRect dst dstRect scaleMode

-- | Stretch a surface.
foreign import ccall unsafe "SDL_StretchSurface"
  c_sdlStretchSurface :: Ptr SDLSurface -> Ptr SDLRect -> Ptr SDLSurface -> Ptr SDLRect -> CInt -> IO CBool

sdlStretchSurface :: Ptr SDLSurface -> Ptr SDLRect -> Ptr SDLSurface -> Ptr SDLRect -> CInt -> IO Bool
sdlStretchSurface src srcRect dst dstRect scaleMode =
  fromCBool <$> c_sdlStretchSurface src srcRect dst dstRect scaleMode

-- | Tiled blit between surfaces.
foreign import ccall unsafe "SDL_BlitSurfaceTiled"
  c_sdlBlitSurfaceTiled :: Ptr SDLSurface -> Ptr SDLRect -> Ptr SDLSurface -> Ptr SDLRect -> IO CBool

sdlBlitSurfaceTiled :: Ptr SDLSurface -> Ptr SDLRect -> Ptr SDLSurface -> Ptr SDLRect -> IO Bool
sdlBlitSurfaceTiled src srcRect dst dstRect =
  fromCBool <$> c_sdlBlitSurfaceTiled src srcRect dst dstRect

-- | Tiled blit with scale between surfaces.
foreign import ccall unsafe "SDL_BlitSurfaceTiledWithScale"
  c_sdlBlitSurfaceTiledWithScale :: Ptr SDLSurface -> Ptr SDLRect -> CFloat -> CInt -> Ptr SDLSurface -> Ptr SDLRect -> IO CBool

sdlBlitSurfaceTiledWithScale :: Ptr SDLSurface -> Ptr SDLRect -> Float -> CInt -> Ptr SDLSurface -> Ptr SDLRect -> IO Bool
sdlBlitSurfaceTiledWithScale src srcRect scale scaleMode dst dstRect =
  fromCBool <$> c_sdlBlitSurfaceTiledWithScale src srcRect (realToFrac scale) scaleMode dst dstRect

-- | 9-grid blit between surfaces.
foreign import ccall unsafe "SDL_BlitSurface9Grid"
  c_sdlBlitSurface9Grid :: Ptr SDLSurface -> Ptr SDLRect -> CInt -> CInt -> CInt -> CInt -> CFloat -> CInt -> Ptr SDLSurface -> Ptr SDLRect -> IO CBool

sdlBlitSurface9Grid :: Ptr SDLSurface -> Ptr SDLRect -> CInt -> CInt -> CInt -> CInt -> Float -> CInt -> Ptr SDLSurface -> Ptr SDLRect -> IO Bool
sdlBlitSurface9Grid src srcRect leftW rightW topH bottomH scale scaleMode dst dstRect =
  fromCBool <$> c_sdlBlitSurface9Grid src srcRect leftW rightW topH bottomH (realToFrac scale) scaleMode dst dstRect

-- | Read a pixel as floats.
foreign import ccall unsafe "SDL_ReadSurfacePixelFloat"
  c_sdlReadSurfacePixelFloat :: Ptr SDLSurface -> CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CBool

sdlReadSurfacePixelFloat :: Ptr SDLSurface -> CInt -> CInt -> IO (Maybe (Float, Float, Float, Float))
sdlReadSurfacePixelFloat surface x y =
  alloca $ \rPtr ->
  alloca $ \gPtr ->
  alloca $ \bPtr ->
  alloca $ \aPtr -> do
    ok <- c_sdlReadSurfacePixelFloat surface x y rPtr gPtr bPtr aPtr
    if fromCBool ok
      then do
        r <- realToFrac <$> peek rPtr
        g <- realToFrac <$> peek gPtr
        b <- realToFrac <$> peek bPtr
        a <- realToFrac <$> peek aPtr
        return $ Just (r, g, b, a)
      else return Nothing

-- | Write a pixel as floats.
foreign import ccall unsafe "SDL_WriteSurfacePixelFloat"
  c_sdlWriteSurfacePixelFloat :: Ptr SDLSurface -> CInt -> CInt -> CFloat -> CFloat -> CFloat -> CFloat -> IO CBool

sdlWriteSurfacePixelFloat :: Ptr SDLSurface -> CInt -> CInt -> Float -> Float -> Float -> Float -> IO Bool
sdlWriteSurfacePixelFloat surface x y r g b a =
  fromCBool <$> c_sdlWriteSurfacePixelFloat surface x y (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
