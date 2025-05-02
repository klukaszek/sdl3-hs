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

module SDL.Surface
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
  ) where

import Foreign.C.Types
import Foreign.Ptr (Ptr, nullPtr, FunPtr, castPtr) -- Added castPtr
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Utils (with, maybeWith, toBool, fromBool)
import Foreign.Marshal.Alloc (alloca)
import Foreign.C.String (CString, withCString)
import Control.Monad (when)
import Data.Word
import Data.Bits (Bits, (.|.), (.&.))

-- Assuming these imports provide the necessary types
import SDL.Pixels (SDLColorspace(..), SDLPixelFormat, SDLPalette, pixelFormatToCUInt, cUIntToPixelFormat, cUIntToColorspace)
import SDL.Properties (SDLPropertiesID(..))
import SDL.Rect (SDLRect)
import SDL.Stdinc (SDLBool) -- Use CBool from Stdinc

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
  , surfaceW       :: CInt             -- ^ Width, read-only
  , surfaceH       :: CInt             -- ^ Height, read-only
  , surfacePitch   :: CInt             -- ^ Bytes between rows, read-only
  , surfacePixels  :: Ptr ()           -- ^ Pointer to pixel data
  , surfaceRefcount :: CInt            -- ^ Reference count (internal use)
  } deriving (Show, Eq)

instance Storable SDLSurface where -- Use the original name
  sizeOf _ = #{size SDL_Surface}
  alignment _ = #{alignment SDL_Surface}
  peek ptr = do
    flagsWord <- #{peek SDL_Surface, flags} ptr :: IO CUInt
    formatWord <- #{peek SDL_Surface, format} ptr :: IO Word32
    w        <- #{peek SDL_Surface, w} ptr
    h        <- #{peek SDL_Surface, h} ptr
    pitch    <- #{peek SDL_Surface, pitch} ptr
    pixels   <- #{peek SDL_Surface, pixels} ptr
    refcount <- #{peek SDL_Surface, refcount} ptr
    -- Convert from raw C types to Haskell types
    let flags = SDLSurfaceFlags flagsWord
    -- Assuming SDLPixelFormat has an Enum instance mapping from Word32
    return $ SDLSurface flags (cUIntToPixelFormat (fromIntegral formatWord)) w h pitch pixels refcount
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

sdlSetSurfaceColorspace :: SDLSurface -> SDLColorspace -> IO Bool
sdlSetSurfaceColorspace surfaceRec (SDLColorspace csVal) =
    with surfaceRec $ \surfacePtr -> -- Temporarily allocates memory, copies surfaceRec, passes pointer
        fromCBool <$> c_sdlSetSurfaceColorspace surfacePtr (fromIntegral csVal)

-- | Get the colorspace used by a surface.
foreign import ccall unsafe "SDL_GetSurfaceColorspace"
  c_sdlGetSurfaceColorspace :: Ptr SDLSurface -> IO Word32

sdlGetSurfaceColorspace :: SDLSurface -> IO SDLColorspace
sdlGetSurfaceColorspace surfaceRec =
    with surfaceRec $ \surfacePtr -> -- Use 'with' to pass pointer to the C function
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
