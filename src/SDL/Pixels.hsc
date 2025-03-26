{-|
Module      : SDL.Pixels
Description : Pixel format management and color manipulation
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

SDL offers facilities for pixel management.

Largely these facilities deal with pixel _format_: what does this set of
bits represent?

If you mostly want to think of a pixel as some combination of red, green,
blue, and maybe alpha intensities, this is all pretty straightforward, and
in many cases, is enough information to build a perfectly fine game.

However, the actual definition of a pixel is more complex than that:

Pixels are a representation of a color in a particular color space.
-}

module SDL.Pixels
  ( -- * Alpha constants
    sdlAlphaOpaque
  , sdlAlphaOpaqueFloat
  , sdlAlphaTransparent
  , sdlAlphaTransparentFloat
    
    -- * Pixel Type
  , SDLPixelType(..)
    
    -- * Bitmap Order
  , SDLBitmapOrder(..)
    
    -- * Packed Order
  , SDLPackedOrder(..)
    
    -- * Array Order
  , SDLArrayOrder(..)
    
    -- * Packed Layout
  , SDLPackedLayout(..)
    
    -- * Pixel Format
  , SDLPixelFormat(..)
    
    -- * Pixel Format Macros
  , sdlDefinePixelFourCC
  , sdlDefinePixelFormat
  , sdlPixelFlag
  , sdlPixelType
  , sdlPixelOrder
  , sdlPixelLayout
  , sdlBitsPerPixel
  , sdlBytesPerPixel
  , sdlIsPixelFormatIndexed
  , sdlIsPixelFormatPacked
  , sdlIsPixelFormatArray
  , sdlIsPixelFormat10Bit
  , sdlIsPixelFormatFloat
  , sdlIsPixelFormatAlpha
  , sdlIsPixelFormatFourCC
  , pixelFormatToWord32
    
    -- * Color Types
  , SDLColorType(..)
  , SDLColorRange(..)
  , SDLColorPrimaries(..)
  , SDLTransferCharacteristics(..)
  , SDLMatrixCoefficients(..)
  , SDLChromaLocation(..)
    
    -- * Colorspace
  , SDLColorspace(..)
  , sdlDefineColorspace
  , sdlColorspaceType
  , sdlColorspaceRange
  , sdlColorspaceChroma
  , sdlColorspacePrimaries
  , sdlColorspaceTransfer
  , sdlColorspaceMatrix
  , sdlIsColorspaceMatrixBT601
  , sdlIsColorspaceMatrixBT709
  , sdlIsColorspaceMatrixBT2020NCL
  , sdlIsColorspaceLimitedRange
  , sdlIsColorspaceFullRange
    
    -- * Color Structures
  , SDLColor(..)
  , SDLFColor(..)
  , SDLPalette(..)
  , SDLPixelFormatDetails(..)
    
    -- * Pixel Format Functions
  , sdlGetPixelFormatName
  , sdlGetMasksForPixelFormat
  , sdlGetPixelFormatForMasks
  , sdlGetPixelFormatDetails
    
    -- * Palette Functions
  , sdlCreatePalette
  , sdlSetPaletteColors
  , sdlDestroyPalette
    
    -- * Color Mapping Functions
  , sdlMapRGB
  , sdlMapRGBA
  , sdlGetRGB
  , sdlGetRGBA
  ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc (alloca)
import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.Word
import Data.Int

-- | A fully opaque 8-bit alpha value
--
-- @since 3.2.0
sdlAlphaOpaque :: Word8
sdlAlphaOpaque = 255

-- | A fully opaque floating point alpha value
--
-- @since 3.2.0
sdlAlphaOpaqueFloat :: Float
sdlAlphaOpaqueFloat = 1.0

-- | A fully transparent 8-bit alpha value
--
-- @since 3.2.0
sdlAlphaTransparent :: Word8
sdlAlphaTransparent = 0

-- | A fully transparent floating point alpha value
--
-- @since 3.2.0
sdlAlphaTransparentFloat :: Float
sdlAlphaTransparentFloat = 0.0

-- | Pixel type
--
-- @since 3.2.0
data SDLPixelType
  = SDLPixelTypeUnknown
  | SDLPixelTypeIndex1
  | SDLPixelTypeIndex4
  | SDLPixelTypeIndex8
  | SDLPixelTypePacked8
  | SDLPixelTypePacked16
  | SDLPixelTypePacked32
  | SDLPixelTypeArrayU8
  | SDLPixelTypeArrayU16
  | SDLPixelTypeArrayU32
  | SDLPixelTypeArrayF16
  | SDLPixelTypeArrayF32
  | SDLPixelTypeIndex2
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Bitmap pixel order, high bit -> low bit
--
-- @since 3.2.0
data SDLBitmapOrder
  = SDLBitmapOrderNone
  | SDLBitmapOrder4321
  | SDLBitmapOrder1234
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Packed component order, high bit -> low bit
--
-- @since 3.2.0
data SDLPackedOrder
  = SDLPackedOrderNone
  | SDLPackedOrderXRGB
  | SDLPackedOrderRGBX
  | SDLPackedOrderARGB
  | SDLPackedOrderRGBA
  | SDLPackedOrderXBGR
  | SDLPackedOrderBGRX
  | SDLPackedOrderABGR
  | SDLPackedOrderBGRA
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Array component order, low byte -> high byte
--
-- @since 3.2.0
data SDLArrayOrder
  = SDLArrayOrderNone
  | SDLArrayOrderRGB
  | SDLArrayOrderRGBA
  | SDLArrayOrderARGB
  | SDLArrayOrderBGR
  | SDLArrayOrderBGRA
  | SDLArrayOrderABGR
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Packed component layout
--
-- @since 3.2.0
data SDLPackedLayout
  = SDLPackedLayoutNone
  | SDLPackedLayout332
  | SDLPackedLayout4444
  | SDLPackedLayout1555
  | SDLPackedLayout5551
  | SDLPackedLayout565
  | SDLPackedLayout8888
  | SDLPackedLayout2101010
  | SDLPackedLayout1010102
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | A macro for defining custom FourCC pixel formats
--
-- @since 3.2.0
sdlDefinePixelFourCC :: Char -> Char -> Char -> Char -> Word32
sdlDefinePixelFourCC a b c d =
  (fromIntegral (fromEnum a) `shiftL` 0) .|.
  (fromIntegral (fromEnum b) `shiftL` 8) .|.
  (fromIntegral (fromEnum c) `shiftL` 16) .|.
  (fromIntegral (fromEnum d) `shiftL` 24)

-- | A macro for defining custom non-FourCC pixel formats
--
-- @since 3.2.0
sdlDefinePixelFormat :: Int -> Int -> Int -> Int -> Int -> Word32
sdlDefinePixelFormat pixelType order layout bits bytes =
  (1 `shiftL` 28) .|.
  ((fromIntegral pixelType) `shiftL` 24) .|.
  ((fromIntegral order) `shiftL` 20) .|.
  ((fromIntegral layout) `shiftL` 16) .|.
  ((fromIntegral bits) `shiftL` 8) .|.
  (fromIntegral bytes)

-- | A macro to retrieve the flags of an SDL_PixelFormat
--
-- @since 3.2.0
sdlPixelFlag :: Word32 -> Int
sdlPixelFlag format = fromIntegral ((format `shiftR` 28) .&. 0x0F)

-- | A macro to retrieve the type of an SDL_PixelFormat
--
-- This is usually a value from the SDLPixelType enumeration.
--
-- @since 3.2.0
sdlPixelType :: Word32 -> Int
sdlPixelType format = fromIntegral ((format `shiftR` 24) .&. 0x0F)

-- | A macro to retrieve the order of an SDL_PixelFormat
--
-- This is usually a value from the SDLBitmapOrder, SDLPackedOrder, or
-- SDLArrayOrder enumerations, depending on the format type.
--
-- @since 3.2.0
sdlPixelOrder :: Word32 -> Int
sdlPixelOrder format = fromIntegral ((format `shiftR` 20) .&. 0x0F)

-- | A macro to retrieve the layout of an SDL_PixelFormat
--
-- This is usually a value from the SDLPackedLayout enumeration, or zero if a
-- layout doesn't make sense for the format type.
--
-- @since 3.2.0
sdlPixelLayout :: Word32 -> Int
sdlPixelLayout format = fromIntegral ((format `shiftR` 16) .&. 0x0F)

-- | A macro to determine an SDL_PixelFormat's bits per pixel
--
-- FourCC formats will report zero here, as it rarely makes sense to measure
-- them per-pixel.
--
-- @since 3.2.0
sdlBitsPerPixel :: Word32 -> Int
sdlBitsPerPixel format =
  if sdlIsPixelFormatFourCC format
  then 0
  else fromIntegral ((format `shiftR` 8) .&. 0xFF)

-- | A macro to determine an SDL_PixelFormat's bytes per pixel
--
-- FourCC formats do their best here, but many of them don't have a meaningful
-- measurement of bytes per pixel.
--
-- @since 3.2.0
sdlBytesPerPixel :: Word32 -> Int
sdlBytesPerPixel format =
  if sdlIsPixelFormatFourCC format
  then if (format == pixelFormatToWord32 SDLPixelFormatYUY2) ||
          (format == pixelFormatToWord32 SDLPixelFormatUYVY) ||
          (format == pixelFormatToWord32 SDLPixelFormatYVYU) ||
          (format == pixelFormatToWord32 SDLPixelFormatP010)
       then 2
       else 1
  else fromIntegral (format .&. 0xFF)

-- | A macro to determine if an SDL_PixelFormat is an indexed format
--
-- @since 3.2.0
sdlIsPixelFormatIndexed :: Word32 -> Bool
sdlIsPixelFormatIndexed format =
  not (sdlIsPixelFormatFourCC format) &&
  ((sdlPixelType format == fromEnum SDLPixelTypeIndex1) ||
   (sdlPixelType format == fromEnum SDLPixelTypeIndex2) ||
   (sdlPixelType format == fromEnum SDLPixelTypeIndex4) ||
   (sdlPixelType format == fromEnum SDLPixelTypeIndex8))

-- | A macro to determine if an SDL_PixelFormat is a packed format
--
-- @since 3.2.0
sdlIsPixelFormatPacked :: Word32 -> Bool
sdlIsPixelFormatPacked format =
  not (sdlIsPixelFormatFourCC format) &&
  ((sdlPixelType format == fromEnum SDLPixelTypePacked8) ||
   (sdlPixelType format == fromEnum SDLPixelTypePacked16) ||
   (sdlPixelType format == fromEnum SDLPixelTypePacked32))

-- | A macro to determine if an SDL_PixelFormat is an array format
--
-- @since 3.2.0
sdlIsPixelFormatArray :: Word32 -> Bool
sdlIsPixelFormatArray format =
  not (sdlIsPixelFormatFourCC format) &&
  ((sdlPixelType format == fromEnum SDLPixelTypeArrayU8) ||
   (sdlPixelType format == fromEnum SDLPixelTypeArrayU16) ||
   (sdlPixelType format == fromEnum SDLPixelTypeArrayU32) ||
   (sdlPixelType format == fromEnum SDLPixelTypeArrayF16) ||
   (sdlPixelType format == fromEnum SDLPixelTypeArrayF32))

-- | A macro to determine if an SDL_PixelFormat is a 10-bit format
--
-- @since 3.2.0
sdlIsPixelFormat10Bit :: Word32 -> Bool
sdlIsPixelFormat10Bit format =
  not (sdlIsPixelFormatFourCC format) &&
  ((sdlPixelType format == fromEnum SDLPixelTypePacked32) &&
   (sdlPixelLayout format == fromEnum SDLPackedLayout2101010))

-- | A macro to determine if an SDL_PixelFormat is a floating point format
--
-- @since 3.2.0
sdlIsPixelFormatFloat :: Word32 -> Bool
sdlIsPixelFormatFloat format =
  not (sdlIsPixelFormatFourCC format) &&
  ((sdlPixelType format == fromEnum SDLPixelTypeArrayF16) ||
   (sdlPixelType format == fromEnum SDLPixelTypeArrayF32))

-- | A macro to determine if an SDL_PixelFormat has an alpha channel
--
-- @since 3.2.0
sdlIsPixelFormatAlpha :: Word32 -> Bool
sdlIsPixelFormatAlpha format =
  (sdlIsPixelFormatPacked format &&
   ((sdlPixelOrder format == fromEnum SDLPackedOrderARGB) ||
    (sdlPixelOrder format == fromEnum SDLPackedOrderRGBA) ||
    (sdlPixelOrder format == fromEnum SDLPackedOrderABGR) ||
    (sdlPixelOrder format == fromEnum SDLPackedOrderBGRA))) ||
  (sdlIsPixelFormatArray format &&
   ((sdlPixelOrder format == fromEnum SDLArrayOrderARGB) ||
    (sdlPixelOrder format == fromEnum SDLArrayOrderRGBA) ||
    (sdlPixelOrder format == fromEnum SDLArrayOrderABGR) ||
    (sdlPixelOrder format == fromEnum SDLArrayOrderBGRA)))

-- | A macro to determine if an SDL_PixelFormat is a "FourCC" format
--
-- This covers custom and other unusual formats.
--
-- @since 3.2.0
sdlIsPixelFormatFourCC :: Word32 -> Bool
sdlIsPixelFormatFourCC format = format /= 0 && sdlPixelFlag format /= 1

-- | Pixel format
--
-- @since 3.2.0
data SDLPixelFormat
  = SDLPixelFormatUnknown
  | SDLPixelFormatIndex1LSB
  | SDLPixelFormatIndex1MSB
  | SDLPixelFormatIndex2LSB
  | SDLPixelFormatIndex2MSB
  | SDLPixelFormatIndex4LSB
  | SDLPixelFormatIndex4MSB
  | SDLPixelFormatIndex8
  | SDLPixelFormatRGB332
  | SDLPixelFormatXRGB4444
  | SDLPixelFormatXBGR4444
  | SDLPixelFormatXRGB1555
  | SDLPixelFormatXBGR1555
  | SDLPixelFormatARGB4444
  | SDLPixelFormatRGBA4444
  | SDLPixelFormatABGR4444
  | SDLPixelFormatBGRA4444
  | SDLPixelFormatARGB1555
  | SDLPixelFormatRGBA5551
  | SDLPixelFormatABGR1555
  | SDLPixelFormatBGRA5551
  | SDLPixelFormatRGB565
  | SDLPixelFormatBGR565
  | SDLPixelFormatRGB24
  | SDLPixelFormatBGR24
  | SDLPixelFormatXRGB8888
  | SDLPixelFormatRGBX8888
  | SDLPixelFormatXBGR8888
  | SDLPixelFormatBGRX8888
  | SDLPixelFormatARGB8888
  | SDLPixelFormatRGBA8888
  | SDLPixelFormatABGR8888
  | SDLPixelFormatBGRA8888
  | SDLPixelFormatXRGB2101010
  | SDLPixelFormatXBGR2101010
  | SDLPixelFormatARGB2101010
  | SDLPixelFormatABGR2101010
  | SDLPixelFormatRGB48
  | SDLPixelFormatBGR48
  | SDLPixelFormatRGBA64
  | SDLPixelFormatARGB64
  | SDLPixelFormatBGRA64
  | SDLPixelFormatABGR64
  | SDLPixelFormatRGB48Float
  | SDLPixelFormatBGR48Float
  | SDLPixelFormatRGBA64Float
  | SDLPixelFormatARGB64Float
  | SDLPixelFormatBGRA64Float
  | SDLPixelFormatABGR64Float
  | SDLPixelFormatRGB96Float
  | SDLPixelFormatBGR96Float
  | SDLPixelFormatRGBA128Float
  | SDLPixelFormatARGB128Float
  | SDLPixelFormatBGRA128Float
  | SDLPixelFormatABGR128Float
  | SDLPixelFormatYV12
  | SDLPixelFormatIYUV
  | SDLPixelFormatYUY2
  | SDLPixelFormatUYVY
  | SDLPixelFormatYVYU
  | SDLPixelFormatNV12
  | SDLPixelFormatNV21
  | SDLPixelFormatP010
  | SDLPixelFormatExternalOES
  | SDLPixelFormatMJPG
  | SDLPixelFormatRGBA32
  | SDLPixelFormatARGB32
  | SDLPixelFormatBGRA32
  | SDLPixelFormatABGR32
  | SDLPixelFormatRGBX32
  | SDLPixelFormatXRGB32
  | SDLPixelFormatBGRX32
  | SDLPixelFormatXBGR32
  deriving (Eq, Ord, Show, Read, Bounded)

instance Enum SDLPixelFormat where
  fromEnum = fromIntegral . pixelFormatToWord32
  toEnum n = case fromIntegral n :: Word32 of
    0           -> SDLPixelFormatUnknown
    0x11100100  -> SDLPixelFormatIndex1LSB
    0x11200100  -> SDLPixelFormatIndex1MSB
    0x1c100200  -> SDLPixelFormatIndex2LSB
    0x1c200200  -> SDLPixelFormatIndex2MSB
    0x12100400  -> SDLPixelFormatIndex4LSB
    0x12200400  -> SDLPixelFormatIndex4MSB
    0x13000801  -> SDLPixelFormatIndex8
    0x14110801  -> SDLPixelFormatRGB332
    0x15120c02  -> SDLPixelFormatXRGB4444
    0x15520c02  -> SDLPixelFormatXBGR4444
    0x15130f02  -> SDLPixelFormatXRGB1555
    0x15530f02  -> SDLPixelFormatXBGR1555
    0x15321002  -> SDLPixelFormatARGB4444
    0x15421002  -> SDLPixelFormatRGBA4444
    0x15721002  -> SDLPixelFormatABGR4444
    0x15821002  -> SDLPixelFormatBGRA4444
    0x15331002  -> SDLPixelFormatARGB1555
    0x15441002  -> SDLPixelFormatRGBA5551
    0x15731002  -> SDLPixelFormatABGR1555
    0x15841002  -> SDLPixelFormatBGRA5551
    0x15151002  -> SDLPixelFormatRGB565
    0x15551002  -> SDLPixelFormatBGR565
    0x17101803  -> SDLPixelFormatRGB24
    0x17401803  -> SDLPixelFormatBGR24
    0x16161804  -> SDLPixelFormatXRGB8888
    0x16261804  -> SDLPixelFormatRGBX8888
    0x16561804  -> SDLPixelFormatXBGR8888
    0x16661804  -> SDLPixelFormatBGRX8888
    0x16362004  -> SDLPixelFormatARGB8888
    0x16462004  -> SDLPixelFormatRGBA8888
    0x16762004  -> SDLPixelFormatABGR8888
    0x16862004  -> SDLPixelFormatBGRA8888
    0x16172004  -> SDLPixelFormatXRGB2101010
    0x16572004  -> SDLPixelFormatXBGR2101010
    0x16372004  -> SDLPixelFormatARGB2101010
    0x16772004  -> SDLPixelFormatABGR2101010
    0x18103006  -> SDLPixelFormatRGB48
    0x18403006  -> SDLPixelFormatBGR48
    0x18204008  -> SDLPixelFormatRGBA64
    0x18304008  -> SDLPixelFormatARGB64
    0x18504008  -> SDLPixelFormatBGRA64
    0x18604008  -> SDLPixelFormatABGR64
    0x1a103006  -> SDLPixelFormatRGB48Float
    0x1a403006  -> SDLPixelFormatBGR48Float
    0x1a204008  -> SDLPixelFormatRGBA64Float
    0x1a304008  -> SDLPixelFormatARGB64Float
    0x1a504008  -> SDLPixelFormatBGRA64Float
    0x1a604008  -> SDLPixelFormatABGR64Float
    0x1b10600c  -> SDLPixelFormatRGB96Float
    0x1b40600c  -> SDLPixelFormatBGR96Float
    0x1b208010  -> SDLPixelFormatRGBA128Float
    0x1b308010  -> SDLPixelFormatARGB128Float
    0x1b508010  -> SDLPixelFormatBGRA128Float
    0x1b608010  -> SDLPixelFormatABGR128Float
    0x32315659  -> SDLPixelFormatYV12
    0x56555949  -> SDLPixelFormatIYUV
    0x32595559  -> SDLPixelFormatYUY2
    0x59565955  -> SDLPixelFormatUYVY
    0x55595659  -> SDLPixelFormatYVYU
    0x3231564e  -> SDLPixelFormatNV12
    0x3132564e  -> SDLPixelFormatNV21
    0x30313050  -> SDLPixelFormatP010
    0x2053454f  -> SDLPixelFormatExternalOES
    0x47504a4d  -> SDLPixelFormatMJPG
    -- Platform-dependent aliases for RGBA byte arrays
#if SDL_BYTEORDER == SDL_BIG_ENDIAN
    v | v == pixelFormatToWord32 SDLPixelFormatRGBA8888 -> SDLPixelFormatRGBA32
    v | v == pixelFormatToWord32 SDLPixelFormatARGB8888 -> SDLPixelFormatARGB32
    v | v == pixelFormatToWord32 SDLPixelFormatBGRA8888 -> SDLPixelFormatBGRA32
    v | v == pixelFormatToWord32 SDLPixelFormatABGR8888 -> SDLPixelFormatABGR32
    v | v == pixelFormatToWord32 SDLPixelFormatRGBX8888 -> SDLPixelFormatRGBX32
    v | v == pixelFormatToWord32 SDLPixelFormatXRGB8888 -> SDLPixelFormatXRGB32
    v | v == pixelFormatToWord32 SDLPixelFormatBGRX8888 -> SDLPixelFormatBGRX32
    v | v == pixelFormatToWord32 SDLPixelFormatXBGR8888 -> SDLPixelFormatXBGR32
#else
    v | v == pixelFormatToWord32 SDLPixelFormatABGR8888 -> SDLPixelFormatRGBA32
    v | v == pixelFormatToWord32 SDLPixelFormatBGRA8888 -> SDLPixelFormatARGB32
    v | v == pixelFormatToWord32 SDLPixelFormatARGB8888 -> SDLPixelFormatBGRA32
    v | v == pixelFormatToWord32 SDLPixelFormatRGBA8888 -> SDLPixelFormatABGR32
    v | v == pixelFormatToWord32 SDLPixelFormatXBGR8888 -> SDLPixelFormatRGBX32
    v | v == pixelFormatToWord32 SDLPixelFormatBGRX8888 -> SDLPixelFormatXRGB32
    v | v == pixelFormatToWord32 SDLPixelFormatXRGB8888 -> SDLPixelFormatBGRX32
    v | v == pixelFormatToWord32 SDLPixelFormatRGBX8888 -> SDLPixelFormatXBGR32
#endif
    _ -> SDLPixelFormatUnknown  -- Default case for unrecognized values

-- | The numeric value of the pixel format
pixelFormatToWord32 :: SDLPixelFormat -> Word32
pixelFormatToWord32 SDLPixelFormatUnknown      = 0
pixelFormatToWord32 SDLPixelFormatIndex1LSB    = 0x11100100
pixelFormatToWord32 SDLPixelFormatIndex1MSB    = 0x11200100
pixelFormatToWord32 SDLPixelFormatIndex2LSB    = 0x1c100200
pixelFormatToWord32 SDLPixelFormatIndex2MSB    = 0x1c200200
pixelFormatToWord32 SDLPixelFormatIndex4LSB    = 0x12100400
pixelFormatToWord32 SDLPixelFormatIndex4MSB    = 0x12200400
pixelFormatToWord32 SDLPixelFormatIndex8       = 0x13000801
pixelFormatToWord32 SDLPixelFormatRGB332       = 0x14110801
pixelFormatToWord32 SDLPixelFormatXRGB4444     = 0x15120c02
pixelFormatToWord32 SDLPixelFormatXBGR4444     = 0x15520c02
pixelFormatToWord32 SDLPixelFormatXRGB1555     = 0x15130f02
pixelFormatToWord32 SDLPixelFormatXBGR1555     = 0x15530f02
pixelFormatToWord32 SDLPixelFormatARGB4444     = 0x15321002
pixelFormatToWord32 SDLPixelFormatRGBA4444     = 0x15421002
pixelFormatToWord32 SDLPixelFormatABGR4444     = 0x15721002
pixelFormatToWord32 SDLPixelFormatBGRA4444     = 0x15821002
pixelFormatToWord32 SDLPixelFormatARGB1555     = 0x15331002
pixelFormatToWord32 SDLPixelFormatRGBA5551     = 0x15441002
pixelFormatToWord32 SDLPixelFormatABGR1555     = 0x15731002
pixelFormatToWord32 SDLPixelFormatBGRA5551     = 0x15841002
pixelFormatToWord32 SDLPixelFormatRGB565       = 0x15151002
pixelFormatToWord32 SDLPixelFormatBGR565       = 0x15551002
pixelFormatToWord32 SDLPixelFormatRGB24        = 0x17101803
pixelFormatToWord32 SDLPixelFormatBGR24        = 0x17401803
pixelFormatToWord32 SDLPixelFormatXRGB8888     = 0x16161804
pixelFormatToWord32 SDLPixelFormatRGBX8888     = 0x16261804
pixelFormatToWord32 SDLPixelFormatXBGR8888     = 0x16561804
pixelFormatToWord32 SDLPixelFormatBGRX8888     = 0x16661804
pixelFormatToWord32 SDLPixelFormatARGB8888     = 0x16362004
pixelFormatToWord32 SDLPixelFormatRGBA8888     = 0x16462004
pixelFormatToWord32 SDLPixelFormatABGR8888     = 0x16762004
pixelFormatToWord32 SDLPixelFormatBGRA8888     = 0x16862004
pixelFormatToWord32 SDLPixelFormatXRGB2101010  = 0x16172004
pixelFormatToWord32 SDLPixelFormatXBGR2101010  = 0x16572004
pixelFormatToWord32 SDLPixelFormatARGB2101010  = 0x16372004
pixelFormatToWord32 SDLPixelFormatABGR2101010  = 0x16772004
pixelFormatToWord32 SDLPixelFormatRGB48        = 0x18103006
pixelFormatToWord32 SDLPixelFormatBGR48        = 0x18403006
pixelFormatToWord32 SDLPixelFormatRGBA64       = 0x18204008
pixelFormatToWord32 SDLPixelFormatARGB64       = 0x18304008
pixelFormatToWord32 SDLPixelFormatBGRA64       = 0x18504008
pixelFormatToWord32 SDLPixelFormatABGR64       = 0x18604008
pixelFormatToWord32 SDLPixelFormatRGB48Float   = 0x1a103006
pixelFormatToWord32 SDLPixelFormatBGR48Float   = 0x1a403006
pixelFormatToWord32 SDLPixelFormatRGBA64Float  = 0x1a204008
pixelFormatToWord32 SDLPixelFormatARGB64Float  = 0x1a304008
pixelFormatToWord32 SDLPixelFormatBGRA64Float  = 0x1a504008
pixelFormatToWord32 SDLPixelFormatABGR64Float  = 0x1a604008
pixelFormatToWord32 SDLPixelFormatRGB96Float   = 0x1b10600c
pixelFormatToWord32 SDLPixelFormatBGR96Float   = 0x1b40600c
pixelFormatToWord32 SDLPixelFormatRGBA128Float = 0x1b208010
pixelFormatToWord32 SDLPixelFormatARGB128Float = 0x1b308010
pixelFormatToWord32 SDLPixelFormatBGRA128Float = 0x1b508010
pixelFormatToWord32 SDLPixelFormatABGR128Float = 0x1b608010
pixelFormatToWord32 SDLPixelFormatYV12         = 0x32315659
pixelFormatToWord32 SDLPixelFormatIYUV         = 0x56555949
pixelFormatToWord32 SDLPixelFormatYUY2         = 0x32595559
pixelFormatToWord32 SDLPixelFormatUYVY         = 0x59565955
pixelFormatToWord32 SDLPixelFormatYVYU         = 0x55595659
pixelFormatToWord32 SDLPixelFormatNV12         = 0x3231564e
pixelFormatToWord32 SDLPixelFormatNV21         = 0x3132564e
pixelFormatToWord32 SDLPixelFormatP010         = 0x30313050
pixelFormatToWord32 SDLPixelFormatExternalOES  = 0x2053454f
pixelFormatToWord32 SDLPixelFormatMJPG         = 0x47504a4d
-- Platform-dependent aliases for RGBA byte arrays
#if SDL_BYTEORDER == SDL_BIG_ENDIAN
pixelFormatToWord32 SDLPixelFormatRGBA32       = pixelFormatToWord32 SDLPixelFormatRGBA8888
pixelFormatToWord32 SDLPixelFormatARGB32       = pixelFormatToWord32 SDLPixelFormatARGB8888
pixelFormatToWord32 SDLPixelFormatBGRA32       = pixelFormatToWord32 SDLPixelFormatBGRA8888
pixelFormatToWord32 SDLPixelFormatABGR32       = pixelFormatToWord32 SDLPixelFormatABGR8888
pixelFormatToWord32 SDLPixelFormatRGBX32       = pixelFormatToWord32 SDLPixelFormatRGBX8888
pixelFormatToWord32 SDLPixelFormatXRGB32       = pixelFormatToWord32 SDLPixelFormatXRGB8888
pixelFormatToWord32 SDLPixelFormatBGRX32       = pixelFormatToWord32 SDLPixelFormatBGRX8888
pixelFormatToWord32 SDLPixelFormatXBGR32       = pixelFormatToWord32 SDLPixelFormatXBGR8888
#else
pixelFormatToWord32 SDLPixelFormatRGBA32       = pixelFormatToWord32 SDLPixelFormatABGR8888
pixelFormatToWord32 SDLPixelFormatARGB32       = pixelFormatToWord32 SDLPixelFormatBGRA8888
pixelFormatToWord32 SDLPixelFormatBGRA32       = pixelFormatToWord32 SDLPixelFormatARGB8888
pixelFormatToWord32 SDLPixelFormatABGR32       = pixelFormatToWord32 SDLPixelFormatRGBA8888
pixelFormatToWord32 SDLPixelFormatRGBX32       = pixelFormatToWord32 SDLPixelFormatXBGR8888
pixelFormatToWord32 SDLPixelFormatXRGB32       = pixelFormatToWord32 SDLPixelFormatBGRX8888
pixelFormatToWord32 SDLPixelFormatBGRX32       = pixelFormatToWord32 SDLPixelFormatXRGB8888
pixelFormatToWord32 SDLPixelFormatXBGR32       = pixelFormatToWord32 SDLPixelFormatRGBX8888
#endif

-- | Colorspace color type
--
-- @since 3.2.0
data SDLColorType
  = SDLColorTypeUnknown
  | SDLColorTypeRGB
  | SDLColorTypeYCbCr
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Colorspace color range
--
-- @since 3.2.0
data SDLColorRange
  = SDLColorRangeUnknown
  | SDLColorRangeLimited -- ^ Narrow range, e.g. 16-235 for 8-bit RGB and luma, and 16-240 for 8-bit chroma
  | SDLColorRangeFull    -- ^ Full range, e.g. 0-255 for 8-bit RGB and luma, and 1-255 for 8-bit chroma
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Colorspace color primaries
--
-- @since 3.2.0
data SDLColorPrimaries
  = SDLColorPrimariesUnknown
  | SDLColorPrimariesBT709        -- ^ ITU-R BT.709-6
  | SDLColorPrimariesUnspecified
  | SDLColorPrimariesBT470M       -- ^ ITU-R BT.470-6 System M
  | SDLColorPrimariesBT470BG      -- ^ ITU-R BT.470-6 System B, G / ITU-R BT.601-7 625
  | SDLColorPrimariesBT601        -- ^ ITU-R BT.601-7 525, SMPTE 170M
  | SDLColorPrimariesSMPTE240     -- ^ SMPTE 240M, functionally the same as SDLColorPrimariesBT601
  | SDLColorPrimariesGenericFilm  -- ^ Generic film (color filters using Illuminant C)
  | SDLColorPrimariesBT2020       -- ^ ITU-R BT.2020-2 / ITU-R BT.2100-0
  | SDLColorPrimariesXYZ          -- ^ SMPTE ST 428-1
  | SDLColorPrimariesSMPTE431     -- ^ SMPTE RP 431-2
  | SDLColorPrimariesSMPTE432     -- ^ SMPTE EG 432-1 / DCI P3
  | SDLColorPrimariesEBU3213      -- ^ EBU Tech. 3213-E
  | SDLColorPrimariesCustom
  deriving (Eq, Ord, Show, Read, Enum)

-- | Colorspace transfer characteristics
--
-- @since 3.2.0
data SDLTransferCharacteristics
  = SDLTransferCharacteristicsUnknown
  | SDLTransferCharacteristicsBT709          -- ^ Rec. ITU-R BT.709-6 / ITU-R BT1361
  | SDLTransferCharacteristicsUnspecified
  | SDLTransferCharacteristicsGamma22        -- ^ ITU-R BT.470-6 System M / ITU-R BT1700 625 PAL & SECAM
  | SDLTransferCharacteristicsGamma28        -- ^ ITU-R BT.470-6 System B, G
  | SDLTransferCharacteristicsBT601          -- ^ SMPTE ST 170M / ITU-R BT.601-7 525 or 625
  | SDLTransferCharacteristicsSMPTE240       -- ^ SMPTE ST 240M
  | SDLTransferCharacteristicsLinear
  | SDLTransferCharacteristicsLog100
  | SDLTransferCharacteristicsLog100Sqrt10
  | SDLTransferCharacteristicsIEC61966       -- ^ IEC 61966-2-4
  | SDLTransferCharacteristicsBT1361         -- ^ ITU-R BT1361 Extended Colour Gamut
  | SDLTransferCharacteristicsSRGB           -- ^ IEC 61966-2-1 (sRGB or sYCC)
  | SDLTransferCharacteristicsBT2020_10bit   -- ^ ITU-R BT2020 for 10-bit system
  | SDLTransferCharacteristicsBT2020_12bit   -- ^ ITU-R BT2020 for 12-bit system
  | SDLTransferCharacteristicsPQ             -- ^ SMPTE ST 2084 for 10-, 12-, 14- and 16-bit systems
  | SDLTransferCharacteristicsSMPTE428       -- ^ SMPTE ST 428-1
  | SDLTransferCharacteristicsHLG            -- ^ ARIB STD-B67, known as "hybrid log-gamma" (HLG)
  | SDLTransferCharacteristicsCustom
  deriving (Eq, Ord, Show, Read, Enum)

-- | Colorspace matrix coefficients
--
-- @since 3.2.0
data SDLMatrixCoefficients
  = SDLMatrixCoefficientsIdentity
  | SDLMatrixCoefficientsBT709                -- ^ ITU-R BT.709-6
  | SDLMatrixCoefficientsUnspecified
  | SDLMatrixCoefficientsFCC                  -- ^ US FCC Title 47
  | SDLMatrixCoefficientsBT470BG              -- ^ ITU-R BT.470-6 System B, G / ITU-R BT.601-7 625, functionally the same as SDLMatrixCoefficientsBT601
  | SDLMatrixCoefficientsBT601                -- ^ ITU-R BT.601-7 525
  | SDLMatrixCoefficientsSMPTE240             -- ^ SMPTE 240M
  | SDLMatrixCoefficientsYCgCo
  | SDLMatrixCoefficientsBT2020NCL            -- ^ ITU-R BT.2020-2 non-constant luminance
  | SDLMatrixCoefficientsBT2020CL             -- ^ ITU-R BT.2020-2 constant luminance
  | SDLMatrixCoefficientsSMPTE2085            -- ^ SMPTE ST 2085
  | SDLMatrixCoefficientsChromaDerivedNCL
  | SDLMatrixCoefficientsChromaDerivedCL
  | SDLMatrixCoefficientsICTCP                -- ^ ITU-R BT.2100-0 ICTCP
  | SDLMatrixCoefficientsCustom
  deriving (Eq, Ord, Show, Read, Enum)

-- | Colorspace chroma sample location
--
-- @since 3.2.0
data SDLChromaLocation
  = SDLChromaLocationNone    -- ^ RGB, no chroma sampling
  | SDLChromaLocationLeft    -- ^ In MPEG-2, MPEG-4, and AVC, Cb and Cr are taken on midpoint of the left-edge of the 2x2 square. In other words, they have the same horizontal location as the top-left pixel, but is shifted one-half pixel down vertically.
  | SDLChromaLocationCenter  -- ^ In JPEG/JFIF, H.261, and MPEG-1, Cb and Cr are taken at the center of the 2x2 square. In other words, they are offset one-half pixel to the right and one-half pixel down compared to the top-left pixel.
  | SDLChromaLocationTopLeft -- ^ In HEVC for BT.2020 and BT.2100 content (in particular on Blu-rays), Cb and Cr are sampled at the same location as the group's top-left Y pixel ("co-sited", "co-located").
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | A macro for defining custom SDL_Colorspace formats
--
-- @since 3.2.0
sdlDefineColorspace :: SDLColorType -> SDLColorRange -> SDLColorPrimaries -> 
                       SDLTransferCharacteristics -> SDLMatrixCoefficients -> 
                       SDLChromaLocation -> Word32
sdlDefineColorspace colorType colorRange colorPrimaries transferChar matrixCoeff chromaLoc =
  ((fromIntegral $ fromEnum colorType) `shiftL` 28) .|.
  ((fromIntegral $ fromEnum colorRange) `shiftL` 24) .|.
  ((fromIntegral $ fromEnum chromaLoc) `shiftL` 20) .|.
  ((fromIntegral $ fromEnum colorPrimaries) `shiftL` 10) .|.
  ((fromIntegral $ fromEnum transferChar) `shiftL` 5) .|.
  (fromIntegral $ fromEnum matrixCoeff)

-- | A macro to retrieve the type of an SDL_Colorspace
--
-- @since 3.2.0
sdlColorspaceType :: Word32 -> SDLColorType
sdlColorspaceType cspace = toEnum $ fromIntegral ((cspace `shiftR` 28) .&. 0x0F)

-- | A macro to retrieve the range of an SDL_Colorspace
--
-- @since 3.2.0
sdlColorspaceRange :: Word32 -> SDLColorRange
sdlColorspaceRange cspace = toEnum $ fromIntegral ((cspace `shiftR` 24) .&. 0x0F)

-- | A macro to retrieve the chroma sample location of an SDL_Colorspace
--
-- @since 3.2.0
sdlColorspaceChroma :: Word32 -> SDLChromaLocation
sdlColorspaceChroma cspace = toEnum $ fromIntegral ((cspace `shiftR` 20) .&. 0x0F)

-- | A macro to retrieve the primaries of an SDL_Colorspace
--
-- @since 3.2.0
sdlColorspacePrimaries :: Word32 -> SDLColorPrimaries
sdlColorspacePrimaries cspace = toEnum $ fromIntegral ((cspace `shiftR` 10) .&. 0x1F)

-- | A macro to retrieve the transfer characteristics of an SDL_Colorspace
--
-- @since 3.2.0
sdlColorspaceTransfer :: Word32 -> SDLTransferCharacteristics
sdlColorspaceTransfer cspace = toEnum $ fromIntegral ((cspace `shiftR` 5) .&. 0x1F)

-- | A macro to retrieve the matrix coefficients of an SDL_Colorspace
--
-- @since 3.2.0
sdlColorspaceMatrix :: Word32 -> SDLMatrixCoefficients
sdlColorspaceMatrix cspace = toEnum $ fromIntegral (cspace .&. 0x1F)

-- | A macro to determine if an SDL_Colorspace uses BT601 (or BT470BG) matrix coefficients
--
-- @since 3.2.0
sdlIsColorspaceMatrixBT601 :: Word32 -> Bool
sdlIsColorspaceMatrixBT601 cspace = 
  sdlColorspaceMatrix cspace == SDLMatrixCoefficientsBT601 || 
  sdlColorspaceMatrix cspace == SDLMatrixCoefficientsBT470BG

-- | A macro to determine if an SDL_Colorspace uses BT709 matrix coefficients
--
-- @since 3.2.0
sdlIsColorspaceMatrixBT709 :: Word32 -> Bool
sdlIsColorspaceMatrixBT709 cspace = sdlColorspaceMatrix cspace == SDLMatrixCoefficientsBT709

-- | A macro to determine if an SDL_Colorspace uses BT2020_NCL matrix coefficients
--
-- @since 3.2.0
sdlIsColorspaceMatrixBT2020NCL :: Word32 -> Bool
sdlIsColorspaceMatrixBT2020NCL cspace = sdlColorspaceMatrix cspace == SDLMatrixCoefficientsBT2020NCL

-- | A macro to determine if an SDL_Colorspace has a limited range
--
-- @since 3.2.0
sdlIsColorspaceLimitedRange :: Word32 -> Bool
sdlIsColorspaceLimitedRange cspace = sdlColorspaceRange cspace /= SDLColorRangeFull

-- | A macro to determine if an SDL_Colorspace has a full range
--
-- @since 3.2.0
sdlIsColorspaceFullRange :: Word32 -> Bool
sdlIsColorspaceFullRange cspace = sdlColorspaceRange cspace == SDLColorRangeFull

-- | Colorspace definitions
--
-- @since 3.2.0
data SDLColorspace
  = SDLColorspaceUnknown
  | SDLColorspaceSRGB            -- ^ Equivalent to DXGI_COLOR_SPACE_RGB_FULL_G22_NONE_P709
  | SDLColorspaceSRGBLinear      -- ^ Equivalent to DXGI_COLOR_SPACE_RGB_FULL_G10_NONE_P709
  | SDLColorspaceHDR10           -- ^ Equivalent to DXGI_COLOR_SPACE_RGB_FULL_G2084_NONE_P2020
  | SDLColorspaceJPEG            -- ^ Equivalent to DXGI_COLOR_SPACE_YCBCR_FULL_G22_NONE_P709_X601
  | SDLColorspaceBT601Limited    -- ^ Equivalent to DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P601
  | SDLColorspaceBT601Full       -- ^ Equivalent to DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P601
  | SDLColorspaceBT709Limited    -- ^ Equivalent to DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P709
  | SDLColorspaceBT709Full       -- ^ Equivalent to DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P709
  | SDLColorspaceBT2020Limited   -- ^ Equivalent to DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P2020
  | SDLColorspaceBT2020Full      -- ^ Equivalent to DXGI_COLOR_SPACE_YCBCR_FULL_G22_LEFT_P2020
  | SDLColorspaceRGBDefault      -- ^ The default colorspace for RGB surfaces if no colorspace is specified
  | SDLColorspaceYUVDefault      -- ^ The default colorspace for YUV surfaces if no colorspace is specified
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

colorspaceToWord32 :: SDLColorspace -> Word32
colorspaceToWord32 SDLColorspaceUnknown      = 0
colorspaceToWord32 SDLColorspaceSRGB         = 0x120005a0
colorspaceToWord32 SDLColorspaceSRGBLinear   = 0x12000500
colorspaceToWord32 SDLColorspaceHDR10        = 0x12002600
colorspaceToWord32 SDLColorspaceJPEG         = 0x220004c6
colorspaceToWord32 SDLColorspaceBT601Limited = 0x211018c6
colorspaceToWord32 SDLColorspaceBT601Full    = 0x221018c6
colorspaceToWord32 SDLColorspaceBT709Limited = 0x21100421
colorspaceToWord32 SDLColorspaceBT709Full    = 0x22100421
colorspaceToWord32 SDLColorspaceBT2020Limited = 0x21102609
colorspaceToWord32 SDLColorspaceBT2020Full   = 0x22102609
colorspaceToWord32 SDLColorspaceRGBDefault   = colorspaceToWord32 SDLColorspaceSRGB
colorspaceToWord32 SDLColorspaceYUVDefault   = colorspaceToWord32 SDLColorspaceJPEG

-- | A structure that represents a color as RGBA components
--
-- @since 3.2.0
data SDLColor = SDLColor
  { colorR :: Word8  -- ^ Red component
  , colorG :: Word8  -- ^ Green component
  , colorB :: Word8  -- ^ Blue component
  , colorA :: Word8  -- ^ Alpha component
  } deriving (Eq, Show, Read)

instance Storable SDLColor where
  sizeOf _ = 4
  alignment _ = 1
  peek ptr = do
    r <- peekByteOff ptr 0
    g <- peekByteOff ptr 1
    b <- peekByteOff ptr 2
    a <- peekByteOff ptr 3
    return $ SDLColor r g b a
  poke ptr (SDLColor r g b a) = do
    pokeByteOff ptr 0 r
    pokeByteOff ptr 1 g
    pokeByteOff ptr 2 b
    pokeByteOff ptr 3 a

-- | A structure that represents a color as RGBA float components
--
-- @since 3.2.0
data SDLFColor = SDLFColor
  { fcolorR :: Float  -- ^ Red component
  , fcolorG :: Float  -- ^ Green component
  , fcolorB :: Float  -- ^ Blue component
  , fcolorA :: Float  -- ^ Alpha component
  } deriving (Eq, Show, Read)

instance Storable SDLFColor where
  sizeOf _ = 16  -- 4 floats * 4 bytes
  alignment _ = 4
  peek ptr = do
    r <- peekByteOff ptr 0
    g <- peekByteOff ptr 4
    b <- peekByteOff ptr 8
    a <- peekByteOff ptr 12
    return $ SDLFColor r g b a
  poke ptr (SDLFColor r g b a) = do
    pokeByteOff ptr 0 r
    pokeByteOff ptr 4 g
    pokeByteOff ptr 8 b
    pokeByteOff ptr 12 a

-- | A set of indexed colors representing a palette
--
-- @since 3.2.0
data SDLPalette = SDLPalette
  { paletteNColors :: Int          -- ^ Number of color entries
  , paletteColors  :: Ptr SDLColor -- ^ Array of colors
  , paletteVersion :: Word32       -- ^ For internal use only
  , paletteRefCount :: Int         -- ^ For internal use only
  } deriving (Eq, Show)

instance Storable SDLPalette where
  sizeOf _ = 24  -- sizeof(int) + sizeof(SDL_Color*) + sizeof(Uint32) + sizeof(int)
  alignment _ = 8
  peek ptr = do
    ncolors <- peekByteOff ptr 0
    colors <- peekByteOff ptr 8
    version <- peekByteOff ptr 16
    refcount <- peekByteOff ptr 20
    return $ SDLPalette ncolors colors version refcount
  poke ptr (SDLPalette ncolors colors version refcount) = do
    pokeByteOff ptr 0 ncolors
    pokeByteOff ptr 8 colors
    pokeByteOff ptr 16 version
    pokeByteOff ptr 20 refcount

-- | Details about the format of a pixel
--
-- @since 3.2.0
data SDLPixelFormatDetails = SDLPixelFormatDetails
  { pixelFormat      :: Word32  -- ^ SDL_PixelFormat value
  , bitsPerPixel     :: Word8   -- ^ Bits per pixel
  , bytesPerPixel    :: Word8   -- ^ Bytes per pixel
  , paddingBytes     :: (Word8, Word8)  -- ^ Padding bytes
  , rMask            :: Word32  -- ^ Red mask
  , gMask            :: Word32  -- ^ Green mask
  , bMask            :: Word32  -- ^ Blue mask
  , aMask            :: Word32  -- ^ Alpha mask
  , rBits            :: Word8   -- ^ Red bits
  , gBits            :: Word8   -- ^ Green bits
  , bBits            :: Word8   -- ^ Blue bits
  , aBits            :: Word8   -- ^ Alpha bits
  , rShift           :: Word8   -- ^ Red shift
  , gShift           :: Word8   -- ^ Green shift
  , bShift           :: Word8   -- ^ Blue shift
  , aShift           :: Word8   -- ^ Alpha shift
  } deriving (Eq, Show)

instance Storable SDLPixelFormatDetails where
  sizeOf _ = 32  -- Calculated from the header
  alignment _ = 4
  peek ptr = do
    format <- peekByteOff ptr 0
    bpp <- peekByteOff ptr 4
    bytes <- peekByteOff ptr 5
    pad1 <- peekByteOff ptr 6
    pad2 <- peekByteOff ptr 7
    rmask <- peekByteOff ptr 8
    gmask <- peekByteOff ptr 12
    bmask <- peekByteOff ptr 16
    amask <- peekByteOff ptr 20
    rbits <- peekByteOff ptr 24
    gbits <- peekByteOff ptr 25
    bbits <- peekByteOff ptr 26
    abits <- peekByteOff ptr 27
    rshift <- peekByteOff ptr 28
    gshift <- peekByteOff ptr 29
    bshift <- peekByteOff ptr 30
    ashift <- peekByteOff ptr 31
    return $ SDLPixelFormatDetails format bpp bytes (pad1, pad2) rmask gmask bmask amask rbits gbits bbits abits rshift gshift bshift ashift
  poke ptr (SDLPixelFormatDetails format bpp bytes (pad1, pad2) rmask gmask bmask amask rbits gbits bbits abits rshift gshift bshift ashift) = do
    pokeByteOff ptr 0 format
    pokeByteOff ptr 4 bpp
    pokeByteOff ptr 5 bytes
    pokeByteOff ptr 6 pad1
    pokeByteOff ptr 7 pad2
    pokeByteOff ptr 8 rmask
    pokeByteOff ptr 12 gmask
    pokeByteOff ptr 16 bmask
    pokeByteOff ptr 20 amask
    pokeByteOff ptr 24 rbits
    pokeByteOff ptr 25 gbits
    pokeByteOff ptr 26 bbits
    pokeByteOff ptr 27 abits
    pokeByteOff ptr 28 rshift
    pokeByteOff ptr 29 gshift
    pokeByteOff ptr 30 bshift
    pokeByteOff ptr 31 ashift

-- | Get the human readable name of a pixel format
--
-- @since 3.2.0
foreign import ccall "SDL_GetPixelFormatName"
  sdlGetPixelFormatNameRaw :: Word32 -> IO CString

-- | Get the human readable name of a pixel format
--
-- @since 3.2.0
sdlGetPixelFormatName :: SDLPixelFormat -> IO String
sdlGetPixelFormatName format = do
  cstr <- sdlGetPixelFormatNameRaw (pixelFormatToWord32 format)
  peekCString cstr

-- | Convert one of the enumerated pixel formats to a bpp value and RGBA masks
--
-- @since 3.2.0
foreign import ccall "SDL_GetMasksForPixelFormat"
  sdlGetMasksForPixelFormatRaw :: Word32 -> Ptr CInt -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO Bool

-- | Convert one of the enumerated pixel formats to a bpp value and RGBA masks
--
-- @since 3.2.0
sdlGetMasksForPixelFormat :: SDLPixelFormat -> IO (Maybe (Int, Word32, Word32, Word32, Word32))
sdlGetMasksForPixelFormat format = 
  alloca $ \bppPtr ->
  alloca $ \rMaskPtr ->
  alloca $ \gMaskPtr ->
  alloca $ \bMaskPtr ->
  alloca $ \aMaskPtr -> do
    success <- sdlGetMasksForPixelFormatRaw (pixelFormatToWord32 format) bppPtr rMaskPtr gMaskPtr bMaskPtr aMaskPtr
    if success
      then do
        bpp <- peek bppPtr
        rMask <- peek rMaskPtr
        gMask <- peek gMaskPtr
        bMask <- peek bMaskPtr
        aMask <- peek aMaskPtr
        return $ Just (fromIntegral bpp, rMask, gMask, bMask, aMask)
      else return Nothing

-- | Convert a bpp value and RGBA masks to an enumerated pixel format
--
-- @since 3.2.0
foreign import ccall "SDL_GetPixelFormatForMasks"
  sdlGetPixelFormatForMasksRaw :: CInt -> Word32 -> Word32 -> Word32 -> Word32 -> IO Word32

-- | Convert a bpp value and RGBA masks to an enumerated pixel format
--
-- @since 3.2.0
sdlGetPixelFormatForMasks :: Int -> Word32 -> Word32 -> Word32 -> Word32 -> IO SDLPixelFormat
sdlGetPixelFormatForMasks bpp rMask gMask bMask aMask = do
  formatValue <- sdlGetPixelFormatForMasksRaw (fromIntegral bpp) rMask gMask bMask aMask
  -- This would need a proper conversion from Word32 to SDLPixelFormat enum
  return SDLPixelFormatUnknown  -- Placeholder, proper implementation would map value to enum

-- | Create an SDL_PixelFormatDetails structure corresponding to a pixel format
--
-- @since 3.2.0
foreign import ccall "SDL_GetPixelFormatDetails"
  sdlGetPixelFormatDetailsRaw :: Word32 -> IO (Ptr SDLPixelFormatDetails)

-- | Create an SDL_PixelFormatDetails structure corresponding to a pixel format
--
-- @since 3.2.0
sdlGetPixelFormatDetails :: SDLPixelFormat -> IO (Maybe SDLPixelFormatDetails)
sdlGetPixelFormatDetails format = do
  detailsPtr <- sdlGetPixelFormatDetailsRaw (pixelFormatToWord32 format)
  if detailsPtr == nullPtr
    then return Nothing
    else do
      details <- peek detailsPtr
      return $ Just details

-- | Create a palette structure with the specified number of color entries
--
-- @since 3.2.0
foreign import ccall "SDL_CreatePalette"
  sdlCreatePalette :: Int -> IO (Ptr SDLPalette)

-- | Set a range of colors in a palette
--
-- @since 3.2.0
foreign import ccall "SDL_SetPaletteColors"
  sdlSetPaletteColors :: Ptr SDLPalette -> Ptr SDLColor -> Int -> Int -> IO Bool

-- | Free a palette created with SDL_CreatePalette()
--
-- @since 3.2.0
foreign import ccall "SDL_DestroyPalette"
  sdlDestroyPalette :: Ptr SDLPalette -> IO ()

-- | Map an RGB triple to an opaque pixel value for a given pixel format
--
-- @since 3.2.0
foreign import ccall "SDL_MapRGB"
  sdlMapRGB :: Ptr SDLPixelFormatDetails -> Ptr SDLPalette -> Word8 -> Word8 -> Word8 -> IO Word32

-- | Map an RGBA quadruple to a pixel value for a given pixel format
--
-- @since 3.2.0
foreign import ccall "SDL_MapRGBA"
  sdlMapRGBA :: Ptr SDLPixelFormatDetails -> Ptr SDLPalette -> Word8 -> Word8 -> Word8 -> Word8 -> IO Word32

-- | Get RGB values from a pixel in the specified format
--
-- @since 3.2.0
foreign import ccall "SDL_GetRGB"
  sdlGetRGB :: Word32 -> Ptr SDLPixelFormatDetails -> Ptr SDLPalette -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()

-- | Get RGBA values from a pixel in the specified format
--
-- @since 3.2.0
foreign import ccall "SDL_GetRGBA"
  sdlGetRGBA :: Word32 -> Ptr SDLPixelFormatDetails -> Ptr SDLPalette -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
