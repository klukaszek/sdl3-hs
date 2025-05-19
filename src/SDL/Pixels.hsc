-- SDL/Pixels.hsc
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-} -- For byte order check

-- |
-- Module      : SDL.Pixels
-- Description : Pixel format management and color manipulation
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- SDL offers facilities for pixel management.
--
-- Largely these facilities deal with pixel _format_: what does this set of
-- bits represent?
--
-- If you mostly want to think of a pixel as some combination of red, green,
-- blue, and maybe alpha intensities, this is all pretty straightforward, and
-- in many cases, is enough information to build a perfectly fine game.
--
-- However, the actual definition of a pixel is more complex than that:
--
-- Pixels are a representation of a color in a particular color space.

#include <SDL3/SDL_pixels.h>
#include <SDL3/SDL_endian.h> -- Needed for SDL_BYTEORDER

module SDL.Pixels
  ( -- * Alpha constants
    pattern SDL_ALPHA_OPAQUE
  , pattern SDL_ALPHA_OPAQUE_FLOAT
  , pattern SDL_ALPHA_TRANSPARENT
  , pattern SDL_ALPHA_TRANSPARENT_FLOAT

    -- * Pixel Type
  , SDLPixelType(..)
  , pattern SDL_PIXELTYPE_UNKNOWN
  , pattern SDL_PIXELTYPE_INDEX1
  , pattern SDL_PIXELTYPE_INDEX4
  , pattern SDL_PIXELTYPE_INDEX8
  , pattern SDL_PIXELTYPE_PACKED8
  , pattern SDL_PIXELTYPE_PACKED16
  , pattern SDL_PIXELTYPE_PACKED32
  , pattern SDL_PIXELTYPE_ARRAYU8
  , pattern SDL_PIXELTYPE_ARRAYU16
  , pattern SDL_PIXELTYPE_ARRAYU32
  , pattern SDL_PIXELTYPE_ARRAYF16
  , pattern SDL_PIXELTYPE_ARRAYF32
  , pattern SDL_PIXELTYPE_INDEX2

    -- * Bitmap Order
  , SDLBitmapOrder(..)
  , pattern SDL_BITMAPORDER_NONE
  , pattern SDL_BITMAPORDER_4321
  , pattern SDL_BITMAPORDER_1234

    -- * Packed Order
  , SDLPackedOrder(..)
  , pattern SDL_PACKEDORDER_NONE
  , pattern SDL_PACKEDORDER_XRGB
  , pattern SDL_PACKEDORDER_RGBX
  , pattern SDL_PACKEDORDER_ARGB
  , pattern SDL_PACKEDORDER_RGBA
  , pattern SDL_PACKEDORDER_XBGR
  , pattern SDL_PACKEDORDER_BGRX
  , pattern SDL_PACKEDORDER_ABGR
  , pattern SDL_PACKEDORDER_BGRA

    -- * Array Order
  , SDLArrayOrder(..)
  , pattern SDL_ARRAYORDER_NONE
  , pattern SDL_ARRAYORDER_RGB
  , pattern SDL_ARRAYORDER_RGBA
  , pattern SDL_ARRAYORDER_ARGB
  , pattern SDL_ARRAYORDER_BGR
  , pattern SDL_ARRAYORDER_BGRA
  , pattern SDL_ARRAYORDER_ABGR

    -- * Packed Layout
  , SDLPackedLayout(..)
  , pattern SDL_PACKEDLAYOUT_NONE
  , pattern SDL_PACKEDLAYOUT_332
  , pattern SDL_PACKEDLAYOUT_4444
  , pattern SDL_PACKEDLAYOUT_1555
  , pattern SDL_PACKEDLAYOUT_5551
  , pattern SDL_PACKEDLAYOUT_565
  , pattern SDL_PACKEDLAYOUT_8888
  , pattern SDL_PACKEDLAYOUT_2101010
  , pattern SDL_PACKEDLAYOUT_1010102

    -- * Pixel Format
  , SDLPixelFormat(..)
  , pattern SDL_PIXELFORMAT_UNKNOWN
  , pattern SDL_PIXELFORMAT_INDEX1LSB
  , pattern SDL_PIXELFORMAT_INDEX1MSB
  , pattern SDL_PIXELFORMAT_INDEX2LSB
  , pattern SDL_PIXELFORMAT_INDEX2MSB
  , pattern SDL_PIXELFORMAT_INDEX4LSB
  , pattern SDL_PIXELFORMAT_INDEX4MSB
  , pattern SDL_PIXELFORMAT_INDEX8
  , pattern SDL_PIXELFORMAT_RGB332
  , pattern SDL_PIXELFORMAT_XRGB4444
  , pattern SDL_PIXELFORMAT_XBGR4444
  , pattern SDL_PIXELFORMAT_XRGB1555
  , pattern SDL_PIXELFORMAT_XBGR1555
  , pattern SDL_PIXELFORMAT_ARGB4444
  , pattern SDL_PIXELFORMAT_RGBA4444
  , pattern SDL_PIXELFORMAT_ABGR4444
  , pattern SDL_PIXELFORMAT_BGRA4444
  , pattern SDL_PIXELFORMAT_ARGB1555
  , pattern SDL_PIXELFORMAT_RGBA5551
  , pattern SDL_PIXELFORMAT_ABGR1555
  , pattern SDL_PIXELFORMAT_BGRA5551
  , pattern SDL_PIXELFORMAT_RGB565
  , pattern SDL_PIXELFORMAT_BGR565
  , pattern SDL_PIXELFORMAT_RGB24
  , pattern SDL_PIXELFORMAT_BGR24
  , pattern SDL_PIXELFORMAT_XRGB8888
  , pattern SDL_PIXELFORMAT_RGBX8888
  , pattern SDL_PIXELFORMAT_XBGR8888
  , pattern SDL_PIXELFORMAT_BGRX8888
  , pattern SDL_PIXELFORMAT_ARGB8888
  , pattern SDL_PIXELFORMAT_RGBA8888
  , pattern SDL_PIXELFORMAT_ABGR8888
  , pattern SDL_PIXELFORMAT_BGRA8888
  , pattern SDL_PIXELFORMAT_XRGB2101010
  , pattern SDL_PIXELFORMAT_XBGR2101010
  , pattern SDL_PIXELFORMAT_ARGB2101010
  , pattern SDL_PIXELFORMAT_ABGR2101010
  , pattern SDL_PIXELFORMAT_RGB48
  , pattern SDL_PIXELFORMAT_BGR48
  , pattern SDL_PIXELFORMAT_RGBA64
  , pattern SDL_PIXELFORMAT_ARGB64
  , pattern SDL_PIXELFORMAT_BGRA64
  , pattern SDL_PIXELFORMAT_ABGR64
  , pattern SDL_PIXELFORMAT_RGB48_FLOAT
  , pattern SDL_PIXELFORMAT_BGR48_FLOAT
  , pattern SDL_PIXELFORMAT_RGBA64_FLOAT
  , pattern SDL_PIXELFORMAT_ARGB64_FLOAT
  , pattern SDL_PIXELFORMAT_BGRA64_FLOAT
  , pattern SDL_PIXELFORMAT_ABGR64_FLOAT
  , pattern SDL_PIXELFORMAT_RGB96_FLOAT
  , pattern SDL_PIXELFORMAT_BGR96_FLOAT
  , pattern SDL_PIXELFORMAT_RGBA128_FLOAT
  , pattern SDL_PIXELFORMAT_ARGB128_FLOAT
  , pattern SDL_PIXELFORMAT_BGRA128_FLOAT
  , pattern SDL_PIXELFORMAT_ABGR128_FLOAT
  , pattern SDL_PIXELFORMAT_YV12
  , pattern SDL_PIXELFORMAT_IYUV
  , pattern SDL_PIXELFORMAT_YUY2
  , pattern SDL_PIXELFORMAT_UYVY
  , pattern SDL_PIXELFORMAT_YVYU
  , pattern SDL_PIXELFORMAT_NV12
  , pattern SDL_PIXELFORMAT_NV21
  , pattern SDL_PIXELFORMAT_P010
  , pattern SDL_PIXELFORMAT_EXTERNAL_OES
  , pattern SDL_PIXELFORMAT_MJPG
  , pattern SDL_PIXELFORMAT_RGBA32
  , pattern SDL_PIXELFORMAT_ARGB32
  , pattern SDL_PIXELFORMAT_BGRA32
  , pattern SDL_PIXELFORMAT_ABGR32
  , pattern SDL_PIXELFORMAT_RGBX32
  , pattern SDL_PIXELFORMAT_XRGB32
  , pattern SDL_PIXELFORMAT_BGRX32
  , pattern SDL_PIXELFORMAT_XBGR32

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

    -- * Color Types
  , SDLColorType(..)
  , pattern SDL_COLOR_TYPE_UNKNOWN
  , pattern SDL_COLOR_TYPE_RGB
  , pattern SDL_COLOR_TYPE_YCBCR
  , SDLColorRange(..)
  , pattern SDL_COLOR_RANGE_UNKNOWN
  , pattern SDL_COLOR_RANGE_LIMITED
  , pattern SDL_COLOR_RANGE_FULL
  , SDLColorPrimaries(..)
  , pattern SDL_COLOR_PRIMARIES_UNKNOWN
  , pattern SDL_COLOR_PRIMARIES_BT709
  , pattern SDL_COLOR_PRIMARIES_UNSPECIFIED
  , pattern SDL_COLOR_PRIMARIES_BT470M
  , pattern SDL_COLOR_PRIMARIES_BT470BG
  , pattern SDL_COLOR_PRIMARIES_BT601
  , pattern SDL_COLOR_PRIMARIES_SMPTE240
  , pattern SDL_COLOR_PRIMARIES_GENERIC_FILM
  , pattern SDL_COLOR_PRIMARIES_BT2020
  , pattern SDL_COLOR_PRIMARIES_XYZ
  , pattern SDL_COLOR_PRIMARIES_SMPTE431
  , pattern SDL_COLOR_PRIMARIES_SMPTE432
  , pattern SDL_COLOR_PRIMARIES_EBU3213
  , pattern SDL_COLOR_PRIMARIES_CUSTOM
  , SDLTransferCharacteristics(..)
  , pattern SDL_TRANSFER_CHARACTERISTICS_UNKNOWN
  , pattern SDL_TRANSFER_CHARACTERISTICS_BT709
  , pattern SDL_TRANSFER_CHARACTERISTICS_UNSPECIFIED
  , pattern SDL_TRANSFER_CHARACTERISTICS_GAMMA22
  , pattern SDL_TRANSFER_CHARACTERISTICS_GAMMA28
  , pattern SDL_TRANSFER_CHARACTERISTICS_BT601
  , pattern SDL_TRANSFER_CHARACTERISTICS_SMPTE240
  , pattern SDL_TRANSFER_CHARACTERISTICS_LINEAR
  , pattern SDL_TRANSFER_CHARACTERISTICS_LOG100
  , pattern SDL_TRANSFER_CHARACTERISTICS_LOG100_SQRT10
  , pattern SDL_TRANSFER_CHARACTERISTICS_IEC61966
  , pattern SDL_TRANSFER_CHARACTERISTICS_BT1361
  , pattern SDL_TRANSFER_CHARACTERISTICS_SRGB
  , pattern SDL_TRANSFER_CHARACTERISTICS_BT2020_10BIT
  , pattern SDL_TRANSFER_CHARACTERISTICS_BT2020_12BIT
  , pattern SDL_TRANSFER_CHARACTERISTICS_PQ
  , pattern SDL_TRANSFER_CHARACTERISTICS_SMPTE428
  , pattern SDL_TRANSFER_CHARACTERISTICS_HLG
  , pattern SDL_TRANSFER_CHARACTERISTICS_CUSTOM
  , SDLMatrixCoefficients(..)
  , pattern SDL_MATRIX_COEFFICIENTS_IDENTITY
  , pattern SDL_MATRIX_COEFFICIENTS_BT709
  , pattern SDL_MATRIX_COEFFICIENTS_UNSPECIFIED
  , pattern SDL_MATRIX_COEFFICIENTS_FCC
  , pattern SDL_MATRIX_COEFFICIENTS_BT470BG
  , pattern SDL_MATRIX_COEFFICIENTS_BT601
  , pattern SDL_MATRIX_COEFFICIENTS_SMPTE240
  , pattern SDL_MATRIX_COEFFICIENTS_YCGCO
  , pattern SDL_MATRIX_COEFFICIENTS_BT2020_NCL
  , pattern SDL_MATRIX_COEFFICIENTS_BT2020_CL
  , pattern SDL_MATRIX_COEFFICIENTS_SMPTE2085
  , pattern SDL_MATRIX_COEFFICIENTS_CHROMA_DERIVED_NCL
  , pattern SDL_MATRIX_COEFFICIENTS_CHROMA_DERIVED_CL
  , pattern SDL_MATRIX_COEFFICIENTS_ICTCP
  , pattern SDL_MATRIX_COEFFICIENTS_CUSTOM
  , SDLChromaLocation(..)
  , pattern SDL_CHROMA_LOCATION_NONE
  , pattern SDL_CHROMA_LOCATION_LEFT
  , pattern SDL_CHROMA_LOCATION_CENTER
  , pattern SDL_CHROMA_LOCATION_TOPLEFT

    -- * Colorspace
  , SDLColorspace(..)
  , pattern SDL_COLORSPACE_UNKNOWN
  , pattern SDL_COLORSPACE_SRGB
  , pattern SDL_COLORSPACE_SRGB_LINEAR
  , pattern SDL_COLORSPACE_HDR10
  , pattern SDL_COLORSPACE_JPEG
  , pattern SDL_COLORSPACE_BT601_LIMITED
  , pattern SDL_COLORSPACE_BT601_FULL
  , pattern SDL_COLORSPACE_BT709_LIMITED
  , pattern SDL_COLORSPACE_BT709_FULL
  , pattern SDL_COLORSPACE_BT2020_LIMITED
  , pattern SDL_COLORSPACE_BT2020_FULL
  , pattern SDL_COLORSPACE_RGB_DEFAULT
  , pattern SDL_COLORSPACE_YUV_DEFAULT
  , sdlDefineColorspace
  , sdlColorspaceType
  , sdlColorspaceRange
  , sdlColorspaceChroma
  , sdlColorspacePrimaries
  , sdlColorspaceTransfer
  , sdlColorspaceMatrix
  , colorspaceToWord32
  , cUIntToColorspace
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
  , pixelFormatToCUInt
  , cUIntToPixelFormat

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
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (toBool, fromBool)
import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.Word
import Data.Int
import Data.Maybe (fromMaybe)

-- * Alpha constants

-- | A fully opaque 8-bit alpha value.
--
-- /Since: 3.2.0/
pattern SDL_ALPHA_OPAQUE :: Word8
pattern SDL_ALPHA_OPAQUE = #{const SDL_ALPHA_OPAQUE}

-- | A fully opaque floating point alpha value.
--
-- /Since: 3.2.0/
pattern SDL_ALPHA_OPAQUE_FLOAT :: Float
pattern SDL_ALPHA_OPAQUE_FLOAT = #{const SDL_ALPHA_OPAQUE_FLOAT}

-- | A fully transparent 8-bit alpha value.
--
-- /Since: 3.2.0/
pattern SDL_ALPHA_TRANSPARENT :: Word8
pattern SDL_ALPHA_TRANSPARENT = #{const SDL_ALPHA_TRANSPARENT}

-- | A fully transparent floating point alpha value.
--
-- /Since: 3.2.0/
pattern SDL_ALPHA_TRANSPARENT_FLOAT :: Float
pattern SDL_ALPHA_TRANSPARENT_FLOAT = #{const SDL_ALPHA_TRANSPARENT_FLOAT}

-- * Pixel Type

-- | Pixel type enumeration.
--
-- /Since: 3.2.0/
newtype SDLPixelType = SDLPixelType CInt deriving (Show, Eq, Ord, Storable)

pattern SDL_PIXELTYPE_UNKNOWN :: SDLPixelType
pattern SDL_PIXELTYPE_UNKNOWN = SDLPixelType #{const SDL_PIXELTYPE_UNKNOWN}
pattern SDL_PIXELTYPE_INDEX1 :: SDLPixelType
pattern SDL_PIXELTYPE_INDEX1 = SDLPixelType #{const SDL_PIXELTYPE_INDEX1}
pattern SDL_PIXELTYPE_INDEX4 :: SDLPixelType
pattern SDL_PIXELTYPE_INDEX4 = SDLPixelType #{const SDL_PIXELTYPE_INDEX4}
pattern SDL_PIXELTYPE_INDEX8 :: SDLPixelType
pattern SDL_PIXELTYPE_INDEX8 = SDLPixelType #{const SDL_PIXELTYPE_INDEX8}
pattern SDL_PIXELTYPE_PACKED8 :: SDLPixelType
pattern SDL_PIXELTYPE_PACKED8 = SDLPixelType #{const SDL_PIXELTYPE_PACKED8}
pattern SDL_PIXELTYPE_PACKED16 :: SDLPixelType
pattern SDL_PIXELTYPE_PACKED16 = SDLPixelType #{const SDL_PIXELTYPE_PACKED16}
pattern SDL_PIXELTYPE_PACKED32 :: SDLPixelType
pattern SDL_PIXELTYPE_PACKED32 = SDLPixelType #{const SDL_PIXELTYPE_PACKED32}
pattern SDL_PIXELTYPE_ARRAYU8 :: SDLPixelType
pattern SDL_PIXELTYPE_ARRAYU8 = SDLPixelType #{const SDL_PIXELTYPE_ARRAYU8}
pattern SDL_PIXELTYPE_ARRAYU16 :: SDLPixelType
pattern SDL_PIXELTYPE_ARRAYU16 = SDLPixelType #{const SDL_PIXELTYPE_ARRAYU16}
pattern SDL_PIXELTYPE_ARRAYU32 :: SDLPixelType
pattern SDL_PIXELTYPE_ARRAYU32 = SDLPixelType #{const SDL_PIXELTYPE_ARRAYU32}
pattern SDL_PIXELTYPE_ARRAYF16 :: SDLPixelType
pattern SDL_PIXELTYPE_ARRAYF16 = SDLPixelType #{const SDL_PIXELTYPE_ARRAYF16}
pattern SDL_PIXELTYPE_ARRAYF32 :: SDLPixelType
pattern SDL_PIXELTYPE_ARRAYF32 = SDLPixelType #{const SDL_PIXELTYPE_ARRAYF32}
pattern SDL_PIXELTYPE_INDEX2 :: SDLPixelType
pattern SDL_PIXELTYPE_INDEX2 = SDLPixelType #{const SDL_PIXELTYPE_INDEX2}

-- * Bitmap Order

-- | Bitmap pixel order, high bit -> low bit.
--
-- /Since: 3.2.0/
newtype SDLBitmapOrder = SDLBitmapOrder CInt deriving (Show, Eq, Ord, Storable)

pattern SDL_BITMAPORDER_NONE :: SDLBitmapOrder
pattern SDL_BITMAPORDER_NONE = SDLBitmapOrder #{const SDL_BITMAPORDER_NONE}
pattern SDL_BITMAPORDER_4321 :: SDLBitmapOrder
pattern SDL_BITMAPORDER_4321 = SDLBitmapOrder #{const SDL_BITMAPORDER_4321}
pattern SDL_BITMAPORDER_1234 :: SDLBitmapOrder
pattern SDL_BITMAPORDER_1234 = SDLBitmapOrder #{const SDL_BITMAPORDER_1234}

-- * Packed Order

-- | Packed component order, high bit -> low bit.
--
-- /Since: 3.2.0/
newtype SDLPackedOrder = SDLPackedOrder CInt deriving (Show, Eq, Ord, Storable)

pattern SDL_PACKEDORDER_NONE :: SDLPackedOrder
pattern SDL_PACKEDORDER_NONE = SDLPackedOrder #{const SDL_PACKEDORDER_NONE}
pattern SDL_PACKEDORDER_XRGB :: SDLPackedOrder
pattern SDL_PACKEDORDER_XRGB = SDLPackedOrder #{const SDL_PACKEDORDER_XRGB}
pattern SDL_PACKEDORDER_RGBX :: SDLPackedOrder
pattern SDL_PACKEDORDER_RGBX = SDLPackedOrder #{const SDL_PACKEDORDER_RGBX}
pattern SDL_PACKEDORDER_ARGB :: SDLPackedOrder
pattern SDL_PACKEDORDER_ARGB = SDLPackedOrder #{const SDL_PACKEDORDER_ARGB}
pattern SDL_PACKEDORDER_RGBA :: SDLPackedOrder
pattern SDL_PACKEDORDER_RGBA = SDLPackedOrder #{const SDL_PACKEDORDER_RGBA}
pattern SDL_PACKEDORDER_XBGR :: SDLPackedOrder
pattern SDL_PACKEDORDER_XBGR = SDLPackedOrder #{const SDL_PACKEDORDER_XBGR}
pattern SDL_PACKEDORDER_BGRX :: SDLPackedOrder
pattern SDL_PACKEDORDER_BGRX = SDLPackedOrder #{const SDL_PACKEDORDER_BGRX}
pattern SDL_PACKEDORDER_ABGR :: SDLPackedOrder
pattern SDL_PACKEDORDER_ABGR = SDLPackedOrder #{const SDL_PACKEDORDER_ABGR}
pattern SDL_PACKEDORDER_BGRA :: SDLPackedOrder
pattern SDL_PACKEDORDER_BGRA = SDLPackedOrder #{const SDL_PACKEDORDER_BGRA}

-- * Array Order

-- | Array component order, low byte -> high byte.
--
-- /Since: 3.2.0/
newtype SDLArrayOrder = SDLArrayOrder CInt deriving (Show, Eq, Ord, Storable)

pattern SDL_ARRAYORDER_NONE :: SDLArrayOrder
pattern SDL_ARRAYORDER_NONE = SDLArrayOrder #{const SDL_ARRAYORDER_NONE}
pattern SDL_ARRAYORDER_RGB :: SDLArrayOrder
pattern SDL_ARRAYORDER_RGB = SDLArrayOrder #{const SDL_ARRAYORDER_RGB}
pattern SDL_ARRAYORDER_RGBA :: SDLArrayOrder
pattern SDL_ARRAYORDER_RGBA = SDLArrayOrder #{const SDL_ARRAYORDER_RGBA}
pattern SDL_ARRAYORDER_ARGB :: SDLArrayOrder
pattern SDL_ARRAYORDER_ARGB = SDLArrayOrder #{const SDL_ARRAYORDER_ARGB}
pattern SDL_ARRAYORDER_BGR :: SDLArrayOrder
pattern SDL_ARRAYORDER_BGR = SDLArrayOrder #{const SDL_ARRAYORDER_BGR}
pattern SDL_ARRAYORDER_BGRA :: SDLArrayOrder
pattern SDL_ARRAYORDER_BGRA = SDLArrayOrder #{const SDL_ARRAYORDER_BGRA}
pattern SDL_ARRAYORDER_ABGR :: SDLArrayOrder
pattern SDL_ARRAYORDER_ABGR = SDLArrayOrder #{const SDL_ARRAYORDER_ABGR}

-- * Packed Layout

-- | Packed component layout.
--
-- /Since: 3.2.0/
newtype SDLPackedLayout = SDLPackedLayout CInt deriving (Show, Eq, Ord, Storable)

pattern SDL_PACKEDLAYOUT_NONE :: SDLPackedLayout
pattern SDL_PACKEDLAYOUT_NONE = SDLPackedLayout #{const SDL_PACKEDLAYOUT_NONE}
pattern SDL_PACKEDLAYOUT_332 :: SDLPackedLayout
pattern SDL_PACKEDLAYOUT_332 = SDLPackedLayout #{const SDL_PACKEDLAYOUT_332}
pattern SDL_PACKEDLAYOUT_4444 :: SDLPackedLayout
pattern SDL_PACKEDLAYOUT_4444 = SDLPackedLayout #{const SDL_PACKEDLAYOUT_4444}
pattern SDL_PACKEDLAYOUT_1555 :: SDLPackedLayout
pattern SDL_PACKEDLAYOUT_1555 = SDLPackedLayout #{const SDL_PACKEDLAYOUT_1555}
pattern SDL_PACKEDLAYOUT_5551 :: SDLPackedLayout
pattern SDL_PACKEDLAYOUT_5551 = SDLPackedLayout #{const SDL_PACKEDLAYOUT_5551}
pattern SDL_PACKEDLAYOUT_565 :: SDLPackedLayout
pattern SDL_PACKEDLAYOUT_565 = SDLPackedLayout #{const SDL_PACKEDLAYOUT_565}
pattern SDL_PACKEDLAYOUT_8888 :: SDLPackedLayout
pattern SDL_PACKEDLAYOUT_8888 = SDLPackedLayout #{const SDL_PACKEDLAYOUT_8888}
pattern SDL_PACKEDLAYOUT_2101010 :: SDLPackedLayout
pattern SDL_PACKEDLAYOUT_2101010 = SDLPackedLayout #{const SDL_PACKEDLAYOUT_2101010}
pattern SDL_PACKEDLAYOUT_1010102 :: SDLPackedLayout
pattern SDL_PACKEDLAYOUT_1010102 = SDLPackedLayout #{const SDL_PACKEDLAYOUT_1010102}

-- * Pixel Format

-- | Pixel format enumeration. This wraps a `CUInt` (unsigned 32-bit integer).
--
-- /Since: 3.2.0/
newtype SDLPixelFormat = SDLPixelFormat CUInt deriving (Show, Eq, Ord, Storable)

pattern SDL_PIXELFORMAT_UNKNOWN :: SDLPixelFormat
pattern SDL_PIXELFORMAT_UNKNOWN = SDLPixelFormat #{const SDL_PIXELFORMAT_UNKNOWN}
pattern SDL_PIXELFORMAT_INDEX1LSB :: SDLPixelFormat
pattern SDL_PIXELFORMAT_INDEX1LSB = SDLPixelFormat #{const SDL_PIXELFORMAT_INDEX1LSB}
pattern SDL_PIXELFORMAT_INDEX1MSB :: SDLPixelFormat
pattern SDL_PIXELFORMAT_INDEX1MSB = SDLPixelFormat #{const SDL_PIXELFORMAT_INDEX1MSB}
pattern SDL_PIXELFORMAT_INDEX2LSB :: SDLPixelFormat
pattern SDL_PIXELFORMAT_INDEX2LSB = SDLPixelFormat #{const SDL_PIXELFORMAT_INDEX2LSB}
pattern SDL_PIXELFORMAT_INDEX2MSB :: SDLPixelFormat
pattern SDL_PIXELFORMAT_INDEX2MSB = SDLPixelFormat #{const SDL_PIXELFORMAT_INDEX2MSB}
pattern SDL_PIXELFORMAT_INDEX4LSB :: SDLPixelFormat
pattern SDL_PIXELFORMAT_INDEX4LSB = SDLPixelFormat #{const SDL_PIXELFORMAT_INDEX4LSB}
pattern SDL_PIXELFORMAT_INDEX4MSB :: SDLPixelFormat
pattern SDL_PIXELFORMAT_INDEX4MSB = SDLPixelFormat #{const SDL_PIXELFORMAT_INDEX4MSB}
pattern SDL_PIXELFORMAT_INDEX8 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_INDEX8 = SDLPixelFormat #{const SDL_PIXELFORMAT_INDEX8}
pattern SDL_PIXELFORMAT_RGB332 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_RGB332 = SDLPixelFormat #{const SDL_PIXELFORMAT_RGB332}
pattern SDL_PIXELFORMAT_XRGB4444 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_XRGB4444 = SDLPixelFormat #{const SDL_PIXELFORMAT_XRGB4444}
pattern SDL_PIXELFORMAT_XBGR4444 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_XBGR4444 = SDLPixelFormat #{const SDL_PIXELFORMAT_XBGR4444}
pattern SDL_PIXELFORMAT_XRGB1555 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_XRGB1555 = SDLPixelFormat #{const SDL_PIXELFORMAT_XRGB1555}
pattern SDL_PIXELFORMAT_XBGR1555 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_XBGR1555 = SDLPixelFormat #{const SDL_PIXELFORMAT_XBGR1555}
pattern SDL_PIXELFORMAT_ARGB4444 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_ARGB4444 = SDLPixelFormat #{const SDL_PIXELFORMAT_ARGB4444}
pattern SDL_PIXELFORMAT_RGBA4444 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_RGBA4444 = SDLPixelFormat #{const SDL_PIXELFORMAT_RGBA4444}
pattern SDL_PIXELFORMAT_ABGR4444 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_ABGR4444 = SDLPixelFormat #{const SDL_PIXELFORMAT_ABGR4444}
pattern SDL_PIXELFORMAT_BGRA4444 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_BGRA4444 = SDLPixelFormat #{const SDL_PIXELFORMAT_BGRA4444}
pattern SDL_PIXELFORMAT_ARGB1555 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_ARGB1555 = SDLPixelFormat #{const SDL_PIXELFORMAT_ARGB1555}
pattern SDL_PIXELFORMAT_RGBA5551 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_RGBA5551 = SDLPixelFormat #{const SDL_PIXELFORMAT_RGBA5551}
pattern SDL_PIXELFORMAT_ABGR1555 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_ABGR1555 = SDLPixelFormat #{const SDL_PIXELFORMAT_ABGR1555}
pattern SDL_PIXELFORMAT_BGRA5551 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_BGRA5551 = SDLPixelFormat #{const SDL_PIXELFORMAT_BGRA5551}
pattern SDL_PIXELFORMAT_RGB565 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_RGB565 = SDLPixelFormat #{const SDL_PIXELFORMAT_RGB565}
pattern SDL_PIXELFORMAT_BGR565 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_BGR565 = SDLPixelFormat #{const SDL_PIXELFORMAT_BGR565}
pattern SDL_PIXELFORMAT_RGB24 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_RGB24 = SDLPixelFormat #{const SDL_PIXELFORMAT_RGB24}
pattern SDL_PIXELFORMAT_BGR24 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_BGR24 = SDLPixelFormat #{const SDL_PIXELFORMAT_BGR24}
pattern SDL_PIXELFORMAT_XRGB8888 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_XRGB8888 = SDLPixelFormat #{const SDL_PIXELFORMAT_XRGB8888}
pattern SDL_PIXELFORMAT_RGBX8888 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_RGBX8888 = SDLPixelFormat #{const SDL_PIXELFORMAT_RGBX8888}
pattern SDL_PIXELFORMAT_XBGR8888 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_XBGR8888 = SDLPixelFormat #{const SDL_PIXELFORMAT_XBGR8888}
pattern SDL_PIXELFORMAT_BGRX8888 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_BGRX8888 = SDLPixelFormat #{const SDL_PIXELFORMAT_BGRX8888}
pattern SDL_PIXELFORMAT_ARGB8888 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_ARGB8888 = SDLPixelFormat #{const SDL_PIXELFORMAT_ARGB8888}
pattern SDL_PIXELFORMAT_RGBA8888 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_RGBA8888 = SDLPixelFormat #{const SDL_PIXELFORMAT_RGBA8888}
pattern SDL_PIXELFORMAT_ABGR8888 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_ABGR8888 = SDLPixelFormat #{const SDL_PIXELFORMAT_ABGR8888}
pattern SDL_PIXELFORMAT_BGRA8888 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_BGRA8888 = SDLPixelFormat #{const SDL_PIXELFORMAT_BGRA8888}
pattern SDL_PIXELFORMAT_XRGB2101010 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_XRGB2101010 = SDLPixelFormat #{const SDL_PIXELFORMAT_XRGB2101010}
pattern SDL_PIXELFORMAT_XBGR2101010 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_XBGR2101010 = SDLPixelFormat #{const SDL_PIXELFORMAT_XBGR2101010}
pattern SDL_PIXELFORMAT_ARGB2101010 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_ARGB2101010 = SDLPixelFormat #{const SDL_PIXELFORMAT_ARGB2101010}
pattern SDL_PIXELFORMAT_ABGR2101010 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_ABGR2101010 = SDLPixelFormat #{const SDL_PIXELFORMAT_ABGR2101010}
pattern SDL_PIXELFORMAT_RGB48 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_RGB48 = SDLPixelFormat #{const SDL_PIXELFORMAT_RGB48}
pattern SDL_PIXELFORMAT_BGR48 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_BGR48 = SDLPixelFormat #{const SDL_PIXELFORMAT_BGR48}
pattern SDL_PIXELFORMAT_RGBA64 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_RGBA64 = SDLPixelFormat #{const SDL_PIXELFORMAT_RGBA64}
pattern SDL_PIXELFORMAT_ARGB64 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_ARGB64 = SDLPixelFormat #{const SDL_PIXELFORMAT_ARGB64}
pattern SDL_PIXELFORMAT_BGRA64 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_BGRA64 = SDLPixelFormat #{const SDL_PIXELFORMAT_BGRA64}
pattern SDL_PIXELFORMAT_ABGR64 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_ABGR64 = SDLPixelFormat #{const SDL_PIXELFORMAT_ABGR64}
pattern SDL_PIXELFORMAT_RGB48_FLOAT :: SDLPixelFormat
pattern SDL_PIXELFORMAT_RGB48_FLOAT = SDLPixelFormat #{const SDL_PIXELFORMAT_RGB48_FLOAT}
pattern SDL_PIXELFORMAT_BGR48_FLOAT :: SDLPixelFormat
pattern SDL_PIXELFORMAT_BGR48_FLOAT = SDLPixelFormat #{const SDL_PIXELFORMAT_BGR48_FLOAT}
pattern SDL_PIXELFORMAT_RGBA64_FLOAT :: SDLPixelFormat
pattern SDL_PIXELFORMAT_RGBA64_FLOAT = SDLPixelFormat #{const SDL_PIXELFORMAT_RGBA64_FLOAT}
pattern SDL_PIXELFORMAT_ARGB64_FLOAT :: SDLPixelFormat
pattern SDL_PIXELFORMAT_ARGB64_FLOAT = SDLPixelFormat #{const SDL_PIXELFORMAT_ARGB64_FLOAT}
pattern SDL_PIXELFORMAT_BGRA64_FLOAT :: SDLPixelFormat
pattern SDL_PIXELFORMAT_BGRA64_FLOAT = SDLPixelFormat #{const SDL_PIXELFORMAT_BGRA64_FLOAT}
pattern SDL_PIXELFORMAT_ABGR64_FLOAT :: SDLPixelFormat
pattern SDL_PIXELFORMAT_ABGR64_FLOAT = SDLPixelFormat #{const SDL_PIXELFORMAT_ABGR64_FLOAT}
pattern SDL_PIXELFORMAT_RGB96_FLOAT :: SDLPixelFormat
pattern SDL_PIXELFORMAT_RGB96_FLOAT = SDLPixelFormat #{const SDL_PIXELFORMAT_RGB96_FLOAT}
pattern SDL_PIXELFORMAT_BGR96_FLOAT :: SDLPixelFormat
pattern SDL_PIXELFORMAT_BGR96_FLOAT = SDLPixelFormat #{const SDL_PIXELFORMAT_BGR96_FLOAT}
pattern SDL_PIXELFORMAT_RGBA128_FLOAT :: SDLPixelFormat
pattern SDL_PIXELFORMAT_RGBA128_FLOAT = SDLPixelFormat #{const SDL_PIXELFORMAT_RGBA128_FLOAT}
pattern SDL_PIXELFORMAT_ARGB128_FLOAT :: SDLPixelFormat
pattern SDL_PIXELFORMAT_ARGB128_FLOAT = SDLPixelFormat #{const SDL_PIXELFORMAT_ARGB128_FLOAT}
pattern SDL_PIXELFORMAT_BGRA128_FLOAT :: SDLPixelFormat
pattern SDL_PIXELFORMAT_BGRA128_FLOAT = SDLPixelFormat #{const SDL_PIXELFORMAT_BGRA128_FLOAT}
pattern SDL_PIXELFORMAT_ABGR128_FLOAT :: SDLPixelFormat
pattern SDL_PIXELFORMAT_ABGR128_FLOAT = SDLPixelFormat #{const SDL_PIXELFORMAT_ABGR128_FLOAT}
pattern SDL_PIXELFORMAT_YV12 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_YV12 = SDLPixelFormat #{const SDL_PIXELFORMAT_YV12}
pattern SDL_PIXELFORMAT_IYUV :: SDLPixelFormat
pattern SDL_PIXELFORMAT_IYUV = SDLPixelFormat #{const SDL_PIXELFORMAT_IYUV}
pattern SDL_PIXELFORMAT_YUY2 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_YUY2 = SDLPixelFormat #{const SDL_PIXELFORMAT_YUY2}
pattern SDL_PIXELFORMAT_UYVY :: SDLPixelFormat
pattern SDL_PIXELFORMAT_UYVY = SDLPixelFormat #{const SDL_PIXELFORMAT_UYVY}
pattern SDL_PIXELFORMAT_YVYU :: SDLPixelFormat
pattern SDL_PIXELFORMAT_YVYU = SDLPixelFormat #{const SDL_PIXELFORMAT_YVYU}
pattern SDL_PIXELFORMAT_NV12 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_NV12 = SDLPixelFormat #{const SDL_PIXELFORMAT_NV12}
pattern SDL_PIXELFORMAT_NV21 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_NV21 = SDLPixelFormat #{const SDL_PIXELFORMAT_NV21}
pattern SDL_PIXELFORMAT_P010 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_P010 = SDLPixelFormat #{const SDL_PIXELFORMAT_P010}
pattern SDL_PIXELFORMAT_EXTERNAL_OES :: SDLPixelFormat
pattern SDL_PIXELFORMAT_EXTERNAL_OES = SDLPixelFormat #{const SDL_PIXELFORMAT_EXTERNAL_OES}
pattern SDL_PIXELFORMAT_MJPG :: SDLPixelFormat
pattern SDL_PIXELFORMAT_MJPG = SDLPixelFormat #{const SDL_PIXELFORMAT_MJPG}

-- Platform-dependent aliases for RGBA byte arrays
#if SDL_BYTEORDER == SDL_BIG_ENDIAN
pattern SDL_PIXELFORMAT_RGBA32 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_RGBA32 = SDLPixelFormat #{const SDL_PIXELFORMAT_RGBA32}
pattern SDL_PIXELFORMAT_ARGB32 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_ARGB32 = SDLPixelFormat #{const SDL_PIXELFORMAT_ARGB32}
pattern SDL_PIXELFORMAT_BGRA32 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_BGRA32 = SDLPixelFormat #{const SDL_PIXELFORMAT_BGRA32}
pattern SDL_PIXELFORMAT_ABGR32 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_ABGR32 = SDLPixelFormat #{const SDL_PIXELFORMAT_ABGR32}
pattern SDL_PIXELFORMAT_RGBX32 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_RGBX32 = SDLPixelFormat #{const SDL_PIXELFORMAT_RGBX32}
pattern SDL_PIXELFORMAT_XRGB32 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_XRGB32 = SDLPixelFormat #{const SDL_PIXELFORMAT_XRGB32}
pattern SDL_PIXELFORMAT_BGRX32 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_BGRX32 = SDLPixelFormat #{const SDL_PIXELFORMAT_BGRX32}
pattern SDL_PIXELFORMAT_XBGR32 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_XBGR32 = SDLPixelFormat #{const SDL_PIXELFORMAT_XBGR32}
#else
pattern SDL_PIXELFORMAT_RGBA32 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_RGBA32 = SDLPixelFormat #{const SDL_PIXELFORMAT_RGBA32}
pattern SDL_PIXELFORMAT_ARGB32 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_ARGB32 = SDLPixelFormat #{const SDL_PIXELFORMAT_ARGB32}
pattern SDL_PIXELFORMAT_BGRA32 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_BGRA32 = SDLPixelFormat #{const SDL_PIXELFORMAT_BGRA32}
pattern SDL_PIXELFORMAT_ABGR32 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_ABGR32 = SDLPixelFormat #{const SDL_PIXELFORMAT_ABGR32}
pattern SDL_PIXELFORMAT_RGBX32 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_RGBX32 = SDLPixelFormat #{const SDL_PIXELFORMAT_RGBX32}
pattern SDL_PIXELFORMAT_XRGB32 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_XRGB32 = SDLPixelFormat #{const SDL_PIXELFORMAT_XRGB32}
pattern SDL_PIXELFORMAT_BGRX32 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_BGRX32 = SDLPixelFormat #{const SDL_PIXELFORMAT_BGRX32}
pattern SDL_PIXELFORMAT_XBGR32 :: SDLPixelFormat
pattern SDL_PIXELFORMAT_XBGR32 = SDLPixelFormat #{const SDL_PIXELFORMAT_XBGR32}
#endif

-- Helper to convert SDLPixelFormat back to CUInt for FFI calls
pixelFormatToCUInt :: SDLPixelFormat -> CUInt
pixelFormatToCUInt (SDLPixelFormat val) = val

-- Helper to convert CUInt from FFI calls to SDLPixelFormat
-- Returns SDL_PIXELFORMAT_UNKNOWN if the value is not recognized.
-- Note: This is verbose but necessary as we can't derive Enum.
cUIntToPixelFormat :: CUInt -> SDLPixelFormat
cUIntToPixelFormat val = case val of
  #{const SDL_PIXELFORMAT_UNKNOWN} -> SDL_PIXELFORMAT_UNKNOWN
  #{const SDL_PIXELFORMAT_INDEX1LSB} -> SDL_PIXELFORMAT_INDEX1LSB
  #{const SDL_PIXELFORMAT_INDEX1MSB} -> SDL_PIXELFORMAT_INDEX1MSB
  #{const SDL_PIXELFORMAT_INDEX2LSB} -> SDL_PIXELFORMAT_INDEX2LSB
  #{const SDL_PIXELFORMAT_INDEX2MSB} -> SDL_PIXELFORMAT_INDEX2MSB
  #{const SDL_PIXELFORMAT_INDEX4LSB} -> SDL_PIXELFORMAT_INDEX4LSB
  #{const SDL_PIXELFORMAT_INDEX4MSB} -> SDL_PIXELFORMAT_INDEX4MSB
  #{const SDL_PIXELFORMAT_INDEX8} -> SDL_PIXELFORMAT_INDEX8
  #{const SDL_PIXELFORMAT_RGB332} -> SDL_PIXELFORMAT_RGB332
  #{const SDL_PIXELFORMAT_XRGB4444} -> SDL_PIXELFORMAT_XRGB4444
  #{const SDL_PIXELFORMAT_XBGR4444} -> SDL_PIXELFORMAT_XBGR4444
  #{const SDL_PIXELFORMAT_XRGB1555} -> SDL_PIXELFORMAT_XRGB1555
  #{const SDL_PIXELFORMAT_XBGR1555} -> SDL_PIXELFORMAT_XBGR1555
  #{const SDL_PIXELFORMAT_ARGB4444} -> SDL_PIXELFORMAT_ARGB4444
  #{const SDL_PIXELFORMAT_RGBA4444} -> SDL_PIXELFORMAT_RGBA4444
  #{const SDL_PIXELFORMAT_ABGR4444} -> SDL_PIXELFORMAT_ABGR4444
  #{const SDL_PIXELFORMAT_BGRA4444} -> SDL_PIXELFORMAT_BGRA4444
  #{const SDL_PIXELFORMAT_ARGB1555} -> SDL_PIXELFORMAT_ARGB1555
  #{const SDL_PIXELFORMAT_RGBA5551} -> SDL_PIXELFORMAT_RGBA5551
  #{const SDL_PIXELFORMAT_ABGR1555} -> SDL_PIXELFORMAT_ABGR1555
  #{const SDL_PIXELFORMAT_BGRA5551} -> SDL_PIXELFORMAT_BGRA5551
  #{const SDL_PIXELFORMAT_RGB565} -> SDL_PIXELFORMAT_RGB565
  #{const SDL_PIXELFORMAT_BGR565} -> SDL_PIXELFORMAT_BGR565
  #{const SDL_PIXELFORMAT_RGB24} -> SDL_PIXELFORMAT_RGB24
  #{const SDL_PIXELFORMAT_BGR24} -> SDL_PIXELFORMAT_BGR24
  #{const SDL_PIXELFORMAT_XRGB8888} -> SDL_PIXELFORMAT_XRGB8888
  #{const SDL_PIXELFORMAT_RGBX8888} -> SDL_PIXELFORMAT_RGBX8888
  #{const SDL_PIXELFORMAT_XBGR8888} -> SDL_PIXELFORMAT_XBGR8888
  #{const SDL_PIXELFORMAT_BGRX8888} -> SDL_PIXELFORMAT_BGRX8888
  #{const SDL_PIXELFORMAT_ARGB8888} -> SDL_PIXELFORMAT_ARGB8888
  #{const SDL_PIXELFORMAT_RGBA8888} -> SDL_PIXELFORMAT_RGBA8888
  #{const SDL_PIXELFORMAT_ABGR8888} -> SDL_PIXELFORMAT_ABGR8888
  #{const SDL_PIXELFORMAT_BGRA8888} -> SDL_PIXELFORMAT_BGRA8888
  #{const SDL_PIXELFORMAT_XRGB2101010} -> SDL_PIXELFORMAT_XRGB2101010
  #{const SDL_PIXELFORMAT_XBGR2101010} -> SDL_PIXELFORMAT_XBGR2101010
  #{const SDL_PIXELFORMAT_ARGB2101010} -> SDL_PIXELFORMAT_ARGB2101010
  #{const SDL_PIXELFORMAT_ABGR2101010} -> SDL_PIXELFORMAT_ABGR2101010
  #{const SDL_PIXELFORMAT_RGB48} -> SDL_PIXELFORMAT_RGB48
  #{const SDL_PIXELFORMAT_BGR48} -> SDL_PIXELFORMAT_BGR48
  #{const SDL_PIXELFORMAT_RGBA64} -> SDL_PIXELFORMAT_RGBA64
  #{const SDL_PIXELFORMAT_ARGB64} -> SDL_PIXELFORMAT_ARGB64
  #{const SDL_PIXELFORMAT_BGRA64} -> SDL_PIXELFORMAT_BGRA64
  #{const SDL_PIXELFORMAT_ABGR64} -> SDL_PIXELFORMAT_ABGR64
  #{const SDL_PIXELFORMAT_RGB48_FLOAT} -> SDL_PIXELFORMAT_RGB48_FLOAT
  #{const SDL_PIXELFORMAT_BGR48_FLOAT} -> SDL_PIXELFORMAT_BGR48_FLOAT
  #{const SDL_PIXELFORMAT_RGBA64_FLOAT} -> SDL_PIXELFORMAT_RGBA64_FLOAT
  #{const SDL_PIXELFORMAT_ARGB64_FLOAT} -> SDL_PIXELFORMAT_ARGB64_FLOAT
  #{const SDL_PIXELFORMAT_BGRA64_FLOAT} -> SDL_PIXELFORMAT_BGRA64_FLOAT
  #{const SDL_PIXELFORMAT_ABGR64_FLOAT} -> SDL_PIXELFORMAT_ABGR64_FLOAT
  #{const SDL_PIXELFORMAT_RGB96_FLOAT} -> SDL_PIXELFORMAT_RGB96_FLOAT
  #{const SDL_PIXELFORMAT_BGR96_FLOAT} -> SDL_PIXELFORMAT_BGR96_FLOAT
  #{const SDL_PIXELFORMAT_RGBA128_FLOAT} -> SDL_PIXELFORMAT_RGBA128_FLOAT
  #{const SDL_PIXELFORMAT_ARGB128_FLOAT} -> SDL_PIXELFORMAT_ARGB128_FLOAT
  #{const SDL_PIXELFORMAT_BGRA128_FLOAT} -> SDL_PIXELFORMAT_BGRA128_FLOAT
  #{const SDL_PIXELFORMAT_ABGR128_FLOAT} -> SDL_PIXELFORMAT_ABGR128_FLOAT
  #{const SDL_PIXELFORMAT_YV12} -> SDL_PIXELFORMAT_YV12
  #{const SDL_PIXELFORMAT_IYUV} -> SDL_PIXELFORMAT_IYUV
  #{const SDL_PIXELFORMAT_YUY2} -> SDL_PIXELFORMAT_YUY2
  #{const SDL_PIXELFORMAT_UYVY} -> SDL_PIXELFORMAT_UYVY
  #{const SDL_PIXELFORMAT_YVYU} -> SDL_PIXELFORMAT_YVYU
  #{const SDL_PIXELFORMAT_NV12} -> SDL_PIXELFORMAT_NV12
  #{const SDL_PIXELFORMAT_NV21} -> SDL_PIXELFORMAT_NV21
  #{const SDL_PIXELFORMAT_P010} -> SDL_PIXELFORMAT_P010
  #{const SDL_PIXELFORMAT_EXTERNAL_OES} -> SDL_PIXELFORMAT_EXTERNAL_OES
  #{const SDL_PIXELFORMAT_MJPG} -> SDL_PIXELFORMAT_MJPG
-- Handle platform-dependent aliases explicitly
#if SDL_BYTEORDER == SDL_BIG_ENDIAN
  v | v == #{const SDL_PIXELFORMAT_RGBA32} -> SDL_PIXELFORMAT_RGBA32
  v | v == #{const SDL_PIXELFORMAT_ARGB32} -> SDL_PIXELFORMAT_ARGB32
  v | v == #{const SDL_PIXELFORMAT_BGRA32} -> SDL_PIXELFORMAT_BGRA32
  v | v == #{const SDL_PIXELFORMAT_ABGR32} -> SDL_PIXELFORMAT_ABGR32
  v | v == #{const SDL_PIXELFORMAT_RGBX32} -> SDL_PIXELFORMAT_RGBX32
  v | v == #{const SDL_PIXELFORMAT_XRGB32} -> SDL_PIXELFORMAT_XRGB32
  v | v == #{const SDL_PIXELFORMAT_BGRX32} -> SDL_PIXELFORMAT_BGRX32
  v | v == #{const SDL_PIXELFORMAT_XBGR32} -> SDL_PIXELFORMAT_XBGR32
#else
  v | v == #{const SDL_PIXELFORMAT_RGBA32} -> SDL_PIXELFORMAT_RGBA32
  v | v == #{const SDL_PIXELFORMAT_ARGB32} -> SDL_PIXELFORMAT_ARGB32
  v | v == #{const SDL_PIXELFORMAT_BGRA32} -> SDL_PIXELFORMAT_BGRA32
  v | v == #{const SDL_PIXELFORMAT_ABGR32} -> SDL_PIXELFORMAT_ABGR32
  v | v == #{const SDL_PIXELFORMAT_RGBX32} -> SDL_PIXELFORMAT_RGBX32
  v | v == #{const SDL_PIXELFORMAT_XRGB32} -> SDL_PIXELFORMAT_XRGB32
  v | v == #{const SDL_PIXELFORMAT_BGRX32} -> SDL_PIXELFORMAT_BGRX32
  v | v == #{const SDL_PIXELFORMAT_XBGR32} -> SDL_PIXELFORMAT_XBGR32
#endif
  _ -> SDL_PIXELFORMAT_UNKNOWN -- Default for unrecognized values

-- * Pixel Format Macros

-- | A macro for defining custom FourCC pixel formats.
--
-- /Since: 3.2.0/
sdlDefinePixelFourCC :: Char -> Char -> Char -> Char -> SDLPixelFormat
sdlDefinePixelFourCC a b c d = SDLPixelFormat $
  (fromIntegral (fromEnum a) `shiftL` 0) .|.
  (fromIntegral (fromEnum b) `shiftL` 8) .|.
  (fromIntegral (fromEnum c) `shiftL` 16) .|.
  (fromIntegral (fromEnum d) `shiftL` 24)

-- | A macro for defining custom non-FourCC pixel formats.
--
-- /Since: 3.2.0/
sdlDefinePixelFormat :: SDLPixelType -> SDLBitmapOrder -> SDLPackedLayout -> Int -> Int -> SDLPixelFormat
sdlDefinePixelFormat (SDLPixelType pixelType) (SDLBitmapOrder order) (SDLPackedLayout layout) bits bytes =
  SDLPixelFormat $
    (1 `shiftL` 28) .|.
    (fromIntegral pixelType `shiftL` 24) .|.
    (fromIntegral order `shiftL` 20) .|.
    (fromIntegral layout `shiftL` 16) .|.
    (fromIntegral bits `shiftL` 8) .|.
    fromIntegral bytes

-- Helper to extract Word32 from SDLPixelFormat for macros
getPixelFormatValue :: SDLPixelFormat -> Word32
getPixelFormatValue (SDLPixelFormat val) = fromIntegral val -- CUInt to Word32

-- | A macro to retrieve the flags of an SDL_PixelFormat.
--
-- /Since: 3.2.0/
sdlPixelFlag :: SDLPixelFormat -> Int
sdlPixelFlag format = fromIntegral ((getPixelFormatValue format `shiftR` 28) .&. 0x0F)

-- | A macro to retrieve the type of an SDL_PixelFormat.
--
-- Returns the raw CInt value. Use `toEnum` or pattern match for `SDLPixelType`.
--
-- /Since: 3.2.0/
sdlPixelType :: SDLPixelFormat -> SDLPixelType
sdlPixelType format = SDLPixelType $ fromIntegral ((getPixelFormatValue format `shiftR` 24) .&. 0x0F)

-- | A macro to retrieve the order of an SDL_PixelFormat.
--
-- Returns the raw CInt value. Use `toEnum` or pattern match for the appropriate order type.
--
-- /Since: 3.2.0/
sdlPixelOrder :: SDLPixelFormat -> CInt
sdlPixelOrder format = fromIntegral ((getPixelFormatValue format `shiftR` 20) .&. 0x0F)

-- | A macro to retrieve the layout of an SDL_PixelFormat.
--
-- Returns the raw CInt value. Use `toEnum` or pattern match for `SDLPackedLayout`.
--
-- /Since: 3.2.0/
sdlPixelLayout :: SDLPixelFormat -> SDLPackedLayout
sdlPixelLayout format = SDLPackedLayout $ fromIntegral ((getPixelFormatValue format `shiftR` 16) .&. 0x0F)

-- | A macro to determine an SDL_PixelFormat's bits per pixel.
--
-- FourCC formats will report zero here, as it rarely makes sense to measure them per-pixel.
--
-- /Since: 3.2.0/
sdlBitsPerPixel :: SDLPixelFormat -> Int
sdlBitsPerPixel format =
  if sdlIsPixelFormatFourCC format
  then 0
  else fromIntegral ((getPixelFormatValue format `shiftR` 8) .&. 0xFF)

-- | A macro to determine an SDL_PixelFormat's bytes per pixel.
--
-- FourCC formats do their best here, but many of them don't have a meaningful measurement of bytes per pixel.
--
-- /Since: 3.2.0/
sdlBytesPerPixel :: SDLPixelFormat -> Int
sdlBytesPerPixel format =
  if sdlIsPixelFormatFourCC format
  then if format == SDL_PIXELFORMAT_YUY2 ||
          format == SDL_PIXELFORMAT_UYVY ||
          format == SDL_PIXELFORMAT_YVYU ||
          format == SDL_PIXELFORMAT_P010
       then 2
       else 1
  else fromIntegral (getPixelFormatValue format .&. 0xFF)

-- | A macro to determine if an SDL_PixelFormat is an indexed format.
--
-- /Since: 3.2.0/
sdlIsPixelFormatIndexed :: SDLPixelFormat -> Bool
sdlIsPixelFormatIndexed format =
  not (sdlIsPixelFormatFourCC format) &&
  let pt = sdlPixelType format
  in pt == SDL_PIXELTYPE_INDEX1 ||
     pt == SDL_PIXELTYPE_INDEX2 ||
     pt == SDL_PIXELTYPE_INDEX4 ||
     pt == SDL_PIXELTYPE_INDEX8

-- | A macro to determine if an SDL_PixelFormat is a packed format.
--
-- /Since: 3.2.0/
sdlIsPixelFormatPacked :: SDLPixelFormat -> Bool
sdlIsPixelFormatPacked format =
  not (sdlIsPixelFormatFourCC format) &&
  let pt = sdlPixelType format
  in pt == SDL_PIXELTYPE_PACKED8 ||
     pt == SDL_PIXELTYPE_PACKED16 ||
     pt == SDL_PIXELTYPE_PACKED32

-- | A macro to determine if an SDL_PixelFormat is an array format.
--
-- /Since: 3.2.0/
sdlIsPixelFormatArray :: SDLPixelFormat -> Bool
sdlIsPixelFormatArray format =
  not (sdlIsPixelFormatFourCC format) &&
  let pt = sdlPixelType format
  in pt == SDL_PIXELTYPE_ARRAYU8 ||
     pt == SDL_PIXELTYPE_ARRAYU16 ||
     pt == SDL_PIXELTYPE_ARRAYU32 ||
     pt == SDL_PIXELTYPE_ARRAYF16 ||
     pt == SDL_PIXELTYPE_ARRAYF32

-- | A macro to determine if an SDL_PixelFormat is a 10-bit format.
--
-- /Since: 3.2.0/
sdlIsPixelFormat10Bit :: SDLPixelFormat -> Bool
sdlIsPixelFormat10Bit format =
  not (sdlIsPixelFormatFourCC format) &&
  sdlPixelType format == SDL_PIXELTYPE_PACKED32 &&
  sdlPixelLayout format == SDL_PACKEDLAYOUT_2101010

-- | A macro to determine if an SDL_PixelFormat is a floating point format.
--
-- /Since: 3.2.0/
sdlIsPixelFormatFloat :: SDLPixelFormat -> Bool
sdlIsPixelFormatFloat format =
  not (sdlIsPixelFormatFourCC format) &&
  let pt = sdlPixelType format
  in pt == SDL_PIXELTYPE_ARRAYF16 ||
     pt == SDL_PIXELTYPE_ARRAYF32

-- | A macro to determine if an SDL_PixelFormat has an alpha channel.
--
-- /Since: 3.2.0/
sdlIsPixelFormatAlpha :: SDLPixelFormat -> Bool
sdlIsPixelFormatAlpha format =
  let po = SDLPackedOrder (sdlPixelOrder format) -- Assuming packed order enum
      ao = SDLArrayOrder (sdlPixelOrder format)  -- Assuming array order enum
  in (sdlIsPixelFormatPacked format &&
      (po == SDL_PACKEDORDER_ARGB ||
       po == SDL_PACKEDORDER_RGBA ||
       po == SDL_PACKEDORDER_ABGR ||
       po == SDL_PACKEDORDER_BGRA)) ||
     (sdlIsPixelFormatArray format &&
      (ao == SDL_ARRAYORDER_ARGB ||
       ao == SDL_ARRAYORDER_RGBA ||
       ao == SDL_ARRAYORDER_ABGR ||
       ao == SDL_ARRAYORDER_BGRA))

-- | A macro to determine if an SDL_PixelFormat is a "FourCC" format.
--
-- This covers custom and other unusual formats.
--
-- /Since: 3.2.0/
sdlIsPixelFormatFourCC :: SDLPixelFormat -> Bool
sdlIsPixelFormatFourCC format = format /= SDL_PIXELFORMAT_UNKNOWN && sdlPixelFlag format /= 1

-- * Color Types

-- | Colorspace color type.
--
-- /Since: 3.2.0/
newtype SDLColorType = SDLColorType CInt deriving (Show, Eq, Ord, Storable)

pattern SDL_COLOR_TYPE_UNKNOWN :: SDLColorType
pattern SDL_COLOR_TYPE_UNKNOWN = SDLColorType #{const SDL_COLOR_TYPE_UNKNOWN}
pattern SDL_COLOR_TYPE_RGB :: SDLColorType
pattern SDL_COLOR_TYPE_RGB = SDLColorType #{const SDL_COLOR_TYPE_RGB}
pattern SDL_COLOR_TYPE_YCBCR :: SDLColorType
pattern SDL_COLOR_TYPE_YCBCR = SDLColorType #{const SDL_COLOR_TYPE_YCBCR}

-- | Colorspace color range.
--
-- /Since: 3.2.0/
newtype SDLColorRange = SDLColorRange CInt deriving (Show, Eq, Ord, Storable)

pattern SDL_COLOR_RANGE_UNKNOWN :: SDLColorRange
pattern SDL_COLOR_RANGE_UNKNOWN = SDLColorRange #{const SDL_COLOR_RANGE_UNKNOWN}
pattern SDL_COLOR_RANGE_LIMITED :: SDLColorRange
pattern SDL_COLOR_RANGE_LIMITED = SDLColorRange #{const SDL_COLOR_RANGE_LIMITED}
pattern SDL_COLOR_RANGE_FULL :: SDLColorRange
pattern SDL_COLOR_RANGE_FULL = SDLColorRange #{const SDL_COLOR_RANGE_FULL}

-- | Colorspace color primaries.
--
-- /Since: 3.2.0/
newtype SDLColorPrimaries = SDLColorPrimaries CInt deriving (Show, Eq, Ord, Storable)

pattern SDL_COLOR_PRIMARIES_UNKNOWN :: SDLColorPrimaries
pattern SDL_COLOR_PRIMARIES_UNKNOWN = SDLColorPrimaries #{const SDL_COLOR_PRIMARIES_UNKNOWN}
pattern SDL_COLOR_PRIMARIES_BT709 :: SDLColorPrimaries
pattern SDL_COLOR_PRIMARIES_BT709 = SDLColorPrimaries #{const SDL_COLOR_PRIMARIES_BT709}
pattern SDL_COLOR_PRIMARIES_UNSPECIFIED :: SDLColorPrimaries
pattern SDL_COLOR_PRIMARIES_UNSPECIFIED = SDLColorPrimaries #{const SDL_COLOR_PRIMARIES_UNSPECIFIED}
pattern SDL_COLOR_PRIMARIES_BT470M :: SDLColorPrimaries
pattern SDL_COLOR_PRIMARIES_BT470M = SDLColorPrimaries #{const SDL_COLOR_PRIMARIES_BT470M}
pattern SDL_COLOR_PRIMARIES_BT470BG :: SDLColorPrimaries
pattern SDL_COLOR_PRIMARIES_BT470BG = SDLColorPrimaries #{const SDL_COLOR_PRIMARIES_BT470BG}
pattern SDL_COLOR_PRIMARIES_BT601 :: SDLColorPrimaries
pattern SDL_COLOR_PRIMARIES_BT601 = SDLColorPrimaries #{const SDL_COLOR_PRIMARIES_BT601}
pattern SDL_COLOR_PRIMARIES_SMPTE240 :: SDLColorPrimaries
pattern SDL_COLOR_PRIMARIES_SMPTE240 = SDLColorPrimaries #{const SDL_COLOR_PRIMARIES_SMPTE240}
pattern SDL_COLOR_PRIMARIES_GENERIC_FILM :: SDLColorPrimaries
pattern SDL_COLOR_PRIMARIES_GENERIC_FILM = SDLColorPrimaries #{const SDL_COLOR_PRIMARIES_GENERIC_FILM}
pattern SDL_COLOR_PRIMARIES_BT2020 :: SDLColorPrimaries
pattern SDL_COLOR_PRIMARIES_BT2020 = SDLColorPrimaries #{const SDL_COLOR_PRIMARIES_BT2020}
pattern SDL_COLOR_PRIMARIES_XYZ :: SDLColorPrimaries
pattern SDL_COLOR_PRIMARIES_XYZ = SDLColorPrimaries #{const SDL_COLOR_PRIMARIES_XYZ}
pattern SDL_COLOR_PRIMARIES_SMPTE431 :: SDLColorPrimaries
pattern SDL_COLOR_PRIMARIES_SMPTE431 = SDLColorPrimaries #{const SDL_COLOR_PRIMARIES_SMPTE431}
pattern SDL_COLOR_PRIMARIES_SMPTE432 :: SDLColorPrimaries
pattern SDL_COLOR_PRIMARIES_SMPTE432 = SDLColorPrimaries #{const SDL_COLOR_PRIMARIES_SMPTE432}
pattern SDL_COLOR_PRIMARIES_EBU3213 :: SDLColorPrimaries
pattern SDL_COLOR_PRIMARIES_EBU3213 = SDLColorPrimaries #{const SDL_COLOR_PRIMARIES_EBU3213}
pattern SDL_COLOR_PRIMARIES_CUSTOM :: SDLColorPrimaries
pattern SDL_COLOR_PRIMARIES_CUSTOM = SDLColorPrimaries #{const SDL_COLOR_PRIMARIES_CUSTOM}

-- | Colorspace transfer characteristics.
--
-- /Since: 3.2.0/
newtype SDLTransferCharacteristics = SDLTransferCharacteristics CInt deriving (Show, Eq, Ord, Storable)

pattern SDL_TRANSFER_CHARACTERISTICS_UNKNOWN :: SDLTransferCharacteristics
pattern SDL_TRANSFER_CHARACTERISTICS_UNKNOWN = SDLTransferCharacteristics #{const SDL_TRANSFER_CHARACTERISTICS_UNKNOWN}
pattern SDL_TRANSFER_CHARACTERISTICS_BT709 :: SDLTransferCharacteristics
pattern SDL_TRANSFER_CHARACTERISTICS_BT709 = SDLTransferCharacteristics #{const SDL_TRANSFER_CHARACTERISTICS_BT709}
pattern SDL_TRANSFER_CHARACTERISTICS_UNSPECIFIED :: SDLTransferCharacteristics
pattern SDL_TRANSFER_CHARACTERISTICS_UNSPECIFIED = SDLTransferCharacteristics #{const SDL_TRANSFER_CHARACTERISTICS_UNSPECIFIED}
pattern SDL_TRANSFER_CHARACTERISTICS_GAMMA22 :: SDLTransferCharacteristics
pattern SDL_TRANSFER_CHARACTERISTICS_GAMMA22 = SDLTransferCharacteristics #{const SDL_TRANSFER_CHARACTERISTICS_GAMMA22}
pattern SDL_TRANSFER_CHARACTERISTICS_GAMMA28 :: SDLTransferCharacteristics
pattern SDL_TRANSFER_CHARACTERISTICS_GAMMA28 = SDLTransferCharacteristics #{const SDL_TRANSFER_CHARACTERISTICS_GAMMA28}
pattern SDL_TRANSFER_CHARACTERISTICS_BT601 :: SDLTransferCharacteristics
pattern SDL_TRANSFER_CHARACTERISTICS_BT601 = SDLTransferCharacteristics #{const SDL_TRANSFER_CHARACTERISTICS_BT601}
pattern SDL_TRANSFER_CHARACTERISTICS_SMPTE240 :: SDLTransferCharacteristics
pattern SDL_TRANSFER_CHARACTERISTICS_SMPTE240 = SDLTransferCharacteristics #{const SDL_TRANSFER_CHARACTERISTICS_SMPTE240}
pattern SDL_TRANSFER_CHARACTERISTICS_LINEAR :: SDLTransferCharacteristics
pattern SDL_TRANSFER_CHARACTERISTICS_LINEAR = SDLTransferCharacteristics #{const SDL_TRANSFER_CHARACTERISTICS_LINEAR}
pattern SDL_TRANSFER_CHARACTERISTICS_LOG100 :: SDLTransferCharacteristics
pattern SDL_TRANSFER_CHARACTERISTICS_LOG100 = SDLTransferCharacteristics #{const SDL_TRANSFER_CHARACTERISTICS_LOG100}
pattern SDL_TRANSFER_CHARACTERISTICS_LOG100_SQRT10 :: SDLTransferCharacteristics
pattern SDL_TRANSFER_CHARACTERISTICS_LOG100_SQRT10 = SDLTransferCharacteristics #{const SDL_TRANSFER_CHARACTERISTICS_LOG100_SQRT10}
pattern SDL_TRANSFER_CHARACTERISTICS_IEC61966 :: SDLTransferCharacteristics
pattern SDL_TRANSFER_CHARACTERISTICS_IEC61966 = SDLTransferCharacteristics #{const SDL_TRANSFER_CHARACTERISTICS_IEC61966}
pattern SDL_TRANSFER_CHARACTERISTICS_BT1361 :: SDLTransferCharacteristics
pattern SDL_TRANSFER_CHARACTERISTICS_BT1361 = SDLTransferCharacteristics #{const SDL_TRANSFER_CHARACTERISTICS_BT1361}
pattern SDL_TRANSFER_CHARACTERISTICS_SRGB :: SDLTransferCharacteristics
pattern SDL_TRANSFER_CHARACTERISTICS_SRGB = SDLTransferCharacteristics #{const SDL_TRANSFER_CHARACTERISTICS_SRGB}
pattern SDL_TRANSFER_CHARACTERISTICS_BT2020_10BIT :: SDLTransferCharacteristics
pattern SDL_TRANSFER_CHARACTERISTICS_BT2020_10BIT = SDLTransferCharacteristics #{const SDL_TRANSFER_CHARACTERISTICS_BT2020_10BIT}
pattern SDL_TRANSFER_CHARACTERISTICS_BT2020_12BIT :: SDLTransferCharacteristics
pattern SDL_TRANSFER_CHARACTERISTICS_BT2020_12BIT = SDLTransferCharacteristics #{const SDL_TRANSFER_CHARACTERISTICS_BT2020_12BIT}
pattern SDL_TRANSFER_CHARACTERISTICS_PQ :: SDLTransferCharacteristics
pattern SDL_TRANSFER_CHARACTERISTICS_PQ = SDLTransferCharacteristics #{const SDL_TRANSFER_CHARACTERISTICS_PQ}
pattern SDL_TRANSFER_CHARACTERISTICS_SMPTE428 :: SDLTransferCharacteristics
pattern SDL_TRANSFER_CHARACTERISTICS_SMPTE428 = SDLTransferCharacteristics #{const SDL_TRANSFER_CHARACTERISTICS_SMPTE428}
pattern SDL_TRANSFER_CHARACTERISTICS_HLG :: SDLTransferCharacteristics
pattern SDL_TRANSFER_CHARACTERISTICS_HLG = SDLTransferCharacteristics #{const SDL_TRANSFER_CHARACTERISTICS_HLG}
pattern SDL_TRANSFER_CHARACTERISTICS_CUSTOM :: SDLTransferCharacteristics
pattern SDL_TRANSFER_CHARACTERISTICS_CUSTOM = SDLTransferCharacteristics #{const SDL_TRANSFER_CHARACTERISTICS_CUSTOM}

-- | Colorspace matrix coefficients.
--
-- /Since: 3.2.0/
newtype SDLMatrixCoefficients = SDLMatrixCoefficients CInt deriving (Show, Eq, Ord, Storable)

pattern SDL_MATRIX_COEFFICIENTS_IDENTITY :: SDLMatrixCoefficients
pattern SDL_MATRIX_COEFFICIENTS_IDENTITY = SDLMatrixCoefficients #{const SDL_MATRIX_COEFFICIENTS_IDENTITY}
pattern SDL_MATRIX_COEFFICIENTS_BT709 :: SDLMatrixCoefficients
pattern SDL_MATRIX_COEFFICIENTS_BT709 = SDLMatrixCoefficients #{const SDL_MATRIX_COEFFICIENTS_BT709}
pattern SDL_MATRIX_COEFFICIENTS_UNSPECIFIED :: SDLMatrixCoefficients
pattern SDL_MATRIX_COEFFICIENTS_UNSPECIFIED = SDLMatrixCoefficients #{const SDL_MATRIX_COEFFICIENTS_UNSPECIFIED}
pattern SDL_MATRIX_COEFFICIENTS_FCC :: SDLMatrixCoefficients
pattern SDL_MATRIX_COEFFICIENTS_FCC = SDLMatrixCoefficients #{const SDL_MATRIX_COEFFICIENTS_FCC}
pattern SDL_MATRIX_COEFFICIENTS_BT470BG :: SDLMatrixCoefficients
pattern SDL_MATRIX_COEFFICIENTS_BT470BG = SDLMatrixCoefficients #{const SDL_MATRIX_COEFFICIENTS_BT470BG}
pattern SDL_MATRIX_COEFFICIENTS_BT601 :: SDLMatrixCoefficients
pattern SDL_MATRIX_COEFFICIENTS_BT601 = SDLMatrixCoefficients #{const SDL_MATRIX_COEFFICIENTS_BT601}
pattern SDL_MATRIX_COEFFICIENTS_SMPTE240 :: SDLMatrixCoefficients
pattern SDL_MATRIX_COEFFICIENTS_SMPTE240 = SDLMatrixCoefficients #{const SDL_MATRIX_COEFFICIENTS_SMPTE240}
pattern SDL_MATRIX_COEFFICIENTS_YCGCO :: SDLMatrixCoefficients
pattern SDL_MATRIX_COEFFICIENTS_YCGCO = SDLMatrixCoefficients #{const SDL_MATRIX_COEFFICIENTS_YCGCO}
pattern SDL_MATRIX_COEFFICIENTS_BT2020_NCL :: SDLMatrixCoefficients
pattern SDL_MATRIX_COEFFICIENTS_BT2020_NCL = SDLMatrixCoefficients #{const SDL_MATRIX_COEFFICIENTS_BT2020_NCL}
pattern SDL_MATRIX_COEFFICIENTS_BT2020_CL :: SDLMatrixCoefficients
pattern SDL_MATRIX_COEFFICIENTS_BT2020_CL = SDLMatrixCoefficients #{const SDL_MATRIX_COEFFICIENTS_BT2020_CL}
pattern SDL_MATRIX_COEFFICIENTS_SMPTE2085 :: SDLMatrixCoefficients
pattern SDL_MATRIX_COEFFICIENTS_SMPTE2085 = SDLMatrixCoefficients #{const SDL_MATRIX_COEFFICIENTS_SMPTE2085}
pattern SDL_MATRIX_COEFFICIENTS_CHROMA_DERIVED_NCL :: SDLMatrixCoefficients
pattern SDL_MATRIX_COEFFICIENTS_CHROMA_DERIVED_NCL = SDLMatrixCoefficients #{const SDL_MATRIX_COEFFICIENTS_CHROMA_DERIVED_NCL}
pattern SDL_MATRIX_COEFFICIENTS_CHROMA_DERIVED_CL :: SDLMatrixCoefficients
pattern SDL_MATRIX_COEFFICIENTS_CHROMA_DERIVED_CL = SDLMatrixCoefficients #{const SDL_MATRIX_COEFFICIENTS_CHROMA_DERIVED_CL}
pattern SDL_MATRIX_COEFFICIENTS_ICTCP :: SDLMatrixCoefficients
pattern SDL_MATRIX_COEFFICIENTS_ICTCP = SDLMatrixCoefficients #{const SDL_MATRIX_COEFFICIENTS_ICTCP}
pattern SDL_MATRIX_COEFFICIENTS_CUSTOM :: SDLMatrixCoefficients
pattern SDL_MATRIX_COEFFICIENTS_CUSTOM = SDLMatrixCoefficients #{const SDL_MATRIX_COEFFICIENTS_CUSTOM}

-- | Colorspace chroma sample location.
--
-- /Since: 3.2.0/
newtype SDLChromaLocation = SDLChromaLocation CInt deriving (Show, Eq, Ord, Storable)

pattern SDL_CHROMA_LOCATION_NONE :: SDLChromaLocation
pattern SDL_CHROMA_LOCATION_NONE = SDLChromaLocation #{const SDL_CHROMA_LOCATION_NONE}
pattern SDL_CHROMA_LOCATION_LEFT :: SDLChromaLocation
pattern SDL_CHROMA_LOCATION_LEFT = SDLChromaLocation #{const SDL_CHROMA_LOCATION_LEFT}
pattern SDL_CHROMA_LOCATION_CENTER :: SDLChromaLocation
pattern SDL_CHROMA_LOCATION_CENTER = SDLChromaLocation #{const SDL_CHROMA_LOCATION_CENTER}
pattern SDL_CHROMA_LOCATION_TOPLEFT :: SDLChromaLocation
pattern SDL_CHROMA_LOCATION_TOPLEFT = SDLChromaLocation #{const SDL_CHROMA_LOCATION_TOPLEFT}

-- * Colorspace

-- | Colorspace definitions enumeration. This wraps a `CUInt`.
--
-- /Since: 3.2.0/
newtype SDLColorspace = SDLColorspace CUInt deriving (Show, Eq, Ord, Storable)

pattern SDL_COLORSPACE_UNKNOWN :: SDLColorspace
pattern SDL_COLORSPACE_UNKNOWN = SDLColorspace #{const SDL_COLORSPACE_UNKNOWN}
pattern SDL_COLORSPACE_SRGB :: SDLColorspace
pattern SDL_COLORSPACE_SRGB = SDLColorspace #{const SDL_COLORSPACE_SRGB}
pattern SDL_COLORSPACE_SRGB_LINEAR :: SDLColorspace
pattern SDL_COLORSPACE_SRGB_LINEAR = SDLColorspace #{const SDL_COLORSPACE_SRGB_LINEAR}
pattern SDL_COLORSPACE_HDR10 :: SDLColorspace
pattern SDL_COLORSPACE_HDR10 = SDLColorspace #{const SDL_COLORSPACE_HDR10}
pattern SDL_COLORSPACE_JPEG :: SDLColorspace
pattern SDL_COLORSPACE_JPEG = SDLColorspace #{const SDL_COLORSPACE_JPEG}
pattern SDL_COLORSPACE_BT601_LIMITED :: SDLColorspace
pattern SDL_COLORSPACE_BT601_LIMITED = SDLColorspace #{const SDL_COLORSPACE_BT601_LIMITED}
pattern SDL_COLORSPACE_BT601_FULL :: SDLColorspace
pattern SDL_COLORSPACE_BT601_FULL = SDLColorspace #{const SDL_COLORSPACE_BT601_FULL}
pattern SDL_COLORSPACE_BT709_LIMITED :: SDLColorspace
pattern SDL_COLORSPACE_BT709_LIMITED = SDLColorspace #{const SDL_COLORSPACE_BT709_LIMITED}
pattern SDL_COLORSPACE_BT709_FULL :: SDLColorspace
pattern SDL_COLORSPACE_BT709_FULL = SDLColorspace #{const SDL_COLORSPACE_BT709_FULL}
pattern SDL_COLORSPACE_BT2020_LIMITED :: SDLColorspace
pattern SDL_COLORSPACE_BT2020_LIMITED = SDLColorspace #{const SDL_COLORSPACE_BT2020_LIMITED}
pattern SDL_COLORSPACE_BT2020_FULL :: SDLColorspace
pattern SDL_COLORSPACE_BT2020_FULL = SDLColorspace #{const SDL_COLORSPACE_BT2020_FULL}
pattern SDL_COLORSPACE_RGB_DEFAULT :: SDLColorspace
pattern SDL_COLORSPACE_RGB_DEFAULT = SDLColorspace #{const SDL_COLORSPACE_RGB_DEFAULT}
pattern SDL_COLORSPACE_YUV_DEFAULT :: SDLColorspace
pattern SDL_COLORSPACE_YUV_DEFAULT = SDLColorspace #{const SDL_COLORSPACE_YUV_DEFAULT}

-- Helper to convert SDLColorspace back to CUInt for FFI calls
colorspaceToCUInt :: SDLColorspace -> CUInt
colorspaceToCUInt (SDLColorspace val) = val

colorspaceToWord32 :: SDLColorspace -> Word32
colorspaceToWord32 (SDLColorspace val) = fromIntegral val

-- Helper to get Word32 value for macro calculations
getColorspaceValue :: SDLColorspace -> Word32
getColorspaceValue (SDLColorspace val) = fromIntegral val

-- Helper to convert CUInt from FFI calls to SDLColorspace
cUIntToColorspace :: CUInt -> SDLColorspace
cUIntToColorspace val = case val of
  #{const SDL_COLORSPACE_UNKNOWN} -> SDL_COLORSPACE_UNKNOWN
  #{const SDL_COLORSPACE_SRGB} -> SDL_COLORSPACE_SRGB
  #{const SDL_COLORSPACE_SRGB_LINEAR} -> SDL_COLORSPACE_SRGB_LINEAR
  #{const SDL_COLORSPACE_HDR10} -> SDL_COLORSPACE_HDR10
  #{const SDL_COLORSPACE_JPEG} -> SDL_COLORSPACE_JPEG
  #{const SDL_COLORSPACE_BT601_LIMITED} -> SDL_COLORSPACE_BT601_LIMITED
  #{const SDL_COLORSPACE_BT601_FULL} -> SDL_COLORSPACE_BT601_FULL
  #{const SDL_COLORSPACE_BT709_LIMITED} -> SDL_COLORSPACE_BT709_LIMITED
  #{const SDL_COLORSPACE_BT709_FULL} -> SDL_COLORSPACE_BT709_FULL
  #{const SDL_COLORSPACE_BT2020_LIMITED} -> SDL_COLORSPACE_BT2020_LIMITED
  #{const SDL_COLORSPACE_BT2020_FULL} -> SDL_COLORSPACE_BT2020_FULL
  v | v == #{const SDL_COLORSPACE_RGB_DEFAULT} -> SDL_COLORSPACE_RGB_DEFAULT
  v | v == #{const SDL_COLORSPACE_YUV_DEFAULT} -> SDL_COLORSPACE_YUV_DEFAULT
  _ -> SDL_COLORSPACE_UNKNOWN -- Default for unrecognized values

-- | A macro for defining custom SDL_Colorspace formats.
--
-- /Since: 3.2.0/
sdlDefineColorspace :: SDLColorType -> SDLColorRange -> SDLColorPrimaries ->
                       SDLTransferCharacteristics -> SDLMatrixCoefficients ->
                       SDLChromaLocation -> SDLColorspace
sdlDefineColorspace (SDLColorType colorType) (SDLColorRange colorRange) (SDLColorPrimaries colorPrimaries)
                    (SDLTransferCharacteristics transferChar) (SDLMatrixCoefficients matrixCoeff)
                    (SDLChromaLocation chromaLoc) =
  SDLColorspace $ fromIntegral $
    ((fromIntegral colorType `shiftL` 28) :: Word32) .|.
    ((fromIntegral colorRange `shiftL` 24) :: Word32) .|.
    ((fromIntegral chromaLoc `shiftL` 20) :: Word32) .|.
    ((fromIntegral colorPrimaries `shiftL` 10) :: Word32) .|.
    ((fromIntegral transferChar `shiftL` 5) :: Word32) .|.
    (fromIntegral matrixCoeff :: Word32)

-- | A macro to retrieve the type of an SDL_Colorspace.
--
-- /Since: 3.2.0/
sdlColorspaceType :: SDLColorspace -> SDLColorType
sdlColorspaceType cspace = SDLColorType $ fromIntegral ((getColorspaceValue cspace `shiftR` 28) .&. 0x0F)

-- | A macro to retrieve the range of an SDL_Colorspace.
--
-- /Since: 3.2.0/
sdlColorspaceRange :: SDLColorspace -> SDLColorRange
sdlColorspaceRange cspace = SDLColorRange $ fromIntegral ((getColorspaceValue cspace `shiftR` 24) .&. 0x0F)

-- | A macro to retrieve the chroma sample location of an SDL_Colorspace.
--
-- /Since: 3.2.0/
sdlColorspaceChroma :: SDLColorspace -> SDLChromaLocation
sdlColorspaceChroma cspace = SDLChromaLocation $ fromIntegral ((getColorspaceValue cspace `shiftR` 20) .&. 0x0F)

-- | A macro to retrieve the primaries of an SDL_Colorspace.
--
-- /Since: 3.2.0/
sdlColorspacePrimaries :: SDLColorspace -> SDLColorPrimaries
sdlColorspacePrimaries cspace = SDLColorPrimaries $ fromIntegral ((getColorspaceValue cspace `shiftR` 10) .&. 0x1F)

-- | A macro to retrieve the transfer characteristics of an SDL_Colorspace.
--
-- /Since: 3.2.0/
sdlColorspaceTransfer :: SDLColorspace -> SDLTransferCharacteristics
sdlColorspaceTransfer cspace = SDLTransferCharacteristics $ fromIntegral ((getColorspaceValue cspace `shiftR` 5) .&. 0x1F)

-- | A macro to retrieve the matrix coefficients of an SDL_Colorspace.
--
-- /Since: 3.2.0/
sdlColorspaceMatrix :: SDLColorspace -> SDLMatrixCoefficients
sdlColorspaceMatrix cspace = SDLMatrixCoefficients $ fromIntegral (getColorspaceValue cspace .&. 0x1F)

-- | A macro to determine if an SDL_Colorspace uses BT601 (or BT470BG) matrix coefficients.
--
-- /Since: 3.2.0/
sdlIsColorspaceMatrixBT601 :: SDLColorspace -> Bool
sdlIsColorspaceMatrixBT601 cspace =
  let matrix = sdlColorspaceMatrix cspace
  in matrix == SDL_MATRIX_COEFFICIENTS_BT601 || matrix == SDL_MATRIX_COEFFICIENTS_BT470BG

-- | A macro to determine if an SDL_Colorspace uses BT709 matrix coefficients.
--
-- /Since: 3.2.0/
sdlIsColorspaceMatrixBT709 :: SDLColorspace -> Bool
sdlIsColorspaceMatrixBT709 cspace = sdlColorspaceMatrix cspace == SDL_MATRIX_COEFFICIENTS_BT709

-- | A macro to determine if an SDL_Colorspace uses BT2020_NCL matrix coefficients.
--
-- /Since: 3.2.0/
sdlIsColorspaceMatrixBT2020NCL :: SDLColorspace -> Bool
sdlIsColorspaceMatrixBT2020NCL cspace = sdlColorspaceMatrix cspace == SDL_MATRIX_COEFFICIENTS_BT2020_NCL

-- | A macro to determine if an SDL_Colorspace has a limited range.
--
-- /Since: 3.2.0/
sdlIsColorspaceLimitedRange :: SDLColorspace -> Bool
sdlIsColorspaceLimitedRange cspace = sdlColorspaceRange cspace /= SDL_COLOR_RANGE_FULL

-- | A macro to determine if an SDL_Colorspace has a full range.
--
-- /Since: 3.2.0/
sdlIsColorspaceFullRange :: SDLColorspace -> Bool
sdlIsColorspaceFullRange cspace = sdlColorspaceRange cspace == SDL_COLOR_RANGE_FULL

-- * Color Structures

-- | A structure that represents a color as RGBA components.
--
-- The bits of this structure can be directly reinterpreted as an
-- integer-packed color which uses the SDL_PIXELFORMAT_RGBA32 format
-- (SDL_PIXELFORMAT_ABGR8888 on little-endian systems and
-- SDL_PIXELFORMAT_RGBA8888 on big-endian systems).
--
-- /Since: 3.2.0/
data SDLColor = SDLColor
  { colorR :: {-# UNPACK #-} !Word8  -- ^ Red component
  , colorG :: {-# UNPACK #-} !Word8  -- ^ Green component
  , colorB :: {-# UNPACK #-} !Word8  -- ^ Blue component
  , colorA :: {-# UNPACK #-} !Word8  -- ^ Alpha component
  } deriving (Eq, Show, Read)

instance Storable SDLColor where
  sizeOf _ = #{size SDL_Color}
  alignment _ = #{alignment SDL_Color}
  peek ptr = do
    r <- #{peek SDL_Color, r} ptr
    g <- #{peek SDL_Color, g} ptr
    b <- #{peek SDL_Color, b} ptr
    a <- #{peek SDL_Color, a} ptr
    return $ SDLColor r g b a
  poke ptr (SDLColor r g b a) = do
    #{poke SDL_Color, r} ptr r
    #{poke SDL_Color, g} ptr g
    #{poke SDL_Color, b} ptr b
    #{poke SDL_Color, a} ptr a

-- | A structure that represents a color as RGBA float components.
--
-- The bits of this structure can be directly reinterpreted as a float-packed
-- color which uses the SDL_PIXELFORMAT_RGBA128_FLOAT format.
--
-- /Since: 3.2.0/
data SDLFColor = SDLFColor
  { fcolorR :: {-# UNPACK #-} !Float  -- ^ Red component
  , fcolorG :: {-# UNPACK #-} !Float  -- ^ Green component
  , fcolorB :: {-# UNPACK #-} !Float  -- ^ Blue component
  , fcolorA :: {-# UNPACK #-} !Float  -- ^ Alpha component
  } deriving (Eq, Show, Read)

instance Storable SDLFColor where
  sizeOf _ = #{size SDL_FColor}
  alignment _ = #{alignment SDL_FColor}
  peek ptr = do
    r <- #{peek SDL_FColor, r} ptr
    g <- #{peek SDL_FColor, g} ptr
    b <- #{peek SDL_FColor, b} ptr
    a <- #{peek SDL_FColor, a} ptr
    return $ SDLFColor r g b a
  poke ptr (SDLFColor r g b a) = do
    #{poke SDL_FColor, r} ptr r
    #{poke SDL_FColor, g} ptr g
    #{poke SDL_FColor, b} ptr b
    #{poke SDL_FColor, a} ptr a

-- | A set of indexed colors representing a palette.
--
-- /Since: 3.2.0/
-- Opaque pointer type, as the C struct contains internal fields.
-- Use API functions to interact.
newtype SDLPalette = SDLPalette (Ptr SDLPalette) deriving (Show, Eq)

-- | Details about the format of a pixel.
--
-- /Since: 3.2.0/
-- Opaque pointer type. Use API functions to interact.
newtype SDLPixelFormatDetails = SDLPixelFormatDetails (Ptr SDLPixelFormatDetails) deriving (Show, Eq)

-- Wrapper for SDLPixelFormatDetails to provide accessors (Read-only)
data PixelFormatDetails = PixelFormatDetails
  { pfFormat        :: SDLPixelFormat
  , pfBitsPerPixel  :: Word8
  , pfBytesPerPixel :: Word8
  , pfRmask         :: Word32
  , pfGmask         :: Word32
  , pfBmask         :: Word32
  , pfAmask         :: Word32
  , pfRbits         :: Word8
  , pfGbits         :: Word8
  , pfBbits         :: Word8
  , pfAbits         :: Word8
  , pfRshift        :: Word8
  , pfGshift        :: Word8
  , pfBshift        :: Word8
  , pfAshift        :: Word8
  } deriving (Eq, Show)

-- Helper to peek the details from the opaque pointer
peekPixelFormatDetails :: SDLPixelFormatDetails -> IO PixelFormatDetails
peekPixelFormatDetails (SDLPixelFormatDetails ptr) = do
    format_val <- #{peek SDL_PixelFormatDetails, format} ptr :: IO CUInt
    bpp  <- #{peek SDL_PixelFormatDetails, bits_per_pixel} ptr
    bps  <- #{peek SDL_PixelFormatDetails, bytes_per_pixel} ptr
    rm   <- #{peek SDL_PixelFormatDetails, Rmask} ptr
    gm   <- #{peek SDL_PixelFormatDetails, Gmask} ptr
    bm   <- #{peek SDL_PixelFormatDetails, Bmask} ptr
    am   <- #{peek SDL_PixelFormatDetails, Amask} ptr
    rb   <- #{peek SDL_PixelFormatDetails, Rbits} ptr
    gb   <- #{peek SDL_PixelFormatDetails, Gbits} ptr
    bb   <- #{peek SDL_PixelFormatDetails, Bbits} ptr
    ab   <- #{peek SDL_PixelFormatDetails, Abits} ptr
    rs   <- #{peek SDL_PixelFormatDetails, Rshift} ptr
    gs   <- #{peek SDL_PixelFormatDetails, Gshift} ptr
    bs   <- #{peek SDL_PixelFormatDetails, Bshift} ptr
    as   <- #{peek SDL_PixelFormatDetails, Ashift} ptr
    pure $ PixelFormatDetails (cUIntToPixelFormat format_val) bpp bps rm gm bm am rb gb bb ab rs gs bs as

-- * Pixel Format Functions

-- | Get the human readable name of a pixel format.
--
-- /Since: 3.2.0/
foreign import ccall unsafe "SDL_GetPixelFormatName"
  c_sdlGetPixelFormatName :: CUInt -> IO CString

sdlGetPixelFormatName :: SDLPixelFormat -> IO String
sdlGetPixelFormatName format = do
  c_sdlGetPixelFormatName (pixelFormatToCUInt format) >>= peekCString

-- | Convert one of the enumerated pixel formats to a bpp value and RGBA masks.
--
-- /Since: 3.2.0/
foreign import ccall unsafe "SDL_GetMasksForPixelFormat"
  c_sdlGetMasksForPixelFormat :: CUInt -> Ptr CInt -> Ptr CUInt -> Ptr CUInt -> Ptr CUInt -> Ptr CUInt -> IO CBool

sdlGetMasksForPixelFormat :: SDLPixelFormat -> IO (Maybe (Int, Word32, Word32, Word32, Word32))
sdlGetMasksForPixelFormat format =
  alloca $ \bppPtr ->
  alloca $ \rMaskPtr ->
  alloca $ \gMaskPtr ->
  alloca $ \bMaskPtr ->
  alloca $ \aMaskPtr -> do
    success <- c_sdlGetMasksForPixelFormat (pixelFormatToCUInt format) bppPtr rMaskPtr gMaskPtr bMaskPtr aMaskPtr
    if toBool success
      then do
        bpp   <- fromIntegral <$> peek bppPtr   :: IO Int
        rMask <- fromIntegral <$> peek rMaskPtr :: IO Word32
        gMask <- fromIntegral <$> peek gMaskPtr :: IO Word32
        bMask <- fromIntegral <$> peek bMaskPtr :: IO Word32
        aMask <- fromIntegral <$> peek aMaskPtr :: IO Word32
        return $ Just (bpp, rMask, gMask, bMask, aMask)
      else return Nothing

-- | Convert a bpp value and RGBA masks to an enumerated pixel format.
--
-- This will return `SDL_PIXELFORMAT_UNKNOWN` if the conversion wasn't possible.
--
-- /Since: 3.2.0/
foreign import ccall unsafe "SDL_GetPixelFormatForMasks"
  c_sdlGetPixelFormatForMasks :: CInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO CUInt

sdlGetPixelFormatForMasks :: Int -> Word32 -> Word32 -> Word32 -> Word32 -> IO SDLPixelFormat
sdlGetPixelFormatForMasks bpp rMask gMask bMask aMask =
  cUIntToPixelFormat <$> c_sdlGetPixelFormatForMasks
    (fromIntegral bpp)       -- Int to CInt
    (fromIntegral rMask)     -- Word32 to CUInt
    (fromIntegral gMask)     -- Word32 to CUInt
    (fromIntegral bMask)     -- Word32 to CUInt
    (fromIntegral aMask)     -- Word32 to CUInt

-- | Create an SDL_PixelFormatDetails structure corresponding to a pixel format.
--
-- Returned structure pointer should not be modified. Use `peekPixelFormatDetails`
-- to get a Haskell record copy.
--
-- /Since: 3.2.0/
foreign import ccall unsafe "SDL_GetPixelFormatDetails"
  c_sdlGetPixelFormatDetails :: CUInt -> IO (Ptr SDLPixelFormatDetails)

sdlGetPixelFormatDetails :: SDLPixelFormat -> IO (Maybe SDLPixelFormatDetails)
sdlGetPixelFormatDetails format = do
  ptr <- c_sdlGetPixelFormatDetails (pixelFormatToCUInt format)
  if ptr == nullPtr
    then return Nothing
    else return (Just (SDLPixelFormatDetails ptr))

-- * Palette Functions

-- | Create a palette structure with the specified number of color entries.
--
-- The palette entries are initialized to white.
--
-- /Since: 3.2.0/
foreign import ccall unsafe "SDL_CreatePalette"
  c_sdlCreatePalette :: CInt -> IO (Ptr SDLPalette)

sdlCreatePalette :: Int -> IO (Maybe SDLPalette)
sdlCreatePalette ncolors = do
  ptr <- c_sdlCreatePalette (fromIntegral ncolors)
  if ptr == nullPtr
    then return Nothing
    else return (Just (SDLPalette ptr))

-- | Set a range of colors in a palette.
--
-- /Since: 3.2.0/
foreign import ccall unsafe "SDL_SetPaletteColors"
  c_sdlSetPaletteColors :: Ptr SDLPalette -> Ptr SDLColor -> CInt -> CInt -> IO CBool

sdlSetPaletteColors :: SDLPalette -> [SDLColor] -> Int -> IO Bool
sdlSetPaletteColors (SDLPalette palettePtr) colors firstcolor =
  withArray colors $ \colorsPtr ->
    toBool <$> c_sdlSetPaletteColors palettePtr colorsPtr (fromIntegral firstcolor) (fromIntegral $ length colors)

-- | Free a palette created with SDL_CreatePalette().
--
-- /Since: 3.2.0/
foreign import ccall unsafe "SDL_DestroyPalette"
  c_sdlDestroyPalette :: Ptr SDLPalette -> IO ()

sdlDestroyPalette :: SDLPalette -> IO ()
sdlDestroyPalette (SDLPalette ptr) = c_sdlDestroyPalette ptr

-- * Color Mapping Functions

-- | Map an RGB triple to an opaque pixel value for a given pixel format.
--
-- /Since: 3.2.0/
foreign import ccall unsafe "SDL_MapRGB"
  c_sdlMapRGB :: Ptr SDLPixelFormatDetails -> Ptr SDLPalette -> Word8 -> Word8 -> Word8 -> IO Word32

sdlMapRGB :: SDLPixelFormatDetails -> Maybe SDLPalette -> Word8 -> Word8 -> Word8 -> IO Word32
sdlMapRGB (SDLPixelFormatDetails formatPtr) mPalette r g b =
  let palettePtr = maybe nullPtr (\(SDLPalette p) -> p) mPalette
  in c_sdlMapRGB formatPtr palettePtr r g b

-- | Map an RGBA quadruple to a pixel value for a given pixel format.
--
-- /Since: 3.2.0/
foreign import ccall unsafe "SDL_MapRGBA"
  c_sdlMapRGBA :: Ptr SDLPixelFormatDetails -> Ptr SDLPalette -> Word8 -> Word8 -> Word8 -> Word8 -> IO Word32

sdlMapRGBA :: SDLPixelFormatDetails -> Maybe SDLPalette -> Word8 -> Word8 -> Word8 -> Word8 -> IO Word32
sdlMapRGBA (SDLPixelFormatDetails formatPtr) mPalette r g b a =
  let palettePtr = maybe nullPtr (\(SDLPalette p) -> p) mPalette
  in c_sdlMapRGBA formatPtr palettePtr r g b a

-- | Get RGB values from a pixel in the specified format.
--
-- /Since: 3.2.0/
foreign import ccall unsafe "SDL_GetRGB"
  c_sdlGetRGB :: Word32 -> Ptr SDLPixelFormatDetails -> Ptr SDLPalette -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()

sdlGetRGB :: Word32 -> SDLPixelFormatDetails -> Maybe SDLPalette -> IO (Word8, Word8, Word8)
sdlGetRGB pixelvalue (SDLPixelFormatDetails formatPtr) mPalette =
  alloca $ \rPtr ->
  alloca $ \gPtr ->
  alloca $ \bPtr -> do
    let palettePtr = maybe nullPtr (\(SDLPalette p) -> p) mPalette
    c_sdlGetRGB pixelvalue formatPtr palettePtr rPtr gPtr bPtr
    (,,) <$> peek rPtr <*> peek gPtr <*> peek bPtr

-- | Get RGBA values from a pixel in the specified format.
--
-- /Since: 3.2.0/
foreign import ccall unsafe "SDL_GetRGBA"
  c_sdlGetRGBA :: Word32 -> Ptr SDLPixelFormatDetails -> Ptr SDLPalette -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()

sdlGetRGBA :: Word32 -> SDLPixelFormatDetails -> Maybe SDLPalette -> IO (Word8, Word8, Word8, Word8)
sdlGetRGBA pixelvalue (SDLPixelFormatDetails formatPtr) mPalette =
  alloca $ \rPtr ->
  alloca $ \gPtr ->
  alloca $ \bPtr ->
  alloca $ \aPtr -> do
    let palettePtr = maybe nullPtr (\(SDLPalette p) -> p) mPalette
    c_sdlGetRGBA pixelvalue formatPtr palettePtr rPtr gPtr bPtr aPtr
    (,,,) <$> peek rPtr <*> peek gPtr <*> peek bPtr <*> peek aPtr
