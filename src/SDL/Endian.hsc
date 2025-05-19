{-|
Module      : SDL.Endian
Description : Byte order conversion functions
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Functions for converting values between different byte orders.
-}

{-# LANGUAGE CPP #-}

module SDL.Endian
  ( -- * Endianness Constants
    sdlLilEndian
  , sdlBigEndian
  , sdlByteOrder
  , sdlFloatWordOrder
    -- * Byte Swapping Functions
  , sdlSwap16
  , sdlSwap32
  , sdlSwap64
  , sdlSwapFloat
    -- * Endian Conversion Macros
  , sdlSwap16LE
  , sdlSwap32LE
  , sdlSwap64LE
  , sdlSwapFloatLE
  , sdlSwap16BE
  , sdlSwap32BE
  , sdlSwap64BE
  , sdlSwapFloatBE
  ) where

import Foreign.C.Types
import Data.Word
import Data.Bits
import Unsafe.Coerce (unsafeCoerce)

-- | Represents little-endian byte order
sdlLilEndian :: Int
sdlLilEndian = 1234

-- | Represents big-endian byte order
sdlBigEndian :: Int
sdlBigEndian = 4321

{-| 
Indicates the byte order of the current system.
Will be either 'sdlLilEndian' or 'sdlBigEndian'.
-}
-- This should be set based on architecture, but for now we'll default to little endian
-- which is the most common
sdlByteOrder :: Int
#if defined(WORDS_BIGENDIAN)
sdlByteOrder = sdlBigEndian
#else
sdlByteOrder = sdlLilEndian
#endif

{-|
Indicates the floating point word order of the current system.
Will be either 'sdlLilEndian' or 'sdlBigEndian'.
-}
-- Usually this follows the general system byte order
sdlFloatWordOrder :: Int
sdlFloatWordOrder = sdlByteOrder

-- | Swaps the byte order of a 16-bit integer
sdlSwap16 :: Word16 -> Word16
sdlSwap16 x = (x `shiftL` 8) .|. (x `shiftR` 8)

-- | Swaps the byte order of a 32-bit integer
sdlSwap32 :: Word32 -> Word32
sdlSwap32 x = 
  ((x `shiftL` 24) .|. 
   ((x `shiftL` 8) .&. 0x00FF0000) .|.
   ((x `shiftR` 8) .&. 0x0000FF00) .|. 
   (x `shiftR` 24))

-- | Swaps the byte order of a 64-bit integer
sdlSwap64 :: Word64 -> Word64
sdlSwap64 x =
  let hi = fromIntegral (x .&. 0xFFFFFFFF) :: Word32
      lo = fromIntegral (x `shiftR` 32) :: Word32
      swappedHi = sdlSwap32 hi
      swappedLo = sdlSwap32 lo
  in (fromIntegral swappedHi `shiftL` 32) .|. fromIntegral swappedLo

-- | Swaps the byte order of a floating point number
sdlSwapFloat :: Float -> Float
sdlSwapFloat x = 
  let bits = floatToWord x
      swapped = sdlSwap32 bits
  in wordToFloat swapped
  where
    floatToWord :: Float -> Word32
    floatToWord = unsafeCoerce
    
    wordToFloat :: Word32 -> Float
    wordToFloat = unsafeCoerce

-- Little endian to native conversion functions
sdlSwap16LE :: Word16 -> Word16
#if defined(WORDS_BIGENDIAN)
sdlSwap16LE = sdlSwap16
#else
sdlSwap16LE x = x
#endif

sdlSwap32LE :: Word32 -> Word32
#if defined(WORDS_BIGENDIAN)
sdlSwap32LE = sdlSwap32
#else
sdlSwap32LE x = x
#endif

sdlSwap64LE :: Word64 -> Word64
#if defined(WORDS_BIGENDIAN)
sdlSwap64LE = sdlSwap64
#else
sdlSwap64LE x = x
#endif

sdlSwapFloatLE :: Float -> Float
#if defined(WORDS_BIGENDIAN)
sdlSwapFloatLE = sdlSwapFloat
#else
sdlSwapFloatLE x = x
#endif

-- Big endian to native conversion functions
sdlSwap16BE :: Word16 -> Word16
#if defined(WORDS_BIGENDIAN)
sdlSwap16BE x = x
#else
sdlSwap16BE = sdlSwap16
#endif

sdlSwap32BE :: Word32 -> Word32
#if defined(WORDS_BIGENDIAN)
sdlSwap32BE x = x
#else
sdlSwap32BE = sdlSwap32
#endif

sdlSwap64BE :: Word64 -> Word64
#if defined(WORDS_BIGENDIAN)
sdlSwap64BE x = x
#else
sdlSwap64BE = sdlSwap64
#endif

sdlSwapFloatBE :: Float -> Float
#if defined(WORDS_BIGENDIAN)
sdlSwapFloatBE x = x
#else
sdlSwapFloatBE = sdlSwapFloat
#endif
