{-|
Module      : SDL.Bits
Description : Bit manipulation functions
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Functions for fiddling with bits and bitmasks.
-}

module SDL3.Bits
  ( -- * Bit Manipulation Functions
    sdlMostSignificantBitIndex32
  , sdlHasExactlyOneBitSet32
  ) where

import Data.Word
import Data.Bits

-- | Get the index of the most significant (set) bit in a 32-bit number.
--
-- Result is undefined when called with 0. This operation can also be stated
-- as "count leading zeroes" and "log base 2".
--
-- The function returns -1 if the input value is 0.
--
-- @since 3.2.0
sdlMostSignificantBitIndex32 :: Word32 -> Int
sdlMostSignificantBitIndex32 0 = -1
sdlMostSignificantBitIndex32 x = go x 0
  where
    go :: Word32 -> Int -> Int
    go 0 _ = -1
    go 1 acc = acc
    go n acc = go (n `shiftR` 1) (acc + 1)

-- | Determine if a unsigned 32-bit value has exactly one bit set.
--
-- If there are no bits set (x is zero), or more than one bit set, this
-- returns False. If any one bit is exclusively set, this returns True.
--
-- @since 3.2.0
sdlHasExactlyOneBitSet32 :: Word32 -> Bool
sdlHasExactlyOneBitSet32 0 = False
sdlHasExactlyOneBitSet32 x = x .&. (x - 1) == 0
