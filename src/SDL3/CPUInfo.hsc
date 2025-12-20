{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

#include <SDL3/SDL_cpuinfo.h>

{-|
Module      : SDL.CPUInfo
Description : CPU feature detection for SDL
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

This module provides bindings to SDL's CPU information and feature detection functionality.
It allows checking for various SIMD instruction sets and getting information about the system's
CPU configuration.

The CPU instruction set checks are available on all platforms, though they'll return False
when checking for features that aren't applicable to the current CPU architecture
(e.g., checking for SSE on ARM or NEON on x86).
-}

module SDL3.CPUInfo
  ( -- * Constants
    sdlCachelineSize

    -- * CPU Information
  , sdlGetNumLogicalCPUCores
  , sdlGetCPUCacheLineSize
  , sdlGetSystemRAM
  , sdlGetSIMDAlignment

    -- * CPU Feature Detection
    -- ** PowerPC Features
  , sdlHasAltiVec

    -- ** x86 Features
  , sdlHasMMX
  , sdlHasSSE
  , sdlHasSSE2
  , sdlHasSSE3
  , sdlHasSSE41
  , sdlHasSSE42
  , sdlHasAVX
  , sdlHasAVX2
  , sdlHasAVX512F

    -- ** ARM Features
  , sdlHasARMSIMD
  , sdlHasNEON

    -- ** LOONGARCH Features
  , sdlHasLSX
  , sdlHasLASX
  ) where

import Foreign.C.Types
import Data.Word

-- | A guess for the cacheline size used for padding.
-- Most x86 processors have a 64 byte cache line.
-- 64-bit PowerPC processors have a 128 byte cache line.
-- We use the larger value to be generally safe.
sdlCachelineSize = (#{const SDL_CACHELINE_SIZE}) :: Int

-- | Get the number of logical CPU cores available.
foreign import ccall "SDL_GetNumLogicalCPUCores"
  sdlGetNumLogicalCPUCores :: IO CInt

-- | Get the L1 cache line size of the CPU in bytes.
foreign import ccall "SDL_GetCPUCacheLineSize"
  sdlGetCPUCacheLineSize :: IO CInt

-- | Check if the CPU has AltiVec features (PowerPC).
foreign import ccall "SDL_HasAltiVec"
  sdlHasAltiVec :: IO Bool

-- | Check if the CPU has MMX features (x86).
foreign import ccall "SDL_HasMMX"
  sdlHasMMX :: IO Bool

-- | Check if the CPU has SSE features (x86).
foreign import ccall "SDL_HasSSE"
  sdlHasSSE :: IO Bool

-- | Check if the CPU has SSE2 features (x86).
foreign import ccall "SDL_HasSSE2"
  sdlHasSSE2 :: IO Bool

-- | Check if the CPU has SSE3 features (x86).
foreign import ccall "SDL_HasSSE3"
  sdlHasSSE3 :: IO Bool

-- | Check if the CPU has SSE4.1 features (x86).
foreign import ccall "SDL_HasSSE41"
  sdlHasSSE41 :: IO Bool

-- | Check if the CPU has SSE4.2 features (x86).
foreign import ccall "SDL_HasSSE42"
  sdlHasSSE42 :: IO Bool

-- | Check if the CPU has AVX features (x86).
foreign import ccall "SDL_HasAVX"
  sdlHasAVX :: IO Bool

-- | Check if the CPU has AVX2 features (x86).
foreign import ccall "SDL_HasAVX2"
  sdlHasAVX2 :: IO Bool

-- | Check if the CPU has AVX-512F (foundation) features (x86).
foreign import ccall "SDL_HasAVX512F"
  sdlHasAVX512F :: IO Bool

-- | Check if the CPU has ARM SIMD (ARMv6) features.
foreign import ccall "SDL_HasARMSIMD"
  sdlHasARMSIMD :: IO Bool

-- | Check if the CPU has NEON (ARM SIMD) features.
foreign import ccall "SDL_HasNEON"
  sdlHasNEON :: IO Bool

-- | Check if the CPU has LSX (LOONGARCH SIMD) features.
foreign import ccall "SDL_HasLSX"
  sdlHasLSX :: IO Bool

-- | Check if the CPU has LASX (LOONGARCH SIMD) features.
foreign import ccall "SDL_HasLASX"
  sdlHasLASX :: IO Bool

-- | Get the amount of RAM configured in the system in MiB.
foreign import ccall "SDL_GetSystemRAM"
  sdlGetSystemRAM :: IO CInt

-- | Get the alignment needed for SIMD allocations on this system.
foreign import ccall "SDL_GetSIMDAlignment" sdlGetSIMDAlignment :: IO #{type size_t}
