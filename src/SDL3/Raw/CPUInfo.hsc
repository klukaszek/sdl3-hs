{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

#include <SDL3/SDL_cpuinfo.h>

module SDL3.Raw.CPUInfo
  ( sdlCachelineSize
  , sdlGetNumLogicalCPUCoresRaw
  , sdlGetCPUCacheLineSizeRaw
  , sdlHasAltiVecRaw
  , sdlHasMMXRaw
  , sdlHasSSERaw
  , sdlHasSSE2Raw
  , sdlHasSSE3Raw
  , sdlHasSSE41Raw
  , sdlHasSSE42Raw
  , sdlHasAVXRaw
  , sdlHasAVX2Raw
  , sdlHasAVX512FRaw
  , sdlHasARMSIMDRaw
  , sdlHasNEONRaw
  , sdlHasLSXRaw
  , sdlHasLASXRaw
  , sdlGetSystemRAMRaw
  , sdlGetSystemPageSizeRaw
  , sdlGetSIMDAlignmentRaw
  ) where

import Foreign.C.Types
import Data.Word (Word64)

sdlCachelineSize :: Int
sdlCachelineSize = #{const SDL_CACHELINE_SIZE}

foreign import ccall "SDL_GetNumLogicalCPUCores"
  sdlGetNumLogicalCPUCoresRaw :: IO CInt

foreign import ccall "SDL_GetCPUCacheLineSize"
  sdlGetCPUCacheLineSizeRaw :: IO CInt

foreign import ccall "SDL_HasAltiVec"
  sdlHasAltiVecRaw :: IO Bool

foreign import ccall "SDL_HasMMX"
  sdlHasMMXRaw :: IO Bool

foreign import ccall "SDL_HasSSE"
  sdlHasSSERaw :: IO Bool

foreign import ccall "SDL_HasSSE2"
  sdlHasSSE2Raw :: IO Bool

foreign import ccall "SDL_HasSSE3"
  sdlHasSSE3Raw :: IO Bool

foreign import ccall "SDL_HasSSE41"
  sdlHasSSE41Raw :: IO Bool

foreign import ccall "SDL_HasSSE42"
  sdlHasSSE42Raw :: IO Bool

foreign import ccall "SDL_HasAVX"
  sdlHasAVXRaw :: IO Bool

foreign import ccall "SDL_HasAVX2"
  sdlHasAVX2Raw :: IO Bool

foreign import ccall "SDL_HasAVX512F"
  sdlHasAVX512FRaw :: IO Bool

foreign import ccall "SDL_HasARMSIMD"
  sdlHasARMSIMDRaw :: IO Bool

foreign import ccall "SDL_HasNEON"
  sdlHasNEONRaw :: IO Bool

foreign import ccall "SDL_HasLSX"
  sdlHasLSXRaw :: IO Bool

foreign import ccall "SDL_HasLASX"
  sdlHasLASXRaw :: IO Bool

foreign import ccall "SDL_GetSystemRAM"
  sdlGetSystemRAMRaw :: IO CInt

foreign import ccall "SDL_GetSystemPageSize"
  sdlGetSystemPageSizeRaw :: IO CInt

foreign import ccall "SDL_GetSIMDAlignment"
  sdlGetSIMDAlignmentRaw :: IO #{type size_t}
