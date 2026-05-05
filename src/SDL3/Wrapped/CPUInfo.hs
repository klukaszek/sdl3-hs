module SDL3.Wrapped.CPUInfo
  ( sdlCachelineSize
  , sdlGetNumLogicalCPUCores
  , sdlGetCPUCacheLineSize
  , sdlGetSystemRAM
  , sdlGetSystemPageSize
  , sdlGetSIMDAlignment
  , sdlHasAltiVec
  , sdlHasMMX
  , sdlHasSSE
  , sdlHasSSE2
  , sdlHasSSE3
  , sdlHasSSE41
  , sdlHasSSE42
  , sdlHasAVX
  , sdlHasAVX2
  , sdlHasAVX512F
  , sdlHasARMSIMD
  , sdlHasNEON
  , sdlHasLSX
  , sdlHasLASX
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.Types
import Data.Word (Word64)
import qualified SDL3.Raw.CPUInfo as Raw

sdlCachelineSize :: Int
sdlCachelineSize = Raw.sdlCachelineSize

sdlGetNumLogicalCPUCores :: MonadIO m => m CInt
sdlGetNumLogicalCPUCores = liftIO Raw.sdlGetNumLogicalCPUCoresRaw

sdlGetCPUCacheLineSize :: MonadIO m => m CInt
sdlGetCPUCacheLineSize = liftIO Raw.sdlGetCPUCacheLineSizeRaw

sdlGetSystemRAM :: MonadIO m => m CInt
sdlGetSystemRAM = liftIO Raw.sdlGetSystemRAMRaw

sdlGetSystemPageSize :: MonadIO m => m CInt
sdlGetSystemPageSize = liftIO Raw.sdlGetSystemPageSizeRaw

sdlGetSIMDAlignment :: MonadIO m => m Word64
sdlGetSIMDAlignment = liftIO Raw.sdlGetSIMDAlignmentRaw

sdlHasAltiVec, sdlHasMMX, sdlHasSSE, sdlHasSSE2, sdlHasSSE3, sdlHasSSE41, sdlHasSSE42, sdlHasAVX, sdlHasAVX2, sdlHasAVX512F, sdlHasARMSIMD, sdlHasNEON, sdlHasLSX, sdlHasLASX :: MonadIO m => m Bool
sdlHasAltiVec = liftIO Raw.sdlHasAltiVecRaw
sdlHasMMX = liftIO Raw.sdlHasMMXRaw
sdlHasSSE = liftIO Raw.sdlHasSSERaw
sdlHasSSE2 = liftIO Raw.sdlHasSSE2Raw
sdlHasSSE3 = liftIO Raw.sdlHasSSE3Raw
sdlHasSSE41 = liftIO Raw.sdlHasSSE41Raw
sdlHasSSE42 = liftIO Raw.sdlHasSSE42Raw
sdlHasAVX = liftIO Raw.sdlHasAVXRaw
sdlHasAVX2 = liftIO Raw.sdlHasAVX2Raw
sdlHasAVX512F = liftIO Raw.sdlHasAVX512FRaw
sdlHasARMSIMD = liftIO Raw.sdlHasARMSIMDRaw
sdlHasNEON = liftIO Raw.sdlHasNEONRaw
sdlHasLSX = liftIO Raw.sdlHasLSXRaw
sdlHasLASX = liftIO Raw.sdlHasLASXRaw
