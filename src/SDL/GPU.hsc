-- SDL/GPU.hsc
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-} -- Keep for potential future use if needed, but ccall is primary
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-} -- For conditional GDK compilation

-- |
-- Module      : SDL.GPU
-- Description : Haskell bindings to SDL_gpu.h
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- This module provides bindings to the SDL GPU subsystem, offering a modern,
-- cross-platform graphics and compute API similar to Vulkan, Metal, and D3D12.
--
-- Refer to the official SDL documentation and CategoryGPU for detailed explanations:
-- https://wiki.libsdl.org/SDL3/CategoryGPU

module SDL.GPU where

#include <SDL3/SDL_gpu.h>
#include <SDL3/SDL_version.h>

-- Haskell Imports
import Foreign.Ptr (Ptr, nullPtr, FunPtr, castPtr, plusPtr)
import Foreign.C.Types
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.Marshal.Alloc (alloca, malloc, free, mallocBytes)
import Foreign.Marshal.Array (withArray, withArrayLen, peekArray, peekArray0, allocaArray)
import Foreign.Marshal.Utils (copyBytes, fromBool, toBool, with, maybeWith, new, withMany)
import Foreign.Storable (Storable(..))
import Data.Word (Word8, Word16, Word32)
import Data.Int (Int32)
import Data.Bits (Bits, (.|.))
import Data.Maybe (fromMaybe)
import Control.Monad (when, unless, forM_, void)
import Control.Exception (bracket)

-- SDL Imports (Assuming these exist from previous work)
import SDL.Error (sdlGetError)
import SDL.Pixels (SDLPixelFormat, SDLFColor(..))
import SDL.Properties (SDLPropertiesID)
import SDL.Rect (SDLRect(..))
import SDL.Surface (SDLSurface, SDLFlipMode(..))
import SDL.Video (SDLWindow(..)) -- Assuming SDLWindow is Ptr SDLWindow internally

-- Helper to convert CBool to Bool more explicitly than fromBool
fromCBool :: CBool -> Bool
fromCBool = toBool

-- Opaque Handle Types
newtype SDLGPUDevice = SDLGPUDevice (Ptr SDLGPUDevice) deriving (Show, Eq)
newtype SDLGPUBuffer = SDLGPUBuffer (Ptr SDLGPUBuffer) deriving (Show, Eq)
newtype SDLGPUTransferBuffer = SDLGPUTransferBuffer (Ptr SDLGPUTransferBuffer) deriving (Show, Eq)
newtype SDLGPUTexture = SDLGPUTexture (Ptr SDLGPUTexture) deriving (Show, Eq)
newtype SDLGPUSampler = SDLGPUSampler (Ptr SDLGPUSampler) deriving (Show, Eq)
newtype SDLGPUShader = SDLGPUShader (Ptr SDLGPUShader) deriving (Show, Eq)
newtype SDLGPUComputePipeline = SDLGPUComputePipeline (Ptr SDLGPUComputePipeline) deriving (Show, Eq)
newtype SDLGPUGraphicsPipeline = SDLGPUGraphicsPipeline (Ptr SDLGPUGraphicsPipeline) deriving (Show, Eq)
newtype SDLGPUCommandBuffer = SDLGPUCommandBuffer (Ptr SDLGPUCommandBuffer) deriving (Show, Eq)
newtype SDLGPURenderPass = SDLGPURenderPass (Ptr SDLGPURenderPass) deriving (Show, Eq)
newtype SDLGPUComputePass = SDLGPUComputePass (Ptr SDLGPUComputePass) deriving (Show, Eq)
newtype SDLGPUCopyPass = SDLGPUCopyPass (Ptr SDLGPUCopyPass) deriving (Show, Eq)
newtype SDLGPUFence = SDLGPUFence (Ptr SDLGPUFence) deriving (Show, Eq)

-- Enum Types (Using CInt as underlying type for enums)

newtype SDLGPUPrimitiveType = SDLGPUPrimitiveType CInt deriving (Show, Eq, Storable)
#{enum SDLGPUPrimitiveType, SDLGPUPrimitiveType
 , sdlGPUPrimitiveTypeTriangleList  = SDL_GPU_PRIMITIVETYPE_TRIANGLELIST
 , sdlGPUPrimitiveTypeTriangleStrip = SDL_GPU_PRIMITIVETYPE_TRIANGLESTRIP
 , sdlGPUPrimitiveTypeLineList      = SDL_GPU_PRIMITIVETYPE_LINELIST
 , sdlGPUPrimitiveTypeLineStrip     = SDL_GPU_PRIMITIVETYPE_LINESTRIP
 , sdlGPUPrimitiveTypePointList     = SDL_GPU_PRIMITIVETYPE_POINTLIST
 }

newtype SDLGPULoadOp = SDLGPULoadOp CInt deriving (Show, Eq, Storable)
#{enum SDLGPULoadOp, SDLGPULoadOp
 , sdlGPULoadOpLoad      = SDL_GPU_LOADOP_LOAD
 , sdlGPULoadOpClear     = SDL_GPU_LOADOP_CLEAR
 , sdlGPULoadOpDontCare  = SDL_GPU_LOADOP_DONT_CARE
 }

newtype SDLGPUStoreOp = SDLGPUStoreOp CInt deriving (Show, Eq, Storable)
#{enum SDLGPUStoreOp, SDLGPUStoreOp
 , sdlGPUStoreOpStore           = SDL_GPU_STOREOP_STORE
 , sdlGPUStoreOpDontCare        = SDL_GPU_STOREOP_DONT_CARE
 , sdlGPUStoreOpResolve         = SDL_GPU_STOREOP_RESOLVE
 , sdlGPUStoreOpResolveAndStore = SDL_GPU_STOREOP_RESOLVE_AND_STORE
 }

newtype SDLGPUIndexElementSize = SDLGPUIndexElementSize CInt deriving (Show, Eq, Storable)
#{enum SDLGPUIndexElementSize, SDLGPUIndexElementSize
 , sdlGPUIndexElementSize16Bit = SDL_GPU_INDEXELEMENTSIZE_16BIT
 , sdlGPUIndexElementSize32Bit = SDL_GPU_INDEXELEMENTSIZE_32BIT
 }

newtype SDLGPUTextureFormat = SDLGPUTextureFormat CInt deriving (Show, Eq, Storable)
#{enum SDLGPUTextureFormat, SDLGPUTextureFormat
 , sdlGPUTextureFormatInvalid = SDL_GPU_TEXTUREFORMAT_INVALID
 , sdlGPUTextureFormatA8_UNORM = SDL_GPU_TEXTUREFORMAT_A8_UNORM
 , sdlGPUTextureFormatR8_UNORM = SDL_GPU_TEXTUREFORMAT_R8_UNORM
 , sdlGPUTextureFormatR8G8_UNORM = SDL_GPU_TEXTUREFORMAT_R8G8_UNORM
 , sdlGPUTextureFormatR8G8B8A8_UNORM = SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM
 , sdlGPUTextureFormatR16_UNORM = SDL_GPU_TEXTUREFORMAT_R16_UNORM
 , sdlGPUTextureFormatR16G16_UNORM = SDL_GPU_TEXTUREFORMAT_R16G16_UNORM
 , sdlGPUTextureFormatR16G16B16A16_UNORM = SDL_GPU_TEXTUREFORMAT_R16G16B16A16_UNORM
 , sdlGPUTextureFormatR10G10B10A2_UNORM = SDL_GPU_TEXTUREFORMAT_R10G10B10A2_UNORM
 , sdlGPUTextureFormatB5G6R5_UNORM = SDL_GPU_TEXTUREFORMAT_B5G6R5_UNORM
 , sdlGPUTextureFormatB5G5R5A1_UNORM = SDL_GPU_TEXTUREFORMAT_B5G5R5A1_UNORM
 , sdlGPUTextureFormatB4G4R4A4_UNORM = SDL_GPU_TEXTUREFORMAT_B4G4R4A4_UNORM
 , sdlGPUTextureFormatB8G8R8A8_UNORM = SDL_GPU_TEXTUREFORMAT_B8G8R8A8_UNORM
 , sdlGPUTextureFormatBC1_RGBA_UNORM = SDL_GPU_TEXTUREFORMAT_BC1_RGBA_UNORM
 , sdlGPUTextureFormatBC2_RGBA_UNORM = SDL_GPU_TEXTUREFORMAT_BC2_RGBA_UNORM
 , sdlGPUTextureFormatBC3_RGBA_UNORM = SDL_GPU_TEXTUREFORMAT_BC3_RGBA_UNORM
 , sdlGPUTextureFormatBC4_R_UNORM = SDL_GPU_TEXTUREFORMAT_BC4_R_UNORM
 , sdlGPUTextureFormatBC5_RG_UNORM = SDL_GPU_TEXTUREFORMAT_BC5_RG_UNORM
 , sdlGPUTextureFormatBC7_RGBA_UNORM = SDL_GPU_TEXTUREFORMAT_BC7_RGBA_UNORM
 , sdlGPUTextureFormatBC6H_RGB_FLOAT = SDL_GPU_TEXTUREFORMAT_BC6H_RGB_FLOAT
 , sdlGPUTextureFormatBC6H_RGB_UFLOAT = SDL_GPU_TEXTUREFORMAT_BC6H_RGB_UFLOAT
 , sdlGPUTextureFormatR8_SNORM = SDL_GPU_TEXTUREFORMAT_R8_SNORM
 , sdlGPUTextureFormatR8G8_SNORM = SDL_GPU_TEXTUREFORMAT_R8G8_SNORM
 , sdlGPUTextureFormatR8G8B8A8_SNORM = SDL_GPU_TEXTUREFORMAT_R8G8B8A8_SNORM
 , sdlGPUTextureFormatR16_SNORM = SDL_GPU_TEXTUREFORMAT_R16_SNORM
 , sdlGPUTextureFormatR16G16_SNORM = SDL_GPU_TEXTUREFORMAT_R16G16_SNORM
 , sdlGPUTextureFormatR16G16B16A16_SNORM = SDL_GPU_TEXTUREFORMAT_R16G16B16A16_SNORM
 , sdlGPUTextureFormatR16_FLOAT = SDL_GPU_TEXTUREFORMAT_R16_FLOAT
 , sdlGPUTextureFormatR16G16_FLOAT = SDL_GPU_TEXTUREFORMAT_R16G16_FLOAT
 , sdlGPUTextureFormatR16G16B16A16_FLOAT = SDL_GPU_TEXTUREFORMAT_R16G16B16A16_FLOAT
 , sdlGPUTextureFormatR32_FLOAT = SDL_GPU_TEXTUREFORMAT_R32_FLOAT
 , sdlGPUTextureFormatR32G32_FLOAT = SDL_GPU_TEXTUREFORMAT_R32G32_FLOAT
 , sdlGPUTextureFormatR32G32B32A32_FLOAT = SDL_GPU_TEXTUREFORMAT_R32G32B32A32_FLOAT
 , sdlGPUTextureFormatR11G11B10_UFLOAT = SDL_GPU_TEXTUREFORMAT_R11G11B10_UFLOAT
 , sdlGPUTextureFormatR8_UINT = SDL_GPU_TEXTUREFORMAT_R8_UINT
 , sdlGPUTextureFormatR8G8_UINT = SDL_GPU_TEXTUREFORMAT_R8G8_UINT
 , sdlGPUTextureFormatR8G8B8A8_UINT = SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UINT
 , sdlGPUTextureFormatR16_UINT = SDL_GPU_TEXTUREFORMAT_R16_UINT
 , sdlGPUTextureFormatR16G16_UINT = SDL_GPU_TEXTUREFORMAT_R16G16_UINT
 , sdlGPUTextureFormatR16G16B16A16_UINT = SDL_GPU_TEXTUREFORMAT_R16G16B16A16_UINT
 , sdlGPUTextureFormatR32_UINT = SDL_GPU_TEXTUREFORMAT_R32_UINT
 , sdlGPUTextureFormatR32G32_UINT = SDL_GPU_TEXTUREFORMAT_R32G32_UINT
 , sdlGPUTextureFormatR32G32B32A32_UINT = SDL_GPU_TEXTUREFORMAT_R32G32B32A32_UINT
 , sdlGPUTextureFormatR8_INT = SDL_GPU_TEXTUREFORMAT_R8_INT
 , sdlGPUTextureFormatR8G8_INT = SDL_GPU_TEXTUREFORMAT_R8G8_INT
 , sdlGPUTextureFormatR8G8B8A8_INT = SDL_GPU_TEXTUREFORMAT_R8G8B8A8_INT
 , sdlGPUTextureFormatR16_INT = SDL_GPU_TEXTUREFORMAT_R16_INT
 , sdlGPUTextureFormatR16G16_INT = SDL_GPU_TEXTUREFORMAT_R16G16_INT
 , sdlGPUTextureFormatR16G16B16A16_INT = SDL_GPU_TEXTUREFORMAT_R16G16B16A16_INT
 , sdlGPUTextureFormatR32_INT = SDL_GPU_TEXTUREFORMAT_R32_INT
 , sdlGPUTextureFormatR32G32_INT = SDL_GPU_TEXTUREFORMAT_R32G32_INT
 , sdlGPUTextureFormatR32G32B32A32_INT = SDL_GPU_TEXTUREFORMAT_R32G32B32A32_INT
 , sdlGPUTextureFormatR8G8B8A8_UNORM_SRGB = SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM_SRGB
 , sdlGPUTextureFormatB8G8R8A8_UNORM_SRGB = SDL_GPU_TEXTUREFORMAT_B8G8R8A8_UNORM_SRGB
 , sdlGPUTextureFormatBC1_RGBA_UNORM_SRGB = SDL_GPU_TEXTUREFORMAT_BC1_RGBA_UNORM_SRGB
 , sdlGPUTextureFormatBC2_RGBA_UNORM_SRGB = SDL_GPU_TEXTUREFORMAT_BC2_RGBA_UNORM_SRGB
 , sdlGPUTextureFormatBC3_RGBA_UNORM_SRGB = SDL_GPU_TEXTUREFORMAT_BC3_RGBA_UNORM_SRGB
 , sdlGPUTextureFormatBC7_RGBA_UNORM_SRGB = SDL_GPU_TEXTUREFORMAT_BC7_RGBA_UNORM_SRGB
 , sdlGPUTextureFormatD16_UNORM = SDL_GPU_TEXTUREFORMAT_D16_UNORM
 , sdlGPUTextureFormatD24_UNORM = SDL_GPU_TEXTUREFORMAT_D24_UNORM
 , sdlGPUTextureFormatD32_FLOAT = SDL_GPU_TEXTUREFORMAT_D32_FLOAT
 , sdlGPUTextureFormatD24_UNORM_S8_UINT = SDL_GPU_TEXTUREFORMAT_D24_UNORM_S8_UINT
 , sdlGPUTextureFormatD32_FLOAT_S8_UINT = SDL_GPU_TEXTUREFORMAT_D32_FLOAT_S8_UINT
 , sdlGPUTextureFormatASTC_4x4_UNORM = SDL_GPU_TEXTUREFORMAT_ASTC_4x4_UNORM
 , sdlGPUTextureFormatASTC_5x4_UNORM = SDL_GPU_TEXTUREFORMAT_ASTC_5x4_UNORM
 , sdlGPUTextureFormatASTC_5x5_UNORM = SDL_GPU_TEXTUREFORMAT_ASTC_5x5_UNORM
 , sdlGPUTextureFormatASTC_6x5_UNORM = SDL_GPU_TEXTUREFORMAT_ASTC_6x5_UNORM
 , sdlGPUTextureFormatASTC_6x6_UNORM = SDL_GPU_TEXTUREFORMAT_ASTC_6x6_UNORM
 , sdlGPUTextureFormatASTC_8x5_UNORM = SDL_GPU_TEXTUREFORMAT_ASTC_8x5_UNORM
 , sdlGPUTextureFormatASTC_8x6_UNORM = SDL_GPU_TEXTUREFORMAT_ASTC_8x6_UNORM
 , sdlGPUTextureFormatASTC_8x8_UNORM = SDL_GPU_TEXTUREFORMAT_ASTC_8x8_UNORM
 , sdlGPUTextureFormatASTC_10x5_UNORM = SDL_GPU_TEXTUREFORMAT_ASTC_10x5_UNORM
 , sdlGPUTextureFormatASTC_10x6_UNORM = SDL_GPU_TEXTUREFORMAT_ASTC_10x6_UNORM
 , sdlGPUTextureFormatASTC_10x8_UNORM = SDL_GPU_TEXTUREFORMAT_ASTC_10x8_UNORM
 , sdlGPUTextureFormatASTC_10x10_UNORM = SDL_GPU_TEXTUREFORMAT_ASTC_10x10_UNORM
 , sdlGPUTextureFormatASTC_12x10_UNORM = SDL_GPU_TEXTUREFORMAT_ASTC_12x10_UNORM
 , sdlGPUTextureFormatASTC_12x12_UNORM = SDL_GPU_TEXTUREFORMAT_ASTC_12x12_UNORM
 , sdlGPUTextureFormatASTC_4x4_UNORM_SRGB = SDL_GPU_TEXTUREFORMAT_ASTC_4x4_UNORM_SRGB
 , sdlGPUTextureFormatASTC_5x4_UNORM_SRGB = SDL_GPU_TEXTUREFORMAT_ASTC_5x4_UNORM_SRGB
 , sdlGPUTextureFormatASTC_5x5_UNORM_SRGB = SDL_GPU_TEXTUREFORMAT_ASTC_5x5_UNORM_SRGB
 , sdlGPUTextureFormatASTC_6x5_UNORM_SRGB = SDL_GPU_TEXTUREFORMAT_ASTC_6x5_UNORM_SRGB
 , sdlGPUTextureFormatASTC_6x6_UNORM_SRGB = SDL_GPU_TEXTUREFORMAT_ASTC_6x6_UNORM_SRGB
 , sdlGPUTextureFormatASTC_8x5_UNORM_SRGB = SDL_GPU_TEXTUREFORMAT_ASTC_8x5_UNORM_SRGB
 , sdlGPUTextureFormatASTC_8x6_UNORM_SRGB = SDL_GPU_TEXTUREFORMAT_ASTC_8x6_UNORM_SRGB
 , sdlGPUTextureFormatASTC_8x8_UNORM_SRGB = SDL_GPU_TEXTUREFORMAT_ASTC_8x8_UNORM_SRGB
 , sdlGPUTextureFormatASTC_10x5_UNORM_SRGB = SDL_GPU_TEXTUREFORMAT_ASTC_10x5_UNORM_SRGB
 , sdlGPUTextureFormatASTC_10x6_UNORM_SRGB = SDL_GPU_TEXTUREFORMAT_ASTC_10x6_UNORM_SRGB
 , sdlGPUTextureFormatASTC_10x8_UNORM_SRGB = SDL_GPU_TEXTUREFORMAT_ASTC_10x8_UNORM_SRGB
 , sdlGPUTextureFormatASTC_10x10_UNORM_SRGB = SDL_GPU_TEXTUREFORMAT_ASTC_10x10_UNORM_SRGB
 , sdlGPUTextureFormatASTC_12x10_UNORM_SRGB = SDL_GPU_TEXTUREFORMAT_ASTC_12x10_UNORM_SRGB
 , sdlGPUTextureFormatASTC_12x12_UNORM_SRGB = SDL_GPU_TEXTUREFORMAT_ASTC_12x12_UNORM_SRGB
 , sdlGPUTextureFormatASTC_4x4_FLOAT = SDL_GPU_TEXTUREFORMAT_ASTC_4x4_FLOAT
 , sdlGPUTextureFormatASTC_5x4_FLOAT = SDL_GPU_TEXTUREFORMAT_ASTC_5x4_FLOAT
 , sdlGPUTextureFormatASTC_5x5_FLOAT = SDL_GPU_TEXTUREFORMAT_ASTC_5x5_FLOAT
 , sdlGPUTextureFormatASTC_6x5_FLOAT = SDL_GPU_TEXTUREFORMAT_ASTC_6x5_FLOAT
 , sdlGPUTextureFormatASTC_6x6_FLOAT = SDL_GPU_TEXTUREFORMAT_ASTC_6x6_FLOAT
 , sdlGPUTextureFormatASTC_8x5_FLOAT = SDL_GPU_TEXTUREFORMAT_ASTC_8x5_FLOAT
 , sdlGPUTextureFormatASTC_8x6_FLOAT = SDL_GPU_TEXTUREFORMAT_ASTC_8x6_FLOAT
 , sdlGPUTextureFormatASTC_8x8_FLOAT = SDL_GPU_TEXTUREFORMAT_ASTC_8x8_FLOAT
 , sdlGPUTextureFormatASTC_10x5_FLOAT = SDL_GPU_TEXTUREFORMAT_ASTC_10x5_FLOAT
 , sdlGPUTextureFormatASTC_10x6_FLOAT = SDL_GPU_TEXTUREFORMAT_ASTC_10x6_FLOAT
 , sdlGPUTextureFormatASTC_10x8_FLOAT = SDL_GPU_TEXTUREFORMAT_ASTC_10x8_FLOAT
 , sdlGPUTextureFormatASTC_10x10_FLOAT = SDL_GPU_TEXTUREFORMAT_ASTC_10x10_FLOAT
 , sdlGPUTextureFormatASTC_12x10_FLOAT = SDL_GPU_TEXTUREFORMAT_ASTC_12x10_FLOAT
 , sdlGPUTextureFormatASTC_12x12_FLOAT = SDL_GPU_TEXTUREFORMAT_ASTC_12x12_FLOAT
 }

newtype SDLGPUTextureType = SDLGPUTextureType CInt deriving (Show, Eq, Storable)
#{enum SDLGPUTextureType, SDLGPUTextureType
 , sdlGPUTextureType2D       = SDL_GPU_TEXTURETYPE_2D
 , sdlGPUTextureType2DArray  = SDL_GPU_TEXTURETYPE_2D_ARRAY
 , sdlGPUTextureType3D       = SDL_GPU_TEXTURETYPE_3D
 , sdlGPUTextureTypeCube     = SDL_GPU_TEXTURETYPE_CUBE
 , sdlGPUTextureTypeCubeArray = SDL_GPU_TEXTURETYPE_CUBE_ARRAY
 }

newtype SDLGPUSampleCount = SDLGPUSampleCount CInt deriving (Show, Eq, Storable)
#{enum SDLGPUSampleCount, SDLGPUSampleCount
 , sdlGPUSampleCount1 = SDL_GPU_SAMPLECOUNT_1
 , sdlGPUSampleCount2 = SDL_GPU_SAMPLECOUNT_2
 , sdlGPUSampleCount4 = SDL_GPU_SAMPLECOUNT_4
 , sdlGPUSampleCount8 = SDL_GPU_SAMPLECOUNT_8
 }

newtype SDLGPUCubeMapFace = SDLGPUCubeMapFace CInt deriving (Show, Eq, Storable)
#{enum SDLGPUCubeMapFace, SDLGPUCubeMapFace
 , sdlGPUCubeMapFacePositiveX = SDL_GPU_CUBEMAPFACE_POSITIVEX
 , sdlGPUCubeMapFaceNegativeX = SDL_GPU_CUBEMAPFACE_NEGATIVEX
 , sdlGPUCubeMapFacePositiveY = SDL_GPU_CUBEMAPFACE_POSITIVEY
 , sdlGPUCubeMapFaceNegativeY = SDL_GPU_CUBEMAPFACE_NEGATIVEY
 , sdlGPUCubeMapFacePositiveZ = SDL_GPU_CUBEMAPFACE_POSITIVEZ
 , sdlGPUCubeMapFaceNegativeZ = SDL_GPU_CUBEMAPFACE_NEGATIVEZ
 }

newtype SDLGPUTransferBufferUsage = SDLGPUTransferBufferUsage CInt deriving (Show, Eq, Storable)
#{enum SDLGPUTransferBufferUsage, SDLGPUTransferBufferUsage
 , sdlGPUTransferBufferUsageUpload   = SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD
 , sdlGPUTransferBufferUsageDownload = SDL_GPU_TRANSFERBUFFERUSAGE_DOWNLOAD
 }

newtype SDLGPUShaderStage = SDLGPUShaderStage CInt deriving (Show, Eq, Storable)
#{enum SDLGPUShaderStage, SDLGPUShaderStage
 , sdlGPUShaderStageVertex   = SDL_GPU_SHADERSTAGE_VERTEX
 , sdlGPUShaderStageFragment = SDL_GPU_SHADERSTAGE_FRAGMENT
 }

newtype SDLGPUVertexElementFormat = SDLGPUVertexElementFormat CInt deriving (Show, Eq, Storable)
#{enum SDLGPUVertexElementFormat, SDLGPUVertexElementFormat
 , sdlGPUVertexElementFormatInvalid      = SDL_GPU_VERTEXELEMENTFORMAT_INVALID
 , sdlGPUVertexElementFormatInt          = SDL_GPU_VERTEXELEMENTFORMAT_INT
 , sdlGPUVertexElementFormatInt2         = SDL_GPU_VERTEXELEMENTFORMAT_INT2
 , sdlGPUVertexElementFormatInt3         = SDL_GPU_VERTEXELEMENTFORMAT_INT3
 , sdlGPUVertexElementFormatInt4         = SDL_GPU_VERTEXELEMENTFORMAT_INT4
 , sdlGPUVertexElementFormatUInt         = SDL_GPU_VERTEXELEMENTFORMAT_UINT
 , sdlGPUVertexElementFormatUInt2        = SDL_GPU_VERTEXELEMENTFORMAT_UINT2
 , sdlGPUVertexElementFormatUInt3        = SDL_GPU_VERTEXELEMENTFORMAT_UINT3
 , sdlGPUVertexElementFormatUInt4        = SDL_GPU_VERTEXELEMENTFORMAT_UINT4
 , sdlGPUVertexElementFormatFloat        = SDL_GPU_VERTEXELEMENTFORMAT_FLOAT
 , sdlGPUVertexElementFormatFloat2       = SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2
 , sdlGPUVertexElementFormatFloat3       = SDL_GPU_VERTEXELEMENTFORMAT_FLOAT3
 , sdlGPUVertexElementFormatFloat4       = SDL_GPU_VERTEXELEMENTFORMAT_FLOAT4
 , sdlGPUVertexElementFormatByte2        = SDL_GPU_VERTEXELEMENTFORMAT_BYTE2
 , sdlGPUVertexElementFormatByte4        = SDL_GPU_VERTEXELEMENTFORMAT_BYTE4
 , sdlGPUVertexElementFormatUByte2       = SDL_GPU_VERTEXELEMENTFORMAT_UBYTE2
 , sdlGPUVertexElementFormatUByte4       = SDL_GPU_VERTEXELEMENTFORMAT_UBYTE4
 , sdlGPUVertexElementFormatByte2Norm    = SDL_GPU_VERTEXELEMENTFORMAT_BYTE2_NORM
 , sdlGPUVertexElementFormatByte4Norm    = SDL_GPU_VERTEXELEMENTFORMAT_BYTE4_NORM
 , sdlGPUVertexElementFormatUByte2Norm   = SDL_GPU_VERTEXELEMENTFORMAT_UBYTE2_NORM
 , sdlGPUVertexElementFormatUByte4Norm   = SDL_GPU_VERTEXELEMENTFORMAT_UBYTE4_NORM
 , sdlGPUVertexElementFormatShort2       = SDL_GPU_VERTEXELEMENTFORMAT_SHORT2
 , sdlGPUVertexElementFormatShort4       = SDL_GPU_VERTEXELEMENTFORMAT_SHORT4
 , sdlGPUVertexElementFormatUShort2      = SDL_GPU_VERTEXELEMENTFORMAT_USHORT2
 , sdlGPUVertexElementFormatUShort4      = SDL_GPU_VERTEXELEMENTFORMAT_USHORT4
 , sdlGPUVertexElementFormatShort2Norm   = SDL_GPU_VERTEXELEMENTFORMAT_SHORT2_NORM
 , sdlGPUVertexElementFormatShort4Norm   = SDL_GPU_VERTEXELEMENTFORMAT_SHORT4_NORM
 , sdlGPUVertexElementFormatUShort2Norm  = SDL_GPU_VERTEXELEMENTFORMAT_USHORT2_NORM
 , sdlGPUVertexElementFormatUShort4Norm  = SDL_GPU_VERTEXELEMENTFORMAT_USHORT4_NORM
 , sdlGPUVertexElementFormatHalf2        = SDL_GPU_VERTEXELEMENTFORMAT_HALF2
 , sdlGPUVertexElementFormatHalf4        = SDL_GPU_VERTEXELEMENTFORMAT_HALF4
 }

newtype SDLGPUVertexInputRate = SDLGPUVertexInputRate CInt deriving (Show, Eq, Storable)
#{enum SDLGPUVertexInputRate, SDLGPUVertexInputRate
 , sdlGPUVertexInputRateVertex   = SDL_GPU_VERTEXINPUTRATE_VERTEX
 , sdlGPUVertexInputRateInstance = SDL_GPU_VERTEXINPUTRATE_INSTANCE
 }

newtype SDLGPUFillMode = SDLGPUFillMode CInt deriving (Show, Eq, Storable)
#{enum SDLGPUFillMode, SDLGPUFillMode
 , sdlGPUFillModeFill = SDL_GPU_FILLMODE_FILL
 , sdlGPUFillModeLine = SDL_GPU_FILLMODE_LINE
 }

newtype SDLGPUCullMode = SDLGPUCullMode CInt deriving (Show, Eq, Storable)
#{enum SDLGPUCullMode, SDLGPUCullMode
 , sdlGPUCullModeNone  = SDL_GPU_CULLMODE_NONE
 , sdlGPUCullModeFront = SDL_GPU_CULLMODE_FRONT
 , sdlGPUCullModeBack  = SDL_GPU_CULLMODE_BACK
 }

newtype SDLGPUFrontFace = SDLGPUFrontFace CInt deriving (Show, Eq, Storable)
#{enum SDLGPUFrontFace, SDLGPUFrontFace
 , sdlGPUFrontFaceCounterClockwise = SDL_GPU_FRONTFACE_COUNTER_CLOCKWISE
 , sdlGPUFrontFaceClockwise        = SDL_GPU_FRONTFACE_CLOCKWISE
 }

newtype SDLGPUCompareOp = SDLGPUCompareOp CInt deriving (Show, Eq, Storable)
#{enum SDLGPUCompareOp, SDLGPUCompareOp
 , sdlGPUCompareOpInvalid          = SDL_GPU_COMPAREOP_INVALID
 , sdlGPUCompareOpNever            = SDL_GPU_COMPAREOP_NEVER
 , sdlGPUCompareOpLess             = SDL_GPU_COMPAREOP_LESS
 , sdlGPUCompareOpEqual            = SDL_GPU_COMPAREOP_EQUAL
 , sdlGPUCompareOpLessOrEqual    = SDL_GPU_COMPAREOP_LESS_OR_EQUAL
 , sdlGPUCompareOpGreater          = SDL_GPU_COMPAREOP_GREATER
 , sdlGPUCompareOpNotEqual         = SDL_GPU_COMPAREOP_NOT_EQUAL
 , sdlGPUCompareOpGreaterOrEqual = SDL_GPU_COMPAREOP_GREATER_OR_EQUAL
 , sdlGPUCompareOpAlways           = SDL_GPU_COMPAREOP_ALWAYS
 }

newtype SDLGPUStencilOp = SDLGPUStencilOp CInt deriving (Show, Eq, Storable)
#{enum SDLGPUStencilOp, SDLGPUStencilOp
 , sdlGPUStencilOpInvalid             = SDL_GPU_STENCILOP_INVALID
 , sdlGPUStencilOpKeep                = SDL_GPU_STENCILOP_KEEP
 , sdlGPUStencilOpZero                = SDL_GPU_STENCILOP_ZERO
 , sdlGPUStencilOpReplace             = SDL_GPU_STENCILOP_REPLACE
 , sdlGPUStencilOpIncrementAndClamp   = SDL_GPU_STENCILOP_INCREMENT_AND_CLAMP
 , sdlGPUStencilOpDecrementAndClamp   = SDL_GPU_STENCILOP_DECREMENT_AND_CLAMP
 , sdlGPUStencilOpInvert              = SDL_GPU_STENCILOP_INVERT
 , sdlGPUStencilOpIncrementAndWrap    = SDL_GPU_STENCILOP_INCREMENT_AND_WRAP
 , sdlGPUStencilOpDecrementAndWrap    = SDL_GPU_STENCILOP_DECREMENT_AND_WRAP
 }

newtype SDLGPUBlendOp = SDLGPUBlendOp CInt deriving (Show, Eq, Storable)
#{enum SDLGPUBlendOp, SDLGPUBlendOp
 , sdlGPUBlendOpInvalid          = SDL_GPU_BLENDOP_INVALID
 , sdlGPUBlendOpAdd              = SDL_GPU_BLENDOP_ADD
 , sdlGPUBlendOpSubtract         = SDL_GPU_BLENDOP_SUBTRACT
 , sdlGPUBlendOpReverseSubtract  = SDL_GPU_BLENDOP_REVERSE_SUBTRACT
 , sdlGPUBlendOpMin              = SDL_GPU_BLENDOP_MIN
 , sdlGPUBlendOpMax              = SDL_GPU_BLENDOP_MAX
 }

newtype SDLGPUBlendFactor = SDLGPUBlendFactor CInt deriving (Show, Eq, Storable)
#{enum SDLGPUBlendFactor, SDLGPUBlendFactor
 , sdlGPUBlendFactorInvalid                = SDL_GPU_BLENDFACTOR_INVALID
 , sdlGPUBlendFactorZero                   = SDL_GPU_BLENDFACTOR_ZERO
 , sdlGPUBlendFactorOne                    = SDL_GPU_BLENDFACTOR_ONE
 , sdlGPUBlendFactorSrcColor               = SDL_GPU_BLENDFACTOR_SRC_COLOR
 , sdlGPUBlendFactorOneMinusSrcColor     = SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_COLOR
 , sdlGPUBlendFactorDstColor               = SDL_GPU_BLENDFACTOR_DST_COLOR
 , sdlGPUBlendFactorOneMinusDstColor     = SDL_GPU_BLENDFACTOR_ONE_MINUS_DST_COLOR
 , sdlGPUBlendFactorSrcAlpha               = SDL_GPU_BLENDFACTOR_SRC_ALPHA
 , sdlGPUBlendFactorOneMinusSrcAlpha     = SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA
 , sdlGPUBlendFactorDstAlpha               = SDL_GPU_BLENDFACTOR_DST_ALPHA
 , sdlGPUBlendFactorOneMinusDstAlpha     = SDL_GPU_BLENDFACTOR_ONE_MINUS_DST_ALPHA
 , sdlGPUBlendFactorConstantColor          = SDL_GPU_BLENDFACTOR_CONSTANT_COLOR
 , sdlGPUBlendFactorOneMinusConstantColor  = SDL_GPU_BLENDFACTOR_ONE_MINUS_CONSTANT_COLOR
 , sdlGPUBlendFactorSrcAlphaSaturate       = SDL_GPU_BLENDFACTOR_SRC_ALPHA_SATURATE
 }

newtype SDLGPUFilter = SDLGPUFilter CInt deriving (Show, Eq, Storable)
#{enum SDLGPUFilter, SDLGPUFilter
 , sdlGPUFilterNearest = SDL_GPU_FILTER_NEAREST
 , sdlGPUFilterLinear  = SDL_GPU_FILTER_LINEAR
 }

newtype SDLGPUSamplerMipmapMode = SDLGPUSamplerMipmapMode CInt deriving (Show, Eq, Storable)
#{enum SDLGPUSamplerMipmapMode, SDLGPUSamplerMipmapMode
 , sdlGPUSamplerMipmapModeNearest = SDL_GPU_SAMPLERMIPMAPMODE_NEAREST
 , sdlGPUSamplerMipmapModeLinear  = SDL_GPU_SAMPLERMIPMAPMODE_LINEAR
 }

newtype SDLGPUSamplerAddressMode = SDLGPUSamplerAddressMode CInt deriving (Show, Eq, Storable)
#{enum SDLGPUSamplerAddressMode, SDLGPUSamplerAddressMode
 , sdlGPUSamplerAddressModeRepeat          = SDL_GPU_SAMPLERADDRESSMODE_REPEAT
 , sdlGPUSamplerAddressModeMirroredRepeat  = SDL_GPU_SAMPLERADDRESSMODE_MIRRORED_REPEAT
 , sdlGPUSamplerAddressModeClampToEdge     = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE
 }

newtype SDLGPUPresentMode = SDLGPUPresentMode CInt deriving (Show, Eq, Storable)
#{enum SDLGPUPresentMode, SDLGPUPresentMode
 , sdlGPUPresentModeVSync     = SDL_GPU_PRESENTMODE_VSYNC
 , sdlGPUPresentModeImmediate = SDL_GPU_PRESENTMODE_IMMEDIATE
 , sdlGPUPresentModeMailbox   = SDL_GPU_PRESENTMODE_MAILBOX
 }

newtype SDLGPUSwapchainComposition = SDLGPUSwapchainComposition CInt deriving (Show, Eq, Storable)
#{enum SDLGPUSwapchainComposition, SDLGPUSwapchainComposition
 , sdlGPUSwapchainCompositionSDR               = SDL_GPU_SWAPCHAINCOMPOSITION_SDR
 , sdlGPUSwapchainCompositionSDRLinear         = SDL_GPU_SWAPCHAINCOMPOSITION_SDR_LINEAR
 , sdlGPUSwapchainCompositionHDRExtendedLinear = SDL_GPU_SWAPCHAINCOMPOSITION_HDR_EXTENDED_LINEAR
 , sdlGPUSwapchainCompositionHDR10_ST2084      = SDL_GPU_SWAPCHAINCOMPOSITION_HDR10_ST2084
 }

-- Bitmask Types
newtype SDLGPUTextureUsageFlags = SDLGPUTextureUsageFlags CUInt
  deriving newtype (Show, Eq, Bits, Num, Storable) -- Num allows bitwise OR with literals

pattern SDL_GPU_TEXTUREUSAGE_SAMPLER                                 :: SDLGPUTextureUsageFlags
pattern SDL_GPU_TEXTUREUSAGE_SAMPLER                                 = SDLGPUTextureUsageFlags #{const SDL_GPU_TEXTUREUSAGE_SAMPLER}
pattern SDL_GPU_TEXTUREUSAGE_COLOR_TARGET                            :: SDLGPUTextureUsageFlags
pattern SDL_GPU_TEXTUREUSAGE_COLOR_TARGET                            = SDLGPUTextureUsageFlags #{const SDL_GPU_TEXTUREUSAGE_COLOR_TARGET}
pattern SDL_GPU_TEXTUREUSAGE_DEPTH_STENCIL_TARGET                    :: SDLGPUTextureUsageFlags
pattern SDL_GPU_TEXTUREUSAGE_DEPTH_STENCIL_TARGET                    = SDLGPUTextureUsageFlags #{const SDL_GPU_TEXTUREUSAGE_DEPTH_STENCIL_TARGET}
pattern SDL_GPU_TEXTUREUSAGE_GRAPHICS_STORAGE_READ                   :: SDLGPUTextureUsageFlags
pattern SDL_GPU_TEXTUREUSAGE_GRAPHICS_STORAGE_READ                   = SDLGPUTextureUsageFlags #{const SDL_GPU_TEXTUREUSAGE_GRAPHICS_STORAGE_READ}
pattern SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_READ                    :: SDLGPUTextureUsageFlags
pattern SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_READ                    = SDLGPUTextureUsageFlags #{const SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_READ}
pattern SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_WRITE                   :: SDLGPUTextureUsageFlags
pattern SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_WRITE                   = SDLGPUTextureUsageFlags #{const SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_WRITE}
pattern SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_SIMULTANEOUS_READ_WRITE :: SDLGPUTextureUsageFlags
pattern SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_SIMULTANEOUS_READ_WRITE = SDLGPUTextureUsageFlags #{const SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_SIMULTANEOUS_READ_WRITE}

newtype SDLGPUBufferUsageFlags = SDLGPUBufferUsageFlags CUInt
  deriving newtype (Show, Eq, Bits, Num, Storable)

pattern SDL_GPU_BUFFERUSAGE_VERTEX                                  :: SDLGPUBufferUsageFlags
pattern SDL_GPU_BUFFERUSAGE_VERTEX                                  = SDLGPUBufferUsageFlags #{const SDL_GPU_BUFFERUSAGE_VERTEX}
pattern SDL_GPU_BUFFERUSAGE_INDEX                                   :: SDLGPUBufferUsageFlags
pattern SDL_GPU_BUFFERUSAGE_INDEX                                   = SDLGPUBufferUsageFlags #{const SDL_GPU_BUFFERUSAGE_INDEX}
pattern SDL_GPU_BUFFERUSAGE_INDIRECT                                :: SDLGPUBufferUsageFlags
pattern SDL_GPU_BUFFERUSAGE_INDIRECT                                = SDLGPUBufferUsageFlags #{const SDL_GPU_BUFFERUSAGE_INDIRECT}
pattern SDL_GPU_BUFFERUSAGE_GRAPHICS_STORAGE_READ                   :: SDLGPUBufferUsageFlags
pattern SDL_GPU_BUFFERUSAGE_GRAPHICS_STORAGE_READ                   = SDLGPUBufferUsageFlags #{const SDL_GPU_BUFFERUSAGE_GRAPHICS_STORAGE_READ}
pattern SDL_GPU_BUFFERUSAGE_COMPUTE_STORAGE_READ                    :: SDLGPUBufferUsageFlags
pattern SDL_GPU_BUFFERUSAGE_COMPUTE_STORAGE_READ                    = SDLGPUBufferUsageFlags #{const SDL_GPU_BUFFERUSAGE_COMPUTE_STORAGE_READ}
pattern SDL_GPU_BUFFERUSAGE_COMPUTE_STORAGE_WRITE                   :: SDLGPUBufferUsageFlags
pattern SDL_GPU_BUFFERUSAGE_COMPUTE_STORAGE_WRITE                   = SDLGPUBufferUsageFlags #{const SDL_GPU_BUFFERUSAGE_COMPUTE_STORAGE_WRITE}

newtype SDLGPUShaderFormat = SDLGPUShaderFormat CUInt
  deriving newtype (Show, Eq, Bits, Num, Storable)

pattern SDL_GPU_SHADERFORMAT_INVALID  :: SDLGPUShaderFormat
pattern SDL_GPU_SHADERFORMAT_INVALID  = SDLGPUShaderFormat #{const SDL_GPU_SHADERFORMAT_INVALID}
pattern SDL_GPU_SHADERFORMAT_PRIVATE  :: SDLGPUShaderFormat
pattern SDL_GPU_SHADERFORMAT_PRIVATE  = SDLGPUShaderFormat #{const SDL_GPU_SHADERFORMAT_PRIVATE}
pattern SDL_GPU_SHADERFORMAT_SPIRV    :: SDLGPUShaderFormat
pattern SDL_GPU_SHADERFORMAT_SPIRV    = SDLGPUShaderFormat #{const SDL_GPU_SHADERFORMAT_SPIRV}
pattern SDL_GPU_SHADERFORMAT_DXBC     :: SDLGPUShaderFormat
pattern SDL_GPU_SHADERFORMAT_DXBC     = SDLGPUShaderFormat #{const SDL_GPU_SHADERFORMAT_DXBC}
pattern SDL_GPU_SHADERFORMAT_DXIL     :: SDLGPUShaderFormat
pattern SDL_GPU_SHADERFORMAT_DXIL     = SDLGPUShaderFormat #{const SDL_GPU_SHADERFORMAT_DXIL}
pattern SDL_GPU_SHADERFORMAT_MSL      :: SDLGPUShaderFormat
pattern SDL_GPU_SHADERFORMAT_MSL      = SDLGPUShaderFormat #{const SDL_GPU_SHADERFORMAT_MSL}
pattern SDL_GPU_SHADERFORMAT_METALLIB :: SDLGPUShaderFormat
pattern SDL_GPU_SHADERFORMAT_METALLIB = SDLGPUShaderFormat #{const SDL_GPU_SHADERFORMAT_METALLIB}

newtype SDLGPUColorComponentFlags = SDLGPUColorComponentFlags CUChar
  deriving newtype (Show, Eq, Bits, Num, Storable)

pattern SDL_GPU_COLORCOMPONENT_R :: SDLGPUColorComponentFlags
pattern SDL_GPU_COLORCOMPONENT_R = SDLGPUColorComponentFlags #{const SDL_GPU_COLORCOMPONENT_R}
pattern SDL_GPU_COLORCOMPONENT_G :: SDLGPUColorComponentFlags
pattern SDL_GPU_COLORCOMPONENT_G = SDLGPUColorComponentFlags #{const SDL_GPU_COLORCOMPONENT_G}
pattern SDL_GPU_COLORCOMPONENT_B :: SDLGPUColorComponentFlags
pattern SDL_GPU_COLORCOMPONENT_B = SDLGPUColorComponentFlags #{const SDL_GPU_COLORCOMPONENT_B}
pattern SDL_GPU_COLORCOMPONENT_A :: SDLGPUColorComponentFlags
pattern SDL_GPU_COLORCOMPONENT_A = SDLGPUColorComponentFlags #{const SDL_GPU_COLORCOMPONENT_A}

-- Struct Types

-- SDL_GPUViewport
data SDLGPUViewport = SDLGPUViewport
  { gpuViewportX        :: Float
  , gpuViewportY        :: Float
  , gpuViewportW        :: Float
  , gpuViewportH        :: Float
  , gpuViewportMinDepth :: Float
  , gpuViewportMaxDepth :: Float
  } deriving (Show, Eq)

instance Storable SDLGPUViewport where
  sizeOf _ = #{size SDL_GPUViewport}
  alignment _ = #{alignment SDL_GPUViewport}
  peek ptr = do
    x         <- realToFrac <$> (#{peek SDL_GPUViewport, x} ptr :: IO CFloat)
    y         <- realToFrac <$> (#{peek SDL_GPUViewport, y} ptr :: IO CFloat)
    w         <- realToFrac <$> (#{peek SDL_GPUViewport, w} ptr :: IO CFloat)
    h         <- realToFrac <$> (#{peek SDL_GPUViewport, h} ptr :: IO CFloat)
    min_depth <- realToFrac <$> (#{peek SDL_GPUViewport, min_depth} ptr :: IO CFloat)
    max_depth <- realToFrac <$> (#{peek SDL_GPUViewport, max_depth} ptr :: IO CFloat)
    return SDLGPUViewport {..}
  poke ptr SDLGPUViewport{..} = do
    #{poke SDL_GPUViewport, x} ptr (realToFrac gpuViewportX :: CFloat)
    #{poke SDL_GPUViewport, y} ptr (realToFrac gpuViewportY :: CFloat)
    #{poke SDL_GPUViewport, w} ptr (realToFrac gpuViewportW :: CFloat)
    #{poke SDL_GPUViewport, h} ptr (realToFrac gpuViewportH :: CFloat)
    #{poke SDL_GPUViewport, min_depth} ptr (realToFrac gpuViewportMinDepth :: CFloat)
    #{poke SDL_GPUViewport, max_depth} ptr (realToFrac gpuViewportMaxDepth :: CFloat)

-- SDL_GPUTextureTransferInfo
data SDLGPUTextureTransferInfo = SDLGPUTextureTransferInfo
  { gpuTexTransferBuffer     :: SDLGPUTransferBuffer
  , gpuTexTransferOffset       :: Word32
  , gpuTexTransferPixelsPerRow :: Word32
  , gpuTexTransferRowsPerLayer :: Word32
  } deriving (Show, Eq)

instance Storable SDLGPUTextureTransferInfo where
  sizeOf _ = #{size SDL_GPUTextureTransferInfo}
  alignment _ = #{alignment SDL_GPUTextureTransferInfo}
  peek ptr = do
    transfer_buffer_ptr <- #{peek SDL_GPUTextureTransferInfo, transfer_buffer} ptr
    let transfer_buffer = SDLGPUTransferBuffer transfer_buffer_ptr
    offset         <- #{peek SDL_GPUTextureTransferInfo, offset} ptr :: IO CUInt
    pixels_per_row <- #{peek SDL_GPUTextureTransferInfo, pixels_per_row} ptr :: IO CUInt
    rows_per_layer <- #{peek SDL_GPUTextureTransferInfo, rows_per_layer} ptr :: IO CUInt
    return SDLGPUTextureTransferInfo
      { gpuTexTransferBuffer = transfer_buffer
      , gpuTexTransferOffset = fromIntegral offset
      , gpuTexTransferPixelsPerRow = fromIntegral pixels_per_row
      , gpuTexTransferRowsPerLayer = fromIntegral rows_per_layer
      }
  poke ptr SDLGPUTextureTransferInfo{..} = do
    let (SDLGPUTransferBuffer transfer_buffer_ptr) = gpuTexTransferBuffer
    #{poke SDL_GPUTextureTransferInfo, transfer_buffer} ptr transfer_buffer_ptr
    #{poke SDL_GPUTextureTransferInfo, offset} ptr (fromIntegral gpuTexTransferOffset :: CUInt)
    #{poke SDL_GPUTextureTransferInfo, pixels_per_row} ptr (fromIntegral gpuTexTransferPixelsPerRow :: CUInt)
    #{poke SDL_GPUTextureTransferInfo, rows_per_layer} ptr (fromIntegral gpuTexTransferRowsPerLayer :: CUInt)

-- SDL_GPUTransferBufferLocation
data SDLGPUTransferBufferLocation = SDLGPUTransferBufferLocation
  { gpuTransferBufferLocBuffer :: SDLGPUTransferBuffer
  , gpuTransferBufferLocOffset :: Word32
  } deriving (Show, Eq)

instance Storable SDLGPUTransferBufferLocation where
  sizeOf _ = #{size SDL_GPUTransferBufferLocation}
  alignment _ = #{alignment SDL_GPUTransferBufferLocation}
  peek ptr = do
    transfer_buffer_ptr <- #{peek SDL_GPUTransferBufferLocation, transfer_buffer} ptr
    offset <- #{peek SDL_GPUTransferBufferLocation, offset} ptr :: IO CUInt
    return SDLGPUTransferBufferLocation
      { gpuTransferBufferLocBuffer = SDLGPUTransferBuffer transfer_buffer_ptr
      , gpuTransferBufferLocOffset = fromIntegral offset
      }
  poke ptr SDLGPUTransferBufferLocation{..} = do
    let (SDLGPUTransferBuffer transfer_buffer_ptr) = gpuTransferBufferLocBuffer
    #{poke SDL_GPUTransferBufferLocation, transfer_buffer} ptr transfer_buffer_ptr
    #{poke SDL_GPUTransferBufferLocation, offset} ptr (fromIntegral gpuTransferBufferLocOffset :: CUInt)

-- SDL_GPUTextureLocation
data SDLGPUTextureLocation = SDLGPUTextureLocation
  { gpuTexLocTexture  :: SDLGPUTexture
  , gpuTexLocMipLevel :: Word32
  , gpuTexLocLayer    :: Word32
  , gpuTexLocX        :: Word32
  , gpuTexLocY        :: Word32
  , gpuTexLocZ        :: Word32
  } deriving (Show, Eq)

instance Storable SDLGPUTextureLocation where
  sizeOf _ = #{size SDL_GPUTextureLocation}
  alignment _ = #{alignment SDL_GPUTextureLocation}
  peek ptr = do
    texture_ptr <- #{peek SDL_GPUTextureLocation, texture} ptr
    mip_level <- #{peek SDL_GPUTextureLocation, mip_level} ptr :: IO CUInt
    layer     <- #{peek SDL_GPUTextureLocation, layer} ptr :: IO CUInt
    x         <- #{peek SDL_GPUTextureLocation, x} ptr :: IO CUInt
    y         <- #{peek SDL_GPUTextureLocation, y} ptr :: IO CUInt
    z         <- #{peek SDL_GPUTextureLocation, z} ptr :: IO CUInt
    return SDLGPUTextureLocation
      { gpuTexLocTexture  = SDLGPUTexture texture_ptr
      , gpuTexLocMipLevel = fromIntegral mip_level
      , gpuTexLocLayer    = fromIntegral layer
      , gpuTexLocX        = fromIntegral x
      , gpuTexLocY        = fromIntegral y
      , gpuTexLocZ        = fromIntegral z
      }
  poke ptr SDLGPUTextureLocation{..} = do
    let (SDLGPUTexture texture_ptr) = gpuTexLocTexture
    #{poke SDL_GPUTextureLocation, texture} ptr texture_ptr
    #{poke SDL_GPUTextureLocation, mip_level} ptr (fromIntegral gpuTexLocMipLevel :: CUInt)
    #{poke SDL_GPUTextureLocation, layer} ptr (fromIntegral gpuTexLocLayer :: CUInt)
    #{poke SDL_GPUTextureLocation, x} ptr (fromIntegral gpuTexLocX :: CUInt)
    #{poke SDL_GPUTextureLocation, y} ptr (fromIntegral gpuTexLocY :: CUInt)
    #{poke SDL_GPUTextureLocation, z} ptr (fromIntegral gpuTexLocZ :: CUInt)

-- SDL_GPUTextureRegion
data SDLGPUTextureRegion = SDLGPUTextureRegion
  { gpuTexRegTexture  :: SDLGPUTexture
  , gpuTexRegMipLevel :: Word32
  , gpuTexRegLayer    :: Word32
  , gpuTexRegX        :: Word32
  , gpuTexRegY        :: Word32
  , gpuTexRegZ        :: Word32
  , gpuTexRegW        :: Word32
  , gpuTexRegH        :: Word32
  , gpuTexRegD        :: Word32
  } deriving (Show, Eq)

instance Storable SDLGPUTextureRegion where
  sizeOf _ = #{size SDL_GPUTextureRegion}
  alignment _ = #{alignment SDL_GPUTextureRegion}
  peek ptr = do
    texture_ptr <- #{peek SDL_GPUTextureRegion, texture} ptr
    mip_level <- #{peek SDL_GPUTextureRegion, mip_level} ptr :: IO CUInt
    layer     <- #{peek SDL_GPUTextureRegion, layer} ptr :: IO CUInt
    x         <- #{peek SDL_GPUTextureRegion, x} ptr :: IO CUInt
    y         <- #{peek SDL_GPUTextureRegion, y} ptr :: IO CUInt
    z         <- #{peek SDL_GPUTextureRegion, z} ptr :: IO CUInt
    w         <- #{peek SDL_GPUTextureRegion, w} ptr :: IO CUInt
    h         <- #{peek SDL_GPUTextureRegion, h} ptr :: IO CUInt
    d         <- #{peek SDL_GPUTextureRegion, d} ptr :: IO CUInt
    return SDLGPUTextureRegion
      { gpuTexRegTexture  = SDLGPUTexture texture_ptr
      , gpuTexRegMipLevel = fromIntegral mip_level
      , gpuTexRegLayer    = fromIntegral layer
      , gpuTexRegX        = fromIntegral x
      , gpuTexRegY        = fromIntegral y
      , gpuTexRegZ        = fromIntegral z
      , gpuTexRegW        = fromIntegral w
      , gpuTexRegH        = fromIntegral h
      , gpuTexRegD        = fromIntegral d
      }
  poke ptr SDLGPUTextureRegion{..} = do
    let (SDLGPUTexture texture_ptr) = gpuTexRegTexture
    #{poke SDL_GPUTextureRegion, texture} ptr texture_ptr
    #{poke SDL_GPUTextureRegion, mip_level} ptr (fromIntegral gpuTexRegMipLevel :: CUInt)
    #{poke SDL_GPUTextureRegion, layer} ptr (fromIntegral gpuTexRegLayer :: CUInt)
    #{poke SDL_GPUTextureRegion, x} ptr (fromIntegral gpuTexRegX :: CUInt)
    #{poke SDL_GPUTextureRegion, y} ptr (fromIntegral gpuTexRegY :: CUInt)
    #{poke SDL_GPUTextureRegion, z} ptr (fromIntegral gpuTexRegZ :: CUInt)
    #{poke SDL_GPUTextureRegion, w} ptr (fromIntegral gpuTexRegW :: CUInt)
    #{poke SDL_GPUTextureRegion, h} ptr (fromIntegral gpuTexRegH :: CUInt)
    #{poke SDL_GPUTextureRegion, d} ptr (fromIntegral gpuTexRegD :: CUInt)

-- SDL_GPUBlitRegion
data SDLGPUBlitRegion = SDLGPUBlitRegion
  { gpuBlitRegTexture           :: SDLGPUTexture
  , gpuBlitRegMipLevel          :: Word32
  , gpuBlitRegLayerOrDepthPlane :: Word32
  , gpuBlitRegX                 :: Word32
  , gpuBlitRegY                 :: Word32
  , gpuBlitRegW                 :: Word32
  , gpuBlitRegH                 :: Word32
  } deriving (Show, Eq)

instance Storable SDLGPUBlitRegion where
  sizeOf _ = #{size SDL_GPUBlitRegion}
  alignment _ = #{alignment SDL_GPUBlitRegion}
  peek ptr = do
    texture_ptr <- #{peek SDL_GPUBlitRegion, texture} ptr
    mip_level <- #{peek SDL_GPUBlitRegion, mip_level} ptr :: IO CUInt
    layer_or_depth_plane <- #{peek SDL_GPUBlitRegion, layer_or_depth_plane} ptr :: IO CUInt
    x <- #{peek SDL_GPUBlitRegion, x} ptr :: IO CUInt
    y <- #{peek SDL_GPUBlitRegion, y} ptr :: IO CUInt
    w <- #{peek SDL_GPUBlitRegion, w} ptr :: IO CUInt
    h <- #{peek SDL_GPUBlitRegion, h} ptr :: IO CUInt
    return SDLGPUBlitRegion
      { gpuBlitRegTexture = SDLGPUTexture texture_ptr
      , gpuBlitRegMipLevel = fromIntegral mip_level
      , gpuBlitRegLayerOrDepthPlane = fromIntegral layer_or_depth_plane
      , gpuBlitRegX = fromIntegral x
      , gpuBlitRegY = fromIntegral y
      , gpuBlitRegW = fromIntegral w
      , gpuBlitRegH = fromIntegral h
      }
  poke ptr SDLGPUBlitRegion{..} = do
    let (SDLGPUTexture texture_ptr) = gpuBlitRegTexture
    #{poke SDL_GPUBlitRegion, texture} ptr texture_ptr
    #{poke SDL_GPUBlitRegion, mip_level} ptr (fromIntegral gpuBlitRegMipLevel :: CUInt)
    #{poke SDL_GPUBlitRegion, layer_or_depth_plane} ptr (fromIntegral gpuBlitRegLayerOrDepthPlane :: CUInt)
    #{poke SDL_GPUBlitRegion, x} ptr (fromIntegral gpuBlitRegX :: CUInt)
    #{poke SDL_GPUBlitRegion, y} ptr (fromIntegral gpuBlitRegY :: CUInt)
    #{poke SDL_GPUBlitRegion, w} ptr (fromIntegral gpuBlitRegW :: CUInt)
    #{poke SDL_GPUBlitRegion, h} ptr (fromIntegral gpuBlitRegH :: CUInt)

-- SDL_GPUBufferLocation
data SDLGPUBufferLocation = SDLGPUBufferLocation
  { gpuBufLocBuffer :: SDLGPUBuffer
  , gpuBufLocOffset :: Word32
  } deriving (Show, Eq)

instance Storable SDLGPUBufferLocation where
  sizeOf _ = #{size SDL_GPUBufferLocation}
  alignment _ = #{alignment SDL_GPUBufferLocation}
  peek ptr = do
    buffer_ptr <- #{peek SDL_GPUBufferLocation, buffer} ptr
    offset <- #{peek SDL_GPUBufferLocation, offset} ptr :: IO CUInt
    return SDLGPUBufferLocation
      { gpuBufLocBuffer = SDLGPUBuffer buffer_ptr
      , gpuBufLocOffset = fromIntegral offset
      }
  poke ptr SDLGPUBufferLocation{..} = do
    let (SDLGPUBuffer buffer_ptr) = gpuBufLocBuffer
    #{poke SDL_GPUBufferLocation, buffer} ptr buffer_ptr
    #{poke SDL_GPUBufferLocation, offset} ptr (fromIntegral gpuBufLocOffset :: CUInt)

-- SDL_GPUBufferRegion
data SDLGPUBufferRegion = SDLGPUBufferRegion
  { gpuBufRegBuffer :: SDLGPUBuffer
  , gpuBufRegOffset :: Word32
  , gpuBufRegSize   :: Word32
  } deriving (Show, Eq)

instance Storable SDLGPUBufferRegion where
  sizeOf _ = #{size SDL_GPUBufferRegion}
  alignment _ = #{alignment SDL_GPUBufferRegion}
  peek ptr = do
    buffer_ptr <- #{peek SDL_GPUBufferRegion, buffer} ptr
    offset <- #{peek SDL_GPUBufferRegion, offset} ptr :: IO CUInt
    size <- #{peek SDL_GPUBufferRegion, size} ptr :: IO CUInt
    return SDLGPUBufferRegion
      { gpuBufRegBuffer = SDLGPUBuffer buffer_ptr
      , gpuBufRegOffset = fromIntegral offset
      , gpuBufRegSize   = fromIntegral size
      }
  poke ptr SDLGPUBufferRegion{..} = do
    let (SDLGPUBuffer buffer_ptr) = gpuBufRegBuffer
    #{poke SDL_GPUBufferRegion, buffer} ptr buffer_ptr
    #{poke SDL_GPUBufferRegion, offset} ptr (fromIntegral gpuBufRegOffset :: CUInt)
    #{poke SDL_GPUBufferRegion, size} ptr (fromIntegral gpuBufRegSize :: CUInt)

-- SDL_GPUIndirectDrawCommand
data SDLGPUIndirectDrawCommand = SDLGPUIndirectDrawCommand
  { gpuIndirectDrawNumVertices   :: Word32
  , gpuIndirectDrawNumInstances  :: Word32
  , gpuIndirectDrawFirstVertex   :: Word32
  , gpuIndirectDrawFirstInstance :: Word32
  } deriving (Show, Eq)

instance Storable SDLGPUIndirectDrawCommand where
  sizeOf _ = #{size SDL_GPUIndirectDrawCommand}
  alignment _ = #{alignment SDL_GPUIndirectDrawCommand}
  peek ptr = do
    num_vertices   <- #{peek SDL_GPUIndirectDrawCommand, num_vertices} ptr :: IO CUInt
    num_instances  <- #{peek SDL_GPUIndirectDrawCommand, num_instances} ptr :: IO CUInt
    first_vertex   <- #{peek SDL_GPUIndirectDrawCommand, first_vertex} ptr :: IO CUInt
    first_instance <- #{peek SDL_GPUIndirectDrawCommand, first_instance} ptr :: IO CUInt
    return SDLGPUIndirectDrawCommand
      { gpuIndirectDrawNumVertices = fromIntegral num_vertices
      , gpuIndirectDrawNumInstances = fromIntegral num_instances
      , gpuIndirectDrawFirstVertex = fromIntegral first_vertex
      , gpuIndirectDrawFirstInstance = fromIntegral first_instance
      }
  poke ptr SDLGPUIndirectDrawCommand{..} = do
    #{poke SDL_GPUIndirectDrawCommand, num_vertices} ptr (fromIntegral gpuIndirectDrawNumVertices :: CUInt)
    #{poke SDL_GPUIndirectDrawCommand, num_instances} ptr (fromIntegral gpuIndirectDrawNumInstances :: CUInt)
    #{poke SDL_GPUIndirectDrawCommand, first_vertex} ptr (fromIntegral gpuIndirectDrawFirstVertex :: CUInt)
    #{poke SDL_GPUIndirectDrawCommand, first_instance} ptr (fromIntegral gpuIndirectDrawFirstInstance :: CUInt)

-- SDL_GPUIndexedIndirectDrawCommand
data SDLGPUIndexedIndirectDrawCommand = SDLGPUIndexedIndirectDrawCommand
  { gpuIdxIndirectDrawNumIndices    :: Word32
  , gpuIdxIndirectDrawNumInstances  :: Word32
  , gpuIdxIndirectDrawFirstIndex    :: Word32
  , gpuIdxIndirectDrawVertexOffset  :: Int32
  , gpuIdxIndirectDrawFirstInstance :: Word32
  } deriving (Show, Eq)

instance Storable SDLGPUIndexedIndirectDrawCommand where
  sizeOf _ = #{size SDL_GPUIndexedIndirectDrawCommand}
  alignment _ = #{alignment SDL_GPUIndexedIndirectDrawCommand}
  peek ptr = do
    num_indices <- #{peek SDL_GPUIndexedIndirectDrawCommand, num_indices} ptr :: IO CUInt
    num_instances <- #{peek SDL_GPUIndexedIndirectDrawCommand, num_instances} ptr :: IO CUInt
    first_index <- #{peek SDL_GPUIndexedIndirectDrawCommand, first_index} ptr :: IO CUInt
    vertex_offset <- #{peek SDL_GPUIndexedIndirectDrawCommand, vertex_offset} ptr :: IO CInt
    first_instance <- #{peek SDL_GPUIndexedIndirectDrawCommand, first_instance} ptr :: IO CUInt
    return SDLGPUIndexedIndirectDrawCommand
      { gpuIdxIndirectDrawNumIndices = fromIntegral num_indices
      , gpuIdxIndirectDrawNumInstances = fromIntegral num_instances
      , gpuIdxIndirectDrawFirstIndex = fromIntegral first_index
      , gpuIdxIndirectDrawVertexOffset = fromIntegral vertex_offset
      , gpuIdxIndirectDrawFirstInstance = fromIntegral first_instance
      }
  poke ptr SDLGPUIndexedIndirectDrawCommand{..} = do
    #{poke SDL_GPUIndexedIndirectDrawCommand, num_indices} ptr (fromIntegral gpuIdxIndirectDrawNumIndices :: CUInt)
    #{poke SDL_GPUIndexedIndirectDrawCommand, num_instances} ptr (fromIntegral gpuIdxIndirectDrawNumInstances :: CUInt)
    #{poke SDL_GPUIndexedIndirectDrawCommand, first_index} ptr (fromIntegral gpuIdxIndirectDrawFirstIndex :: CUInt)
    #{poke SDL_GPUIndexedIndirectDrawCommand, vertex_offset} ptr (fromIntegral gpuIdxIndirectDrawVertexOffset :: CInt)
    #{poke SDL_GPUIndexedIndirectDrawCommand, first_instance} ptr (fromIntegral gpuIdxIndirectDrawFirstInstance :: CUInt)

-- SDL_GPUIndirectDispatchCommand
data SDLGPUIndirectDispatchCommand = SDLGPUIndirectDispatchCommand
  { gpuIndirectDispatchGroupCountX :: Word32
  , gpuIndirectDispatchGroupCountY :: Word32
  , gpuIndirectDispatchGroupCountZ :: Word32
  } deriving (Show, Eq)

instance Storable SDLGPUIndirectDispatchCommand where
  sizeOf _ = #{size SDL_GPUIndirectDispatchCommand}
  alignment _ = #{alignment SDL_GPUIndirectDispatchCommand}
  peek ptr = do
    groupcount_x <- #{peek SDL_GPUIndirectDispatchCommand, groupcount_x} ptr :: IO CUInt
    groupcount_y <- #{peek SDL_GPUIndirectDispatchCommand, groupcount_y} ptr :: IO CUInt
    groupcount_z <- #{peek SDL_GPUIndirectDispatchCommand, groupcount_z} ptr :: IO CUInt
    return SDLGPUIndirectDispatchCommand
      { gpuIndirectDispatchGroupCountX = fromIntegral groupcount_x
      , gpuIndirectDispatchGroupCountY = fromIntegral groupcount_y
      , gpuIndirectDispatchGroupCountZ = fromIntegral groupcount_z
      }
  poke ptr SDLGPUIndirectDispatchCommand{..} = do
    #{poke SDL_GPUIndirectDispatchCommand, groupcount_x} ptr (fromIntegral gpuIndirectDispatchGroupCountX :: CUInt)
    #{poke SDL_GPUIndirectDispatchCommand, groupcount_y} ptr (fromIntegral gpuIndirectDispatchGroupCountY :: CUInt)
    #{poke SDL_GPUIndirectDispatchCommand, groupcount_z} ptr (fromIntegral gpuIndirectDispatchGroupCountZ :: CUInt)

-- SDL_GPUSamplerCreateInfo
data SDLGPUSamplerCreateInfo = SDLGPUSamplerCreateInfo
  { gpuSamplerInfoMinFilter        :: SDLGPUFilter
  , gpuSamplerInfoMagFilter        :: SDLGPUFilter
  , gpuSamplerInfoMipmapMode       :: SDLGPUSamplerMipmapMode
  , gpuSamplerInfoAddressModeU     :: SDLGPUSamplerAddressMode
  , gpuSamplerInfoAddressModeV     :: SDLGPUSamplerAddressMode
  , gpuSamplerInfoAddressModeW     :: SDLGPUSamplerAddressMode
  , gpuSamplerInfoMipLodBias       :: Float
  , gpuSamplerInfoMaxAnisotropy    :: Float
  , gpuSamplerInfoCompareOp        :: SDLGPUCompareOp
  , gpuSamplerInfoMinLod           :: Float
  , gpuSamplerInfoMaxLod           :: Float
  , gpuSamplerInfoEnableAnisotropy :: Bool
  , gpuSamplerInfoEnableCompare    :: Bool
  , gpuSamplerInfoProps            :: SDLPropertiesID -- Word32
  } deriving (Show, Eq)

instance Storable SDLGPUSamplerCreateInfo where
  sizeOf _ = #{size SDL_GPUSamplerCreateInfo}
  alignment _ = #{alignment SDL_GPUSamplerCreateInfo}
  peek ptr = do
    min_filter        <- SDLGPUFilter              <$> (#{peek SDL_GPUSamplerCreateInfo, min_filter} ptr        :: IO CInt)
    mag_filter        <- SDLGPUFilter              <$> (#{peek SDL_GPUSamplerCreateInfo, mag_filter} ptr        :: IO CInt)
    mipmap_mode       <- SDLGPUSamplerMipmapMode   <$> (#{peek SDL_GPUSamplerCreateInfo, mipmap_mode} ptr       :: IO CInt)
    address_mode_u    <- SDLGPUSamplerAddressMode  <$> (#{peek SDL_GPUSamplerCreateInfo, address_mode_u} ptr     :: IO CInt)
    address_mode_v    <- SDLGPUSamplerAddressMode  <$> (#{peek SDL_GPUSamplerCreateInfo, address_mode_v} ptr     :: IO CInt)
    address_mode_w    <- SDLGPUSamplerAddressMode  <$> (#{peek SDL_GPUSamplerCreateInfo, address_mode_w} ptr     :: IO CInt)
    mip_lod_bias      <- realToFrac                <$> (#{peek SDL_GPUSamplerCreateInfo, mip_lod_bias} ptr      :: IO CFloat)
    max_anisotropy    <- realToFrac                <$> (#{peek SDL_GPUSamplerCreateInfo, max_anisotropy} ptr    :: IO CFloat)
    compare_op        <- SDLGPUCompareOp           <$> (#{peek SDL_GPUSamplerCreateInfo, compare_op} ptr        :: IO CInt)
    min_lod           <- realToFrac                <$> (#{peek SDL_GPUSamplerCreateInfo, min_lod} ptr           :: IO CFloat)
    max_lod           <- realToFrac                <$> (#{peek SDL_GPUSamplerCreateInfo, max_lod} ptr           :: IO CFloat)
    enable_anisotropy <- toBool                    <$> (#{peek SDL_GPUSamplerCreateInfo, enable_anisotropy} ptr :: IO CBool)
    enable_compare    <- toBool                    <$> (#{peek SDL_GPUSamplerCreateInfo, enable_compare} ptr    :: IO CBool)
    props             <-                             (#{peek SDL_GPUSamplerCreateInfo, props} ptr             :: IO SDLPropertiesID) -- Corrected: Peek the newtype directly
    return SDLGPUSamplerCreateInfo {..} -- Removed record field assignments for brevity, assumes RecordWildCards
  poke ptr SDLGPUSamplerCreateInfo{..} = do
    #{poke SDL_GPUSamplerCreateInfo, min_filter} ptr        ( (\(SDLGPUFilter            i) -> i) gpuSamplerInfoMinFilter        :: CInt)
    #{poke SDL_GPUSamplerCreateInfo, mag_filter} ptr        ( (\(SDLGPUFilter            i) -> i) gpuSamplerInfoMagFilter        :: CInt)
    #{poke SDL_GPUSamplerCreateInfo, mipmap_mode} ptr       ( (\(SDLGPUSamplerMipmapMode i) -> i) gpuSamplerInfoMipmapMode       :: CInt)
    #{poke SDL_GPUSamplerCreateInfo, address_mode_u} ptr     ( (\(SDLGPUSamplerAddressMode i) -> i) gpuSamplerInfoAddressModeU     :: CInt)
    #{poke SDL_GPUSamplerCreateInfo, address_mode_v} ptr     ( (\(SDLGPUSamplerAddressMode i) -> i) gpuSamplerInfoAddressModeV     :: CInt)
    #{poke SDL_GPUSamplerCreateInfo, address_mode_w} ptr     ( (\(SDLGPUSamplerAddressMode i) -> i) gpuSamplerInfoAddressModeW     :: CInt)
    #{poke SDL_GPUSamplerCreateInfo, mip_lod_bias} ptr      (realToFrac gpuSamplerInfoMipLodBias       :: CFloat)
    #{poke SDL_GPUSamplerCreateInfo, max_anisotropy} ptr    (realToFrac gpuSamplerInfoMaxAnisotropy    :: CFloat)
    #{poke SDL_GPUSamplerCreateInfo, compare_op} ptr        ( (\(SDLGPUCompareOp         i) -> i) gpuSamplerInfoCompareOp        :: CInt)
    #{poke SDL_GPUSamplerCreateInfo, min_lod} ptr           (realToFrac gpuSamplerInfoMinLod           :: CFloat)
    #{poke SDL_GPUSamplerCreateInfo, max_lod} ptr           (realToFrac gpuSamplerInfoMaxLod           :: CFloat)
    #{poke SDL_GPUSamplerCreateInfo, enable_anisotropy} ptr (fromBool gpuSamplerInfoEnableAnisotropy :: CBool)
    #{poke SDL_GPUSamplerCreateInfo, enable_compare} ptr    (fromBool gpuSamplerInfoEnableCompare    :: CBool)
    #{poke SDL_GPUSamplerCreateInfo, padding1} ptr (0 :: CUChar)
    #{poke SDL_GPUSamplerCreateInfo, padding2} ptr (0 :: CUChar)
    #{poke SDL_GPUSamplerCreateInfo, props} ptr             gpuSamplerInfoProps -- Corrected: Poke the newtype directly

-- SDL_GPUVertexBufferDescription
data SDLGPUVertexBufferDescription = SDLGPUVertexBufferDescription
  { gpuVertexBufferDescSlot            :: Word32
  , gpuVertexBufferDescPitch           :: Word32
  , gpuVertexBufferDescInputRate       :: SDLGPUVertexInputRate
  , gpuVertexBufferDescInstanceStepRate :: Word32
  } deriving (Show, Eq)

instance Storable SDLGPUVertexBufferDescription where
  sizeOf _ = #{size SDL_GPUVertexBufferDescription}
  alignment _ = #{alignment SDL_GPUVertexBufferDescription}
  peek ptr = do
    slot <- #{peek SDL_GPUVertexBufferDescription, slot} ptr :: IO CUInt
    pitch <- #{peek SDL_GPUVertexBufferDescription, pitch} ptr :: IO CUInt
    input_rate <- #{peek SDL_GPUVertexBufferDescription, input_rate} ptr
    instance_step_rate <- #{peek SDL_GPUVertexBufferDescription, instance_step_rate} ptr :: IO CUInt
    return SDLGPUVertexBufferDescription
      { gpuVertexBufferDescSlot = fromIntegral slot
      , gpuVertexBufferDescPitch = fromIntegral pitch
      , gpuVertexBufferDescInputRate = input_rate
      , gpuVertexBufferDescInstanceStepRate = fromIntegral instance_step_rate
      }
  poke ptr SDLGPUVertexBufferDescription{..} = do
    #{poke SDL_GPUVertexBufferDescription, slot} ptr (fromIntegral gpuVertexBufferDescSlot :: CUInt)
    #{poke SDL_GPUVertexBufferDescription, pitch} ptr (fromIntegral gpuVertexBufferDescPitch :: CUInt)
    #{poke SDL_GPUVertexBufferDescription, input_rate} ptr gpuVertexBufferDescInputRate
    #{poke SDL_GPUVertexBufferDescription, instance_step_rate} ptr (fromIntegral gpuVertexBufferDescInstanceStepRate :: CUInt)

-- SDL_GPUVertexAttribute
data SDLGPUVertexAttribute = SDLGPUVertexAttribute
  { gpuVertexAttribLocation    :: Word32
  , gpuVertexAttribBufferSlot :: Word32
  , gpuVertexAttribFormat     :: SDLGPUVertexElementFormat
  , gpuVertexAttribOffset     :: Word32
  } deriving (Show, Eq)

instance Storable SDLGPUVertexAttribute where
  sizeOf _ = #{size SDL_GPUVertexAttribute}
  alignment _ = #{alignment SDL_GPUVertexAttribute}
  peek ptr = do
    location    <- #{peek SDL_GPUVertexAttribute, location} ptr :: IO CUInt
    buffer_slot <- #{peek SDL_GPUVertexAttribute, buffer_slot} ptr :: IO CUInt
    format      <- #{peek SDL_GPUVertexAttribute, format} ptr
    offset      <- #{peek SDL_GPUVertexAttribute, offset} ptr :: IO CUInt
    return SDLGPUVertexAttribute
      { gpuVertexAttribLocation = fromIntegral location
      , gpuVertexAttribBufferSlot = fromIntegral buffer_slot
      , gpuVertexAttribFormat = format
      , gpuVertexAttribOffset = fromIntegral offset
      }
  poke ptr SDLGPUVertexAttribute{..} = do
    #{poke SDL_GPUVertexAttribute, location} ptr (fromIntegral gpuVertexAttribLocation :: CUInt)
    #{poke SDL_GPUVertexAttribute, buffer_slot} ptr (fromIntegral gpuVertexAttribBufferSlot :: CUInt)
    #{poke SDL_GPUVertexAttribute, format} ptr gpuVertexAttribFormat
    #{poke SDL_GPUVertexAttribute, offset} ptr (fromIntegral gpuVertexAttribOffset :: CUInt)

-- SDL_GPUVertexInputState (Requires careful marshalling of arrays)
data SDLGPUVertexInputState = SDLGPUVertexInputState
  { gpuVertexInputStateVertexBuffers :: [SDLGPUVertexBufferDescription]
  , gpuVertexInputStateVertexAttribs :: [SDLGPUVertexAttribute]
  } deriving (Show, Eq)

-- Helper for withGPUVertexInputState
withGPUVertexInputState :: SDLGPUVertexInputState -> (Ptr SDLGPUVertexInputState -> IO a) -> IO a
withGPUVertexInputState SDLGPUVertexInputState{..} f =
  withArrayLen gpuVertexInputStateVertexBuffers $ \numBufs bufsPtr ->
  withArrayLen gpuVertexInputStateVertexAttribs $ \numAttrs attrsPtr ->
  alloca $ \(statePtr :: Ptr SDLGPUVertexInputState) -> do -- Added Type Annotation
    #{poke SDL_GPUVertexInputState, vertex_buffer_descriptions} statePtr bufsPtr
    #{poke SDL_GPUVertexInputState, num_vertex_buffers} statePtr (fromIntegral numBufs :: CUInt)
    #{poke SDL_GPUVertexInputState, vertex_attributes} statePtr attrsPtr
    #{poke SDL_GPUVertexInputState, num_vertex_attributes} statePtr (fromIntegral numAttrs :: CUInt)
    f statePtr -- Pass the typed pointer

-- Read-only version (less useful for CreateInfo)
peekGPUVertexInputState :: Ptr () -> IO SDLGPUVertexInputState
peekGPUVertexInputState ptr = do
  let statePtr = castPtr ptr :: Ptr (()) -- Adjust based on actual C struct def if needed
  bufsPtr <- #{peek SDL_GPUVertexInputState, vertex_buffer_descriptions} statePtr
  numBufs <- fromIntegral <$> (#{peek SDL_GPUVertexInputState, num_vertex_buffers} statePtr :: IO CUInt)
  attrsPtr <- #{peek SDL_GPUVertexInputState, vertex_attributes} statePtr
  numAttrs <- fromIntegral <$> (#{peek SDL_GPUVertexInputState, num_vertex_attributes} statePtr :: IO CUInt)
  bufs <- peekArray numBufs bufsPtr
  attrs <- peekArray numAttrs attrsPtr
  return SDLGPUVertexInputState { gpuVertexInputStateVertexBuffers = bufs, gpuVertexInputStateVertexAttribs = attrs }


-- SDL_GPUStencilOpState
data SDLGPUStencilOpState = SDLGPUStencilOpState
  { gpuStencilOpStateFailOp       :: SDLGPUStencilOp
  , gpuStencilOpStatePassOp       :: SDLGPUStencilOp
  , gpuStencilOpStateDepthFailOp  :: SDLGPUStencilOp
  , gpuStencilOpStateCompareOp    :: SDLGPUCompareOp
  } deriving (Show, Eq)

instance Storable SDLGPUStencilOpState where
  sizeOf _ = #{size SDL_GPUStencilOpState}
  alignment _ = #{alignment SDL_GPUStencilOpState}
  peek ptr = do
    fail_op <- SDLGPUStencilOp <$> (#{peek SDL_GPUStencilOpState, fail_op} ptr :: IO CInt) -- Added annotation & constructor
    pass_op <- SDLGPUStencilOp <$> (#{peek SDL_GPUStencilOpState, pass_op} ptr :: IO CInt) -- Added annotation & constructor
    depth_fail_op <- SDLGPUStencilOp <$> (#{peek SDL_GPUStencilOpState, depth_fail_op} ptr :: IO CInt) -- Added annotation & constructor
    compare_op <- SDLGPUCompareOp <$> (#{peek SDL_GPUStencilOpState, compare_op} ptr :: IO CInt) -- Added annotation & constructor
    return SDLGPUStencilOpState {..}
  poke ptr SDLGPUStencilOpState{..} = do
    #{poke SDL_GPUStencilOpState, fail_op} ptr ( (\(SDLGPUStencilOp i) -> i) gpuStencilOpStateFailOp :: CInt) -- Added deconstructor & annotation
    #{poke SDL_GPUStencilOpState, pass_op} ptr ( (\(SDLGPUStencilOp i) -> i) gpuStencilOpStatePassOp :: CInt) -- Added deconstructor & annotation
    #{poke SDL_GPUStencilOpState, depth_fail_op} ptr ( (\(SDLGPUStencilOp i) -> i) gpuStencilOpStateDepthFailOp :: CInt) -- Added deconstructor & annotation
    #{poke SDL_GPUStencilOpState, compare_op} ptr ( (\(SDLGPUCompareOp i) -> i) gpuStencilOpStateCompareOp :: CInt) -- Added deconstructor & annotation

-- SDL_GPUColorTargetBlendState
data SDLGPUColorTargetBlendState = SDLGPUColorTargetBlendState
  { gpuColorTargetBlendSrcColorFactor   :: SDLGPUBlendFactor
  , gpuColorTargetBlendDstColorFactor   :: SDLGPUBlendFactor
  , gpuColorTargetBlendColorOp          :: SDLGPUBlendOp
  , gpuColorTargetBlendSrcAlphaFactor   :: SDLGPUBlendFactor
  , gpuColorTargetBlendDstAlphaFactor   :: SDLGPUBlendFactor
  , gpuColorTargetBlendAlphaOp          :: SDLGPUBlendOp
  , gpuColorTargetBlendColorWriteMask   :: SDLGPUColorComponentFlags
  , gpuColorTargetBlendEnableBlend      :: Bool
  , gpuColorTargetBlendEnableColorWrite :: Bool
  } deriving (Show, Eq)

instance Storable SDLGPUColorTargetBlendState where
  sizeOf _ = #{size SDL_GPUColorTargetBlendState}
  alignment _ = #{alignment SDL_GPUColorTargetBlendState}
  peek ptr = do
    src_color_blendfactor <- SDLGPUBlendFactor         <$> (#{peek SDL_GPUColorTargetBlendState, src_color_blendfactor} ptr :: IO CInt)   -- Added annotation & constructor
    dst_color_blendfactor <- SDLGPUBlendFactor         <$> (#{peek SDL_GPUColorTargetBlendState, dst_color_blendfactor} ptr :: IO CInt)   -- Added annotation & constructor
    color_blend_op        <- SDLGPUBlendOp             <$> (#{peek SDL_GPUColorTargetBlendState, color_blend_op} ptr        :: IO CInt)   -- Added annotation & constructor
    src_alpha_blendfactor <- SDLGPUBlendFactor         <$> (#{peek SDL_GPUColorTargetBlendState, src_alpha_blendfactor} ptr :: IO CInt)   -- Added annotation & constructor
    dst_alpha_blendfactor <- SDLGPUBlendFactor         <$> (#{peek SDL_GPUColorTargetBlendState, dst_alpha_blendfactor} ptr :: IO CInt)   -- Added annotation & constructor
    alpha_blend_op        <- SDLGPUBlendOp             <$> (#{peek SDL_GPUColorTargetBlendState, alpha_blend_op} ptr        :: IO CInt)   -- Added annotation & constructor
    color_write_mask      <- SDLGPUColorComponentFlags <$> (#{peek SDL_GPUColorTargetBlendState, color_write_mask} ptr      :: IO CUChar) -- Added annotation & constructor
    enable_blend          <- toBool                    <$> (#{peek SDL_GPUColorTargetBlendState, enable_blend} ptr          :: IO CBool)  -- Added annotation
    enable_color_write_mask <- toBool                <$> (#{peek SDL_GPUColorTargetBlendState, enable_color_write_mask} ptr :: IO CBool)  -- Added annotation
    return SDLGPUColorTargetBlendState {..}
  poke ptr SDLGPUColorTargetBlendState{..} = do
    #{poke SDL_GPUColorTargetBlendState, src_color_blendfactor} ptr ( (\(SDLGPUBlendFactor i) -> i)         gpuColorTargetBlendSrcColorFactor   :: CInt)   -- Added deconstructor & annotation
    #{poke SDL_GPUColorTargetBlendState, dst_color_blendfactor} ptr ( (\(SDLGPUBlendFactor i) -> i)         gpuColorTargetBlendDstColorFactor   :: CInt)   -- Added deconstructor & annotation
    #{poke SDL_GPUColorTargetBlendState, color_blend_op} ptr        ( (\(SDLGPUBlendOp i) -> i)             gpuColorTargetBlendColorOp          :: CInt)   -- Added deconstructor & annotation
    #{poke SDL_GPUColorTargetBlendState, src_alpha_blendfactor} ptr ( (\(SDLGPUBlendFactor i) -> i)         gpuColorTargetBlendSrcAlphaFactor   :: CInt)   -- Added deconstructor & annotation
    #{poke SDL_GPUColorTargetBlendState, dst_alpha_blendfactor} ptr ( (\(SDLGPUBlendFactor i) -> i)         gpuColorTargetBlendDstAlphaFactor   :: CInt)   -- Added deconstructor & annotation
    #{poke SDL_GPUColorTargetBlendState, alpha_blend_op} ptr        ( (\(SDLGPUBlendOp i) -> i)             gpuColorTargetBlendAlphaOp          :: CInt)   -- Added deconstructor & annotation
    #{poke SDL_GPUColorTargetBlendState, color_write_mask} ptr      ( (\(SDLGPUColorComponentFlags i) -> i) gpuColorTargetBlendColorWriteMask   :: CUChar) -- Added deconstructor & annotation
    #{poke SDL_GPUColorTargetBlendState, enable_blend} ptr          (fromBool gpuColorTargetBlendEnableBlend      :: CBool)  -- Added annotation
    #{poke SDL_GPUColorTargetBlendState, enable_color_write_mask} ptr (fromBool gpuColorTargetBlendEnableColorWrite :: CBool)  -- Added annotation
    #{poke SDL_GPUColorTargetBlendState, padding1} ptr (0 :: CUChar) -- Explicit padding
    #{poke SDL_GPUColorTargetBlendState, padding2} ptr (0 :: CUChar) -- Explicit padding

-- SDL_GPUShaderCreateInfo
data SDLGPUShaderCreateInfo = SDLGPUShaderCreateInfo
  { gpuShaderInfoCode             :: Ptr Word8 -- Pointer to shader code bytes
  , gpuShaderInfoCodeSize         :: CSize
  , gpuShaderInfoEntryPoint       :: String -- Managed String
  , gpuShaderInfoFormat           :: SDLGPUShaderFormat
  , gpuShaderInfoStage            :: SDLGPUShaderStage
  , gpuShaderInfoNumSamplers        :: Word32
  , gpuShaderInfoNumStorageTextures :: Word32
  , gpuShaderInfoNumStorageBuffers  :: Word32
  , gpuShaderInfoNumUniformBuffers  :: Word32
  , gpuShaderInfoProps            :: SDLPropertiesID
  } deriving (Show, Eq)

-- No direct Storable instance. Marshal using 'with'.
withGPUShaderCreateInfo :: SDLGPUShaderCreateInfo -> (Ptr SDLGPUShaderCreateInfo -> IO a) -> IO a
withGPUShaderCreateInfo SDLGPUShaderCreateInfo{..} f =
  withCString gpuShaderInfoEntryPoint $ \cEntrypoint ->
  alloca $ \(ptr :: Ptr SDLGPUShaderCreateInfo) -> do -- Added Type Annotation
    let (CSize codeSize) = gpuShaderInfoCodeSize
    #{poke SDL_GPUShaderCreateInfo, code_size} ptr codeSize
    #{poke SDL_GPUShaderCreateInfo, code} ptr gpuShaderInfoCode
    #{poke SDL_GPUShaderCreateInfo, entrypoint} ptr cEntrypoint
    #{poke SDL_GPUShaderCreateInfo, format} ptr gpuShaderInfoFormat
    #{poke SDL_GPUShaderCreateInfo, stage} ptr gpuShaderInfoStage
    #{poke SDL_GPUShaderCreateInfo, num_samplers} ptr (fromIntegral gpuShaderInfoNumSamplers :: CUInt)
    #{poke SDL_GPUShaderCreateInfo, num_storage_textures} ptr (fromIntegral gpuShaderInfoNumStorageTextures :: CUInt)
    #{poke SDL_GPUShaderCreateInfo, num_storage_buffers} ptr (fromIntegral gpuShaderInfoNumStorageBuffers :: CUInt)
    #{poke SDL_GPUShaderCreateInfo, num_uniform_buffers} ptr (fromIntegral gpuShaderInfoNumUniformBuffers :: CUInt)
    #{poke SDL_GPUShaderCreateInfo, props} ptr gpuShaderInfoProps
    f ptr -- Pass typed pointer


-- SDL_GPUTextureCreateInfo
data SDLGPUTextureCreateInfo = SDLGPUTextureCreateInfo
  { gpuTextureInfoType             :: SDLGPUTextureType
  , gpuTextureInfoFormat           :: SDLGPUTextureFormat
  , gpuTextureInfoUsage            :: SDLGPUTextureUsageFlags
  , gpuTextureInfoWidth            :: Word32
  , gpuTextureInfoHeight           :: Word32
  , gpuTextureInfoLayerCountOrDepth :: Word32
  , gpuTextureInfoNumLevels        :: Word32
  , gpuTextureInfoSampleCount      :: SDLGPUSampleCount
  , gpuTextureInfoProps            :: SDLPropertiesID
  } deriving (Show, Eq)

instance Storable SDLGPUTextureCreateInfo where
  sizeOf _ = #{size SDL_GPUTextureCreateInfo}
  alignment _ = #{alignment SDL_GPUTextureCreateInfo}
  peek ptr = do
    type_ <- #{peek SDL_GPUTextureCreateInfo, type} ptr
    format <- #{peek SDL_GPUTextureCreateInfo, format} ptr
    usage <- #{peek SDL_GPUTextureCreateInfo, usage} ptr
    width <- fromIntegral <$> (#{peek SDL_GPUTextureCreateInfo, width} ptr :: IO CUInt)
    height <- fromIntegral <$> (#{peek SDL_GPUTextureCreateInfo, height} ptr :: IO CUInt)
    layer_count_or_depth <- fromIntegral <$> (#{peek SDL_GPUTextureCreateInfo, layer_count_or_depth} ptr :: IO CUInt)
    num_levels <- fromIntegral <$> (#{peek SDL_GPUTextureCreateInfo, num_levels} ptr :: IO CUInt)
    sample_count <- #{peek SDL_GPUTextureCreateInfo, sample_count} ptr
    props <- #{peek SDL_GPUTextureCreateInfo, props} ptr
    return SDLGPUTextureCreateInfo
      { gpuTextureInfoType = type_
      , gpuTextureInfoFormat = format
      , gpuTextureInfoUsage = usage
      , gpuTextureInfoWidth = width
      , gpuTextureInfoHeight = height
      , gpuTextureInfoLayerCountOrDepth = layer_count_or_depth
      , gpuTextureInfoNumLevels = num_levels
      , gpuTextureInfoSampleCount = sample_count
      , gpuTextureInfoProps = props
      }
  poke ptr SDLGPUTextureCreateInfo{..} = do
    #{poke SDL_GPUTextureCreateInfo, type} ptr gpuTextureInfoType
    #{poke SDL_GPUTextureCreateInfo, format} ptr gpuTextureInfoFormat
    #{poke SDL_GPUTextureCreateInfo, usage} ptr gpuTextureInfoUsage
    #{poke SDL_GPUTextureCreateInfo, width} ptr (fromIntegral gpuTextureInfoWidth :: CUInt)
    #{poke SDL_GPUTextureCreateInfo, height} ptr (fromIntegral gpuTextureInfoHeight :: CUInt)
    #{poke SDL_GPUTextureCreateInfo, layer_count_or_depth} ptr (fromIntegral gpuTextureInfoLayerCountOrDepth :: CUInt)
    #{poke SDL_GPUTextureCreateInfo, num_levels} ptr (fromIntegral gpuTextureInfoNumLevels :: CUInt)
    #{poke SDL_GPUTextureCreateInfo, sample_count} ptr gpuTextureInfoSampleCount
    #{poke SDL_GPUTextureCreateInfo, props} ptr gpuTextureInfoProps

-- SDL_GPUBufferCreateInfo
data SDLGPUBufferCreateInfo = SDLGPUBufferCreateInfo
  { gpuBufferInfoUsage :: SDLGPUBufferUsageFlags
  , gpuBufferInfoSize  :: Word32
  , gpuBufferInfoProps :: SDLPropertiesID
  } deriving (Show, Eq)

instance Storable SDLGPUBufferCreateInfo where
  sizeOf _ = #{size SDL_GPUBufferCreateInfo}
  alignment _ = #{alignment SDL_GPUBufferCreateInfo}
  peek ptr = do
    usage <- SDLGPUBufferUsageFlags <$> (#{peek SDL_GPUBufferCreateInfo, usage} ptr :: IO CUInt)
    size <- fromIntegral <$> (#{peek SDL_GPUBufferCreateInfo, size} ptr :: IO CUInt)
    props <- #{peek SDL_GPUBufferCreateInfo, props} ptr :: IO SDLPropertiesID -- Corrected: Peek the newtype directly
    return SDLGPUBufferCreateInfo {..}
  poke ptr SDLGPUBufferCreateInfo{..} = do
    #{poke SDL_GPUBufferCreateInfo, usage} ptr ( (\(SDLGPUBufferUsageFlags i) -> i) gpuBufferInfoUsage :: CUInt)
    #{poke SDL_GPUBufferCreateInfo, size} ptr (fromIntegral gpuBufferInfoSize :: CUInt)
    #{poke SDL_GPUBufferCreateInfo, props} ptr gpuBufferInfoProps -- Corrected: Poke the newtype directly

-- SDL_GPUTransferBufferCreateInfo
data SDLGPUTransferBufferCreateInfo = SDLGPUTransferBufferCreateInfo
  { gpuTransferBufferInfoUsage :: SDLGPUTransferBufferUsage
  , gpuTransferBufferInfoSize  :: Word32
  , gpuTransferBufferInfoProps :: SDLPropertiesID
  } deriving (Show, Eq)

instance Storable SDLGPUTransferBufferCreateInfo where
  sizeOf _ = #{size SDL_GPUTransferBufferCreateInfo}
  alignment _ = #{alignment SDL_GPUTransferBufferCreateInfo}
  peek ptr = do
    usage <- SDLGPUTransferBufferUsage <$> (#{peek SDL_GPUTransferBufferCreateInfo, usage} ptr :: IO CInt)
    size <- fromIntegral <$> (#{peek SDL_GPUTransferBufferCreateInfo, size} ptr :: IO CUInt)
    props <- #{peek SDL_GPUTransferBufferCreateInfo, props} ptr :: IO SDLPropertiesID -- Corrected: Peek the newtype directly
    return SDLGPUTransferBufferCreateInfo {..}
  poke ptr SDLGPUTransferBufferCreateInfo{..} = do
    #{poke SDL_GPUTransferBufferCreateInfo, usage} ptr ( (\(SDLGPUTransferBufferUsage i) -> i) gpuTransferBufferInfoUsage :: CInt)
    #{poke SDL_GPUTransferBufferCreateInfo, size} ptr (fromIntegral gpuTransferBufferInfoSize :: CUInt)
    #{poke SDL_GPUTransferBufferCreateInfo, props} ptr gpuTransferBufferInfoProps -- Corrected: Poke the newtype directly

-- Pipeline State Structures

-- SDL_GPURasterizerState
data SDLGPURasterizerState = SDLGPURasterizerState -- Corrected: Removed extra space
  { gpuRasterizerFillMode             :: SDLGPUFillMode
  , gpuRasterizerCullMode             :: SDLGPUCullMode
  , gpuRasterizerFrontFace            :: SDLGPUFrontFace
  , gpuRasterizerDepthBiasConstantFactor :: Float
  , gpuRasterizerDepthBiasClamp       :: Float
  , gpuRasterizerDepthBiasSlopeFactor :: Float
  , gpuRasterizerEnableDepthBias      :: Bool
  , gpuRasterizerEnableDepthClip      :: Bool
  } deriving (Show, Eq)

instance Storable SDLGPURasterizerState where
  sizeOf _ = #{size SDL_GPURasterizerState}
  alignment _ = #{alignment SDL_GPURasterizerState}
  peek ptr = do
    fill_mode <- SDLGPUFillMode   <$> (#{peek SDL_GPURasterizerState, fill_mode} ptr :: IO CInt)
    cull_mode <- SDLGPUCullMode   <$> (#{peek SDL_GPURasterizerState, cull_mode} ptr :: IO CInt)
    front_face <- SDLGPUFrontFace <$> (#{peek SDL_GPURasterizerState, front_face} ptr :: IO CInt)
    depth_bias_constant_factor <- realToFrac <$> (#{peek SDL_GPURasterizerState, depth_bias_constant_factor} ptr :: IO CFloat)
    depth_bias_clamp <- realToFrac           <$> (#{peek SDL_GPURasterizerState, depth_bias_clamp} ptr :: IO CFloat)
    depth_bias_slope_factor <- realToFrac    <$> (#{peek SDL_GPURasterizerState, depth_bias_slope_factor} ptr :: IO CFloat)
    enable_depth_bias <- toBool              <$> (#{peek SDL_GPURasterizerState, enable_depth_bias} ptr :: IO CBool) -- Added :: IO CBool
    enable_depth_clip <- toBool              <$> (#{peek SDL_GPURasterizerState, enable_depth_clip} ptr :: IO CBool) -- Added :: IO CBool
    return SDLGPURasterizerState {..}
  poke ptr SDLGPURasterizerState{..} = do
    #{poke SDL_GPURasterizerState, fill_mode} ptr ( (\(SDLGPUFillMode i) -> i)   gpuRasterizerFillMode :: CInt)
    #{poke SDL_GPURasterizerState, cull_mode} ptr ( (\(SDLGPUCullMode i) -> i)   gpuRasterizerCullMode :: CInt)
    #{poke SDL_GPURasterizerState, front_face} ptr ( (\(SDLGPUFrontFace i) -> i) gpuRasterizerFrontFace :: CInt)
    #{poke SDL_GPURasterizerState, depth_bias_constant_factor} ptr (realToFrac gpuRasterizerDepthBiasConstantFactor :: CFloat)
    #{poke SDL_GPURasterizerState, depth_bias_clamp} ptr (realToFrac gpuRasterizerDepthBiasClamp :: CFloat)
    #{poke SDL_GPURasterizerState, depth_bias_slope_factor} ptr (realToFrac gpuRasterizerDepthBiasSlopeFactor :: CFloat)
    #{poke SDL_GPURasterizerState, enable_depth_bias} ptr (fromBool gpuRasterizerEnableDepthBias :: CBool) -- Added :: CBool
    #{poke SDL_GPURasterizerState, enable_depth_clip} ptr (fromBool gpuRasterizerEnableDepthClip :: CBool) -- Added :: CBool
    #{poke SDL_GPURasterizerState, padding1} ptr (0 :: CUChar)
    #{poke SDL_GPURasterizerState, padding2} ptr (0 :: CUChar)

-- SDL_GPUMultisampleState
data SDLGPUMultisampleState = SDLGPUMultisampleState -- Corrected: Removed extra space
  { gpuMultisampleSampleCount         :: SDLGPUSampleCount
  , gpuMultisampleSampleMask          :: Word32 -- Reserved, use 0
  , gpuMultisampleEnableMask          :: Bool   -- Reserved, use False
  , gpuMultisampleEnableAlphaToCoverage :: Bool
  } deriving (Show, Eq)

instance Storable SDLGPUMultisampleState where
  sizeOf _ = #{size SDL_GPUMultisampleState}
  alignment _ = #{alignment SDL_GPUMultisampleState}
  peek ptr = do
    sample_count <- SDLGPUSampleCount <$> (#{peek SDL_GPUMultisampleState, sample_count} ptr :: IO CInt) -- Added annotation & constructor
    sample_mask <- fromIntegral       <$> (#{peek SDL_GPUMultisampleState, sample_mask} ptr :: IO CUInt)
    enable_mask <- toBool             <$> (#{peek SDL_GPUMultisampleState, enable_mask} ptr :: IO CBool) -- Added :: IO CBool
    enable_alpha_to_coverage <- toBool <$> (#{peek SDL_GPUMultisampleState, enable_alpha_to_coverage} ptr :: IO CBool) -- Added :: IO CBool
    return SDLGPUMultisampleState {..}
  poke ptr SDLGPUMultisampleState{..} = do
    #{poke SDL_GPUMultisampleState, sample_count} ptr ( (\(SDLGPUSampleCount i) -> i) gpuMultisampleSampleCount :: CInt) -- Added deconstructor & annotation
    #{poke SDL_GPUMultisampleState, sample_mask} ptr (fromIntegral gpuMultisampleSampleMask :: CUInt)
    #{poke SDL_GPUMultisampleState, enable_mask} ptr (fromBool gpuMultisampleEnableMask :: CBool) -- Added :: CBool
    #{poke SDL_GPUMultisampleState, enable_alpha_to_coverage} ptr (fromBool gpuMultisampleEnableAlphaToCoverage :: CBool) -- Added :: CBool
    #{poke SDL_GPUMultisampleState, padding2} ptr (0 :: CUChar)
    #{poke SDL_GPUMultisampleState, padding3} ptr (0 :: CUChar)

-- SDL_GPUDepthStencilState
data SDLGPUDepthStencilState = SDLGPUDepthStencilState -- Corrected: Removed extra space
  { gpuDepthStencilCompareOp       :: SDLGPUCompareOp
  , gpuDepthStencilBackStencilState  :: SDLGPUStencilOpState
  , gpuDepthStencilFrontStencilState :: SDLGPUStencilOpState
  , gpuDepthStencilCompareMask     :: Word8
  , gpuDepthStencilWriteMask       :: Word8
  , gpuDepthStencilEnableDepthTest   :: Bool
  , gpuDepthStencilEnableDepthWrite  :: Bool
  , gpuDepthStencilEnableStencilTest :: Bool
  } deriving (Show, Eq)

instance Storable SDLGPUDepthStencilState where
  sizeOf _ = #{size SDL_GPUDepthStencilState}
  alignment _ = #{alignment SDL_GPUDepthStencilState}
  peek ptr = do
    compare_op <- SDLGPUCompareOp <$> (#{peek SDL_GPUDepthStencilState, compare_op} ptr :: IO CInt) -- Added annotation & constructor
    back_stencil_state <- peek (#{ptr SDL_GPUDepthStencilState, back_stencil_state} ptr :: Ptr SDLGPUStencilOpState) -- Added pointer type
    front_stencil_state <- peek (#{ptr SDL_GPUDepthStencilState, front_stencil_state} ptr :: Ptr SDLGPUStencilOpState) -- Added pointer type
    compare_mask <- #{peek SDL_GPUDepthStencilState, compare_mask} ptr
    write_mask <- #{peek SDL_GPUDepthStencilState, write_mask} ptr
    enable_depth_test <- toBool   <$> (#{peek SDL_GPUDepthStencilState, enable_depth_test} ptr :: IO CBool) -- Added :: IO CBool
    enable_depth_write <- toBool  <$> (#{peek SDL_GPUDepthStencilState, enable_depth_write} ptr :: IO CBool) -- Added :: IO CBool
    enable_stencil_test <- toBool <$> (#{peek SDL_GPUDepthStencilState, enable_stencil_test} ptr :: IO CBool) -- Added :: IO CBool
    return SDLGPUDepthStencilState {..}
  poke ptr SDLGPUDepthStencilState{..} = do
    #{poke SDL_GPUDepthStencilState, compare_op} ptr ( (\(SDLGPUCompareOp i) -> i) gpuDepthStencilCompareOp :: CInt) -- Added deconstructor & annotation
    poke (#{ptr SDL_GPUDepthStencilState, back_stencil_state} ptr) gpuDepthStencilBackStencilState
    poke (#{ptr SDL_GPUDepthStencilState, front_stencil_state} ptr) gpuDepthStencilFrontStencilState
    #{poke SDL_GPUDepthStencilState, compare_mask} ptr gpuDepthStencilCompareMask
    #{poke SDL_GPUDepthStencilState, write_mask} ptr gpuDepthStencilWriteMask
    #{poke SDL_GPUDepthStencilState, enable_depth_test} ptr (fromBool gpuDepthStencilEnableDepthTest :: CBool) -- Added :: CBool
    #{poke SDL_GPUDepthStencilState, enable_depth_write} ptr (fromBool gpuDepthStencilEnableDepthWrite :: CBool) -- Added :: CBool
    #{poke SDL_GPUDepthStencilState, enable_stencil_test} ptr (fromBool gpuDepthStencilEnableStencilTest :: CBool) -- Added :: CBool
    #{poke SDL_GPUDepthStencilState, padding1} ptr (0 :: CUChar)
    #{poke SDL_GPUDepthStencilState, padding2} ptr (0 :: CUChar)
    #{poke SDL_GPUDepthStencilState, padding3} ptr (0 :: CUChar)

-- SDL_GPUColorTargetDescription
data SDLGPUColorTargetDescription = SDLGPUColorTargetDescription
  { gpuColorTargetDescFormat     :: SDLGPUTextureFormat
  , gpuColorTargetDescBlendState :: SDLGPUColorTargetBlendState
  } deriving (Show, Eq)

instance Storable SDLGPUColorTargetDescription where
  sizeOf _ = #{size SDL_GPUColorTargetDescription}
  alignment _ = #{alignment SDL_GPUColorTargetDescription}
  peek ptr = do
    format <- SDLGPUTextureFormat <$> (#{peek SDL_GPUColorTargetDescription, format} ptr :: IO CInt) -- Added annotation & constructor
    blend_state <- peek (#{ptr SDL_GPUColorTargetDescription, blend_state} ptr :: Ptr SDLGPUColorTargetBlendState) -- Added type annotation
    return SDLGPUColorTargetDescription {..}
  poke ptr SDLGPUColorTargetDescription{..} = do
    #{poke SDL_GPUColorTargetDescription, format} ptr ( (\(SDLGPUTextureFormat i) -> i) gpuColorTargetDescFormat :: CInt) -- Added deconstructor & annotation
    poke (#{ptr SDL_GPUColorTargetDescription, blend_state} ptr) gpuColorTargetDescBlendState

-- SDL_GPUGraphicsPipelineTargetInfo (Requires marshalling arrays)
data SDLGPUGraphicsPipelineTargetInfo = SDLGPUGraphicsPipelineTargetInfo
  { gpuGraphicsTargetInfoColorTargets       :: [SDLGPUColorTargetDescription]
  , gpuGraphicsTargetInfoDepthStencilFormat :: SDLGPUTextureFormat
  , gpuGraphicsTargetInfoHasDepthStencil    :: Bool
  } deriving (Show, Eq)

-- No direct Storable instance.
withGPUGraphicsPipelineTargetInfo :: SDLGPUGraphicsPipelineTargetInfo -> (Ptr SDLGPUGraphicsPipelineTargetInfo -> IO a) -> IO a
withGPUGraphicsPipelineTargetInfo SDLGPUGraphicsPipelineTargetInfo{..} f =
  withArrayLen gpuGraphicsTargetInfoColorTargets $ \numTargets targetsPtr ->
  alloca $ \(ptr :: Ptr SDLGPUGraphicsPipelineTargetInfo) -> do -- Added Type Annotation
    #{poke SDL_GPUGraphicsPipelineTargetInfo, color_target_descriptions} ptr targetsPtr
    #{poke SDL_GPUGraphicsPipelineTargetInfo, num_color_targets} ptr (fromIntegral numTargets :: CUInt)
    #{poke SDL_GPUGraphicsPipelineTargetInfo, depth_stencil_format} ptr gpuGraphicsTargetInfoDepthStencilFormat
    #{poke SDL_GPUGraphicsPipelineTargetInfo, has_depth_stencil_target} ptr (fromBool gpuGraphicsTargetInfoHasDepthStencil :: CBool) -- Added :: CBool
    #{poke SDL_GPUGraphicsPipelineTargetInfo, padding1} ptr (0 :: CUChar)
    #{poke SDL_GPUGraphicsPipelineTargetInfo, padding2} ptr (0 :: CUChar)
    #{poke SDL_GPUGraphicsPipelineTargetInfo, padding3} ptr (0 :: CUChar)
    f ptr -- Pass the typed pointer

-- SDL_GPUGraphicsPipelineCreateInfo
data SDLGPUGraphicsPipelineCreateInfo = SDLGPUGraphicsPipelineCreateInfo
  { gpuGraphicsPipelineInfoVertexShader      :: SDLGPUShader
  , gpuGraphicsPipelineInfoFragmentShader    :: SDLGPUShader
  , gpuGraphicsPipelineInfoVertexInputState  :: SDLGPUVertexInputState
  , gpuGraphicsPipelineInfoPrimitiveType     :: SDLGPUPrimitiveType
  , gpuGraphicsPipelineInfoRasterizerState   :: SDLGPURasterizerState -- Corrected type name
  , gpuGraphicsPipelineInfoMultisampleState  :: SDLGPUMultisampleState -- Corrected type name
  , gpuGraphicsPipelineInfoDepthStencilState :: SDLGPUDepthStencilState -- Corrected type name
  , gpuGraphicsPipelineInfoTargetInfo        :: SDLGPUGraphicsPipelineTargetInfo
  , gpuGraphicsPipelineInfoProps             :: SDLPropertiesID
  } deriving (Show, Eq)

-- No direct Storable instance. Marshal using helpers.
-- Use the helper in withGPUGraphicsPipelineCreateInfo
withGPUGraphicsPipelineCreateInfo :: SDLGPUGraphicsPipelineCreateInfo -> (Ptr () -> IO a) -> IO a
withGPUGraphicsPipelineCreateInfo SDLGPUGraphicsPipelineCreateInfo{..} f =
  withGPUVertexInputState gpuGraphicsPipelineInfoVertexInputState $ \vertexInputPtr -> -- Receives typed Ptr
  withGPUGraphicsPipelineTargetInfo gpuGraphicsPipelineInfoTargetInfo $ \targetInfoPtr -> -- Receives typed Ptr
  alloca $ \(ptr :: Ptr SDLGPUGraphicsPipelineCreateInfo) -> do -- Added Type Annotation
    let (SDLGPUShader vsPtr) = gpuGraphicsPipelineInfoVertexShader
    let (SDLGPUShader fsPtr) = gpuGraphicsPipelineInfoFragmentShader
    #{poke SDL_GPUGraphicsPipelineCreateInfo, vertex_shader} ptr vsPtr
    #{poke SDL_GPUGraphicsPipelineCreateInfo, fragment_shader} ptr fsPtr

    let vertexInputStateSize = #{size SDL_GPUVertexInputState}
    let targetInfoSize       = #{size SDL_GPUGraphicsPipelineTargetInfo}

    -- Copy using typed pointers
    copyBytes (#{ptr SDL_GPUGraphicsPipelineCreateInfo, vertex_input_state} ptr) vertexInputPtr vertexInputStateSize
    #{poke SDL_GPUGraphicsPipelineCreateInfo, primitive_type} ptr gpuGraphicsPipelineInfoPrimitiveType -- Enums have Storable via newtype
    poke (#{ptr SDL_GPUGraphicsPipelineCreateInfo, rasterizer_state} ptr) gpuGraphicsPipelineInfoRasterizerState
    poke (#{ptr SDL_GPUGraphicsPipelineCreateInfo, multisample_state} ptr) gpuGraphicsPipelineInfoMultisampleState
    poke (#{ptr SDL_GPUGraphicsPipelineCreateInfo, depth_stencil_state} ptr) gpuGraphicsPipelineInfoDepthStencilState
    copyBytes (#{ptr SDL_GPUGraphicsPipelineCreateInfo, target_info} ptr) targetInfoPtr targetInfoSize
    #{poke SDL_GPUGraphicsPipelineCreateInfo, props} ptr gpuGraphicsPipelineInfoProps
    f (castPtr ptr)
  
-- SDL_GPUComputePipelineCreateInfo
data SDLGPUComputePipelineCreateInfo = SDLGPUComputePipelineCreateInfo
  { gpuComputePipelineInfoCode                      :: Ptr Word8
  , gpuComputePipelineInfoCodeSize                  :: CSize
  , gpuComputePipelineInfoEntryPoint                :: String
  , gpuComputePipelineInfoFormat                    :: SDLGPUShaderFormat
  , gpuComputePipelineInfoNumSamplers               :: Word32
  , gpuComputePipelineInfoNumReadonlyStorageTextures:: Word32
  , gpuComputePipelineInfoNumReadonlyStorageBuffers :: Word32
  , gpuComputePipelineInfoNumReadWriteStorageTextures :: Word32
  , gpuComputePipelineInfoNumReadWriteStorageBuffers  :: Word32
  , gpuComputePipelineInfoNumUniformBuffers         :: Word32
  , gpuComputePipelineInfoThreadCountX              :: Word32
  , gpuComputePipelineInfoThreadCountY              :: Word32
  , gpuComputePipelineInfoThreadCountZ              :: Word32
  , gpuComputePipelineInfoProps                     :: SDLPropertiesID
  } deriving (Show, Eq)

-- No direct Storable instance. Marshal using 'with'.
withGPUComputePipelineCreateInfo :: SDLGPUComputePipelineCreateInfo -> (Ptr SDLGPUComputePipelineCreateInfo -> IO a) -> IO a
withGPUComputePipelineCreateInfo SDLGPUComputePipelineCreateInfo{..} f =
  withCString gpuComputePipelineInfoEntryPoint $ \cEntrypoint ->
  alloca $ \(ptr :: Ptr SDLGPUComputePipelineCreateInfo) -> do -- Added Type Annotation
    let (CSize codeSize) = gpuComputePipelineInfoCodeSize
    #{poke SDL_GPUComputePipelineCreateInfo, code_size} ptr codeSize
    #{poke SDL_GPUComputePipelineCreateInfo, code} ptr gpuComputePipelineInfoCode
    #{poke SDL_GPUComputePipelineCreateInfo, entrypoint} ptr cEntrypoint
    #{poke SDL_GPUComputePipelineCreateInfo, format} ptr gpuComputePipelineInfoFormat
    #{poke SDL_GPUComputePipelineCreateInfo, num_samplers} ptr (fromIntegral gpuComputePipelineInfoNumSamplers :: CUInt)
    #{poke SDL_GPUComputePipelineCreateInfo, num_readonly_storage_textures} ptr (fromIntegral gpuComputePipelineInfoNumReadonlyStorageTextures :: CUInt)
    #{poke SDL_GPUComputePipelineCreateInfo, num_readonly_storage_buffers} ptr (fromIntegral gpuComputePipelineInfoNumReadonlyStorageBuffers :: CUInt)
    #{poke SDL_GPUComputePipelineCreateInfo, num_readwrite_storage_textures} ptr (fromIntegral gpuComputePipelineInfoNumReadWriteStorageTextures :: CUInt)
    #{poke SDL_GPUComputePipelineCreateInfo, num_readwrite_storage_buffers} ptr (fromIntegral gpuComputePipelineInfoNumReadWriteStorageBuffers :: CUInt)
    #{poke SDL_GPUComputePipelineCreateInfo, num_uniform_buffers} ptr (fromIntegral gpuComputePipelineInfoNumUniformBuffers :: CUInt)
    #{poke SDL_GPUComputePipelineCreateInfo, threadcount_x} ptr (fromIntegral gpuComputePipelineInfoThreadCountX :: CUInt)
    #{poke SDL_GPUComputePipelineCreateInfo, threadcount_y} ptr (fromIntegral gpuComputePipelineInfoThreadCountY :: CUInt)
    #{poke SDL_GPUComputePipelineCreateInfo, threadcount_z} ptr (fromIntegral gpuComputePipelineInfoThreadCountZ :: CUInt)
    #{poke SDL_GPUComputePipelineCreateInfo, props} ptr gpuComputePipelineInfoProps
    f ptr -- Pass typed pointer


-- SDL_GPUColorTargetInfo
data SDLGPUColorTargetInfo = SDLGPUColorTargetInfo
  { gpuColorTargetInfoTexture           :: SDLGPUTexture
  , gpuColorTargetInfoMipLevel          :: Word32
  , gpuColorTargetInfoLayerOrDepthPlane :: Word32
  , gpuColorTargetInfoClearColor        :: SDLFColor
  , gpuColorTargetInfoLoadOp            :: SDLGPULoadOp
  , gpuColorTargetInfoStoreOp           :: SDLGPUStoreOp
  , gpuColorTargetInfoResolveTexture    :: Maybe SDLGPUTexture -- Optional resolve target
  , gpuColorTargetInfoResolveMipLevel   :: Word32
  , gpuColorTargetInfoResolveLayer      :: Word32
  , gpuColorTargetInfoCycle             :: Bool
  , gpuColorTargetInfoCycleResolve      :: Bool
  } deriving (Show, Eq)

instance Storable SDLGPUColorTargetInfo where
  sizeOf _ = #{size SDL_GPUColorTargetInfo}
  alignment _ = #{alignment SDL_GPUColorTargetInfo}
  peek ptr = do
    texture_ptr <- #{peek SDL_GPUColorTargetInfo, texture} ptr
    mip_level <- fromIntegral <$> (#{peek SDL_GPUColorTargetInfo, mip_level} ptr :: IO CUInt)
    layer_or_depth_plane <- fromIntegral <$> (#{peek SDL_GPUColorTargetInfo, layer_or_depth_plane} ptr :: IO CUInt)
    clear_color <- peek (#{ptr SDL_GPUColorTargetInfo, clear_color} ptr :: Ptr SDLFColor) -- Added pointer type
    load_op <- SDLGPULoadOp  <$> (#{peek SDL_GPUColorTargetInfo, load_op} ptr :: IO CInt) -- Added annotation & constructor
    store_op <- SDLGPUStoreOp <$> (#{peek SDL_GPUColorTargetInfo, store_op} ptr :: IO CInt) -- Added annotation & constructor
    resolve_texture_ptr <- #{peek SDL_GPUColorTargetInfo, resolve_texture} ptr
    resolve_mip_level <- fromIntegral <$> (#{peek SDL_GPUColorTargetInfo, resolve_mip_level} ptr :: IO CUInt)
    resolve_layer <- fromIntegral <$> (#{peek SDL_GPUColorTargetInfo, resolve_layer} ptr :: IO CUInt)
    cycle_ <- toBool <$> (#{peek SDL_GPUColorTargetInfo, cycle} ptr :: IO CBool) -- Added :: IO CBool
    cycle_resolve_texture <- toBool <$> (#{peek SDL_GPUColorTargetInfo, cycle_resolve_texture} ptr :: IO CBool) -- Added :: IO CBool
    let resolveTexture = if resolve_texture_ptr == nullPtr then Nothing else Just (SDLGPUTexture resolve_texture_ptr)
    return SDLGPUColorTargetInfo {..} -- Using RecordWildCards
  poke ptr SDLGPUColorTargetInfo{..} = do
    let (SDLGPUTexture texture_ptr) = gpuColorTargetInfoTexture
    #{poke SDL_GPUColorTargetInfo, texture} ptr texture_ptr
    #{poke SDL_GPUColorTargetInfo, mip_level} ptr (fromIntegral gpuColorTargetInfoMipLevel :: CUInt)
    #{poke SDL_GPUColorTargetInfo, layer_or_depth_plane} ptr (fromIntegral gpuColorTargetInfoLayerOrDepthPlane :: CUInt)
    poke (#{ptr SDL_GPUColorTargetInfo, clear_color} ptr) gpuColorTargetInfoClearColor
    #{poke SDL_GPUColorTargetInfo, load_op} ptr ( (\(SDLGPULoadOp i) -> i) gpuColorTargetInfoLoadOp :: CInt) -- Added deconstructor & annotation
    #{poke SDL_GPUColorTargetInfo, store_op} ptr ( (\(SDLGPUStoreOp i) -> i) gpuColorTargetInfoStoreOp :: CInt) -- Added deconstructor & annotation
    let resolveTexturePtr = case gpuColorTargetInfoResolveTexture of
                              Just (SDLGPUTexture p) -> p
                              Nothing -> nullPtr
    #{poke SDL_GPUColorTargetInfo, resolve_texture} ptr resolveTexturePtr
    #{poke SDL_GPUColorTargetInfo, resolve_mip_level} ptr (fromIntegral gpuColorTargetInfoResolveMipLevel :: CUInt)
    #{poke SDL_GPUColorTargetInfo, resolve_layer} ptr (fromIntegral gpuColorTargetInfoResolveLayer :: CUInt)
    #{poke SDL_GPUColorTargetInfo, cycle} ptr (fromBool gpuColorTargetInfoCycle :: CBool) -- Added :: CBool
    #{poke SDL_GPUColorTargetInfo, cycle_resolve_texture} ptr (fromBool gpuColorTargetInfoCycleResolve :: CBool) -- Added :: CBool
    #{poke SDL_GPUColorTargetInfo, padding1} ptr (0 :: CUChar)
    #{poke SDL_GPUColorTargetInfo, padding2} ptr (0 :: CUChar)

-- SDL_GPUDepthStencilTargetInfo
data SDLGPUDepthStencilTargetInfo = SDLGPUDepthStencilTargetInfo
  { gpuDepthStencilTargetTexture       :: SDLGPUTexture
  , gpuDepthStencilTargetClearDepth    :: Float
  , gpuDepthStencilTargetLoadOp        :: SDLGPULoadOp
  , gpuDepthStencilTargetStoreOp       :: SDLGPUStoreOp
  , gpuDepthStencilTargetStencilLoadOp :: SDLGPULoadOp
  , gpuDepthStencilTargetStencilStoreOp:: SDLGPUStoreOp
  , gpuDepthStencilTargetCycle         :: Bool
  , gpuDepthStencilTargetClearStencil  :: Word8
  } deriving (Show, Eq)

instance Storable SDLGPUDepthStencilTargetInfo where
  sizeOf _ = #{size SDL_GPUDepthStencilTargetInfo}
  alignment _ = #{alignment SDL_GPUDepthStencilTargetInfo}
  peek ptr = do
    texture_ptr <- #{peek SDL_GPUDepthStencilTargetInfo, texture} ptr
    clear_depth <- realToFrac <$> (#{peek SDL_GPUDepthStencilTargetInfo, clear_depth} ptr :: IO CFloat)
    load_op <- SDLGPULoadOp <$> (#{peek SDL_GPUDepthStencilTargetInfo, load_op} ptr :: IO CInt) -- Added annotation & constructor
    store_op <- SDLGPUStoreOp <$> (#{peek SDL_GPUDepthStencilTargetInfo, store_op} ptr :: IO CInt) -- Added annotation & constructor
    stencil_load_op <- SDLGPULoadOp <$> (#{peek SDL_GPUDepthStencilTargetInfo, stencil_load_op} ptr :: IO CInt) -- Added annotation & constructor
    stencil_store_op <- SDLGPUStoreOp <$> (#{peek SDL_GPUDepthStencilTargetInfo, stencil_store_op} ptr :: IO CInt) -- Added annotation & constructor
    cycle_ <- toBool <$> (#{peek SDL_GPUDepthStencilTargetInfo, cycle} ptr :: IO CBool) -- Added :: IO CBool
    clear_stencil <- #{peek SDL_GPUDepthStencilTargetInfo, clear_stencil} ptr
    return SDLGPUDepthStencilTargetInfo {..}
  poke ptr SDLGPUDepthStencilTargetInfo{..} = do
    let (SDLGPUTexture texture_ptr) = gpuDepthStencilTargetTexture
    #{poke SDL_GPUDepthStencilTargetInfo, texture} ptr texture_ptr
    #{poke SDL_GPUDepthStencilTargetInfo, clear_depth} ptr (realToFrac gpuDepthStencilTargetClearDepth :: CFloat)
    #{poke SDL_GPUDepthStencilTargetInfo, load_op} ptr ( (\(SDLGPULoadOp i) -> i) gpuDepthStencilTargetLoadOp :: CInt) -- Added deconstructor & annotation
    #{poke SDL_GPUDepthStencilTargetInfo, store_op} ptr ( (\(SDLGPUStoreOp i) -> i) gpuDepthStencilTargetStoreOp :: CInt) -- Added deconstructor & annotation
    #{poke SDL_GPUDepthStencilTargetInfo, stencil_load_op} ptr ( (\(SDLGPULoadOp i) -> i) gpuDepthStencilTargetStencilLoadOp :: CInt) -- Added deconstructor & annotation
    #{poke SDL_GPUDepthStencilTargetInfo, stencil_store_op} ptr ( (\(SDLGPUStoreOp i) -> i) gpuDepthStencilTargetStencilStoreOp :: CInt) -- Added deconstructor & annotation
    #{poke SDL_GPUDepthStencilTargetInfo, cycle} ptr (fromBool gpuDepthStencilTargetCycle :: CBool) -- Added :: CBool
    #{poke SDL_GPUDepthStencilTargetInfo, clear_stencil} ptr gpuDepthStencilTargetClearStencil
    #{poke SDL_GPUDepthStencilTargetInfo, padding1} ptr (0 :: CUChar)
    #{poke SDL_GPUDepthStencilTargetInfo, padding2} ptr (0 :: CUChar)


-- SDL_GPUBlitInfo
data SDLGPUBlitInfo = SDLGPUBlitInfo
    { gpuBlitInfoSource      :: SDLGPUBlitRegion
    , gpuBlitInfoDestination :: SDLGPUBlitRegion
    , gpuBlitInfoLoadOp      :: SDLGPULoadOp
    , gpuBlitInfoClearColor  :: SDLFColor
    , gpuBlitInfoFlipMode    :: SDLFlipMode
    , gpuBlitInfoFilter      :: SDLGPUFilter
    , gpuBlitInfoCycle       :: Bool
    } deriving (Show, Eq)

instance Storable SDLGPUBlitInfo where
    sizeOf _ = #{size SDL_GPUBlitInfo}
    alignment _ = #{alignment SDL_GPUBlitInfo}
    peek ptr = do
        source <- peek (#{ptr SDL_GPUBlitInfo, source} ptr :: Ptr SDLGPUBlitRegion) -- Added Annotation
        destination <- peek (#{ptr SDL_GPUBlitInfo, destination} ptr :: Ptr SDLGPUBlitRegion) -- Added Annotation
        load_op <- SDLGPULoadOp <$> (#{peek SDL_GPUBlitInfo, load_op} ptr :: IO CInt) -- Added Annotation & Constructor
        clear_color <- peek (#{ptr SDL_GPUBlitInfo, clear_color} ptr :: Ptr SDLFColor) -- Added Annotation
        flip_mode_int <- #{peek SDL_GPUBlitInfo, flip_mode} ptr :: IO CInt
        let flip_mode = toEnum (fromIntegral flip_mode_int) :: SDLFlipMode
        filter_ <- SDLGPUFilter <$> (#{peek SDL_GPUBlitInfo, filter} ptr :: IO CInt) -- Added Annotation & Constructor
        cycle_ <- toBool <$> (#{peek SDL_GPUBlitInfo, cycle} ptr :: IO CBool) -- Added Annotation
        return SDLGPUBlitInfo {..}
    poke ptr SDLGPUBlitInfo{..} = do
        poke (#{ptr SDL_GPUBlitInfo, source} ptr) gpuBlitInfoSource
        poke (#{ptr SDL_GPUBlitInfo, destination} ptr) gpuBlitInfoDestination
        #{poke SDL_GPUBlitInfo, load_op} ptr ( (\(SDLGPULoadOp i) -> i) gpuBlitInfoLoadOp :: CInt) -- Added Deconstructor & Annotation
        poke (#{ptr SDL_GPUBlitInfo, clear_color} ptr) gpuBlitInfoClearColor
        #{poke SDL_GPUBlitInfo, flip_mode} ptr (fromIntegral (fromEnum gpuBlitInfoFlipMode) :: CInt)
        #{poke SDL_GPUBlitInfo, filter} ptr ( (\(SDLGPUFilter i) -> i) gpuBlitInfoFilter :: CInt) -- Added Deconstructor & Annotation
        #{poke SDL_GPUBlitInfo, cycle} ptr (fromBool gpuBlitInfoCycle :: CBool) -- Added Annotation
        #{poke SDL_GPUBlitInfo, padding1} ptr (0 :: CUChar)
        #{poke SDL_GPUBlitInfo, padding2} ptr (0 :: CUChar)
        #{poke SDL_GPUBlitInfo, padding3} ptr (0 :: CUChar)

-- SDL_GPUBufferBinding
data SDLGPUBufferBinding = SDLGPUBufferBinding
  { gpuBufBindBuffer :: SDLGPUBuffer
  , gpuBufBindOffset :: Word32
  } deriving (Show, Eq)

instance Storable SDLGPUBufferBinding where
  sizeOf _ = #{size SDL_GPUBufferBinding}
  alignment _ = #{alignment SDL_GPUBufferBinding}
  peek ptr = do
    buffer_ptr <- #{peek SDL_GPUBufferBinding, buffer} ptr
    offset <- fromIntegral <$> (#{peek SDL_GPUBufferBinding, offset} ptr :: IO CUInt)
    return SDLGPUBufferBinding
      { gpuBufBindBuffer = SDLGPUBuffer buffer_ptr
      , gpuBufBindOffset = offset
      }
  poke ptr SDLGPUBufferBinding{..} = do
    let (SDLGPUBuffer buffer_ptr) = gpuBufBindBuffer
    #{poke SDL_GPUBufferBinding, buffer} ptr buffer_ptr
    #{poke SDL_GPUBufferBinding, offset} ptr (fromIntegral gpuBufBindOffset :: CUInt)

-- SDL_GPUTextureSamplerBinding
data SDLGPUTextureSamplerBinding = SDLGPUTextureSamplerBinding
  { gpuTexSampBindTexture :: SDLGPUTexture
  , gpuTexSampBindSampler :: SDLGPUSampler
  } deriving (Show, Eq)

instance Storable SDLGPUTextureSamplerBinding where
  sizeOf _ = #{size SDL_GPUTextureSamplerBinding}
  alignment _ = #{alignment SDL_GPUTextureSamplerBinding}
  peek ptr = do
    texture_ptr <- #{peek SDL_GPUTextureSamplerBinding, texture} ptr
    sampler_ptr <- #{peek SDL_GPUTextureSamplerBinding, sampler} ptr
    return SDLGPUTextureSamplerBinding
      { gpuTexSampBindTexture = SDLGPUTexture texture_ptr
      , gpuTexSampBindSampler = SDLGPUSampler sampler_ptr
      }
  poke ptr SDLGPUTextureSamplerBinding{..} = do
    let (SDLGPUTexture texture_ptr) = gpuTexSampBindTexture
    let (SDLGPUSampler sampler_ptr) = gpuTexSampBindSampler
    #{poke SDL_GPUTextureSamplerBinding, texture} ptr texture_ptr
    #{poke SDL_GPUTextureSamplerBinding, sampler} ptr sampler_ptr

-- SDL_GPUStorageBufferReadWriteBinding
data SDLGPUStorageBufferReadWriteBinding = SDLGPUStorageBufferReadWriteBinding
  { gpuStoreBufRWBindBuffer :: SDLGPUBuffer
  , gpuStoreBufRWBindCycle  :: Bool
  } deriving (Show, Eq)

instance Storable SDLGPUStorageBufferReadWriteBinding where
  sizeOf _ = #{size SDL_GPUStorageBufferReadWriteBinding}
  alignment _ = #{alignment SDL_GPUStorageBufferReadWriteBinding}
  peek ptr = do
    buffer_ptr <- #{peek SDL_GPUStorageBufferReadWriteBinding, buffer} ptr
    cycle_ <- toBool <$> (#{peek SDL_GPUStorageBufferReadWriteBinding, cycle} ptr :: IO CBool) -- Added Annotation
    return SDLGPUStorageBufferReadWriteBinding
      { gpuStoreBufRWBindBuffer = SDLGPUBuffer buffer_ptr
      , gpuStoreBufRWBindCycle = cycle_
      }
  poke ptr SDLGPUStorageBufferReadWriteBinding{..} = do
    let (SDLGPUBuffer buffer_ptr) = gpuStoreBufRWBindBuffer
    #{poke SDL_GPUStorageBufferReadWriteBinding, buffer} ptr buffer_ptr
    #{poke SDL_GPUStorageBufferReadWriteBinding, cycle} ptr (fromBool gpuStoreBufRWBindCycle :: CBool) -- Added Annotation
    #{poke SDL_GPUStorageBufferReadWriteBinding, padding1} ptr (0 :: CUChar)
    #{poke SDL_GPUStorageBufferReadWriteBinding, padding2} ptr (0 :: CUChar)
    #{poke SDL_GPUStorageBufferReadWriteBinding, padding3} ptr (0 :: CUChar)

-- SDL_GPUStorageTextureReadWriteBinding
data SDLGPUStorageTextureReadWriteBinding = SDLGPUStorageTextureReadWriteBinding
  { gpuStoreTexRWBindTexture  :: SDLGPUTexture
  , gpuStoreTexRWBindMipLevel :: Word32
  , gpuStoreTexRWBindLayer    :: Word32
  , gpuStoreTexRWBindCycle    :: Bool
  } deriving (Show, Eq)

instance Storable SDLGPUStorageTextureReadWriteBinding where
  sizeOf _ = #{size SDL_GPUStorageTextureReadWriteBinding}
  alignment _ = #{alignment SDL_GPUStorageTextureReadWriteBinding}
  peek ptr = do
    texture_ptr <- #{peek SDL_GPUStorageTextureReadWriteBinding, texture} ptr
    mip_level <- fromIntegral <$> (#{peek SDL_GPUStorageTextureReadWriteBinding, mip_level} ptr :: IO CUInt)
    layer <- fromIntegral <$> (#{peek SDL_GPUStorageTextureReadWriteBinding, layer} ptr :: IO CUInt)
    cycle_ <- toBool <$> (#{peek SDL_GPUStorageTextureReadWriteBinding, cycle} ptr :: IO CBool) -- Added Annotation
    return SDLGPUStorageTextureReadWriteBinding
      { gpuStoreTexRWBindTexture = SDLGPUTexture texture_ptr
      , gpuStoreTexRWBindMipLevel = mip_level
      , gpuStoreTexRWBindLayer = layer
      , gpuStoreTexRWBindCycle = cycle_
      }
  poke ptr SDLGPUStorageTextureReadWriteBinding{..} = do
    let (SDLGPUTexture texture_ptr) = gpuStoreTexRWBindTexture
    #{poke SDL_GPUStorageTextureReadWriteBinding, texture} ptr texture_ptr
    #{poke SDL_GPUStorageTextureReadWriteBinding, mip_level} ptr (fromIntegral gpuStoreTexRWBindMipLevel :: CUInt)
    #{poke SDL_GPUStorageTextureReadWriteBinding, layer} ptr (fromIntegral gpuStoreTexRWBindLayer :: CUInt)
    #{poke SDL_GPUStorageTextureReadWriteBinding, cycle} ptr (fromBool gpuStoreTexRWBindCycle :: CBool) -- Added Annotation
    #{poke SDL_GPUStorageTextureReadWriteBinding, padding1} ptr (0 :: CUChar)
    #{poke SDL_GPUStorageTextureReadWriteBinding, padding2} ptr (0 :: CUChar)
    #{poke SDL_GPUStorageTextureReadWriteBinding, padding3} ptr (0 :: CUChar)

-- | Checks for GPU runtime support based on shader formats and optional driver name.
foreign import ccall unsafe "SDL_GPUSupportsShaderFormats"
  c_sdlGPUSupportsShaderFormats :: SDLGPUShaderFormat -> CString -> IO CBool

sdlGPUSupportsShaderFormats :: SDLGPUShaderFormat -> Maybe String -> IO Bool
sdlGPUSupportsShaderFormats shaderFormat mName =
  maybeWith withCString mName $ \cName ->
    fromCBool <$> c_sdlGPUSupportsShaderFormats shaderFormat cName

-- | Checks for GPU runtime support based on properties.
foreign import ccall unsafe "SDL_GPUSupportsProperties"
  c_sdlGPUSupportsProperties :: SDLPropertiesID -> IO CBool

sdlGPUSupportsProperties :: SDLPropertiesID -> IO Bool
sdlGPUSupportsProperties props = fromCBool <$> c_sdlGPUSupportsProperties props

-- | Creates a GPU context.
foreign import ccall unsafe "SDL_CreateGPUDevice"
  c_sdlCreateGPUDevice :: SDLGPUShaderFormat -> CBool -> CString -> IO (Ptr SDLGPUDevice)

sdlCreateGPUDevice :: SDLGPUShaderFormat -> Bool -> Maybe String -> IO (Maybe SDLGPUDevice)
sdlCreateGPUDevice shaderFormat debugMode mName =
  maybeWith withCString mName $ \cName -> do
    ptr <- c_sdlCreateGPUDevice shaderFormat (fromBool debugMode) cName
    if ptr == nullPtr
      then return Nothing
      else return $ Just (SDLGPUDevice ptr)

-- | Creates a GPU context using properties.
foreign import ccall unsafe "SDL_CreateGPUDeviceWithProperties"
  c_sdlCreateGPUDeviceWithProperties :: SDLPropertiesID -> IO (Ptr SDLGPUDevice)

sdlCreateGPUDeviceWithProperties :: SDLPropertiesID -> IO (Maybe SDLGPUDevice)
sdlCreateGPUDeviceWithProperties props = do
  ptr <- c_sdlCreateGPUDeviceWithProperties props
  if ptr == nullPtr
    then return Nothing
    else return $ Just (SDLGPUDevice ptr)

-- Property string constants
pattern SDL_PROP_GPU_DEVICE_CREATE_DEBUGMODE_BOOLEAN          = "SDL.gpu.device.create.debugmode"
pattern SDL_PROP_GPU_DEVICE_CREATE_PREFERLOWPOWER_BOOLEAN     = "SDL.gpu.device.create.preferlowpower"
pattern SDL_PROP_GPU_DEVICE_CREATE_VERBOSE_BOOLEAN            = "SDL.gpu.device.create.verbose"
pattern SDL_PROP_GPU_DEVICE_CREATE_NAME_STRING                = "SDL.gpu.device.create.name"
pattern SDL_PROP_GPU_DEVICE_CREATE_SHADERS_PRIVATE_BOOLEAN    = "SDL.gpu.device.create.shaders.private"
pattern SDL_PROP_GPU_DEVICE_CREATE_SHADERS_SPIRV_BOOLEAN      = "SDL.gpu.device.create.shaders.spirv"
pattern SDL_PROP_GPU_DEVICE_CREATE_SHADERS_DXBC_BOOLEAN       = "SDL.gpu.device.create.shaders.dxbc"
pattern SDL_PROP_GPU_DEVICE_CREATE_SHADERS_DXIL_BOOLEAN       = "SDL.gpu.device.create.shaders.dxil"
pattern SDL_PROP_GPU_DEVICE_CREATE_SHADERS_MSL_BOOLEAN        = "SDL.gpu.device.create.shaders.msl"
pattern SDL_PROP_GPU_DEVICE_CREATE_SHADERS_METALLIB_BOOLEAN   = "SDL.gpu.device.create.shaders.metallib"
pattern SDL_PROP_GPU_DEVICE_CREATE_D3D12_SEMANTIC_NAME_STRING = "SDL.gpu.device.create.d3d12.semantic"

-- | Destroys a GPU context.
foreign import ccall unsafe "SDL_DestroyGPUDevice"
  c_sdlDestroyGPUDevice :: Ptr SDLGPUDevice -> IO ()

sdlDestroyGPUDevice :: SDLGPUDevice -> IO ()
sdlDestroyGPUDevice (SDLGPUDevice device) = c_sdlDestroyGPUDevice device

-- | Get the number of GPU drivers compiled into SDL.
foreign import ccall unsafe "SDL_GetNumGPUDrivers"
  c_sdlGetNumGPUDrivers :: IO CInt

sdlGetNumGPUDrivers :: IO Int
sdlGetNumGPUDrivers = fromIntegral <$> c_sdlGetNumGPUDrivers

-- | Get the name of a built in GPU driver.
foreign import ccall unsafe "SDL_GetGPUDriver"
  c_sdlGetGPUDriver :: CInt -> IO CString

sdlGetGPUDriver :: Int -> IO (Maybe String)
sdlGetGPUDriver index
  | index < 0 = return Nothing
  | otherwise = do
      numDrivers <- sdlGetNumGPUDrivers
      if index >= numDrivers
        then return Nothing
        else do
          cstr <- c_sdlGetGPUDriver (fromIntegral index)
          if cstr == nullPtr
            then return Nothing
            else Just <$> peekCString cstr

-- | Returns the name of the backend used to create this GPU context.
foreign import ccall unsafe "SDL_GetGPUDeviceDriver"
  c_sdlGetGPUDeviceDriver :: Ptr SDLGPUDevice -> IO CString

sdlGetGPUDeviceDriver :: SDLGPUDevice -> IO (Maybe String)
sdlGetGPUDeviceDriver (SDLGPUDevice device) = do
  cstr <- c_sdlGetGPUDeviceDriver device
  if cstr == nullPtr
    then return Nothing
    else Just <$> peekCString cstr

-- | Returns the supported shader formats for this GPU context.
foreign import ccall unsafe "SDL_GetGPUShaderFormats"
  c_sdlGetGPUShaderFormats :: Ptr SDLGPUDevice -> IO SDLGPUShaderFormat

sdlGetGPUShaderFormats :: SDLGPUDevice -> IO SDLGPUShaderFormat
sdlGetGPUShaderFormats (SDLGPUDevice device) = c_sdlGetGPUShaderFormats device

#if SDL_VERSION_ATLEAST(3, 4, 0)
-- | Get the properties associated with a GPU device.
-- Requires SDL >= 3.4.0
foreign import ccall unsafe "SDL_GetGPUDeviceProperties"
  c_sdlGetGPUDeviceProperties :: Ptr SDLGPUDevice -> IO SDLPropertiesID

sdlGetGPUDeviceProperties :: SDLGPUDevice -> IO SDLPropertiesID -- Returns 0 on failure which is handled by SDL.Properties
sdlGetGPUDeviceProperties (SDLGPUDevice device) = c_sdlGetGPUDeviceProperties device

pattern SDL_PROP_GPU_DEVICE_NAME_STRING           = "SDL.gpu.device.name"
pattern SDL_PROP_GPU_DEVICE_DRIVER_NAME_STRING    = "SDL.gpu.device.driver_name"
pattern SDL_PROP_GPU_DEVICE_DRIVER_VERSION_STRING = "SDL.gpu.device.driver_version"
pattern SDL_PROP_GPU_DEVICE_DRIVER_INFO_STRING    = "SDL.gpu.device.driver_info"
#endif

-- | Creates a pipeline object to be used in a compute workflow.
foreign import ccall unsafe "SDL_CreateGPUComputePipeline"
  c_sdlCreateGPUComputePipeline :: Ptr SDLGPUDevice -> Ptr () -> IO (Ptr SDLGPUComputePipeline)

sdlCreateGPUComputePipeline :: SDLGPUDevice -> SDLGPUComputePipelineCreateInfo -> IO (Maybe SDLGPUComputePipeline)
sdlCreateGPUComputePipeline (SDLGPUDevice dev) createInfo =
  withGPUComputePipelineCreateInfo createInfo $ \createInfoPtr -> do -- Receives typed Ptr
    ptr <- c_sdlCreateGPUComputePipeline dev (castPtr createInfoPtr) -- Cast back if FFI needs Ptr ()
    if ptr == nullPtr
      then return Nothing
      else return $ Just (SDLGPUComputePipeline ptr)

pattern SDL_PROP_GPU_COMPUTEPIPELINE_CREATE_NAME_STRING = "SDL.gpu.computepipeline.create.name"

-- | Creates a pipeline object to be used in a graphics workflow.
foreign import ccall unsafe "SDL_CreateGPUGraphicsPipeline"
  c_sdlCreateGPUGraphicsPipeline :: Ptr SDLGPUDevice -> Ptr () -> IO (Ptr SDLGPUGraphicsPipeline)

sdlCreateGPUGraphicsPipeline :: SDLGPUDevice -> SDLGPUGraphicsPipelineCreateInfo -> IO (Maybe SDLGPUGraphicsPipeline)
sdlCreateGPUGraphicsPipeline (SDLGPUDevice dev) createInfo =
  withGPUGraphicsPipelineCreateInfo createInfo $ \createInfoPtr -> do
    ptr <- c_sdlCreateGPUGraphicsPipeline dev createInfoPtr
    if ptr == nullPtr
      then return Nothing
      else return $ Just (SDLGPUGraphicsPipeline ptr)

pattern SDL_PROP_GPU_GRAPHICSPIPELINE_CREATE_NAME_STRING = "SDL.gpu.graphicspipeline.create.name"

-- | Creates a sampler object.
foreign import ccall unsafe "SDL_CreateGPUSampler"
  c_sdlCreateGPUSampler :: Ptr SDLGPUDevice -> Ptr SDLGPUSamplerCreateInfo -> IO (Ptr SDLGPUSampler)

sdlCreateGPUSampler :: SDLGPUDevice -> SDLGPUSamplerCreateInfo -> IO (Maybe SDLGPUSampler)
sdlCreateGPUSampler (SDLGPUDevice dev) createInfo =
  with createInfo $ \createInfoPtr -> do
    ptr <- c_sdlCreateGPUSampler dev createInfoPtr
    if ptr == nullPtr
      then return Nothing
      else return $ Just (SDLGPUSampler ptr)

pattern SDL_PROP_GPU_SAMPLER_CREATE_NAME_STRING = "SDL.gpu.sampler.create.name"

-- | Creates a shader to be used when creating a graphics pipeline.
foreign import ccall unsafe "SDL_CreateGPUShader"
  c_sdlCreateGPUShader :: Ptr SDLGPUDevice -> Ptr () -> IO (Ptr SDLGPUShader)

sdlCreateGPUShader :: SDLGPUDevice -> SDLGPUShaderCreateInfo -> IO (Maybe SDLGPUShader)
sdlCreateGPUShader (SDLGPUDevice dev) createInfo =
  withGPUShaderCreateInfo createInfo $ \createInfoPtr -> do -- Receives typed Ptr
    ptr <- c_sdlCreateGPUShader dev (castPtr createInfoPtr) -- Cast back for C FFI if needed, though direct typed Ptr might work
    if ptr == nullPtr
      then return Nothing
      else return $ Just (SDLGPUShader ptr)

pattern SDL_PROP_GPU_SHADER_CREATE_NAME_STRING = "SDL.gpu.shader.create.name"

-- | Creates a texture object.
foreign import ccall unsafe "SDL_CreateGPUTexture"
  c_sdlCreateGPUTexture :: Ptr SDLGPUDevice -> Ptr SDLGPUTextureCreateInfo -> IO (Ptr SDLGPUTexture)

sdlCreateGPUTexture :: SDLGPUDevice -> SDLGPUTextureCreateInfo -> IO (Maybe SDLGPUTexture)
sdlCreateGPUTexture (SDLGPUDevice dev) createInfo =
  with createInfo $ \createInfoPtr -> do
    ptr <- c_sdlCreateGPUTexture dev createInfoPtr
    if ptr == nullPtr
      then return Nothing
      else return $ Just (SDLGPUTexture ptr)

pattern SDL_PROP_GPU_TEXTURE_CREATE_D3D12_CLEAR_R_FLOAT         = "SDL.gpu.texture.create.d3d12.clear.r"
pattern SDL_PROP_GPU_TEXTURE_CREATE_D3D12_CLEAR_G_FLOAT         = "SDL.gpu.texture.create.d3d12.clear.g"
pattern SDL_PROP_GPU_TEXTURE_CREATE_D3D12_CLEAR_B_FLOAT         = "SDL.gpu.texture.create.d3d12.clear.b"
pattern SDL_PROP_GPU_TEXTURE_CREATE_D3D12_CLEAR_A_FLOAT         = "SDL.gpu.texture.create.d3d12.clear.a"
pattern SDL_PROP_GPU_TEXTURE_CREATE_D3D12_CLEAR_DEPTH_FLOAT     = "SDL.gpu.texture.create.d3d12.clear.depth"
pattern SDL_PROP_GPU_TEXTURE_CREATE_D3D12_CLEAR_STENCIL_NUMBER  = "SDL.gpu.texture.create.d3d12.clear.stencil"
pattern SDL_PROP_GPU_TEXTURE_CREATE_NAME_STRING                 = "SDL.gpu.texture.create.name"

-- | Creates a buffer object.
foreign import ccall unsafe "SDL_CreateGPUBuffer"
  c_sdlCreateGPUBuffer :: Ptr SDLGPUDevice -> Ptr SDLGPUBufferCreateInfo -> IO (Ptr SDLGPUBuffer)

sdlCreateGPUBuffer :: SDLGPUDevice -> SDLGPUBufferCreateInfo -> IO (Maybe SDLGPUBuffer)
sdlCreateGPUBuffer (SDLGPUDevice dev) createInfo =
  with createInfo $ \createInfoPtr -> do
    ptr <- c_sdlCreateGPUBuffer dev createInfoPtr
    if ptr == nullPtr
      then return Nothing
      else return $ Just (SDLGPUBuffer ptr)

pattern SDL_PROP_GPU_BUFFER_CREATE_NAME_STRING = "SDL.gpu.buffer.create.name"

-- | Creates a transfer buffer.
foreign import ccall unsafe "SDL_CreateGPUTransferBuffer"
  c_sdlCreateGPUTransferBuffer :: Ptr SDLGPUDevice -> Ptr SDLGPUTransferBufferCreateInfo -> IO (Ptr SDLGPUTransferBuffer)

sdlCreateGPUTransferBuffer :: SDLGPUDevice -> SDLGPUTransferBufferCreateInfo -> IO (Maybe SDLGPUTransferBuffer)
sdlCreateGPUTransferBuffer (SDLGPUDevice dev) createInfo =
  with createInfo $ \createInfoPtr -> do
    ptr <- c_sdlCreateGPUTransferBuffer dev createInfoPtr
    if ptr == nullPtr
      then return Nothing
      else return $ Just (SDLGPUTransferBuffer ptr)

pattern SDL_PROP_GPU_TRANSFERBUFFER_CREATE_NAME_STRING = "SDL.gpu.transferbuffer.create.name"

-- Debug Naming (Use with caution regarding thread safety)

-- | Sets an arbitrary string constant to label a buffer. (Not thread safe)
foreign import ccall unsafe "SDL_SetGPUBufferName"
  c_sdlSetGPUBufferName :: Ptr SDLGPUDevice -> Ptr SDLGPUBuffer -> CString -> IO ()

sdlSetGPUBufferName :: SDLGPUDevice -> SDLGPUBuffer -> String -> IO ()
sdlSetGPUBufferName (SDLGPUDevice dev) (SDLGPUBuffer buf) name =
  withCString name $ c_sdlSetGPUBufferName dev buf

-- | Sets an arbitrary string constant to label a texture. (Not thread safe)
foreign import ccall unsafe "SDL_SetGPUTextureName"
  c_sdlSetGPUTextureName :: Ptr SDLGPUDevice -> Ptr SDLGPUTexture -> CString -> IO ()

sdlSetGPUTextureName :: SDLGPUDevice -> SDLGPUTexture -> String -> IO ()
sdlSetGPUTextureName (SDLGPUDevice dev) (SDLGPUTexture tex) name =
  withCString name $ c_sdlSetGPUTextureName dev tex

-- | Inserts an arbitrary string label into the command buffer callstream.
foreign import ccall unsafe "SDL_InsertGPUDebugLabel"
  c_sdlInsertGPUDebugLabel :: Ptr SDLGPUCommandBuffer -> CString -> IO ()

sdlInsertGPUDebugLabel :: SDLGPUCommandBuffer -> String -> IO ()
sdlInsertGPUDebugLabel (SDLGPUCommandBuffer cmdbuf) label =
  withCString label $ c_sdlInsertGPUDebugLabel cmdbuf

-- | Begins a debug group with an arbitary name.
foreign import ccall unsafe "SDL_PushGPUDebugGroup"
  c_sdlPushGPUDebugGroup :: Ptr SDLGPUCommandBuffer -> CString -> IO ()

sdlPushGPUDebugGroup :: SDLGPUCommandBuffer -> String -> IO ()
sdlPushGPUDebugGroup (SDLGPUCommandBuffer cmdbuf) name =
  withCString name $ c_sdlPushGPUDebugGroup cmdbuf

-- | Ends the most-recently pushed debug group.
foreign import ccall unsafe "SDL_PopGPUDebugGroup"
  c_sdlPopGPUDebugGroup :: Ptr SDLGPUCommandBuffer -> IO ()

sdlPopGPUDebugGroup :: SDLGPUCommandBuffer -> IO ()
sdlPopGPUDebugGroup (SDLGPUCommandBuffer cmdbuf) = c_sdlPopGPUDebugGroup cmdbuf

-- Disposal Functions (Already defined earlier, included for completeness)

-- | Frees the given texture as soon as it is safe to do so.
foreign import ccall unsafe "SDL_ReleaseGPUTexture"
  c_sdlReleaseGPUTexture :: Ptr SDLGPUDevice -> Ptr SDLGPUTexture -> IO ()

sdlReleaseGPUTexture :: SDLGPUDevice -> SDLGPUTexture -> IO ()
sdlReleaseGPUTexture (SDLGPUDevice dev) (SDLGPUTexture tex) = c_sdlReleaseGPUTexture dev tex

-- | Frees the given sampler as soon as it is safe to do so.
foreign import ccall unsafe "SDL_ReleaseGPUSampler"
  c_sdlReleaseGPUSampler :: Ptr SDLGPUDevice -> Ptr SDLGPUSampler -> IO ()

sdlReleaseGPUSampler :: SDLGPUDevice -> SDLGPUSampler -> IO ()
sdlReleaseGPUSampler (SDLGPUDevice dev) (SDLGPUSampler samp) = c_sdlReleaseGPUSampler dev samp

-- | Frees the given buffer as soon as it is safe to do so.
foreign import ccall unsafe "SDL_ReleaseGPUBuffer"
  c_sdlReleaseGPUBuffer :: Ptr SDLGPUDevice -> Ptr SDLGPUBuffer -> IO ()

sdlReleaseGPUBuffer :: SDLGPUDevice -> SDLGPUBuffer -> IO ()
sdlReleaseGPUBuffer (SDLGPUDevice dev) (SDLGPUBuffer buf) = c_sdlReleaseGPUBuffer dev buf

-- | Frees the given transfer buffer as soon as it is safe to do so.
foreign import ccall unsafe "SDL_ReleaseGPUTransferBuffer"
  c_sdlReleaseGPUTransferBuffer :: Ptr SDLGPUDevice -> Ptr SDLGPUTransferBuffer -> IO ()

sdlReleaseGPUTransferBuffer :: SDLGPUDevice -> SDLGPUTransferBuffer -> IO ()
sdlReleaseGPUTransferBuffer (SDLGPUDevice dev) (SDLGPUTransferBuffer tbuf) = c_sdlReleaseGPUTransferBuffer dev tbuf

-- | Frees the given compute pipeline as soon as it is safe to do so.
foreign import ccall unsafe "SDL_ReleaseGPUComputePipeline"
  c_sdlReleaseGPUComputePipeline :: Ptr SDLGPUDevice -> Ptr SDLGPUComputePipeline -> IO ()

sdlReleaseGPUComputePipeline :: SDLGPUDevice -> SDLGPUComputePipeline -> IO ()
sdlReleaseGPUComputePipeline (SDLGPUDevice dev) (SDLGPUComputePipeline pipe) = c_sdlReleaseGPUComputePipeline dev pipe

-- | Frees the given shader as soon as it is safe to do so.
foreign import ccall unsafe "SDL_ReleaseGPUShader"
  c_sdlReleaseGPUShader :: Ptr SDLGPUDevice -> Ptr SDLGPUShader -> IO ()

sdlReleaseGPUShader :: SDLGPUDevice -> SDLGPUShader -> IO ()
sdlReleaseGPUShader (SDLGPUDevice dev) (SDLGPUShader shader) = c_sdlReleaseGPUShader dev shader

-- | Frees the given graphics pipeline as soon as it is safe to do so.
foreign import ccall unsafe "SDL_ReleaseGPUGraphicsPipeline"
  c_sdlReleaseGPUGraphicsPipeline :: Ptr SDLGPUDevice -> Ptr SDLGPUGraphicsPipeline -> IO ()

sdlReleaseGPUGraphicsPipeline :: SDLGPUDevice -> SDLGPUGraphicsPipeline -> IO ()
sdlReleaseGPUGraphicsPipeline (SDLGPUDevice dev) (SDLGPUGraphicsPipeline pipe) = c_sdlReleaseGPUGraphicsPipeline dev pipe

-- | Releases a fence obtained from sdlSubmitGPUCommandBufferAndAcquireFence.
foreign import ccall unsafe "SDL_ReleaseGPUFence"
  c_sdlReleaseGPUFence :: Ptr SDLGPUDevice -> Ptr SDLGPUFence -> IO ()

sdlReleaseGPUFence :: SDLGPUDevice -> SDLGPUFence -> IO ()
sdlReleaseGPUFence (SDLGPUDevice dev) (SDLGPUFence fence) = c_sdlReleaseGPUFence dev fence

-- Command Buffer Acquisition (Already defined earlier)

-- | Acquire a command buffer.
foreign import ccall unsafe "SDL_AcquireGPUCommandBuffer"
  c_sdlAcquireGPUCommandBuffer :: Ptr SDLGPUDevice -> IO (Ptr SDLGPUCommandBuffer)

sdlAcquireGPUCommandBuffer :: SDLGPUDevice -> IO (Maybe SDLGPUCommandBuffer)
sdlAcquireGPUCommandBuffer (SDLGPUDevice dev) = do
  ptr <- c_sdlAcquireGPUCommandBuffer dev
  if ptr == nullPtr
    then return Nothing
    else return $ Just (SDLGPUCommandBuffer ptr)


-- Uniform Data

-- | Pushes data to a vertex uniform slot on the command buffer.
foreign import ccall unsafe "SDL_PushGPUVertexUniformData"
  c_sdlPushGPUVertexUniformData :: Ptr SDLGPUCommandBuffer -> CUInt -> Ptr () -> CUInt -> IO ()

sdlPushGPUVertexUniformData :: Storable a => SDLGPUCommandBuffer -> Word32 -> a -> IO ()
sdlPushGPUVertexUniformData cmdbuf slotIndex dat =
    with dat $ \dataPtr ->
        sdlPushGPUVertexUniformDataRaw cmdbuf slotIndex (castPtr dataPtr) (fromIntegral (sizeOf dat))

sdlPushGPUVertexUniformDataRaw :: SDLGPUCommandBuffer -> Word32 -> Ptr () -> Word32 -> IO ()
sdlPushGPUVertexUniformDataRaw (SDLGPUCommandBuffer cmdbuf) slotIndex dataPtr len =
    c_sdlPushGPUVertexUniformData cmdbuf (fromIntegral slotIndex) dataPtr (fromIntegral len)

-- | Pushes data to a fragment uniform slot on the command buffer.
foreign import ccall unsafe "SDL_PushGPUFragmentUniformData"
  c_sdlPushGPUFragmentUniformData :: Ptr SDLGPUCommandBuffer -> CUInt -> Ptr () -> CUInt -> IO ()

sdlPushGPUFragmentUniformData :: Storable a => SDLGPUCommandBuffer -> Word32 -> a -> IO ()
sdlPushGPUFragmentUniformData cmdbuf slotIndex dat =
    with dat $ \dataPtr ->
        sdlPushGPUFragmentUniformDataRaw cmdbuf slotIndex (castPtr dataPtr) (fromIntegral (sizeOf dat))

sdlPushGPUFragmentUniformDataRaw :: SDLGPUCommandBuffer -> Word32 -> Ptr () -> Word32 -> IO ()
sdlPushGPUFragmentUniformDataRaw (SDLGPUCommandBuffer cmdbuf) slotIndex dataPtr len =
    c_sdlPushGPUFragmentUniformData cmdbuf (fromIntegral slotIndex) dataPtr (fromIntegral len)

-- | Pushes data to a uniform slot on the command buffer (for compute).
foreign import ccall unsafe "SDL_PushGPUComputeUniformData"
  c_sdlPushGPUComputeUniformData :: Ptr SDLGPUCommandBuffer -> CUInt -> Ptr () -> CUInt -> IO ()

sdlPushGPUComputeUniformData :: Storable a => SDLGPUCommandBuffer -> Word32 -> a -> IO ()
sdlPushGPUComputeUniformData cmdbuf slotIndex dat =
    with dat $ \dataPtr ->
        sdlPushGPUComputeUniformDataRaw cmdbuf slotIndex (castPtr dataPtr) (fromIntegral (sizeOf dat))

sdlPushGPUComputeUniformDataRaw :: SDLGPUCommandBuffer -> Word32 -> Ptr () -> Word32 -> IO ()
sdlPushGPUComputeUniformDataRaw (SDLGPUCommandBuffer cmdbuf) slotIndex dataPtr len =
    c_sdlPushGPUComputeUniformData cmdbuf (fromIntegral slotIndex) dataPtr (fromIntegral len)

-- Graphics Pass

-- | Begins a render pass on a command buffer.
foreign import ccall unsafe "SDL_BeginGPURenderPass"
  c_sdlBeginGPURenderPass :: Ptr SDLGPUCommandBuffer -> Ptr SDLGPUColorTargetInfo -> CUInt -> Ptr SDLGPUDepthStencilTargetInfo -> IO (Ptr SDLGPURenderPass)

sdlBeginGPURenderPass :: SDLGPUCommandBuffer -> [SDLGPUColorTargetInfo] -> Maybe SDLGPUDepthStencilTargetInfo -> IO (Maybe SDLGPURenderPass)
sdlBeginGPURenderPass (SDLGPUCommandBuffer cmdbuf) colorTargets mDepthStencil =
  withArrayLen colorTargets $ \numColorTargets colorTargetsPtr ->
  maybeWith with mDepthStencil $ \depthStencilPtr -> do -- 'with' handles NULL case for Maybe
    ptr <- c_sdlBeginGPURenderPass cmdbuf colorTargetsPtr (fromIntegral numColorTargets) depthStencilPtr
    if ptr == nullPtr
      then return Nothing
      else return $ Just (SDLGPURenderPass ptr)

-- | Binds a graphics pipeline on a render pass.
foreign import ccall unsafe "SDL_BindGPUGraphicsPipeline"
  c_sdlBindGPUGraphicsPipeline :: Ptr SDLGPURenderPass -> Ptr SDLGPUGraphicsPipeline -> IO ()

sdlBindGPUGraphicsPipeline :: SDLGPURenderPass -> SDLGPUGraphicsPipeline -> IO ()
sdlBindGPUGraphicsPipeline (SDLGPURenderPass rp) (SDLGPUGraphicsPipeline pipe) =
  c_sdlBindGPUGraphicsPipeline rp pipe

-- | Sets the current viewport state.
foreign import ccall unsafe "SDL_SetGPUViewport"
  c_sdlSetGPUViewport :: Ptr SDLGPURenderPass -> Ptr SDLGPUViewport -> IO ()

sdlSetGPUViewport :: SDLGPURenderPass -> SDLGPUViewport -> IO ()
sdlSetGPUViewport (SDLGPURenderPass rp) viewport =
  with viewport $ c_sdlSetGPUViewport rp

-- | Sets the current scissor state.
foreign import ccall unsafe "SDL_SetGPUScissor"
  c_sdlSetGPUScissor :: Ptr SDLGPURenderPass -> Ptr SDLRect -> IO ()

sdlSetGPUScissor :: SDLGPURenderPass -> SDLRect -> IO ()
sdlSetGPUScissor (SDLGPURenderPass rp) scissor =
  with scissor $ c_sdlSetGPUScissor rp

-- | Sets the current blend constants.
foreign import ccall unsafe "SDL_SetGPUBlendConstants"
  c_sdlSetGPUBlendConstants :: Ptr SDLGPURenderPass -> Ptr SDLFColor -> IO ()

sdlSetGPUBlendConstants :: SDLGPURenderPass -> SDLFColor -> IO ()
sdlSetGPUBlendConstants (SDLGPURenderPass rp) blendConstants =
  with blendConstants $ c_sdlSetGPUBlendConstants rp

-- | Sets the current stencil reference value.
foreign import ccall unsafe "SDL_SetGPUStencilReference"
  c_sdlSetGPUStencilReference :: Ptr SDLGPURenderPass -> CUChar -> IO ()

sdlSetGPUStencilReference :: SDLGPURenderPass -> Word8 -> IO ()
sdlSetGPUStencilReference (SDLGPURenderPass rp) reference =
  c_sdlSetGPUStencilReference rp (fromIntegral reference)

-- | Binds vertex buffers.
foreign import ccall unsafe "SDL_BindGPUVertexBuffers"
  c_sdlBindGPUVertexBuffers :: Ptr SDLGPURenderPass -> CUInt -> Ptr SDLGPUBufferBinding -> CUInt -> IO ()

sdlBindGPUVertexBuffers :: SDLGPURenderPass -> Word32 -> [SDLGPUBufferBinding] -> IO ()
sdlBindGPUVertexBuffers (SDLGPURenderPass rp) firstSlot bindings =
  withArrayLen bindings $ \numBindings bindingsPtr ->
    c_sdlBindGPUVertexBuffers rp (fromIntegral firstSlot) bindingsPtr (fromIntegral numBindings)

-- | Binds an index buffer.
foreign import ccall unsafe "SDL_BindGPUIndexBuffer"
  c_sdlBindGPUIndexBuffer :: Ptr SDLGPURenderPass -> Ptr SDLGPUBufferBinding -> SDLGPUIndexElementSize -> IO ()

sdlBindGPUIndexBuffer :: SDLGPURenderPass -> SDLGPUBufferBinding -> SDLGPUIndexElementSize -> IO ()
sdlBindGPUIndexBuffer (SDLGPURenderPass rp) binding indexElementSize =
  with binding $ \bindingPtr ->
    c_sdlBindGPUIndexBuffer rp bindingPtr indexElementSize

-- | Binds texture-sampler pairs for use on the vertex shader.
-- FFI signature uses Ptr () for the array pointer due to const qualifier.
foreign import ccall unsafe "SDL_BindGPUVertexSamplers"
  c_sdlBindGPUVertexSamplers :: Ptr SDLGPURenderPass -> CUInt -> Ptr () -> CUInt -> IO ()

sdlBindGPUVertexSamplers :: SDLGPURenderPass -> Word32 -> [SDLGPUTextureSamplerBinding] -> IO ()
sdlBindGPUVertexSamplers (SDLGPURenderPass rp) firstSlot bindings =
  withArrayLen bindings $ \numBindings bindingsPtr ->
    c_sdlBindGPUVertexSamplers rp (fromIntegral firstSlot) (castPtr bindingsPtr) (fromIntegral numBindings)

-- | Binds storage textures for use on the vertex shader.
-- FFI signature uses Ptr () for the array pointer due to const qualifier.
foreign import ccall unsafe "SDL_BindGPUVertexStorageTextures"
  c_sdlBindGPUVertexStorageTextures :: Ptr SDLGPURenderPass -> CUInt -> Ptr () -> CUInt -> IO ()

sdlBindGPUVertexStorageTextures :: SDLGPURenderPass -> Word32 -> [SDLGPUTexture] -> IO ()
sdlBindGPUVertexStorageTextures (SDLGPURenderPass rp) firstSlot textures =
  withArrayLen (map (\(SDLGPUTexture p) -> p) textures) $ \numBindings texturesPtr ->
    c_sdlBindGPUVertexStorageTextures rp (fromIntegral firstSlot) (castPtr texturesPtr) (fromIntegral numBindings)

-- | Binds storage buffers for use on the vertex shader.
-- FFI signature uses Ptr () for the array pointer due to const qualifier.
foreign import ccall unsafe "SDL_BindGPUVertexStorageBuffers"
  c_sdlBindGPUVertexStorageBuffers :: Ptr SDLGPURenderPass -> CUInt -> Ptr () -> CUInt -> IO ()

sdlBindGPUVertexStorageBuffers :: SDLGPURenderPass -> Word32 -> [SDLGPUBuffer] -> IO ()
sdlBindGPUVertexStorageBuffers (SDLGPURenderPass rp) firstSlot buffers =
  withArrayLen (map (\(SDLGPUBuffer p) -> p) buffers) $ \numBindings buffersPtr ->
    c_sdlBindGPUVertexStorageBuffers rp (fromIntegral firstSlot) (castPtr buffersPtr) (fromIntegral numBindings)

-- | Binds texture-sampler pairs for use on the fragment shader.
-- FFI signature uses Ptr () for the array pointer due to const qualifier.
foreign import ccall unsafe "SDL_BindGPUFragmentSamplers"
  c_sdlBindGPUFragmentSamplers :: Ptr SDLGPURenderPass -> CUInt -> Ptr () -> CUInt -> IO ()

sdlBindGPUFragmentSamplers :: SDLGPURenderPass -> Word32 -> [SDLGPUTextureSamplerBinding] -> IO ()
sdlBindGPUFragmentSamplers (SDLGPURenderPass rp) firstSlot bindings =
  withArrayLen bindings $ \numBindings bindingsPtr ->
    c_sdlBindGPUFragmentSamplers rp (fromIntegral firstSlot) (castPtr bindingsPtr) (fromIntegral numBindings)

-- | Binds storage textures for use on the fragment shader.
-- FFI signature uses Ptr () for the array pointer due to const qualifier.
foreign import ccall unsafe "SDL_BindGPUFragmentStorageTextures"
  c_sdlBindGPUFragmentStorageTextures :: Ptr SDLGPURenderPass -> CUInt -> Ptr () -> CUInt -> IO ()

sdlBindGPUFragmentStorageTextures :: SDLGPURenderPass -> Word32 -> [SDLGPUTexture] -> IO ()
sdlBindGPUFragmentStorageTextures (SDLGPURenderPass rp) firstSlot textures =
  withArrayLen (map (\(SDLGPUTexture p) -> p) textures) $ \numBindings texturesPtr ->
    c_sdlBindGPUFragmentStorageTextures rp (fromIntegral firstSlot) (castPtr texturesPtr) (fromIntegral numBindings)

-- | Binds storage buffers for use on the fragment shader.
-- FFI signature uses Ptr () for the array pointer due to const qualifier.
foreign import ccall unsafe "SDL_BindGPUFragmentStorageBuffers"
  c_sdlBindGPUFragmentStorageBuffers :: Ptr SDLGPURenderPass -> CUInt -> Ptr () -> CUInt -> IO ()

sdlBindGPUFragmentStorageBuffers :: SDLGPURenderPass -> Word32 -> [SDLGPUBuffer] -> IO ()
sdlBindGPUFragmentStorageBuffers (SDLGPURenderPass rp) firstSlot buffers =
  withArrayLen (map (\(SDLGPUBuffer p) -> p) buffers) $ \numBindings buffersPtr ->
    c_sdlBindGPUFragmentStorageBuffers rp (fromIntegral firstSlot) (castPtr buffersPtr) (fromIntegral numBindings)

-- Drawing

-- | Draws data using bound graphics state with an index buffer and instancing enabled.
foreign import ccall unsafe "SDL_DrawGPUIndexedPrimitives"
  c_sdlDrawGPUIndexedPrimitives :: Ptr SDLGPURenderPass -> CUInt -> CUInt -> CUInt -> CInt -> CUInt -> IO ()

sdlDrawGPUIndexedPrimitives :: SDLGPURenderPass -> Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()
sdlDrawGPUIndexedPrimitives (SDLGPURenderPass rp) numIndices numInstances firstIndex vertexOffset firstInstance =
  c_sdlDrawGPUIndexedPrimitives rp
    (fromIntegral numIndices)
    (fromIntegral numInstances)
    (fromIntegral firstIndex)
    (fromIntegral vertexOffset)
    (fromIntegral firstInstance)

-- | Draws data using bound graphics state.
foreign import ccall unsafe "SDL_DrawGPUPrimitives"
  c_sdlDrawGPUPrimitives :: Ptr SDLGPURenderPass -> CUInt -> CUInt -> CUInt -> CUInt -> IO ()

sdlDrawGPUPrimitives :: SDLGPURenderPass -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
sdlDrawGPUPrimitives (SDLGPURenderPass rp) numVertices numInstances firstVertex firstInstance =
  c_sdlDrawGPUPrimitives rp
    (fromIntegral numVertices)
    (fromIntegral numInstances)
    (fromIntegral firstVertex)
    (fromIntegral firstInstance)

-- | Draws data using bound graphics state and with draw parameters set from a buffer.
foreign import ccall unsafe "SDL_DrawGPUPrimitivesIndirect"
  c_sdlDrawGPUPrimitivesIndirect :: Ptr SDLGPURenderPass -> Ptr SDLGPUBuffer -> CUInt -> CUInt -> IO ()

sdlDrawGPUPrimitivesIndirect :: SDLGPURenderPass -> SDLGPUBuffer -> Word32 -> Word32 -> IO ()
sdlDrawGPUPrimitivesIndirect (SDLGPURenderPass rp) (SDLGPUBuffer buf) offset drawCount =
  c_sdlDrawGPUPrimitivesIndirect rp buf (fromIntegral offset) (fromIntegral drawCount)

-- | Draws data using bound graphics state with an index buffer enabled and with draw parameters set from a buffer.
foreign import ccall unsafe "SDL_DrawGPUIndexedPrimitivesIndirect"
  c_sdlDrawGPUIndexedPrimitivesIndirect :: Ptr SDLGPURenderPass -> Ptr SDLGPUBuffer -> CUInt -> CUInt -> IO ()

sdlDrawGPUIndexedPrimitivesIndirect :: SDLGPURenderPass -> SDLGPUBuffer -> Word32 -> Word32 -> IO ()
sdlDrawGPUIndexedPrimitivesIndirect (SDLGPURenderPass rp) (SDLGPUBuffer buf) offset drawCount =
  c_sdlDrawGPUIndexedPrimitivesIndirect rp buf (fromIntegral offset) (fromIntegral drawCount)

-- | Ends the given render pass.
foreign import ccall unsafe "SDL_EndGPURenderPass"
  c_sdlEndGPURenderPass :: Ptr SDLGPURenderPass -> IO ()

sdlEndGPURenderPass :: SDLGPURenderPass -> IO ()
sdlEndGPURenderPass (SDLGPURenderPass rp) = c_sdlEndGPURenderPass rp

-- Compute Pass

-- | Begins a compute pass on a command buffer.
-- FFI signature uses Ptr () for array pointers due to const qualifier.
foreign import ccall unsafe "SDL_BeginGPUComputePass"
  c_sdlBeginGPUComputePass :: Ptr SDLGPUCommandBuffer -> Ptr () -> CUInt -> Ptr () -> CUInt -> IO (Ptr SDLGPUComputePass)

sdlBeginGPUComputePass :: SDLGPUCommandBuffer -> [SDLGPUStorageTextureReadWriteBinding] -> [SDLGPUStorageBufferReadWriteBinding] -> IO (Maybe SDLGPUComputePass)
sdlBeginGPUComputePass (SDLGPUCommandBuffer cmdbuf) texBindings bufBindings =
  withArrayLen texBindings $ \numTex texPtr ->
  withArrayLen bufBindings $ \numBuf bufPtr -> do
    ptr <- c_sdlBeginGPUComputePass cmdbuf (castPtr texPtr) (fromIntegral numTex) (castPtr bufPtr) (fromIntegral numBuf)
    if ptr == nullPtr
      then return Nothing
      else return $ Just (SDLGPUComputePass ptr)

-- | Binds a compute pipeline.
foreign import ccall unsafe "SDL_BindGPUComputePipeline"
  c_sdlBindGPUComputePipeline :: Ptr SDLGPUComputePass -> Ptr SDLGPUComputePipeline -> IO ()

sdlBindGPUComputePipeline :: SDLGPUComputePass -> SDLGPUComputePipeline -> IO ()
sdlBindGPUComputePipeline (SDLGPUComputePass cp) (SDLGPUComputePipeline pipe) =
  c_sdlBindGPUComputePipeline cp pipe

-- | Binds texture-sampler pairs for use on the compute shader.
-- FFI signature uses Ptr () for the array pointer due to const qualifier.
foreign import ccall unsafe "SDL_BindGPUComputeSamplers"
  c_sdlBindGPUComputeSamplers :: Ptr SDLGPUComputePass -> CUInt -> Ptr () -> CUInt -> IO ()

sdlBindGPUComputeSamplers :: SDLGPUComputePass -> Word32 -> [SDLGPUTextureSamplerBinding] -> IO ()
sdlBindGPUComputeSamplers (SDLGPUComputePass cp) firstSlot bindings =
  withArrayLen bindings $ \numBindings bindingsPtr ->
    c_sdlBindGPUComputeSamplers cp (fromIntegral firstSlot) (castPtr bindingsPtr) (fromIntegral numBindings)

-- | Binds storage textures as readonly for use on the compute pipeline.
-- FFI signature uses Ptr () for the array pointer due to const qualifier.
foreign import ccall unsafe "SDL_BindGPUComputeStorageTextures"
  c_sdlBindGPUComputeStorageTextures :: Ptr SDLGPUComputePass -> CUInt -> Ptr () -> CUInt -> IO ()

sdlBindGPUComputeStorageTextures :: SDLGPUComputePass -> Word32 -> [SDLGPUTexture] -> IO ()
sdlBindGPUComputeStorageTextures (SDLGPUComputePass cp) firstSlot textures =
  withArrayLen (map (\(SDLGPUTexture p) -> p) textures) $ \numBindings texturesPtr ->
    c_sdlBindGPUComputeStorageTextures cp (fromIntegral firstSlot) (castPtr texturesPtr) (fromIntegral numBindings)

-- | Binds storage buffers as readonly for use on the compute pipeline.
-- FFI signature uses Ptr () for the array pointer due to const qualifier.
foreign import ccall unsafe "SDL_BindGPUComputeStorageBuffers"
  c_sdlBindGPUComputeStorageBuffers :: Ptr SDLGPUComputePass -> CUInt -> Ptr () -> CUInt -> IO ()

sdlBindGPUComputeStorageBuffers :: SDLGPUComputePass -> Word32 -> [SDLGPUBuffer] -> IO ()
sdlBindGPUComputeStorageBuffers (SDLGPUComputePass cp) firstSlot buffers =
  withArrayLen (map (\(SDLGPUBuffer p) -> p) buffers) $ \numBindings buffersPtr ->
    c_sdlBindGPUComputeStorageBuffers cp (fromIntegral firstSlot) (castPtr buffersPtr) (fromIntegral numBindings)

-- | Dispatches compute work.
foreign import ccall unsafe "SDL_DispatchGPUCompute"
  c_sdlDispatchGPUCompute :: Ptr SDLGPUComputePass -> CUInt -> CUInt -> CUInt -> IO ()

sdlDispatchGPUCompute :: SDLGPUComputePass -> Word32 -> Word32 -> Word32 -> IO ()
sdlDispatchGPUCompute (SDLGPUComputePass cp) groupCountX groupCountY groupCountZ =
  c_sdlDispatchGPUCompute cp (fromIntegral groupCountX) (fromIntegral groupCountY) (fromIntegral groupCountZ)

-- | Dispatches compute work with parameters set from a buffer.
foreign import ccall unsafe "SDL_DispatchGPUComputeIndirect"
  c_sdlDispatchGPUComputeIndirect :: Ptr SDLGPUComputePass -> Ptr SDLGPUBuffer -> CUInt -> IO ()

sdlDispatchGPUComputeIndirect :: SDLGPUComputePass -> SDLGPUBuffer -> Word32 -> IO ()
sdlDispatchGPUComputeIndirect (SDLGPUComputePass cp) (SDLGPUBuffer buf) offset =
  c_sdlDispatchGPUComputeIndirect cp buf (fromIntegral offset)

-- | Ends the current compute pass.
foreign import ccall unsafe "SDL_EndGPUComputePass"
  c_sdlEndGPUComputePass :: Ptr SDLGPUComputePass -> IO ()

sdlEndGPUComputePass :: SDLGPUComputePass -> IO ()
sdlEndGPUComputePass (SDLGPUComputePass cp) = c_sdlEndGPUComputePass cp

-- TransferBuffer Data (Already defined earlier)

-- | Maps a transfer buffer into application address space.
foreign import ccall unsafe "SDL_MapGPUTransferBuffer"
  c_sdlMapGPUTransferBuffer :: Ptr SDLGPUDevice -> Ptr SDLGPUTransferBuffer -> CBool -> IO (Ptr ())

sdlMapGPUTransferBuffer :: SDLGPUDevice -> SDLGPUTransferBuffer -> Bool -> IO (Maybe (Ptr ()))
sdlMapGPUTransferBuffer (SDLGPUDevice dev) (SDLGPUTransferBuffer tbuf) cycle = do
  ptr <- c_sdlMapGPUTransferBuffer dev tbuf (fromBool cycle)
  if ptr == nullPtr
    then return Nothing
    else return $ Just ptr

-- | Unmaps a previously mapped transfer buffer.
foreign import ccall unsafe "SDL_UnmapGPUTransferBuffer"
  c_sdlUnmapGPUTransferBuffer :: Ptr SDLGPUDevice -> Ptr SDLGPUTransferBuffer -> IO ()

sdlUnmapGPUTransferBuffer :: SDLGPUDevice -> SDLGPUTransferBuffer -> IO ()
sdlUnmapGPUTransferBuffer (SDLGPUDevice dev) (SDLGPUTransferBuffer tbuf) = c_sdlUnmapGPUTransferBuffer dev tbuf

-- Copy Pass

-- | Begins a copy pass on a command buffer.
foreign import ccall unsafe "SDL_BeginGPUCopyPass"
  c_sdlBeginGPUCopyPass :: Ptr SDLGPUCommandBuffer -> IO (Ptr SDLGPUCopyPass)

sdlBeginGPUCopyPass :: SDLGPUCommandBuffer -> IO (Maybe SDLGPUCopyPass)
sdlBeginGPUCopyPass (SDLGPUCommandBuffer cmdbuf) = do
    ptr <- c_sdlBeginGPUCopyPass cmdbuf
    if ptr == nullPtr then return Nothing else return (Just (SDLGPUCopyPass ptr))

-- | Uploads data from a transfer buffer to a texture.
foreign import ccall unsafe "SDL_UploadToGPUTexture"
  c_sdlUploadToGPUTexture :: Ptr SDLGPUCopyPass -> Ptr SDLGPUTextureTransferInfo -> Ptr SDLGPUTextureRegion -> CBool -> IO ()

sdlUploadToGPUTexture :: SDLGPUCopyPass -> SDLGPUTextureTransferInfo -> SDLGPUTextureRegion -> Bool -> IO ()
sdlUploadToGPUTexture (SDLGPUCopyPass cp) source destination cycle =
  with source $ \srcPtr ->
  with destination $ \dstPtr ->
  c_sdlUploadToGPUTexture cp srcPtr dstPtr (fromBool cycle)

-- | Uploads data from a transfer buffer to a buffer.
foreign import ccall unsafe "SDL_UploadToGPUBuffer"
  c_sdlUploadToGPUBuffer :: Ptr SDLGPUCopyPass -> Ptr SDLGPUTransferBufferLocation -> Ptr SDLGPUBufferRegion -> CBool -> IO ()

sdlUploadToGPUBuffer :: SDLGPUCopyPass -> SDLGPUTransferBufferLocation -> SDLGPUBufferRegion -> Bool -> IO ()
sdlUploadToGPUBuffer (SDLGPUCopyPass cp) source destination cycle =
  with source $ \srcPtr ->
  with destination $ \dstPtr ->
  c_sdlUploadToGPUBuffer cp srcPtr dstPtr (fromBool cycle)

-- | Performs a texture-to-texture copy.
foreign import ccall unsafe "SDL_CopyGPUTextureToTexture"
  c_sdlCopyGPUTextureToTexture :: Ptr SDLGPUCopyPass -> Ptr SDLGPUTextureLocation -> Ptr SDLGPUTextureLocation -> CUInt -> CUInt -> CUInt -> CBool -> IO ()

sdlCopyGPUTextureToTexture :: SDLGPUCopyPass -> SDLGPUTextureLocation -> SDLGPUTextureLocation -> Word32 -> Word32 -> Word32 -> Bool -> IO ()
sdlCopyGPUTextureToTexture (SDLGPUCopyPass cp) source destination w h d cycle =
  with source $ \srcPtr ->
  with destination $ \dstPtr ->
  c_sdlCopyGPUTextureToTexture cp srcPtr dstPtr (fromIntegral w) (fromIntegral h) (fromIntegral d) (fromBool cycle)

-- | Performs a buffer-to-buffer copy.
foreign import ccall unsafe "SDL_CopyGPUBufferToBuffer"
  c_sdlCopyGPUBufferToBuffer :: Ptr SDLGPUCopyPass -> Ptr SDLGPUBufferLocation -> Ptr SDLGPUBufferLocation -> CUInt -> CBool -> IO ()

sdlCopyGPUBufferToBuffer :: SDLGPUCopyPass -> SDLGPUBufferLocation -> SDLGPUBufferLocation -> Word32 -> Bool -> IO ()
sdlCopyGPUBufferToBuffer (SDLGPUCopyPass cp) source destination size cycle =
  with source $ \srcPtr ->
  with destination $ \dstPtr ->
  c_sdlCopyGPUBufferToBuffer cp srcPtr dstPtr (fromIntegral size) (fromBool cycle)

-- | Copies data from a texture to a transfer buffer on the GPU timeline.
foreign import ccall unsafe "SDL_DownloadFromGPUTexture"
  c_sdlDownloadFromGPUTexture :: Ptr SDLGPUCopyPass -> Ptr SDLGPUTextureRegion -> Ptr SDLGPUTextureTransferInfo -> IO ()

sdlDownloadFromGPUTexture :: SDLGPUCopyPass -> SDLGPUTextureRegion -> SDLGPUTextureTransferInfo -> IO ()
sdlDownloadFromGPUTexture (SDLGPUCopyPass cp) source destination =
  with source $ \srcPtr ->
  with destination $ \dstPtr ->
  c_sdlDownloadFromGPUTexture cp srcPtr dstPtr

-- | Copies data from a buffer to a transfer buffer on the GPU timeline.
foreign import ccall unsafe "SDL_DownloadFromGPUBuffer"
  c_sdlDownloadFromGPUBuffer :: Ptr SDLGPUCopyPass -> Ptr SDLGPUBufferRegion -> Ptr SDLGPUTransferBufferLocation -> IO ()

sdlDownloadFromGPUBuffer :: SDLGPUCopyPass -> SDLGPUBufferRegion -> SDLGPUTransferBufferLocation -> IO ()
sdlDownloadFromGPUBuffer (SDLGPUCopyPass cp) source destination =
  with source $ \srcPtr ->
  with destination $ \dstPtr ->
  c_sdlDownloadFromGPUBuffer cp srcPtr dstPtr

-- | Ends the current copy pass.
foreign import ccall unsafe "SDL_EndGPUCopyPass"
  c_sdlEndGPUCopyPass :: Ptr SDLGPUCopyPass -> IO ()

sdlEndGPUCopyPass :: SDLGPUCopyPass -> IO ()
sdlEndGPUCopyPass (SDLGPUCopyPass cp) = c_sdlEndGPUCopyPass cp

-- | Generates mipmaps for the given texture.
foreign import ccall unsafe "SDL_GenerateMipmapsForGPUTexture"
  c_sdlGenerateMipmapsForGPUTexture :: Ptr SDLGPUCommandBuffer -> Ptr SDLGPUTexture -> IO ()

sdlGenerateMipmapsForGPUTexture :: SDLGPUCommandBuffer -> SDLGPUTexture -> IO ()
sdlGenerateMipmapsForGPUTexture (SDLGPUCommandBuffer cmdbuf) (SDLGPUTexture tex) =
  c_sdlGenerateMipmapsForGPUTexture cmdbuf tex

-- | Blits from a source texture region to a destination texture region.
foreign import ccall unsafe "SDL_BlitGPUTexture"
  c_sdlBlitGPUTexture :: Ptr SDLGPUCommandBuffer -> Ptr SDLGPUBlitInfo -> IO ()

sdlBlitGPUTexture :: SDLGPUCommandBuffer -> SDLGPUBlitInfo -> IO ()
sdlBlitGPUTexture (SDLGPUCommandBuffer cmdbuf) blitInfo =
  with blitInfo $ c_sdlBlitGPUTexture cmdbuf

-- Submission/Presentation (Most already defined earlier)

-- | Determines whether a swapchain composition is supported by the window.
foreign import ccall unsafe "SDL_WindowSupportsGPUSwapchainComposition"
  c_sdlWindowSupportsGPUSwapchainComposition :: Ptr SDLGPUDevice -> Ptr SDLWindow -> SDLGPUSwapchainComposition -> IO CBool

sdlWindowSupportsGPUSwapchainComposition :: SDLGPUDevice -> SDLWindow -> SDLGPUSwapchainComposition -> IO Bool
sdlWindowSupportsGPUSwapchainComposition (SDLGPUDevice dev) (SDLWindow win) comp =
  fromCBool <$> c_sdlWindowSupportsGPUSwapchainComposition dev win comp

-- | Determines whether a presentation mode is supported by the window.
foreign import ccall unsafe "SDL_WindowSupportsGPUPresentMode"
  c_sdlWindowSupportsGPUPresentMode :: Ptr SDLGPUDevice -> Ptr SDLWindow -> SDLGPUPresentMode -> IO CBool

sdlWindowSupportsGPUPresentMode :: SDLGPUDevice -> SDLWindow -> SDLGPUPresentMode -> IO Bool
sdlWindowSupportsGPUPresentMode (SDLGPUDevice dev) (SDLWindow win) mode =
  fromCBool <$> c_sdlWindowSupportsGPUPresentMode dev win mode

-- | Claims a window, creating a swapchain structure for it.
foreign import ccall safe "SDL_ClaimWindowForGPUDevice" -- Safe due to window system interaction
  c_sdlClaimWindowForGPUDevice :: Ptr SDLGPUDevice -> Ptr SDLWindow -> IO CBool

sdlClaimWindowForGPUDevice :: SDLGPUDevice -> SDLWindow -> IO Bool
sdlClaimWindowForGPUDevice (SDLGPUDevice dev) (SDLWindow win) =
  fromCBool <$> c_sdlClaimWindowForGPUDevice dev win

-- | Unclaims a window, destroying its swapchain structure.
foreign import ccall safe "SDL_ReleaseWindowFromGPUDevice" -- Safe due to window system interaction
  c_sdlReleaseWindowFromGPUDevice :: Ptr SDLGPUDevice -> Ptr SDLWindow -> IO ()

sdlReleaseWindowFromGPUDevice :: SDLGPUDevice -> SDLWindow -> IO ()
sdlReleaseWindowFromGPUDevice (SDLGPUDevice dev) (SDLWindow win) =
  c_sdlReleaseWindowFromGPUDevice dev win

-- | Changes the swapchain parameters for the given claimed window.
foreign import ccall safe "SDL_SetGPUSwapchainParameters" -- Safe due to potential swapchain recreation
  c_sdlSetGPUSwapchainParameters :: Ptr SDLGPUDevice -> Ptr SDLWindow -> SDLGPUSwapchainComposition -> SDLGPUPresentMode -> IO CBool

sdlSetGPUSwapchainParameters :: SDLGPUDevice -> SDLWindow -> SDLGPUSwapchainComposition -> SDLGPUPresentMode -> IO Bool
sdlSetGPUSwapchainParameters (SDLGPUDevice dev) (SDLWindow win) comp mode =
  fromCBool <$> c_sdlSetGPUSwapchainParameters dev win comp mode

-- | Configures the maximum allowed number of frames in flight.
foreign import ccall safe "SDL_SetGPUAllowedFramesInFlight" -- Safe due to potential flush/stall
  c_sdlSetGPUAllowedFramesInFlight :: Ptr SDLGPUDevice -> CUInt -> IO CBool

sdlSetGPUAllowedFramesInFlight :: SDLGPUDevice -> Word32 -> IO Bool
sdlSetGPUAllowedFramesInFlight (SDLGPUDevice dev) allowedFrames =
  fromCBool <$> c_sdlSetGPUAllowedFramesInFlight dev (fromIntegral allowedFrames)

-- | Obtains the texture format of the swapchain for the given window.
foreign import ccall unsafe "SDL_GetGPUSwapchainTextureFormat"
  c_sdlGetGPUSwapchainTextureFormat :: Ptr SDLGPUDevice -> Ptr SDLWindow -> IO SDLGPUTextureFormat

sdlGetGPUSwapchainTextureFormat :: SDLGPUDevice -> SDLWindow -> IO SDLGPUTextureFormat
sdlGetGPUSwapchainTextureFormat (SDLGPUDevice dev) (SDLWindow win) =
  c_sdlGetGPUSwapchainTextureFormat dev win

-- | Acquire a texture to use in presentation.
-- FFI signature uses Ptr () for the double pointer to avoid C type mismatch warnings.
foreign import ccall safe "SDL_AcquireGPUSwapchainTexture" -- Safe due to potential waiting/sync
  c_sdlAcquireGPUSwapchainTexture :: Ptr SDLGPUCommandBuffer -> Ptr SDLWindow -> Ptr () -> Ptr CUInt -> Ptr CUInt -> IO CBool

sdlAcquireGPUSwapchainTexture :: SDLGPUCommandBuffer -> SDLWindow -> IO (Maybe (SDLGPUTexture, Word32, Word32))
sdlAcquireGPUSwapchainTexture (SDLGPUCommandBuffer cmdbuf) (SDLWindow win) =
  alloca $ \texPtrPtr -> -- texPtrPtr is Ptr (Ptr SDLGPUTexture)
  alloca $ \widthPtr ->
  alloca $ \heightPtr -> do
    -- Cast the Ptr (Ptr SDLGPUTexture) to Ptr () before passing to C
    success <- c_sdlAcquireGPUSwapchainTexture cmdbuf win (castPtr texPtrPtr) widthPtr heightPtr
    if not (fromCBool success)
      then return Nothing -- Error occurred
      else do
        texPtr <- peek texPtrPtr -- texPtr is Ptr SDLGPUTexture
        if texPtr == nullPtr
          then return Nothing -- Success, but no texture available (e.g., too many frames in flight)
          else do
            width <- fromIntegral <$> peek widthPtr
            height <- fromIntegral <$> peek heightPtr
            return $ Just (SDLGPUTexture texPtr, width, height) -- Wrap in newtype

-- | Blocks the thread until a swapchain texture is available to be acquired.
foreign import ccall safe "SDL_WaitForGPUSwapchain" -- Safe because it blocks
  c_sdlWaitForGPUSwapchain :: Ptr SDLGPUDevice -> Ptr SDLWindow -> IO CBool

sdlWaitForGPUSwapchain :: SDLGPUDevice -> SDLWindow -> IO Bool
sdlWaitForGPUSwapchain (SDLGPUDevice dev) (SDLWindow win) =
  fromCBool <$> c_sdlWaitForGPUSwapchain dev win

-- | Blocks the thread until a swapchain texture is available, then acquires it.
-- FFI signature uses Ptr () for the double pointer to avoid C type mismatch warnings.
foreign import ccall safe "SDL_WaitAndAcquireGPUSwapchainTexture" -- Safe because it blocks
  c_sdlWaitAndAcquireGPUSwapchainTexture :: Ptr SDLGPUCommandBuffer -> Ptr SDLWindow -> Ptr () -> Ptr CUInt -> Ptr CUInt -> IO CBool

sdlWaitAndAcquireGPUSwapchainTexture :: SDLGPUCommandBuffer -> SDLWindow -> IO (Maybe (SDLGPUTexture, Word32, Word32))
sdlWaitAndAcquireGPUSwapchainTexture (SDLGPUCommandBuffer cmdbuf) (SDLWindow win) =
  alloca $ \texPtrPtr -> -- texPtrPtr is Ptr (Ptr SDLGPUTexture)
  alloca $ \widthPtr ->
  alloca $ \heightPtr -> do
    -- Cast the Ptr (Ptr SDLGPUTexture) to Ptr () before passing to C
    success <- c_sdlWaitAndAcquireGPUSwapchainTexture cmdbuf win (castPtr texPtrPtr) widthPtr heightPtr
    if not (fromCBool success)
      then return Nothing -- Error occurred
      else do
        texPtr <- peek texPtrPtr -- texPtr is Ptr SDLGPUTexture
        if texPtr == nullPtr
          then return Nothing -- Success, but no texture acquired (e.g., minimized window)
          else do
            width <- fromIntegral <$> peek widthPtr
            height <- fromIntegral <$> peek heightPtr
            return $ Just (SDLGPUTexture texPtr, width, height) -- Wrap in newtype

-- | Submits a command buffer so its commands can be processed on the GPU.
foreign import ccall unsafe "SDL_SubmitGPUCommandBuffer"
  c_sdlSubmitGPUCommandBuffer :: Ptr SDLGPUCommandBuffer -> IO CBool

sdlSubmitGPUCommandBuffer :: SDLGPUCommandBuffer -> IO Bool
sdlSubmitGPUCommandBuffer (SDLGPUCommandBuffer cmdbuf) = fromCBool <$> c_sdlSubmitGPUCommandBuffer cmdbuf

-- | Submits a command buffer and acquires a fence.
foreign import ccall unsafe "SDL_SubmitGPUCommandBufferAndAcquireFence"
  c_sdlSubmitGPUCommandBufferAndAcquireFence :: Ptr SDLGPUCommandBuffer -> IO (Ptr SDLGPUFence)

sdlSubmitGPUCommandBufferAndAcquireFence :: SDLGPUCommandBuffer -> IO (Maybe SDLGPUFence)
sdlSubmitGPUCommandBufferAndAcquireFence (SDLGPUCommandBuffer cmdbuf) = do
  ptr <- c_sdlSubmitGPUCommandBufferAndAcquireFence cmdbuf
  if ptr == nullPtr
    then return Nothing
    else return $ Just (SDLGPUFence ptr)

-- | Cancels a command buffer.
foreign import ccall unsafe "SDL_CancelGPUCommandBuffer"
  c_sdlCancelGPUCommandBuffer :: Ptr SDLGPUCommandBuffer -> IO CBool

sdlCancelGPUCommandBuffer :: SDLGPUCommandBuffer -> IO Bool
sdlCancelGPUCommandBuffer (SDLGPUCommandBuffer cmdbuf) = fromCBool <$> c_sdlCancelGPUCommandBuffer cmdbuf

-- | Blocks the thread until the GPU is completely idle.
foreign import ccall safe "SDL_WaitForGPUIdle" -- Safe because it blocks
  c_sdlWaitForGPUIdle :: Ptr SDLGPUDevice -> IO CBool

sdlWaitForGPUIdle :: SDLGPUDevice -> IO Bool
sdlWaitForGPUIdle (SDLGPUDevice dev) = fromCBool <$> c_sdlWaitForGPUIdle dev

-- | Blocks the thread until the given fences are signaled.
-- FFI signature uses Ptr () for the double pointer to avoid C type mismatch warnings.
foreign import ccall safe "SDL_WaitForGPUFences" -- Safe because it blocks
  c_sdlWaitForGPUFences :: Ptr SDLGPUDevice -> CBool -> Ptr () -> CUInt -> IO CBool

sdlWaitForGPUFences :: SDLGPUDevice -> Bool -> [SDLGPUFence] -> IO Bool
sdlWaitForGPUFences (SDLGPUDevice dev) waitAll fences =
  withArrayLen (map (\(SDLGPUFence p) -> p) fences) $ \len fencePtrs -> -- fencePtrs is Ptr (Ptr SDLGPUFence)
    -- Cast the Ptr (Ptr SDLGPUFence) to Ptr () before passing to C
    fromCBool <$> c_sdlWaitForGPUFences dev (fromBool waitAll) (castPtr fencePtrs) (fromIntegral len)

-- | Checks the status of a fence.
foreign import ccall unsafe "SDL_QueryGPUFence"
  c_sdlQueryGPUFence :: Ptr SDLGPUDevice -> Ptr SDLGPUFence -> IO CBool

sdlQueryGPUFence :: SDLGPUDevice -> SDLGPUFence -> IO Bool
sdlQueryGPUFence (SDLGPUDevice dev) (SDLGPUFence fence) = fromCBool <$> c_sdlQueryGPUFence dev fence


-- Format Info (Already defined earlier)

-- | Obtains the texel block size for a texture format.
foreign import ccall unsafe "SDL_GPUTextureFormatTexelBlockSize"
  c_sdlGPUTextureFormatTexelBlockSize :: SDLGPUTextureFormat -> IO CUInt

sdlGPUTextureFormatTexelBlockSize :: SDLGPUTextureFormat -> IO Word32
sdlGPUTextureFormatTexelBlockSize format =
    fromIntegral <$> c_sdlGPUTextureFormatTexelBlockSize format

-- | Determines whether a texture format is supported for a given type and usage.
foreign import ccall unsafe "SDL_GPUTextureSupportsFormat"
  c_sdlGPUTextureSupportsFormat :: Ptr SDLGPUDevice -> SDLGPUTextureFormat -> SDLGPUTextureType -> SDLGPUTextureUsageFlags -> IO CBool

sdlGPUTextureSupportsFormat :: SDLGPUDevice -> SDLGPUTextureFormat -> SDLGPUTextureType -> SDLGPUTextureUsageFlags -> IO Bool
sdlGPUTextureSupportsFormat (SDLGPUDevice dev) format texType usage =
  fromCBool <$> c_sdlGPUTextureSupportsFormat dev format texType usage

-- | Determines if a sample count for a texture format is supported.
foreign import ccall unsafe "SDL_GPUTextureSupportsSampleCount"
  c_sdlGPUTextureSupportsSampleCount :: Ptr SDLGPUDevice -> SDLGPUTextureFormat -> SDLGPUSampleCount -> IO CBool

sdlGPUTextureSupportsSampleCount :: SDLGPUDevice -> SDLGPUTextureFormat -> SDLGPUSampleCount -> IO Bool
sdlGPUTextureSupportsSampleCount (SDLGPUDevice dev) format sampleCount =
  fromCBool <$> c_sdlGPUTextureSupportsSampleCount dev format sampleCount

-- | Calculate the size in bytes of a texture format with dimensions.
foreign import ccall unsafe "SDL_CalculateGPUTextureFormatSize"
  c_sdlCalculateGPUTextureFormatSize :: SDLGPUTextureFormat -> CUInt -> CUInt -> CUInt -> IO CUInt

sdlCalculateGPUTextureFormatSize :: SDLGPUTextureFormat -> Word32 -> Word32 -> Word32 -> IO Word32
sdlCalculateGPUTextureFormatSize format w h depthOrLayerCount =
  fromIntegral <$> c_sdlCalculateGPUTextureFormatSize
    format
    (fromIntegral w)
    (fromIntegral h)
    (fromIntegral depthOrLayerCount)


#ifdef SDL_PLATFORM_GDK
-- | Call this to suspend GPU operation on Xbox. (GDK Only)
foreign import ccall safe "SDL_GDKSuspendGPU" -- Safe due to interaction with OS/platform
  c_sdlGDKSuspendGPU :: Ptr SDLGPUDevice -> IO ()

sdlGDKSuspendGPU :: SDLGPUDevice -> IO ()
sdlGDKSuspendGPU (SDLGPUDevice dev) = c_sdlGDKSuspendGPU dev

-- | Call this to resume GPU operation on Xbox. (GDK Only)
foreign import ccall safe "SDL_GDKResumeGPU" -- Safe due to interaction with OS/platform
  c_sdlGDKResumeGPU :: Ptr SDLGPUDevice -> IO ()

sdlGDKResumeGPU :: SDLGPUDevice -> IO ()
sdlGDKResumeGPU (SDLGPUDevice dev) = c_sdlGDKResumeGPU dev
#endif /* SDL_PLATFORM_GDK */
