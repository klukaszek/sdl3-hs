{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms #-}

#include <SDL3/SDL_version.h>

module SDL3.Wrapped.GPU
  ( -- * Types
    -- ** Opaque Handles
    SDLGPUDevice(..)
  , SDLGPUBuffer(..)
  , SDLGPUTransferBuffer(..)
  , SDLGPUTexture(..)
  , SDLGPUSampler(..)
  , SDLGPUShader(..)
  , SDLGPUComputePipeline(..)
  , SDLGPUGraphicsPipeline(..)
  , SDLGPUCommandBuffer(..)
  , SDLGPURenderPass(..)
  , SDLGPUComputePass(..)
  , SDLGPUCopyPass(..)
  , SDLGPUFence(..)
    -- ** Enums
  , SDLGPUPrimitiveType(..)
  , SDLGPULoadOp(..)
  , SDLGPUStoreOp(..)
  , SDLGPUIndexElementSize(..)
  , SDLGPUTextureFormat(..)
  , SDLGPUTextureType(..)
  , SDLGPUSampleCount(..)
  , SDLGPUCubeMapFace(..)
  , SDLGPUTransferBufferUsage(..)
  , SDLGPUShaderStage(..)
  , SDLGPUVertexElementFormat(..)
  , SDLGPUVertexInputRate(..)
  , SDLGPUFillMode(..)
  , SDLGPUCullMode(..)
  , SDLGPUFrontFace(..)
  , SDLGPUCompareOp(..)
  , SDLGPUStencilOp(..)
  , SDLGPUBlendOp(..)
  , SDLGPUBlendFactor(..)
  , SDLGPUFilter(..)
  , SDLGPUSamplerMipmapMode(..)
  , SDLGPUSamplerAddressMode(..)
  , SDLGPUPresentMode(..)
  , SDLGPUSwapchainComposition(..)
    -- ** Bitmasks
  , SDLGPUTextureUsageFlags(..)
  , SDLGPUBufferUsageFlags(..)
  , SDLGPUShaderFormat(..)
  , SDLGPUColorComponentFlags(..)
    -- ** Structs (Simple/Storable)
  , SDLGPUViewport(..)
  , SDLGPUTextureTransferInfo(..)
  , SDLGPUTransferBufferLocation(..)
  , SDLGPUTextureLocation(..)
  , SDLGPUTextureRegion(..)
  , SDLGPUBlitRegion(..)
  , SDLGPUBufferLocation(..)
  , SDLGPUBufferRegion(..)
  , SDLGPUIndirectDrawCommand(..)
  , SDLGPUIndexedIndirectDrawCommand(..)
  , SDLGPUIndirectDispatchCommand(..)
  , SDLGPUBufferBinding(..)
  , SDLGPUTextureSamplerBinding(..)
  , SDLGPUStorageBufferReadWriteBinding(..)
  , SDLGPUStorageTextureReadWriteBinding(..)
    -- ** Structs (Create Info / State - with field accessors)
    -- *** Sampler
  , SDLGPUSamplerCreateInfo(..)
    -- *** Vertex Input
  , SDLGPUVertexBufferDescription(..)
  , SDLGPUVertexAttribute(..)
  , SDLGPUVertexInputState(..)
    -- *** Depth/Stencil/Blend
  , SDLGPUStencilOpState(..)
  , SDLGPUColorTargetBlendState(..)
    -- *** Shader
  , SDLGPUShaderCreateInfo(..)
    -- *** Resource Creation
  , SDLGPUTextureCreateInfo(..)
  , SDLGPUBufferCreateInfo(..)
  , SDLGPUTransferBufferCreateInfo(..)
    -- *** Pipeline State
  , SDLGPURasterizerState(..)
  , SDLGPUMultisampleState(..)
  , SDLGPUDepthStencilState(..)
  , SDLGPUColorTargetDescription(..)
  , SDLGPUGraphicsPipelineTargetInfo(..)
  , SDLGPUGraphicsPipelineCreateInfo(..)
  , SDLGPUComputePipelineCreateInfo(..)
    -- *** Pass Info
  , SDLGPUColorTargetInfo(..)
  , SDLGPUDepthStencilTargetInfo(..)
  , SDLGPUBlitInfo(..)

    -- ** Enums
  , pattern SDL_GPU_PRIMITIVETYPE_TRIANGLELIST
  , pattern SDL_GPU_PRIMITIVETYPE_TRIANGLESTRIP
  , pattern SDL_GPU_PRIMITIVETYPE_LINELIST
  , pattern SDL_GPU_PRIMITIVETYPE_LINESTRIP
  , pattern SDL_GPU_PRIMITIVETYPE_POINTLIST
  , pattern SDL_GPU_LOADOP_LOAD
  , pattern SDL_GPU_LOADOP_CLEAR
  , pattern SDL_GPU_LOADOP_DONT_CARE
  , pattern SDL_GPU_STOREOP_STORE
  , pattern SDL_GPU_STOREOP_DONT_CARE
  , pattern SDL_GPU_STOREOP_RESOLVE
  , pattern SDL_GPU_STOREOP_RESOLVE_AND_STORE
  , pattern SDL_GPU_INDEXELEMENTSIZE_16BIT
  , pattern SDL_GPU_INDEXELEMENTSIZE_32BIT
  , pattern SDL_GPU_TEXTUREFORMAT_INVALID
  , pattern SDL_GPU_TEXTUREFORMAT_A8_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_R8_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_R8G8_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_R16_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_R16G16_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_R16G16B16A16_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_R10G10B10A2_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_B5G6R5_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_B5G5R5A1_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_B4G4R4A4_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_B8G8R8A8_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_BC1_RGBA_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_BC2_RGBA_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_BC3_RGBA_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_BC4_R_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_BC5_RG_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_BC7_RGBA_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_BC6H_RGB_FLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_BC6H_RGB_UFLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_R8_SNORM
  , pattern SDL_GPU_TEXTUREFORMAT_R8G8_SNORM
  , pattern SDL_GPU_TEXTUREFORMAT_R8G8B8A8_SNORM
  , pattern SDL_GPU_TEXTUREFORMAT_R16_SNORM
  , pattern SDL_GPU_TEXTUREFORMAT_R16G16_SNORM
  , pattern SDL_GPU_TEXTUREFORMAT_R16G16B16A16_SNORM
  , pattern SDL_GPU_TEXTUREFORMAT_R16_FLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_R16G16_FLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_R16G16B16A16_FLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_R32_FLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_R32G32_FLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_R32G32B32A32_FLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_R11G11B10_UFLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_R8_UINT
  , pattern SDL_GPU_TEXTUREFORMAT_R8G8_UINT
  , pattern SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UINT
  , pattern SDL_GPU_TEXTUREFORMAT_R16_UINT
  , pattern SDL_GPU_TEXTUREFORMAT_R16G16_UINT
  , pattern SDL_GPU_TEXTUREFORMAT_R16G16B16A16_UINT
  , pattern SDL_GPU_TEXTUREFORMAT_R32_UINT
  , pattern SDL_GPU_TEXTUREFORMAT_R32G32_UINT
  , pattern SDL_GPU_TEXTUREFORMAT_R32G32B32A32_UINT
  , pattern SDL_GPU_TEXTUREFORMAT_R8_INT
  , pattern SDL_GPU_TEXTUREFORMAT_R8G8_INT
  , pattern SDL_GPU_TEXTUREFORMAT_R8G8B8A8_INT
  , pattern SDL_GPU_TEXTUREFORMAT_R16_INT
  , pattern SDL_GPU_TEXTUREFORMAT_R16G16_INT
  , pattern SDL_GPU_TEXTUREFORMAT_R16G16B16A16_INT
  , pattern SDL_GPU_TEXTUREFORMAT_R32_INT
  , pattern SDL_GPU_TEXTUREFORMAT_R32G32_INT
  , pattern SDL_GPU_TEXTUREFORMAT_R32G32B32A32_INT
  , pattern SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM_SRGB
  , pattern SDL_GPU_TEXTUREFORMAT_B8G8R8A8_UNORM_SRGB
  , pattern SDL_GPU_TEXTUREFORMAT_BC1_RGBA_UNORM_SRGB
  , pattern SDL_GPU_TEXTUREFORMAT_BC2_RGBA_UNORM_SRGB
  , pattern SDL_GPU_TEXTUREFORMAT_BC3_RGBA_UNORM_SRGB
  , pattern SDL_GPU_TEXTUREFORMAT_BC7_RGBA_UNORM_SRGB
  , pattern SDL_GPU_TEXTUREFORMAT_D16_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_D24_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_D32_FLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_D24_UNORM_S8_UINT
  , pattern SDL_GPU_TEXTUREFORMAT_D32_FLOAT_S8_UINT
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_4x4_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_5x4_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_5x5_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_6x5_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_6x6_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_8x5_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_8x6_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_8x8_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_10x5_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_10x6_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_10x8_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_10x10_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_12x10_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_12x12_UNORM
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_4x4_UNORM_SRGB
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_5x4_UNORM_SRGB
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_5x5_UNORM_SRGB
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_6x5_UNORM_SRGB
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_6x6_UNORM_SRGB
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_8x5_UNORM_SRGB
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_8x6_UNORM_SRGB
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_8x8_UNORM_SRGB
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_10x5_UNORM_SRGB
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_10x6_UNORM_SRGB
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_10x8_UNORM_SRGB
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_10x10_UNORM_SRGB
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_12x10_UNORM_SRGB
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_12x12_UNORM_SRGB
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_4x4_FLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_5x4_FLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_5x5_FLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_6x5_FLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_6x6_FLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_8x5_FLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_8x6_FLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_8x8_FLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_10x5_FLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_10x6_FLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_10x8_FLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_10x10_FLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_12x10_FLOAT
  , pattern SDL_GPU_TEXTUREFORMAT_ASTC_12x12_FLOAT
  , pattern SDL_GPU_TEXTURETYPE_2D
  , pattern SDL_GPU_TEXTURETYPE_2D_ARRAY
  , pattern SDL_GPU_TEXTURETYPE_3D
  , pattern SDL_GPU_TEXTURETYPE_CUBE
  , pattern SDL_GPU_TEXTURETYPE_CUBE_ARRAY
  , pattern SDL_GPU_SAMPLECOUNT_1
  , pattern SDL_GPU_SAMPLECOUNT_2
  , pattern SDL_GPU_SAMPLECOUNT_4
  , pattern SDL_GPU_SAMPLECOUNT_8
  , pattern SDL_GPU_CUBEMAPFACE_POSITIVEX
  , pattern SDL_GPU_CUBEMAPFACE_NEGATIVEX
  , pattern SDL_GPU_CUBEMAPFACE_POSITIVEY
  , pattern SDL_GPU_CUBEMAPFACE_NEGATIVEY
  , pattern SDL_GPU_CUBEMAPFACE_POSITIVEZ
  , pattern SDL_GPU_CUBEMAPFACE_NEGATIVEZ
  , pattern SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD
  , pattern SDL_GPU_TRANSFERBUFFERUSAGE_DOWNLOAD
  , pattern SDL_GPU_SHADERSTAGE_VERTEX
  , pattern SDL_GPU_SHADERSTAGE_FRAGMENT
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_INVALID
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_INT
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_INT2
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_INT3
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_INT4
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_UINT
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_UINT2
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_UINT3
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_UINT4
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_FLOAT
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_FLOAT3
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_FLOAT4
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_BYTE2
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_BYTE4
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_UBYTE2
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_UBYTE4
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_BYTE2_NORM
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_BYTE4_NORM
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_UBYTE2_NORM
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_UBYTE4_NORM
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_SHORT2
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_SHORT4
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_USHORT2
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_USHORT4
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_SHORT2_NORM
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_SHORT4_NORM
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_USHORT2_NORM
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_USHORT4_NORM
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_HALF2
  , pattern SDL_GPU_VERTEXELEMENTFORMAT_HALF4
  , pattern SDL_GPU_VERTEXINPUTRATE_VERTEX
  , pattern SDL_GPU_VERTEXINPUTRATE_INSTANCE
  , pattern SDL_GPU_FILLMODE_FILL
  , pattern SDL_GPU_FILLMODE_LINE
  , pattern SDL_GPU_CULLMODE_NONE
  , pattern SDL_GPU_CULLMODE_FRONT
  , pattern SDL_GPU_CULLMODE_BACK
  , pattern SDL_GPU_FRONTFACE_COUNTER_CLOCKWISE
  , pattern SDL_GPU_FRONTFACE_CLOCKWISE
  , pattern SDL_GPU_COMPAREOP_INVALID
  , pattern SDL_GPU_COMPAREOP_NEVER
  , pattern SDL_GPU_COMPAREOP_LESS
  , pattern SDL_GPU_COMPAREOP_EQUAL
  , pattern SDL_GPU_COMPAREOP_LESS_OR_EQUAL
  , pattern SDL_GPU_COMPAREOP_GREATER
  , pattern SDL_GPU_COMPAREOP_NOT_EQUAL
  , pattern SDL_GPU_COMPAREOP_GREATER_OR_EQUAL
  , pattern SDL_GPU_COMPAREOP_ALWAYS
  , pattern SDL_GPU_STENCILOP_INVALID
  , pattern SDL_GPU_STENCILOP_KEEP
  , pattern SDL_GPU_STENCILOP_ZERO
  , pattern SDL_GPU_STENCILOP_REPLACE
  , pattern SDL_GPU_STENCILOP_INCREMENT_AND_CLAMP
  , pattern SDL_GPU_STENCILOP_DECREMENT_AND_CLAMP
  , pattern SDL_GPU_STENCILOP_INVERT
  , pattern SDL_GPU_STENCILOP_INCREMENT_AND_WRAP
  , pattern SDL_GPU_STENCILOP_DECREMENT_AND_WRAP
  , pattern SDL_GPU_BLENDOP_INVALID
  , pattern SDL_GPU_BLENDOP_ADD
  , pattern SDL_GPU_BLENDOP_SUBTRACT
  , pattern SDL_GPU_BLENDOP_REVERSE_SUBTRACT
  , pattern SDL_GPU_BLENDOP_MIN
  , pattern SDL_GPU_BLENDOP_MAX
  , pattern SDL_GPU_BLENDFACTOR_INVALID
  , pattern SDL_GPU_BLENDFACTOR_ZERO
  , pattern SDL_GPU_BLENDFACTOR_ONE
  , pattern SDL_GPU_BLENDFACTOR_SRC_COLOR
  , pattern SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_COLOR
  , pattern SDL_GPU_BLENDFACTOR_DST_COLOR
  , pattern SDL_GPU_BLENDFACTOR_ONE_MINUS_DST_COLOR
  , pattern SDL_GPU_BLENDFACTOR_SRC_ALPHA
  , pattern SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA
  , pattern SDL_GPU_BLENDFACTOR_DST_ALPHA
  , pattern SDL_GPU_BLENDFACTOR_ONE_MINUS_DST_ALPHA
  , pattern SDL_GPU_BLENDFACTOR_CONSTANT_COLOR
  , pattern SDL_GPU_BLENDFACTOR_ONE_MINUS_CONSTANT_COLOR
  , pattern SDL_GPU_BLENDFACTOR_SRC_ALPHA_SATURATE
  , pattern SDL_GPU_FILTER_NEAREST
  , pattern SDL_GPU_FILTER_LINEAR
  , pattern SDL_GPU_SAMPLERMIPMAPMODE_NEAREST
  , pattern SDL_GPU_SAMPLERMIPMAPMODE_LINEAR
  , pattern SDL_GPU_SAMPLERADDRESSMODE_REPEAT
  , pattern SDL_GPU_SAMPLERADDRESSMODE_MIRRORED_REPEAT
  , pattern SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE
  , pattern SDL_GPU_PRESENTMODE_VSYNC
  , pattern SDL_GPU_PRESENTMODE_IMMEDIATE
  , pattern SDL_GPU_PRESENTMODE_MAILBOX
  , pattern SDL_GPU_SWAPCHAINCOMPOSITION_SDR
  , pattern SDL_GPU_SWAPCHAINCOMPOSITION_SDR_LINEAR
  , pattern SDL_GPU_SWAPCHAINCOMPOSITION_HDR_EXTENDED_LINEAR
  , pattern SDL_GPU_SWAPCHAINCOMPOSITION_HDR10_ST2084
    -- ** Bitmasks
  , pattern SDL_GPU_TEXTUREUSAGE_SAMPLER
  , pattern SDL_GPU_TEXTUREUSAGE_COLOR_TARGET
  , pattern SDL_GPU_TEXTUREUSAGE_DEPTH_STENCIL_TARGET
  , pattern SDL_GPU_TEXTUREUSAGE_GRAPHICS_STORAGE_READ
  , pattern SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_READ
  , pattern SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_WRITE
  , pattern SDL_GPU_TEXTUREUSAGE_COMPUTE_STORAGE_SIMULTANEOUS_READ_WRITE
  , pattern SDL_GPU_BUFFERUSAGE_VERTEX
  , pattern SDL_GPU_BUFFERUSAGE_INDEX
  , pattern SDL_GPU_BUFFERUSAGE_INDIRECT
  , pattern SDL_GPU_BUFFERUSAGE_GRAPHICS_STORAGE_READ
  , pattern SDL_GPU_BUFFERUSAGE_COMPUTE_STORAGE_READ
  , pattern SDL_GPU_BUFFERUSAGE_COMPUTE_STORAGE_WRITE
  , pattern SDL_GPU_SHADERFORMAT_INVALID
  , pattern SDL_GPU_SHADERFORMAT_PRIVATE
  , pattern SDL_GPU_SHADERFORMAT_SPIRV
  , pattern SDL_GPU_SHADERFORMAT_DXBC
  , pattern SDL_GPU_SHADERFORMAT_DXIL
  , pattern SDL_GPU_SHADERFORMAT_MSL
  , pattern SDL_GPU_SHADERFORMAT_METALLIB
  , pattern SDL_GPU_COLORCOMPONENT_R
  , pattern SDL_GPU_COLORCOMPONENT_G
  , pattern SDL_GPU_COLORCOMPONENT_B
  , pattern SDL_GPU_COLORCOMPONENT_A
    -- ** Property Strings
  , pattern SDL_PROP_GPU_DEVICE_CREATE_DEBUGMODE_BOOLEAN
  , pattern SDL_PROP_GPU_DEVICE_CREATE_PREFERLOWPOWER_BOOLEAN
  , pattern SDL_PROP_GPU_DEVICE_CREATE_VERBOSE_BOOLEAN
  , pattern SDL_PROP_GPU_DEVICE_CREATE_NAME_STRING
  , pattern SDL_PROP_GPU_DEVICE_CREATE_FEATURE_CLIP_DISTANCE_BOOLEAN
  , pattern SDL_PROP_GPU_DEVICE_CREATE_FEATURE_DEPTH_CLAMPING_BOOLEAN
  , pattern SDL_PROP_GPU_DEVICE_CREATE_FEATURE_INDIRECT_DRAW_FIRST_INSTANCE_BOOLEAN
  , pattern SDL_PROP_GPU_DEVICE_CREATE_FEATURE_ANISOTROPY_BOOLEAN
  , pattern SDL_PROP_GPU_DEVICE_CREATE_SHADERS_PRIVATE_BOOLEAN
  , pattern SDL_PROP_GPU_DEVICE_CREATE_SHADERS_SPIRV_BOOLEAN
  , pattern SDL_PROP_GPU_DEVICE_CREATE_SHADERS_DXBC_BOOLEAN
  , pattern SDL_PROP_GPU_DEVICE_CREATE_SHADERS_DXIL_BOOLEAN
  , pattern SDL_PROP_GPU_DEVICE_CREATE_SHADERS_MSL_BOOLEAN
  , pattern SDL_PROP_GPU_DEVICE_CREATE_SHADERS_METALLIB_BOOLEAN
  , pattern SDL_PROP_GPU_DEVICE_CREATE_D3D12_ALLOW_FEWER_RESOURCE_SLOTS_BOOLEAN
  , pattern SDL_PROP_GPU_DEVICE_CREATE_D3D12_SEMANTIC_NAME_STRING
  , pattern SDL_PROP_GPU_DEVICE_CREATE_VULKAN_REQUIRE_HARDWARE_ACCELERATION_BOOLEAN
  , pattern SDL_PROP_GPU_DEVICE_CREATE_VULKAN_OPTIONS_POINTER
#if SDL_VERSION_ATLEAST(3, 4, 0)
  , pattern SDL_PROP_GPU_DEVICE_NAME_STRING
  , pattern SDL_PROP_GPU_DEVICE_DRIVER_NAME_STRING
  , pattern SDL_PROP_GPU_DEVICE_DRIVER_VERSION_STRING
  , pattern SDL_PROP_GPU_DEVICE_DRIVER_INFO_STRING
#endif
  , pattern SDL_PROP_GPU_COMPUTEPIPELINE_CREATE_NAME_STRING
  , pattern SDL_PROP_GPU_GRAPHICSPIPELINE_CREATE_NAME_STRING
  , pattern SDL_PROP_GPU_SAMPLER_CREATE_NAME_STRING
  , pattern SDL_PROP_GPU_SHADER_CREATE_NAME_STRING
  , pattern SDL_PROP_GPU_TEXTURE_CREATE_D3D12_CLEAR_R_FLOAT
  , pattern SDL_PROP_GPU_TEXTURE_CREATE_D3D12_CLEAR_G_FLOAT
  , pattern SDL_PROP_GPU_TEXTURE_CREATE_D3D12_CLEAR_B_FLOAT
  , pattern SDL_PROP_GPU_TEXTURE_CREATE_D3D12_CLEAR_A_FLOAT
  , pattern SDL_PROP_GPU_TEXTURE_CREATE_D3D12_CLEAR_DEPTH_FLOAT
  , pattern SDL_PROP_GPU_TEXTURE_CREATE_D3D12_CLEAR_STENCIL_NUMBER
  , pattern SDL_PROP_GPU_TEXTURE_CREATE_NAME_STRING
  , pattern SDL_PROP_GPU_BUFFER_CREATE_NAME_STRING
  , pattern SDL_PROP_GPU_TRANSFERBUFFER_CREATE_NAME_STRING


    -- * Functions
    -- ** Device Management
  , sdlGPUSupportsShaderFormats
  , sdlGPUSupportsProperties
  , sdlCreateGPUDevice
  , sdlCreateGPUDeviceWithProperties
  , sdlDestroyGPUDevice
  , sdlGetNumGPUDrivers
  , sdlGetGPUDriver
  , sdlGetGPUDeviceDriver
  , sdlGetGPUShaderFormats
#if SDL_VERSION_ATLEAST(3, 4, 0)
  , sdlGetGPUDeviceProperties
  , sdlGetPixelFormatFromGPUTextureFormat
  , sdlGetGPUTextureFormatFromPixelFormat
#endif
    -- ** State Creation
  , sdlCreateGPUComputePipeline
  , sdlCreateGPUGraphicsPipeline
  , sdlCreateGPUSampler
  , sdlCreateGPUShader
  , sdlCreateGPUTexture
  , sdlCreateGPUBuffer
  , sdlCreateGPUTransferBuffer
    -- ** Debug Naming
  , sdlSetGPUBufferName
  , sdlSetGPUTextureName
  , sdlInsertGPUDebugLabel
  , sdlPushGPUDebugGroup
  , sdlPopGPUDebugGroup
    -- ** Disposal
  , sdlReleaseGPUTexture
  , sdlReleaseGPUSampler
  , sdlReleaseGPUBuffer
  , sdlReleaseGPUTransferBuffer
  , sdlReleaseGPUComputePipeline
  , sdlReleaseGPUShader
  , sdlReleaseGPUGraphicsPipeline
  , sdlReleaseGPUFence
    -- ** Command Buffer Acquisition
  , sdlAcquireGPUCommandBuffer
    -- ** Uniform Data
  , sdlPushGPUVertexUniformData
  , sdlPushGPUFragmentUniformData
  , sdlPushGPUComputeUniformData
    -- ** Graphics Pass
  , sdlBeginGPURenderPass
  , sdlBindGPUGraphicsPipeline
  , sdlSetGPUViewport
  , sdlSetGPUScissor
  , sdlSetGPUBlendConstants
  , sdlSetGPUStencilReference
  , sdlBindGPUVertexBuffers
  , sdlBindGPUIndexBuffer
  , sdlBindGPUVertexSamplers
  , sdlBindGPUVertexStorageTextures
  , sdlBindGPUVertexStorageBuffers
  , sdlBindGPUFragmentSamplers
  , sdlBindGPUFragmentStorageTextures
  , sdlBindGPUFragmentStorageBuffers
  , sdlDrawGPUIndexedPrimitives
  , sdlDrawGPUPrimitives
  , sdlDrawGPUPrimitivesIndirect
  , sdlDrawGPUIndexedPrimitivesIndirect
  , sdlEndGPURenderPass
    -- ** Compute Pass
  , sdlBeginGPUComputePass
  , sdlBindGPUComputePipeline
  , sdlBindGPUComputeSamplers
  , sdlBindGPUComputeStorageTextures
  , sdlBindGPUComputeStorageBuffers
  , sdlDispatchGPUCompute
  , sdlDispatchGPUComputeIndirect
  , sdlEndGPUComputePass
    -- ** Transfer Buffer Data
  , sdlMapGPUTransferBuffer
  , sdlUnmapGPUTransferBuffer
    -- ** Copy Pass & Related
  , sdlBeginGPUCopyPass
  , sdlUploadToGPUTexture
  , sdlUploadToGPUBuffer
  , sdlCopyGPUTextureToTexture
  , sdlCopyGPUBufferToBuffer
  , sdlDownloadFromGPUTexture
  , sdlDownloadFromGPUBuffer
  , sdlEndGPUCopyPass
  , sdlGenerateMipmapsForGPUTexture
  , sdlBlitGPUTexture
    -- ** Submission & Presentation
  , sdlWindowSupportsGPUSwapchainComposition
  , sdlWindowSupportsGPUPresentMode
  , sdlClaimWindowForGPUDevice
  , sdlReleaseWindowFromGPUDevice
  , sdlSetGPUSwapchainParameters
  , sdlSetGPUAllowedFramesInFlight
  , sdlGetGPUSwapchainTextureFormat
  , sdlAcquireGPUSwapchainTexture
  , sdlWaitForGPUSwapchain
  , sdlWaitAndAcquireGPUSwapchainTexture
  , sdlSubmitGPUCommandBuffer
  , sdlSubmitGPUCommandBufferAndAcquireFence
  , sdlCancelGPUCommandBuffer
  , sdlWaitForGPUIdle
  , sdlWaitForGPUFences
  , sdlQueryGPUFence
    -- ** Format Info
  , sdlGPUTextureFormatTexelBlockSize
  , sdlGPUTextureSupportsFormat
  , sdlGPUTextureSupportsSampleCount
  , sdlCalculateGPUTextureFormatSize
    -- ** GDK Specific (Conditional)
#ifdef SDL_PLATFORM_GDK
  , sdlGDKSuspendGPU
  , sdlGDKResumeGPU
#endif
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import SDL3.Raw.GPU hiding
  ( sdlGPUSupportsShaderFormats
  , sdlGPUSupportsProperties
  , sdlCreateGPUDevice
  , sdlCreateGPUDeviceWithProperties
  , sdlDestroyGPUDevice
  , sdlGetNumGPUDrivers
  , sdlGetGPUDriver
  , sdlGetGPUDeviceDriver
  , sdlGetGPUShaderFormats
  , sdlGetGPUDeviceProperties
  , sdlGetPixelFormatFromGPUTextureFormat
  , sdlGetGPUTextureFormatFromPixelFormat
  , sdlCreateGPUComputePipeline
  , sdlCreateGPUGraphicsPipeline
  , sdlCreateGPUSampler
  , sdlCreateGPUShader
  , sdlCreateGPUTexture
  , sdlCreateGPUBuffer
  , sdlCreateGPUTransferBuffer
  , sdlSetGPUBufferName
  , sdlSetGPUTextureName
  , sdlInsertGPUDebugLabel
  , sdlPushGPUDebugGroup
  , sdlPopGPUDebugGroup
  , sdlReleaseGPUTexture
  , sdlReleaseGPUSampler
  , sdlReleaseGPUBuffer
  , sdlReleaseGPUTransferBuffer
  , sdlReleaseGPUComputePipeline
  , sdlReleaseGPUShader
  , sdlReleaseGPUGraphicsPipeline
  , sdlReleaseGPUFence
  , sdlAcquireGPUCommandBuffer
  , sdlPushGPUVertexUniformData
  , sdlPushGPUFragmentUniformData
  , sdlPushGPUComputeUniformData
  , sdlBeginGPURenderPass
  , sdlBindGPUGraphicsPipeline
  , sdlSetGPUViewport
  , sdlSetGPUScissor
  , sdlSetGPUBlendConstants
  , sdlSetGPUStencilReference
  , sdlBindGPUVertexBuffers
  , sdlBindGPUIndexBuffer
  , sdlBindGPUVertexSamplers
  , sdlBindGPUVertexStorageTextures
  , sdlBindGPUVertexStorageBuffers
  , sdlBindGPUFragmentSamplers
  , sdlBindGPUFragmentStorageTextures
  , sdlBindGPUFragmentStorageBuffers
  , sdlDrawGPUIndexedPrimitives
  , sdlDrawGPUPrimitives
  , sdlDrawGPUPrimitivesIndirect
  , sdlDrawGPUIndexedPrimitivesIndirect
  , sdlEndGPURenderPass
  , sdlBeginGPUComputePass
  , sdlBindGPUComputePipeline
  , sdlBindGPUComputeSamplers
  , sdlBindGPUComputeStorageTextures
  , sdlBindGPUComputeStorageBuffers
  , sdlDispatchGPUCompute
  , sdlDispatchGPUComputeIndirect
  , sdlEndGPUComputePass
  , sdlMapGPUTransferBuffer
  , sdlUnmapGPUTransferBuffer
  , sdlBeginGPUCopyPass
  , sdlUploadToGPUTexture
  , sdlUploadToGPUBuffer
  , sdlCopyGPUTextureToTexture
  , sdlCopyGPUBufferToBuffer
  , sdlDownloadFromGPUTexture
  , sdlDownloadFromGPUBuffer
  , sdlEndGPUCopyPass
  , sdlGenerateMipmapsForGPUTexture
  , sdlBlitGPUTexture
  , sdlWindowSupportsGPUSwapchainComposition
  , sdlWindowSupportsGPUPresentMode
  , sdlClaimWindowForGPUDevice
  , sdlReleaseWindowFromGPUDevice
  , sdlSetGPUSwapchainParameters
  , sdlSetGPUAllowedFramesInFlight
  , sdlGetGPUSwapchainTextureFormat
  , sdlAcquireGPUSwapchainTexture
  , sdlWaitForGPUSwapchain
  , sdlWaitAndAcquireGPUSwapchainTexture
  , sdlSubmitGPUCommandBuffer
  , sdlSubmitGPUCommandBufferAndAcquireFence
  , sdlCancelGPUCommandBuffer
  , sdlWaitForGPUIdle
  , sdlWaitForGPUFences
  , sdlQueryGPUFence
  , sdlGPUTextureFormatTexelBlockSize
  , sdlGPUTextureSupportsFormat
  , sdlGPUTextureSupportsSampleCount
  , sdlCalculateGPUTextureFormatSize
  , sdlGDKSuspendGPU
  , sdlGDKResumeGPU
  )
import qualified SDL3.Raw.GPU as Raw

liftIO0 :: MonadIO m => IO a -> m a
liftIO0 = liftIO

liftIO1 :: MonadIO m => (x1 -> IO r) -> x1 -> m r
liftIO1 f x1 = liftIO (f x1)

liftIO2 :: MonadIO m => (x1 -> x2 -> IO r) -> x1 -> x2 -> m r
liftIO2 f x1 x2 = liftIO (f x1 x2)

liftIO3 :: MonadIO m => (x1 -> x2 -> x3 -> IO r) -> x1 -> x2 -> x3 -> m r
liftIO3 f x1 x2 x3 = liftIO (f x1 x2 x3)

liftIO4 :: MonadIO m => (x1 -> x2 -> x3 -> x4 -> IO r) -> x1 -> x2 -> x3 -> x4 -> m r
liftIO4 f x1 x2 x3 x4 = liftIO (f x1 x2 x3 x4)

liftIO5 :: MonadIO m => (x1 -> x2 -> x3 -> x4 -> x5 -> IO r) -> x1 -> x2 -> x3 -> x4 -> x5 -> m r
liftIO5 f x1 x2 x3 x4 x5 = liftIO (f x1 x2 x3 x4 x5)

liftIO6 :: MonadIO m => (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> IO r) -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> m r
liftIO6 f x1 x2 x3 x4 x5 x6 = liftIO (f x1 x2 x3 x4 x5 x6)

liftIO7 :: MonadIO m => (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> IO r) -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> m r
liftIO7 f x1 x2 x3 x4 x5 x6 x7 = liftIO (f x1 x2 x3 x4 x5 x6 x7)

#if SDL_VERSION_ATLEAST(3, 4, 0)
#endif
sdlGPUSupportsShaderFormats = liftIO2 Raw.sdlGPUSupportsShaderFormats
sdlGPUSupportsProperties = liftIO1 Raw.sdlGPUSupportsProperties
sdlCreateGPUDevice = liftIO3 Raw.sdlCreateGPUDevice
sdlCreateGPUDeviceWithProperties = liftIO1 Raw.sdlCreateGPUDeviceWithProperties
sdlDestroyGPUDevice = liftIO1 Raw.sdlDestroyGPUDevice
sdlGetNumGPUDrivers = liftIO0 Raw.sdlGetNumGPUDrivers
sdlGetGPUDriver = liftIO1 Raw.sdlGetGPUDriver
sdlGetGPUDeviceDriver = liftIO1 Raw.sdlGetGPUDeviceDriver
sdlGetGPUShaderFormats = liftIO1 Raw.sdlGetGPUShaderFormats
#if SDL_VERSION_ATLEAST(3, 4, 0)
sdlGetGPUDeviceProperties = liftIO1 Raw.sdlGetGPUDeviceProperties
sdlGetPixelFormatFromGPUTextureFormat = liftIO1 Raw.sdlGetPixelFormatFromGPUTextureFormat
sdlGetGPUTextureFormatFromPixelFormat = liftIO1 Raw.sdlGetGPUTextureFormatFromPixelFormat
#endif
sdlCreateGPUComputePipeline = liftIO2 Raw.sdlCreateGPUComputePipeline
sdlCreateGPUGraphicsPipeline = liftIO2 Raw.sdlCreateGPUGraphicsPipeline
sdlCreateGPUSampler = liftIO2 Raw.sdlCreateGPUSampler
sdlCreateGPUShader = liftIO2 Raw.sdlCreateGPUShader
sdlCreateGPUTexture = liftIO2 Raw.sdlCreateGPUTexture
sdlCreateGPUBuffer = liftIO2 Raw.sdlCreateGPUBuffer
sdlCreateGPUTransferBuffer = liftIO2 Raw.sdlCreateGPUTransferBuffer
sdlSetGPUBufferName = liftIO3 Raw.sdlSetGPUBufferName
sdlSetGPUTextureName = liftIO3 Raw.sdlSetGPUTextureName
sdlInsertGPUDebugLabel = liftIO2 Raw.sdlInsertGPUDebugLabel
sdlPushGPUDebugGroup = liftIO2 Raw.sdlPushGPUDebugGroup
sdlPopGPUDebugGroup = liftIO1 Raw.sdlPopGPUDebugGroup
sdlReleaseGPUTexture = liftIO2 Raw.sdlReleaseGPUTexture
sdlReleaseGPUSampler = liftIO2 Raw.sdlReleaseGPUSampler
sdlReleaseGPUBuffer = liftIO2 Raw.sdlReleaseGPUBuffer
sdlReleaseGPUTransferBuffer = liftIO2 Raw.sdlReleaseGPUTransferBuffer
sdlReleaseGPUComputePipeline = liftIO2 Raw.sdlReleaseGPUComputePipeline
sdlReleaseGPUShader = liftIO2 Raw.sdlReleaseGPUShader
sdlReleaseGPUGraphicsPipeline = liftIO2 Raw.sdlReleaseGPUGraphicsPipeline
sdlReleaseGPUFence = liftIO2 Raw.sdlReleaseGPUFence
sdlAcquireGPUCommandBuffer = liftIO1 Raw.sdlAcquireGPUCommandBuffer
sdlPushGPUVertexUniformData = liftIO3 Raw.sdlPushGPUVertexUniformData
sdlPushGPUFragmentUniformData = liftIO3 Raw.sdlPushGPUFragmentUniformData
sdlPushGPUComputeUniformData = liftIO3 Raw.sdlPushGPUComputeUniformData
sdlBeginGPURenderPass = liftIO3 Raw.sdlBeginGPURenderPass
sdlBindGPUGraphicsPipeline = liftIO2 Raw.sdlBindGPUGraphicsPipeline
sdlSetGPUViewport = liftIO2 Raw.sdlSetGPUViewport
sdlSetGPUScissor = liftIO2 Raw.sdlSetGPUScissor
sdlSetGPUBlendConstants = liftIO2 Raw.sdlSetGPUBlendConstants
sdlSetGPUStencilReference = liftIO2 Raw.sdlSetGPUStencilReference
sdlBindGPUVertexBuffers = liftIO3 Raw.sdlBindGPUVertexBuffers
sdlBindGPUIndexBuffer = liftIO3 Raw.sdlBindGPUIndexBuffer
sdlBindGPUVertexSamplers = liftIO3 Raw.sdlBindGPUVertexSamplers
sdlBindGPUVertexStorageTextures = liftIO3 Raw.sdlBindGPUVertexStorageTextures
sdlBindGPUVertexStorageBuffers = liftIO3 Raw.sdlBindGPUVertexStorageBuffers
sdlBindGPUFragmentSamplers = liftIO3 Raw.sdlBindGPUFragmentSamplers
sdlBindGPUFragmentStorageTextures = liftIO3 Raw.sdlBindGPUFragmentStorageTextures
sdlBindGPUFragmentStorageBuffers = liftIO3 Raw.sdlBindGPUFragmentStorageBuffers
sdlDrawGPUIndexedPrimitives = liftIO6 Raw.sdlDrawGPUIndexedPrimitives
sdlDrawGPUPrimitives = liftIO5 Raw.sdlDrawGPUPrimitives
sdlDrawGPUPrimitivesIndirect = liftIO4 Raw.sdlDrawGPUPrimitivesIndirect
sdlDrawGPUIndexedPrimitivesIndirect = liftIO4 Raw.sdlDrawGPUIndexedPrimitivesIndirect
sdlEndGPURenderPass = liftIO1 Raw.sdlEndGPURenderPass
sdlBeginGPUComputePass = liftIO3 Raw.sdlBeginGPUComputePass
sdlBindGPUComputePipeline = liftIO2 Raw.sdlBindGPUComputePipeline
sdlBindGPUComputeSamplers = liftIO3 Raw.sdlBindGPUComputeSamplers
sdlBindGPUComputeStorageTextures = liftIO3 Raw.sdlBindGPUComputeStorageTextures
sdlBindGPUComputeStorageBuffers = liftIO3 Raw.sdlBindGPUComputeStorageBuffers
sdlDispatchGPUCompute = liftIO4 Raw.sdlDispatchGPUCompute
sdlDispatchGPUComputeIndirect = liftIO3 Raw.sdlDispatchGPUComputeIndirect
sdlEndGPUComputePass = liftIO1 Raw.sdlEndGPUComputePass
sdlMapGPUTransferBuffer = liftIO3 Raw.sdlMapGPUTransferBuffer
sdlUnmapGPUTransferBuffer = liftIO2 Raw.sdlUnmapGPUTransferBuffer
sdlBeginGPUCopyPass = liftIO1 Raw.sdlBeginGPUCopyPass
sdlUploadToGPUTexture = liftIO4 Raw.sdlUploadToGPUTexture
sdlUploadToGPUBuffer = liftIO4 Raw.sdlUploadToGPUBuffer
sdlCopyGPUTextureToTexture = liftIO7 Raw.sdlCopyGPUTextureToTexture
sdlCopyGPUBufferToBuffer = liftIO5 Raw.sdlCopyGPUBufferToBuffer
sdlDownloadFromGPUTexture = liftIO3 Raw.sdlDownloadFromGPUTexture
sdlDownloadFromGPUBuffer = liftIO3 Raw.sdlDownloadFromGPUBuffer
sdlEndGPUCopyPass = liftIO1 Raw.sdlEndGPUCopyPass
sdlGenerateMipmapsForGPUTexture = liftIO2 Raw.sdlGenerateMipmapsForGPUTexture
sdlBlitGPUTexture = liftIO2 Raw.sdlBlitGPUTexture
sdlWindowSupportsGPUSwapchainComposition = liftIO3 Raw.sdlWindowSupportsGPUSwapchainComposition
sdlWindowSupportsGPUPresentMode = liftIO3 Raw.sdlWindowSupportsGPUPresentMode
sdlClaimWindowForGPUDevice = liftIO2 Raw.sdlClaimWindowForGPUDevice
sdlReleaseWindowFromGPUDevice = liftIO2 Raw.sdlReleaseWindowFromGPUDevice
sdlSetGPUSwapchainParameters = liftIO4 Raw.sdlSetGPUSwapchainParameters
sdlSetGPUAllowedFramesInFlight = liftIO2 Raw.sdlSetGPUAllowedFramesInFlight
sdlGetGPUSwapchainTextureFormat = liftIO2 Raw.sdlGetGPUSwapchainTextureFormat
sdlAcquireGPUSwapchainTexture = liftIO2 Raw.sdlAcquireGPUSwapchainTexture
sdlWaitForGPUSwapchain = liftIO2 Raw.sdlWaitForGPUSwapchain
sdlWaitAndAcquireGPUSwapchainTexture = liftIO2 Raw.sdlWaitAndAcquireGPUSwapchainTexture
sdlSubmitGPUCommandBuffer = liftIO1 Raw.sdlSubmitGPUCommandBuffer
sdlSubmitGPUCommandBufferAndAcquireFence = liftIO1 Raw.sdlSubmitGPUCommandBufferAndAcquireFence
sdlCancelGPUCommandBuffer = liftIO1 Raw.sdlCancelGPUCommandBuffer
sdlWaitForGPUIdle = liftIO1 Raw.sdlWaitForGPUIdle
sdlWaitForGPUFences = liftIO3 Raw.sdlWaitForGPUFences
sdlQueryGPUFence = liftIO2 Raw.sdlQueryGPUFence
sdlGPUTextureFormatTexelBlockSize = liftIO1 Raw.sdlGPUTextureFormatTexelBlockSize
sdlGPUTextureSupportsFormat = liftIO4 Raw.sdlGPUTextureSupportsFormat
sdlGPUTextureSupportsSampleCount = liftIO3 Raw.sdlGPUTextureSupportsSampleCount
sdlCalculateGPUTextureFormatSize = liftIO4 Raw.sdlCalculateGPUTextureFormatSize
#ifdef SDL_PLATFORM_GDK
sdlGDKSuspendGPU = liftIO1 Raw.sdlGDKSuspendGPU
sdlGDKResumeGPU = liftIO1 Raw.sdlGDKResumeGPU
#endif
