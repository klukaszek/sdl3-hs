{-|
Module      : SDL.BlendMode
Description : Blend mode functionality
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Blend modes decide how two colors will mix together. There are both
standard modes for basic needs and a means to create custom modes,
dictating what sort of math to do on what color components.
-}

module SDL.BlendMode
  ( -- * Blend Mode Types
    SDLBlendMode
  , SDLBlendOperation(..)
  , SDLBlendFactor(..)
    
    -- * Predefined Blend Modes
  , sdlBlendModeNone
  , sdlBlendModeBlend
  , sdlBlendModeBlendPremultiplied
  , sdlBlendModeAdd
  , sdlBlendModeAddPremultiplied
  , sdlBlendModeMod
  , sdlBlendModeMul
  , sdlBlendModeInvalid
    
    -- * Custom Blend Mode Functions
  , sdlComposeCustomBlendMode
  ) where

import Data.Word
import Foreign.C.Types

-- | A set of blend modes used in drawing operations
--
-- These predefined blend modes are supported everywhere.
--
-- Additional values may be obtained from 'sdlComposeCustomBlendMode'.
--
-- @since 3.2.0
type SDLBlendMode = Word32

-- | No blending: dstRGBA = srcRGBA
sdlBlendModeNone :: SDLBlendMode
sdlBlendModeNone = 0x00000000

-- | Alpha blending: dstRGB = (srcRGB * srcA) + (dstRGB * (1-srcA)), dstA = srcA + (dstA * (1-srcA))
sdlBlendModeBlend :: SDLBlendMode
sdlBlendModeBlend = 0x00000001

-- | Pre-multiplied alpha blending: dstRGBA = srcRGBA + (dstRGBA * (1-srcA))
sdlBlendModeBlendPremultiplied :: SDLBlendMode
sdlBlendModeBlendPremultiplied = 0x00000010

-- | Additive blending: dstRGB = (srcRGB * srcA) + dstRGB, dstA = dstA
sdlBlendModeAdd :: SDLBlendMode
sdlBlendModeAdd = 0x00000002

-- | Pre-multiplied additive blending: dstRGB = srcRGB + dstRGB, dstA = dstA
sdlBlendModeAddPremultiplied :: SDLBlendMode
sdlBlendModeAddPremultiplied = 0x00000020

-- | Color modulate: dstRGB = srcRGB * dstRGB, dstA = dstA
sdlBlendModeMod :: SDLBlendMode
sdlBlendModeMod = 0x00000004

-- | Color multiply: dstRGB = (srcRGB * dstRGB) + (dstRGB * (1-srcA)), dstA = dstA
sdlBlendModeMul :: SDLBlendMode
sdlBlendModeMul = 0x00000008

-- | Invalid blend mode
sdlBlendModeInvalid :: SDLBlendMode
sdlBlendModeInvalid = 0x7FFFFFFF

-- | The blend operation used when combining source and destination pixel components
--
-- @since 3.2.0
data SDLBlendOperation
  = SDLBlendOperationAdd          -- ^ dst + src: supported by all renderers
  | SDLBlendOperationSubtract     -- ^ src - dst: supported by D3D, OpenGL, OpenGLES, and Vulkan
  | SDLBlendOperationRevSubtract  -- ^ dst - src: supported by D3D, OpenGL, OpenGLES, and Vulkan
  | SDLBlendOperationMinimum      -- ^ min(dst, src): supported by D3D, OpenGL, OpenGLES, and Vulkan
  | SDLBlendOperationMaximum      -- ^ max(dst, src): supported by D3D, OpenGL, OpenGLES, and Vulkan
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Convert SDLBlendOperation to CInt for FFI
blendOperationToCInt :: SDLBlendOperation -> CInt
blendOperationToCInt SDLBlendOperationAdd         = 0x1
blendOperationToCInt SDLBlendOperationSubtract    = 0x2
blendOperationToCInt SDLBlendOperationRevSubtract = 0x3
blendOperationToCInt SDLBlendOperationMinimum     = 0x4
blendOperationToCInt SDLBlendOperationMaximum     = 0x5

-- | The normalized factor used to multiply pixel components
--
-- The blend factors are multiplied with the pixels from a drawing operation
-- (src) and the pixels from the render target (dst) before the blend
-- operation. The comma-separated factors listed above are always applied in
-- the component order red, green, blue, and alpha.
--
-- @since 3.2.0
data SDLBlendFactor
  = SDLBlendFactorZero                -- ^ 0, 0, 0, 0
  | SDLBlendFactorOne                 -- ^ 1, 1, 1, 1
  | SDLBlendFactorSrcColor            -- ^ srcR, srcG, srcB, srcA
  | SDLBlendFactorOneMinusSrcColor    -- ^ 1-srcR, 1-srcG, 1-srcB, 1-srcA
  | SDLBlendFactorSrcAlpha            -- ^ srcA, srcA, srcA, srcA
  | SDLBlendFactorOneMinusSrcAlpha    -- ^ 1-srcA, 1-srcA, 1-srcA, 1-srcA
  | SDLBlendFactorDstColor            -- ^ dstR, dstG, dstB, dstA
  | SDLBlendFactorOneMinusDstColor    -- ^ 1-dstR, 1-dstG, 1-dstB, 1-dstA
  | SDLBlendFactorDstAlpha            -- ^ dstA, dstA, dstA, dstA
  | SDLBlendFactorOneMinusDstAlpha    -- ^ 1-dstA, 1-dstA, 1-dstA, 1-dstA
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Convert SDLBlendFactor to CInt for FFI
blendFactorToCInt :: SDLBlendFactor -> CInt
blendFactorToCInt SDLBlendFactorZero                = 0x1
blendFactorToCInt SDLBlendFactorOne                 = 0x2
blendFactorToCInt SDLBlendFactorSrcColor            = 0x3
blendFactorToCInt SDLBlendFactorOneMinusSrcColor    = 0x4
blendFactorToCInt SDLBlendFactorSrcAlpha            = 0x5
blendFactorToCInt SDLBlendFactorOneMinusSrcAlpha    = 0x6
blendFactorToCInt SDLBlendFactorDstColor            = 0x7
blendFactorToCInt SDLBlendFactorOneMinusDstColor    = 0x8
blendFactorToCInt SDLBlendFactorDstAlpha            = 0x9
blendFactorToCInt SDLBlendFactorOneMinusDstAlpha    = 0xA

-- | Compose a custom blend mode for renderers
--
-- The functions SDL_SetRenderDrawBlendMode and SDL_SetTextureBlendMode accept
-- the SDL_BlendMode returned by this function if the renderer supports it.
--
-- A blend mode controls how the pixels from a drawing operation (source) get
-- combined with the pixels from the render target (destination). First, the
-- components of the source and destination pixels get multiplied with their
-- blend factors. Then, the blend operation takes the two products and
-- calculates the result that will get stored in the render target.
--
-- Expressed in pseudocode, it would look like this:
--
-- @
-- dstRGB = colorOperation(srcRGB * srcColorFactor, dstRGB * dstColorFactor);
-- dstA = alphaOperation(srcA * srcAlphaFactor, dstA * dstAlphaFactor);
-- @
--
-- The red, green, and blue components are always multiplied with the first,
-- second, and third components of the SDL_BlendFactor, respectively. The
-- fourth component is not used.
--
-- The alpha component is always multiplied with the fourth component of the
-- SDL_BlendFactor. The other components are not used in the alpha
-- calculation.
--
-- Support for these blend modes varies for each renderer.
--
-- @since 3.2.0
foreign import ccall "SDL_ComposeCustomBlendMode"
  sdlComposeCustomBlendModeRaw :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO SDLBlendMode

-- | Compose a custom blend mode for renderers
--
-- @since 3.2.0
sdlComposeCustomBlendMode :: SDLBlendFactor -> SDLBlendFactor -> SDLBlendOperation -> 
                             SDLBlendFactor -> SDLBlendFactor -> SDLBlendOperation -> 
                             IO SDLBlendMode
sdlComposeCustomBlendMode srcColorFactor dstColorFactor colorOperation 
                          srcAlphaFactor dstAlphaFactor alphaOperation =
  sdlComposeCustomBlendModeRaw 
    (blendFactorToCInt srcColorFactor)
    (blendFactorToCInt dstColorFactor)
    (blendOperationToCInt colorOperation)
    (blendFactorToCInt srcAlphaFactor)
    (blendFactorToCInt dstAlphaFactor)
    (blendOperationToCInt alphaOperation)
