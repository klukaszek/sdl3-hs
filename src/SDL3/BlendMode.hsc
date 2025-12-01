{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

{-|
Module      : SDL.BlendMode
Description : Blend mode functionality
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Blend modes decide how two colors will mix together. There are both
standard modes for basic needs and a means to create custom modes,
dictating what sort of math to do on what color components.
-}

module SDL3.BlendMode
  ( -- * Blend Mode Types
    SDLBlendMode(..)
  , pattern SDL_BLENDMODE_NONE
  , pattern SDL_BLENDMODE_BLEND
  , pattern SDL_BLENDMODE_BLEND_PREMULTIPLIED
  , pattern SDL_BLENDMODE_ADD
  , pattern SDL_BLENDMODE_ADD_PREMULTIPLIED
  , pattern SDL_BLENDMODE_MOD
  , pattern SDL_BLENDMODE_MUL
  , pattern SDL_BLENDMODE_INVALID

  , SDLBlendOperation(..)
  , pattern SDL_BLENDOPERATION_ADD
  , pattern SDL_BLENDOPERATION_SUBTRACT
  , pattern SDL_BLENDOPERATION_REV_SUBTRACT
  , pattern SDL_BLENDOPERATION_MINIMUM
  , pattern SDL_BLENDOPERATION_MAXIMUM

  , SDLBlendFactor(..)
  , pattern SDL_BLENDFACTOR_ZERO
  , pattern SDL_BLENDFACTOR_ONE
  , pattern SDL_BLENDFACTOR_SRC_COLOR
  , pattern SDL_BLENDFACTOR_ONE_MINUS_SRC_COLOR
  , pattern SDL_BLENDFACTOR_SRC_ALPHA
  , pattern SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA
  , pattern SDL_BLENDFACTOR_DST_COLOR
  , pattern SDL_BLENDFACTOR_ONE_MINUS_DST_COLOR
  , pattern SDL_BLENDFACTOR_DST_ALPHA
  , pattern SDL_BLENDFACTOR_ONE_MINUS_DST_ALPHA

   -- * Custom Blend Mode Functions
  , sdlComposeCustomBlendMode
  ) where

import Data.Bits (Bits(..))
import Foreign.Storable (Storable(..))
import Data.Word
import Foreign.C.Types

#include <SDL3/SDL_blendmode.h>

newtype SDLBlendMode = SDLBlendMode Word32
  deriving newtype (Show, Eq, Ord, Storable, Bits, Num)

-- | No blending: dstRGBA = srcRGBA
pattern SDL_BLENDMODE_NONE               = (#{const SDL_BLENDMODE_NONE}) :: SDLBlendMode
-- | Alpha blending: dstRGB = (srcRGB * srcA) + (dstRGB * (1-srcA)), dstA = srcA + (dstA * (1-srcA))
pattern SDL_BLENDMODE_BLEND              = (#{const SDL_BLENDMODE_BLEND}) :: SDLBlendMode
-- | Pre-multiplied alpha blending: dstRGBA = srcRGBA + (dstRGBA * (1-srcA))
pattern SDL_BLENDMODE_BLEND_PREMULTIPLIED = (#{const SDL_BLENDMODE_BLEND_PREMULTIPLIED}) :: SDLBlendMode
-- | Additive blending: dstRGB = (srcRGB * srcA) + dstRGB, dstA = dstA
pattern SDL_BLENDMODE_ADD                = (#{const SDL_BLENDMODE_ADD}) :: SDLBlendMode
-- | Pre-multiplied additive blending: dstRGB = srcRGB + dstRGB, dstA = dstA
pattern SDL_BLENDMODE_ADD_PREMULTIPLIED  = (#{const SDL_BLENDMODE_ADD_PREMULTIPLIED}) :: SDLBlendMode
-- | Color modulate: dstRGB = srcRGB * dstRGB, dstA = dstA
pattern SDL_BLENDMODE_MOD                = (#{const SDL_BLENDMODE_MOD}) :: SDLBlendMode
-- | Color multiply: dstRGB = (srcRGB * dstRGB) + (dstRGB * (1-srcA)), dstA = dstA
pattern SDL_BLENDMODE_MUL                = (#{const SDL_BLENDMODE_MUL}) :: SDLBlendMode
-- | Invalid blend mode
pattern SDL_BLENDMODE_INVALID            = (#{const SDL_BLENDMODE_INVALID}) :: SDLBlendMode

-- | The blend operation used when combining source and destination pixel components.
newtype SDLBlendOperation = SDLBlendOperation CInt
  deriving newtype (Show, Eq, Ord, Storable, Bits, Num)

pattern SDL_BLENDOPERATION_ADD          =  (#{const SDL_BLENDOPERATION_ADD}) ::  SDLBlendOperation         -- ^ dst + src: supported by all renderers
pattern SDL_BLENDOPERATION_SUBTRACT     = (#{const SDL_BLENDOPERATION_SUBTRACT}) :: SDLBlendOperation     -- ^ src - dst: supported by D3D, OpenGL, OpenGLES, and Vulkan
pattern SDL_BLENDOPERATION_REV_SUBTRACT = (#{const SDL_BLENDOPERATION_REV_SUBTRACT}) :: SDLBlendOperation -- ^ dst - src: supported by D3D, OpenGL, OpenGLES, and Vulkan
pattern SDL_BLENDOPERATION_MINIMUM      = (#{const SDL_BLENDOPERATION_MINIMUM}) :: SDLBlendOperation      -- ^ min(dst, src): supported by D3D, OpenGL, OpenGLES, and Vulkan
pattern SDL_BLENDOPERATION_MAXIMUM      = (#{const SDL_BLENDOPERATION_MAXIMUM}) :: SDLBlendOperation     -- ^ max(dst, src): supported by D3D, OpenGL, OpenGLES, and Vulkan

-- | The normalized factor used to multiply pixel components.
newtype SDLBlendFactor = SDLBlendFactor CInt
  deriving newtype (Show, Eq, Ord, Storable, Bits, Num)

pattern SDL_BLENDFACTOR_ZERO                = #{const SDL_BLENDFACTOR_ZERO} :: SDLBlendFactor                -- ^ 0, 0, 0, 0
pattern SDL_BLENDFACTOR_ONE                 =  #{const SDL_BLENDFACTOR_ONE} :: SDLBlendFactor                 -- ^ 1, 1, 1, 1
pattern SDL_BLENDFACTOR_SRC_COLOR           =  #{const SDL_BLENDFACTOR_SRC_COLOR} :: SDLBlendFactor           -- ^ srcR, srcG, srcB, srcA
pattern SDL_BLENDFACTOR_ONE_MINUS_SRC_COLOR =  #{const SDL_BLENDFACTOR_ONE_MINUS_SRC_COLOR} :: SDLBlendFactor -- ^ 1-srcR, 1-srcG, 1-srcB, 1-srcA
pattern SDL_BLENDFACTOR_SRC_ALPHA           =  #{const SDL_BLENDFACTOR_SRC_ALPHA} :: SDLBlendFactor           -- ^ srcA, srcA, srcA, srcA
pattern SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA =  #{const SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA} :: SDLBlendFactor -- ^ 1-srcA, 1-srcA, 1-srcA, 1-srcA
pattern SDL_BLENDFACTOR_DST_COLOR           =  #{const SDL_BLENDFACTOR_DST_COLOR} :: SDLBlendFactor           -- ^ dstR, dstG, dstB, dstA
pattern SDL_BLENDFACTOR_ONE_MINUS_DST_COLOR =  #{const SDL_BLENDFACTOR_ONE_MINUS_DST_COLOR} :: SDLBlendFactor -- ^ 1-dstR, 1-dstG, 1-dstB, 1-dstA
pattern SDL_BLENDFACTOR_DST_ALPHA           =  #{const SDL_BLENDFACTOR_DST_ALPHA} :: SDLBlendFactor           -- ^ dstA, dstA, dstA, dstA
pattern SDL_BLENDFACTOR_ONE_MINUS_DST_ALPHA =  #{const SDL_BLENDFACTOR_ONE_MINUS_DST_ALPHA} :: SDLBlendFactor -- ^ 1-dstA, 1-dstA, 1-dstA, 1-dstA

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
foreign import ccall "SDL_ComposeCustomBlendMode"
  sdlComposeCustomBlendModeRaw :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO SDLBlendMode

-- | Compose a custom blend mode for renderers
sdlComposeCustomBlendMode :: SDLBlendFactor -> SDLBlendFactor -> SDLBlendOperation ->
                             SDLBlendFactor -> SDLBlendFactor -> SDLBlendOperation ->
                             IO SDLBlendMode
sdlComposeCustomBlendMode (SDLBlendFactor srcColorFactor) (SDLBlendFactor dstColorFactor) (SDLBlendOperation colorOperation)
                          (SDLBlendFactor srcAlphaFactor) (SDLBlendFactor dstAlphaFactor) (SDLBlendOperation alphaOperation) =
  sdlComposeCustomBlendModeRaw
    srcColorFactor
    dstColorFactor
    colorOperation
    srcAlphaFactor
    dstAlphaFactor
    alphaOperation
