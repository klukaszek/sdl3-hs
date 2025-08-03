-- SDL/Render.hsc
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

-- |
-- Module      : SDL.Render
-- Description : Haskell bindings to SDL_render.h
-- Copyright   : (c)  Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Bindings to the SDL 2D rendering API.
-- See <https://wiki.libsdl.org/SDL3/CategoryRender>

module SDL.Render
  ( -- * Types
    -- ** Opaque Handles
    SDLRenderer(..)
  , SDLTexture(..)
  , SDLGPURenderState(..)
    -- ** Enums
  , SDLTextureAccess(..)
  , SDLTextureAddressMode(..)
  , SDLRendererLogicalPresentation(..)
    -- ** Structs
  , SDLVertex(..)
  , SDLGPURenderStateDesc(..) -- Note: CreateInfo struct

    -- * Pattern Synonyms
    -- ** Constants
  , pattern SDL_SOFTWARE_RENDERER
  , pattern SDL_DEBUG_TEXT_FONT_CHARACTER_SIZE
  , pattern SDL_RENDERER_VSYNC_DISABLED
  , pattern SDL_RENDERER_VSYNC_ADAPTIVE
    -- ** Enums
  , pattern SDL_TEXTUREACCESS_STATIC
  , pattern SDL_TEXTUREACCESS_STREAMING
  , pattern SDL_TEXTUREACCESS_TARGET
  , pattern SDL_TEXTURE_ADDRESS_INVALID
  , pattern SDL_TEXTURE_ADDRESS_AUTO
  , pattern SDL_TEXTURE_ADDRESS_CLAMP
  , pattern SDL_TEXTURE_ADDRESS_WRAP
  , pattern SDL_LOGICAL_PRESENTATION_DISABLED
  , pattern SDL_LOGICAL_PRESENTATION_STRETCH
  , pattern SDL_LOGICAL_PRESENTATION_LETTERBOX
  , pattern SDL_LOGICAL_PRESENTATION_OVERSCAN
  , pattern SDL_LOGICAL_PRESENTATION_INTEGER_SCALE
    -- ** Property Strings (Renderer Creation)
  , pattern SDL_PROP_RENDERER_CREATE_NAME_STRING
  , pattern SDL_PROP_RENDERER_CREATE_WINDOW_POINTER
  , pattern SDL_PROP_RENDERER_CREATE_SURFACE_POINTER
  , pattern SDL_PROP_RENDERER_CREATE_OUTPUT_COLORSPACE_NUMBER
  , pattern SDL_PROP_RENDERER_CREATE_PRESENT_VSYNC_NUMBER
  , pattern SDL_PROP_RENDERER_CREATE_GPU_SHADERS_SPIRV_BOOLEAN
  , pattern SDL_PROP_RENDERER_CREATE_GPU_SHADERS_DXIL_BOOLEAN
  , pattern SDL_PROP_RENDERER_CREATE_GPU_SHADERS_MSL_BOOLEAN
  , pattern SDL_PROP_RENDERER_CREATE_VULKAN_INSTANCE_POINTER
  , pattern SDL_PROP_RENDERER_CREATE_VULKAN_SURFACE_NUMBER
  , pattern SDL_PROP_RENDERER_CREATE_VULKAN_PHYSICAL_DEVICE_POINTER
  , pattern SDL_PROP_RENDERER_CREATE_VULKAN_DEVICE_POINTER
  , pattern SDL_PROP_RENDERER_CREATE_VULKAN_GRAPHICS_QUEUE_FAMILY_INDEX_NUMBER
  , pattern SDL_PROP_RENDERER_CREATE_VULKAN_PRESENT_QUEUE_FAMILY_INDEX_NUMBER
    -- ** Property Strings (Renderer Query)
  , pattern SDL_PROP_RENDERER_NAME_STRING
  , pattern SDL_PROP_RENDERER_WINDOW_POINTER
  , pattern SDL_PROP_RENDERER_SURFACE_POINTER
  , pattern SDL_PROP_RENDERER_VSYNC_NUMBER
  , pattern SDL_PROP_RENDERER_MAX_TEXTURE_SIZE_NUMBER
  , pattern SDL_PROP_RENDERER_TEXTURE_FORMATS_POINTER
  , pattern SDL_PROP_RENDERER_OUTPUT_COLORSPACE_NUMBER
  , pattern SDL_PROP_RENDERER_HDR_ENABLED_BOOLEAN
  , pattern SDL_PROP_RENDERER_SDR_WHITE_POINT_FLOAT
  , pattern SDL_PROP_RENDERER_HDR_HEADROOM_FLOAT
  , pattern SDL_PROP_RENDERER_D3D9_DEVICE_POINTER
  , pattern SDL_PROP_RENDERER_D3D11_DEVICE_POINTER
  , pattern SDL_PROP_RENDERER_D3D11_SWAPCHAIN_POINTER
  , pattern SDL_PROP_RENDERER_D3D12_DEVICE_POINTER
  , pattern SDL_PROP_RENDERER_D3D12_SWAPCHAIN_POINTER
  , pattern SDL_PROP_RENDERER_D3D12_COMMAND_QUEUE_POINTER
  , pattern SDL_PROP_RENDERER_VULKAN_INSTANCE_POINTER
  , pattern SDL_PROP_RENDERER_VULKAN_SURFACE_NUMBER
  , pattern SDL_PROP_RENDERER_VULKAN_PHYSICAL_DEVICE_POINTER
  , pattern SDL_PROP_RENDERER_VULKAN_DEVICE_POINTER
  , pattern SDL_PROP_RENDERER_VULKAN_GRAPHICS_QUEUE_FAMILY_INDEX_NUMBER
  , pattern SDL_PROP_RENDERER_VULKAN_PRESENT_QUEUE_FAMILY_INDEX_NUMBER
  , pattern SDL_PROP_RENDERER_VULKAN_SWAPCHAIN_IMAGE_COUNT_NUMBER
  , pattern SDL_PROP_RENDERER_GPU_DEVICE_POINTER
    -- ** Property Strings (Texture Creation)
  , pattern SDL_PROP_TEXTURE_CREATE_COLORSPACE_NUMBER
  , pattern SDL_PROP_TEXTURE_CREATE_FORMAT_NUMBER
  , pattern SDL_PROP_TEXTURE_CREATE_ACCESS_NUMBER
  , pattern SDL_PROP_TEXTURE_CREATE_WIDTH_NUMBER
  , pattern SDL_PROP_TEXTURE_CREATE_HEIGHT_NUMBER
  , pattern SDL_PROP_TEXTURE_CREATE_SDR_WHITE_POINT_FLOAT
  , pattern SDL_PROP_TEXTURE_CREATE_HDR_HEADROOM_FLOAT
  , pattern SDL_PROP_TEXTURE_CREATE_D3D11_TEXTURE_POINTER
  , pattern SDL_PROP_TEXTURE_CREATE_D3D11_TEXTURE_U_POINTER
  , pattern SDL_PROP_TEXTURE_CREATE_D3D11_TEXTURE_V_POINTER
  , pattern SDL_PROP_TEXTURE_CREATE_D3D12_TEXTURE_POINTER
  , pattern SDL_PROP_TEXTURE_CREATE_D3D12_TEXTURE_U_POINTER
  , pattern SDL_PROP_TEXTURE_CREATE_D3D12_TEXTURE_V_POINTER
  , pattern SDL_PROP_TEXTURE_CREATE_METAL_PIXELBUFFER_POINTER
  , pattern SDL_PROP_TEXTURE_CREATE_OPENGL_TEXTURE_NUMBER
  , pattern SDL_PROP_TEXTURE_CREATE_OPENGL_TEXTURE_UV_NUMBER
  , pattern SDL_PROP_TEXTURE_CREATE_OPENGL_TEXTURE_U_NUMBER
  , pattern SDL_PROP_TEXTURE_CREATE_OPENGL_TEXTURE_V_NUMBER
  , pattern SDL_PROP_TEXTURE_CREATE_OPENGLES2_TEXTURE_NUMBER
  , pattern SDL_PROP_TEXTURE_CREATE_OPENGLES2_TEXTURE_UV_NUMBER
  , pattern SDL_PROP_TEXTURE_CREATE_OPENGLES2_TEXTURE_U_NUMBER
  , pattern SDL_PROP_TEXTURE_CREATE_OPENGLES2_TEXTURE_V_NUMBER
  , pattern SDL_PROP_TEXTURE_CREATE_VULKAN_TEXTURE_NUMBER
    -- ** Property Strings (Texture Query)
  , pattern SDL_PROP_TEXTURE_COLORSPACE_NUMBER
  , pattern SDL_PROP_TEXTURE_FORMAT_NUMBER
  , pattern SDL_PROP_TEXTURE_ACCESS_NUMBER
  , pattern SDL_PROP_TEXTURE_WIDTH_NUMBER
  , pattern SDL_PROP_TEXTURE_HEIGHT_NUMBER
  , pattern SDL_PROP_TEXTURE_SDR_WHITE_POINT_FLOAT
  , pattern SDL_PROP_TEXTURE_HDR_HEADROOM_FLOAT
  , pattern SDL_PROP_TEXTURE_D3D11_TEXTURE_POINTER
  , pattern SDL_PROP_TEXTURE_D3D11_TEXTURE_U_POINTER
  , pattern SDL_PROP_TEXTURE_D3D11_TEXTURE_V_POINTER
  , pattern SDL_PROP_TEXTURE_D3D12_TEXTURE_POINTER
  , pattern SDL_PROP_TEXTURE_D3D12_TEXTURE_U_POINTER
  , pattern SDL_PROP_TEXTURE_D3D12_TEXTURE_V_POINTER
  , pattern SDL_PROP_TEXTURE_OPENGL_TEXTURE_NUMBER
  , pattern SDL_PROP_TEXTURE_OPENGL_TEXTURE_UV_NUMBER
  , pattern SDL_PROP_TEXTURE_OPENGL_TEXTURE_U_NUMBER
  , pattern SDL_PROP_TEXTURE_OPENGL_TEXTURE_V_NUMBER
  , pattern SDL_PROP_TEXTURE_OPENGL_TEXTURE_TARGET_NUMBER
  , pattern SDL_PROP_TEXTURE_OPENGL_TEX_W_FLOAT
  , pattern SDL_PROP_TEXTURE_OPENGL_TEX_H_FLOAT
  , pattern SDL_PROP_TEXTURE_OPENGLES2_TEXTURE_NUMBER
  , pattern SDL_PROP_TEXTURE_OPENGLES2_TEXTURE_UV_NUMBER
  , pattern SDL_PROP_TEXTURE_OPENGLES2_TEXTURE_U_NUMBER
  , pattern SDL_PROP_TEXTURE_OPENGLES2_TEXTURE_V_NUMBER
  , pattern SDL_PROP_TEXTURE_OPENGLES2_TEXTURE_TARGET_NUMBER
  , pattern SDL_PROP_TEXTURE_VULKAN_TEXTURE_NUMBER

    -- * Functions
    -- ** Driver Info
  , sdlGetNumRenderDrivers
  , sdlGetRenderDriver
    -- ** Renderer Creation / Destruction
  , sdlCreateWindowAndRenderer
  , sdlCreateRenderer
  , sdlCreateRendererWithProperties
  , sdlCreateGPURenderer
  , sdlCreateSoftwareRenderer
  , sdlDestroyRenderer
    -- ** Renderer Query
  , sdlGetRenderer
  , sdlGetRenderWindow
  , sdlGetRendererName
  , sdlGetRendererProperties
  , sdlGetRenderOutputSize
  , sdlGetCurrentRenderOutputSize
    -- ** Texture Creation / Destruction
  , sdlCreateTexture
  , sdlCreateTextureFromSurface
  , sdlCreateTextureWithProperties
  , sdlDestroyTexture
    -- ** Texture Query
  , sdlGetTextureProperties
  , sdlGetRendererFromTexture
  , sdlGetTextureSize
    -- ** Texture Manipulation
  , sdlSetTextureColorMod
  , sdlSetTextureColorModFloat
  , sdlGetTextureColorMod
  , sdlGetTextureColorModFloat
  , sdlSetTextureAlphaMod
  , sdlSetTextureAlphaModFloat
  , sdlGetTextureAlphaMod
  , sdlGetTextureAlphaModFloat
  , sdlSetTextureBlendMode
  , sdlGetTextureBlendMode
  , sdlSetTextureScaleMode
  , sdlGetTextureScaleMode
  , sdlUpdateTexture
  , sdlUpdateYUVTexture
  , sdlUpdateNVTexture
  , sdlLockTexture
  , sdlLockTextureToSurface
  , sdlUnlockTexture
    -- ** Render Target
  , sdlSetRenderTarget
  , sdlGetRenderTarget
    -- ** Logical Presentation & Coordinates
  , sdlSetRenderLogicalPresentation
  , sdlGetRenderLogicalPresentation
  , sdlGetRenderLogicalPresentationRect
  , sdlRenderCoordinatesFromWindow
  , sdlRenderCoordinatesToWindow
  , sdlConvertEventToRenderCoordinates
    -- ** Viewport & Clipping
  , sdlSetRenderViewport
  , sdlGetRenderViewport
  , sdlRenderViewportSet
  , sdlGetRenderSafeArea
  , sdlSetRenderClipRect
  , sdlGetRenderClipRect
  , sdlRenderClipEnabled
    -- ** Render State
  , sdlSetRenderScale
  , sdlGetRenderScale
  , sdlSetRenderDrawColor
  , sdlSetRenderDrawColorFloat
  , sdlGetRenderDrawColor
  , sdlGetRenderDrawColorFloat
  , sdlSetRenderColorScale
  , sdlGetRenderColorScale
  , sdlSetRenderDrawBlendMode
  , sdlGetRenderDrawBlendMode
  , sdlSetRenderTextureAddressMode
  , sdlGetRenderTextureAddressMode
    -- ** Clearing
  , sdlRenderClear
    -- ** Drawing Primitives
  , sdlRenderPoint
  , sdlRenderPoints
  , sdlRenderLine
  , sdlRenderLines
  , sdlRenderRect
  , sdlRenderRects
  , sdlRenderFillRect
  , sdlRenderFillRects
    -- ** Drawing Textures
  , sdlRenderTexture
  , sdlRenderTextureRotated
  , sdlRenderTextureAffine
  , sdlRenderTextureTiled
  , sdlRenderTexture9Grid
  , sdlRenderTexture9GridTiled
    -- ** Drawing Geometry
  , sdlRenderGeometry
  , sdlRenderGeometryRaw
    -- ** Reading Pixels
  , sdlRenderReadPixels
    -- ** Presentation
  , sdlRenderPresent
  , sdlSetRenderVSync
  , sdlGetRenderVSync
    -- ** Synchronization / Backend Access
  , sdlFlushRenderer
  , sdlGetRenderMetalLayer
  , sdlGetRenderMetalCommandEncoder
  , sdlAddVulkanRenderSemaphores
    -- ** Debugging
  , sdlRenderDebugText
  , sdlRenderDebugTextFormat
    -- ** Texture Defaults
  , sdlSetDefaultTextureScaleMode
  , sdlGetDefaultTextureScaleMode
    -- ** Custom GPU State (GPU Renderer)
  , sdlCreateGPURenderState
  , sdlSetGPURenderStateFragmentUniforms
  , sdlSetRenderGPUState
  , sdlDestroyGPURenderState
  ) where

#include <SDL3/SDL_render.h>
#include <SDL3/SDL_version.h>

-- Haskell Imports
import Foreign.Ptr (Ptr, nullPtr, FunPtr, castPtr, plusPtr)
import Foreign.C.Types
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.Marshal.Alloc (alloca, malloc, free, mallocBytes, allocaBytes)
import Foreign.Marshal.Array (withArray, withArrayLen, peekArray, allocaArray)
import Foreign.Marshal.Utils (copyBytes, fromBool, toBool, with, maybeWith, new, withMany)
import Foreign.Storable (Storable(..))
import Data.Word (Word8, Word16, Word32)
import Data.Int (Int32, Int64)
import Data.Bits (Bits, (.|.))
import Data.Maybe (fromMaybe)
import Control.Monad (when, unless, forM_, void)
import Control.Exception (bracket)

-- SDL Imports
import SDL.BlendMode (SDLBlendMode(..))
import SDL.Error (sdlGetError)
import SDL.Events (SDLEvent) -- Assuming Ptr SDLEvent underneath
import SDL.Pixels (SDLPixelFormat(..), SDLFColor(..), pixelFormatToCUInt)
import SDL.Properties (SDLPropertiesID(..))
import SDL.Rect (SDLRect(..), SDLFPoint(..), SDLFRect(..))
import SDL.Surface (SDLSurface(..), SDLFlipMode(..), SDLScaleMode(..))
import SDL.Video (SDLWindow(..), SDLWindowFlags)
import SDL.GPU (SDLGPUShaderFormat(..), SDLGPUDevice(..), SDLGPUShader(..), SDLGPUTextureSamplerBinding(..), SDLGPUTexture(..), SDLGPUBuffer(..)) -- Import necessary GPU types

-- Opaque Handle Types
newtype SDLRenderer = SDLRenderer (Ptr SDLRenderer) deriving (Show, Eq)
newtype SDLTexture = SDLTexture (Ptr SDLTexture) deriving (Show, Eq)
newtype SDLGPURenderState = SDLGPURenderState (Ptr SDLGPURenderState) deriving (Show, Eq)

-- Enum Types
newtype SDLTextureAccess = SDLTextureAccess CInt deriving (Show, Eq, Storable)

pattern SDL_TEXTUREACCESS_STATIC :: SDLTextureAccess
pattern SDL_TEXTUREACCESS_STATIC = SDLTextureAccess #{const SDL_TEXTUREACCESS_STATIC}
pattern SDL_TEXTUREACCESS_STREAMING :: SDLTextureAccess
pattern SDL_TEXTUREACCESS_STREAMING = SDLTextureAccess #{const SDL_TEXTUREACCESS_STREAMING}
pattern SDL_TEXTUREACCESS_TARGET :: SDLTextureAccess
pattern SDL_TEXTUREACCESS_TARGET = SDLTextureAccess #{const SDL_TEXTUREACCESS_TARGET}

newtype SDLTextureAddressMode = SDLTextureAddressMode CInt deriving (Show, Eq, Storable)

pattern SDL_TEXTURE_ADDRESS_INVALID :: SDLTextureAddressMode
pattern SDL_TEXTURE_ADDRESS_INVALID = SDLTextureAddressMode (-1)
pattern SDL_TEXTURE_ADDRESS_AUTO :: SDLTextureAddressMode
pattern SDL_TEXTURE_ADDRESS_AUTO = SDLTextureAddressMode #{const SDL_TEXTURE_ADDRESS_AUTO}
pattern SDL_TEXTURE_ADDRESS_CLAMP :: SDLTextureAddressMode
pattern SDL_TEXTURE_ADDRESS_CLAMP = SDLTextureAddressMode #{const SDL_TEXTURE_ADDRESS_CLAMP}
pattern SDL_TEXTURE_ADDRESS_WRAP :: SDLTextureAddressMode
pattern SDL_TEXTURE_ADDRESS_WRAP = SDLTextureAddressMode #{const SDL_TEXTURE_ADDRESS_WRAP}

newtype SDLRendererLogicalPresentation = SDLRendererLogicalPresentation CInt deriving (Show, Eq, Storable)

pattern SDL_LOGICAL_PRESENTATION_DISABLED :: SDLRendererLogicalPresentation
pattern SDL_LOGICAL_PRESENTATION_DISABLED = SDLRendererLogicalPresentation #{const SDL_LOGICAL_PRESENTATION_DISABLED}
pattern SDL_LOGICAL_PRESENTATION_STRETCH :: SDLRendererLogicalPresentation
pattern SDL_LOGICAL_PRESENTATION_STRETCH = SDLRendererLogicalPresentation #{const SDL_LOGICAL_PRESENTATION_STRETCH}
pattern SDL_LOGICAL_PRESENTATION_LETTERBOX :: SDLRendererLogicalPresentation
pattern SDL_LOGICAL_PRESENTATION_LETTERBOX = SDLRendererLogicalPresentation #{const SDL_LOGICAL_PRESENTATION_LETTERBOX}
pattern SDL_LOGICAL_PRESENTATION_OVERSCAN :: SDLRendererLogicalPresentation
pattern SDL_LOGICAL_PRESENTATION_OVERSCAN = SDLRendererLogicalPresentation #{const SDL_LOGICAL_PRESENTATION_OVERSCAN}
pattern SDL_LOGICAL_PRESENTATION_INTEGER_SCALE :: SDLRendererLogicalPresentation
pattern SDL_LOGICAL_PRESENTATION_INTEGER_SCALE = SDLRendererLogicalPresentation #{const SDL_LOGICAL_PRESENTATION_INTEGER_SCALE}

-- Struct Types
data SDLVertex = SDLVertex
    { vertexPosition :: SDLFPoint
    , vertexColor    :: SDLFColor
    , vertexTexCoord :: SDLFPoint
    } deriving (Show, Eq)

instance Storable SDLVertex where
    sizeOf _ = #{size SDL_Vertex}
    alignment _ = #{alignment SDL_Vertex}
    peek ptr = do
        pos <- peek (#{ptr SDL_Vertex, position} ptr :: Ptr SDLFPoint)
        col <- peek (#{ptr SDL_Vertex, color} ptr    :: Ptr SDLFColor)
        tex <- peek (#{ptr SDL_Vertex, tex_coord} ptr :: Ptr SDLFPoint)
        return SDLVertex { vertexPosition = pos, vertexColor = col, vertexTexCoord = tex }
    poke ptr SDLVertex{..} = do
        poke (#{ptr SDL_Vertex, position} ptr) vertexPosition
        poke (#{ptr SDL_Vertex, color} ptr)    vertexColor
        poke (#{ptr SDL_Vertex, tex_coord} ptr) vertexTexCoord

-- Constants
pattern SDL_SOFTWARE_RENDERER :: String
pattern SDL_SOFTWARE_RENDERER = "software"

pattern SDL_DEBUG_TEXT_FONT_CHARACTER_SIZE :: Int
pattern SDL_DEBUG_TEXT_FONT_CHARACTER_SIZE = #{const SDL_DEBUG_TEXT_FONT_CHARACTER_SIZE}

pattern SDL_RENDERER_VSYNC_DISABLED :: Int
pattern SDL_RENDERER_VSYNC_DISABLED = #{const SDL_RENDERER_VSYNC_DISABLED}
pattern SDL_RENDERER_VSYNC_ADAPTIVE :: Int
pattern SDL_RENDERER_VSYNC_ADAPTIVE = #{const SDL_RENDERER_VSYNC_ADAPTIVE}


-- Function Bindings

-- ** Driver Info **
foreign import ccall unsafe "SDL_GetNumRenderDrivers"
  c_sdlGetNumRenderDrivers :: IO CInt

sdlGetNumRenderDrivers :: IO Int
sdlGetNumRenderDrivers = fromIntegral <$> c_sdlGetNumRenderDrivers

foreign import ccall unsafe "SDL_GetRenderDriver"
  c_sdlGetRenderDriver :: CInt -> IO CString

sdlGetRenderDriver :: Int -> IO (Maybe String)
sdlGetRenderDriver index
  | index < 0 = return Nothing
  | otherwise = do
      numDrivers <- sdlGetNumRenderDrivers
      if index >= numDrivers
        then return Nothing
        else do
          cstr <- c_sdlGetRenderDriver (fromIntegral index)
          if cstr == nullPtr
            then return Nothing
            else Just <$> peekCString cstr

-- ** Renderer Creation / Destruction **
foreign import ccall safe "SDL_CreateWindowAndRenderer" -- Safe due to OS interaction
  c_sdlCreateWindowAndRenderer :: CString -> CInt -> CInt -> CUInt -> Ptr (Ptr SDLWindow) -> Ptr (Ptr SDLRenderer) -> IO CBool

-- Update Haskell wrapper ONE MORE TIME
sdlCreateWindowAndRenderer :: String -> Int -> Int -> CUInt -> IO (Maybe (SDLWindow, SDLRenderer))
sdlCreateWindowAndRenderer title width height flags = -- Changed type here
  withCString title $ \cTitle ->
  alloca $ \windowPtrPtr ->
  alloca $ \rendererPtrPtr -> do
    -- No need for fromIntegral anymore
    success <- c_sdlCreateWindowAndRenderer cTitle (fromIntegral width) (fromIntegral height) flags windowPtrPtr rendererPtrPtr
    if not (toBool success)
      then return Nothing
      else do
        windowPtr <- peek windowPtrPtr
        rendererPtr <- peek rendererPtrPtr
        if windowPtr == nullPtr || rendererPtr == nullPtr
          then return Nothing
          else return $ Just (SDLWindow windowPtr, SDLRenderer rendererPtr)

foreign import ccall safe "SDL_CreateRenderer" -- Safe due to window system interaction
  c_sdlCreateRenderer :: Ptr SDLWindow -> CString -> IO (Ptr SDLRenderer)

sdlCreateRenderer :: SDLWindow -> Maybe String -> IO (Maybe SDLRenderer)
sdlCreateRenderer (SDLWindow win) mName =
  maybeWith withCString mName $ \cName -> do
    ptr <- c_sdlCreateRenderer win cName
    if ptr == nullPtr
      then return Nothing
      else return $ Just (SDLRenderer ptr)

foreign import ccall safe "SDL_CreateRendererWithProperties" -- Safe due to potential OS interaction
  c_sdlCreateRendererWithProperties :: SDLPropertiesID -> IO (Ptr SDLRenderer)

sdlCreateRendererWithProperties :: SDLPropertiesID -> IO (Maybe SDLRenderer)
sdlCreateRendererWithProperties props = do
  ptr <- c_sdlCreateRendererWithProperties props
  if ptr == nullPtr
    then return Nothing
    else return $ Just (SDLRenderer ptr)

foreign import ccall safe "SDL_CreateGPURenderer" -- Safe due to GPU device creation
    c_sdlCreateGPURenderer :: Ptr SDLWindow -> SDLGPUShaderFormat -> Ptr (Ptr SDLGPUDevice) -> IO (Ptr SDLRenderer)

sdlCreateGPURenderer :: SDLWindow -> SDLGPUShaderFormat -> IO (Maybe (SDLRenderer, SDLGPUDevice))
sdlCreateGPURenderer (SDLWindow win) formatFlags =
    alloca $ \devicePtrPtr -> do
        rendererPtr <- c_sdlCreateGPURenderer win formatFlags devicePtrPtr
        if rendererPtr == nullPtr
            then return Nothing
            else do
                devicePtr <- peek devicePtrPtr
                if devicePtr == nullPtr -- Should not happen if renderer creation succeeded
                    then return Nothing -- Or perhaps destroy the renderer and return Nothing? Depends on SDL guarantees.
                    else return $ Just (SDLRenderer rendererPtr, SDLGPUDevice devicePtr)

foreign import ccall unsafe "SDL_CreateSoftwareRenderer"
  c_sdlCreateSoftwareRenderer :: Ptr SDLSurface -> IO (Ptr SDLRenderer)

sdlCreateSoftwareRenderer :: Ptr SDLSurface -> IO (Maybe SDLRenderer) -- Accepts raw Ptr SDLSurface
sdlCreateSoftwareRenderer surface = do
  ptr <- c_sdlCreateSoftwareRenderer surface
  if ptr == nullPtr
    then return Nothing
    else return $ Just (SDLRenderer ptr)

foreign import ccall safe "SDL_DestroyRenderer" -- Safe due to potential cleanup
  c_sdlDestroyRenderer :: Ptr SDLRenderer -> IO ()

sdlDestroyRenderer :: SDLRenderer -> IO ()
sdlDestroyRenderer (SDLRenderer renderer) = c_sdlDestroyRenderer renderer

-- ** Renderer Query **
foreign import ccall unsafe "SDL_GetRenderer"
  c_sdlGetRenderer :: Ptr SDLWindow -> IO (Ptr SDLRenderer)

sdlGetRenderer :: SDLWindow -> IO (Maybe SDLRenderer)
sdlGetRenderer (SDLWindow win) = do
  ptr <- c_sdlGetRenderer win
  if ptr == nullPtr
    then return Nothing
    else return $ Just (SDLRenderer ptr)

foreign import ccall unsafe "SDL_GetRenderWindow"
  c_sdlGetRenderWindow :: Ptr SDLRenderer -> IO (Ptr SDLWindow)

sdlGetRenderWindow :: SDLRenderer -> IO (Maybe SDLWindow)
sdlGetRenderWindow (SDLRenderer renderer) = do
  ptr <- c_sdlGetRenderWindow renderer
  if ptr == nullPtr
    then return Nothing
    else return $ Just (SDLWindow ptr)

foreign import ccall unsafe "SDL_GetRendererName"
  c_sdlGetRendererName :: Ptr SDLRenderer -> IO CString

sdlGetRendererName :: SDLRenderer -> IO (Maybe String)
sdlGetRendererName (SDLRenderer renderer) = do
  cstr <- c_sdlGetRendererName renderer
  if cstr == nullPtr
    then return Nothing
    else Just <$> peekCString cstr

foreign import ccall unsafe "SDL_GetRendererProperties"
  c_sdlGetRendererProperties :: Ptr SDLRenderer -> IO SDLPropertiesID

sdlGetRendererProperties :: SDLRenderer -> IO SDLPropertiesID
sdlGetRendererProperties (SDLRenderer renderer) = c_sdlGetRendererProperties renderer

foreign import ccall unsafe "SDL_GetRenderOutputSize"
  c_sdlGetRenderOutputSize :: Ptr SDLRenderer -> Ptr CInt -> Ptr CInt -> IO CBool

sdlGetRenderOutputSize :: SDLRenderer -> IO (Maybe (Int, Int))
sdlGetRenderOutputSize (SDLRenderer renderer) =
  alloca $ \wPtr ->
  alloca $ \hPtr -> do
    success <- c_sdlGetRenderOutputSize renderer wPtr hPtr
    if not (toBool success)
      then return Nothing
      else do
        w <- fromIntegral <$> peek wPtr
        h <- fromIntegral <$> peek hPtr
        return $ Just (w, h)

foreign import ccall unsafe "SDL_GetCurrentRenderOutputSize"
  c_sdlGetCurrentRenderOutputSize :: Ptr SDLRenderer -> Ptr CInt -> Ptr CInt -> IO CBool

sdlGetCurrentRenderOutputSize :: SDLRenderer -> IO (Maybe (Int, Int))
sdlGetCurrentRenderOutputSize (SDLRenderer renderer) =
  alloca $ \wPtr ->
  alloca $ \hPtr -> do
    success <- c_sdlGetCurrentRenderOutputSize renderer wPtr hPtr
    if not (toBool success)
      then return Nothing
      else do
        w <- fromIntegral <$> peek wPtr
        h <- fromIntegral <$> peek hPtr
        return $ Just (w, h)

-- ** Texture Creation / Destruction **
foreign import ccall unsafe "SDL_CreateTexture"
  c_sdlCreateTexture :: Ptr SDLRenderer -> CUInt -> SDLTextureAccess -> CInt -> CInt -> IO (Ptr SDLTexture)

sdlCreateTexture :: SDLRenderer -> SDLPixelFormat -> SDLTextureAccess -> Int -> Int -> IO (Maybe SDLTexture)
sdlCreateTexture (SDLRenderer renderer) format access w h = do
  ptr <- c_sdlCreateTexture renderer (pixelFormatToCUInt format) access (fromIntegral w) (fromIntegral h)
  if ptr == nullPtr
    then return Nothing
    else return $ Just (SDLTexture ptr)

foreign import ccall unsafe "SDL_CreateTextureFromSurface"
  c_sdlCreateTextureFromSurface :: Ptr SDLRenderer -> Ptr SDLSurface -> IO (Ptr SDLTexture)

sdlCreateTextureFromSurface :: SDLRenderer -> Ptr SDLSurface -> IO (Maybe SDLTexture) -- Accepts raw Ptr SDLSurface
sdlCreateTextureFromSurface (SDLRenderer renderer) surface = do
  ptr <- c_sdlCreateTextureFromSurface renderer surface
  if ptr == nullPtr
    then return Nothing
    else return $ Just (SDLTexture ptr)

foreign import ccall unsafe "SDL_CreateTextureWithProperties"
  c_sdlCreateTextureWithProperties :: Ptr SDLRenderer -> SDLPropertiesID -> IO (Ptr SDLTexture)

sdlCreateTextureWithProperties :: SDLRenderer -> SDLPropertiesID -> IO (Maybe SDLTexture)
sdlCreateTextureWithProperties (SDLRenderer renderer) props = do
  ptr <- c_sdlCreateTextureWithProperties renderer props
  if ptr == nullPtr
    then return Nothing
    else return $ Just (SDLTexture ptr)

foreign import ccall unsafe "SDL_DestroyTexture"
  c_sdlDestroyTexture :: Ptr SDLTexture -> IO ()

sdlDestroyTexture :: SDLTexture -> IO ()
sdlDestroyTexture (SDLTexture tex) = c_sdlDestroyTexture tex

-- ** Texture Query **
foreign import ccall unsafe "SDL_GetTextureProperties"
  c_sdlGetTextureProperties :: Ptr SDLTexture -> IO SDLPropertiesID

sdlGetTextureProperties :: SDLTexture -> IO SDLPropertiesID
sdlGetTextureProperties (SDLTexture tex) = c_sdlGetTextureProperties tex

foreign import ccall unsafe "SDL_GetRendererFromTexture"
  c_sdlGetRendererFromTexture :: Ptr SDLTexture -> IO (Ptr SDLRenderer)

sdlGetRendererFromTexture :: SDLTexture -> IO (Maybe SDLRenderer)
sdlGetRendererFromTexture (SDLTexture tex) = do
    ptr <- c_sdlGetRendererFromTexture tex
    if ptr == nullPtr
        then return Nothing
        else return $ Just (SDLRenderer ptr)

foreign import ccall unsafe "SDL_GetTextureSize"
  c_sdlGetTextureSize :: Ptr SDLTexture -> Ptr CFloat -> Ptr CFloat -> IO CBool

sdlGetTextureSize :: SDLTexture -> IO (Maybe (Float, Float))
sdlGetTextureSize (SDLTexture tex) =
  alloca $ \wPtr ->
  alloca $ \hPtr -> do
    success <- c_sdlGetTextureSize tex wPtr hPtr
    if not (toBool success)
      then return Nothing
      else do
        w <- realToFrac <$> peek wPtr
        h <- realToFrac <$> peek hPtr
        return $ Just (w, h)

-- ** Texture Manipulation **
foreign import ccall unsafe "SDL_SetTextureColorMod"
  c_sdlSetTextureColorMod :: Ptr SDLTexture -> Word8 -> Word8 -> Word8 -> IO CBool

sdlSetTextureColorMod :: SDLTexture -> Word8 -> Word8 -> Word8 -> IO Bool
sdlSetTextureColorMod (SDLTexture tex) r g b = toBool <$> c_sdlSetTextureColorMod tex r g b

foreign import ccall unsafe "SDL_SetTextureColorModFloat"
  c_sdlSetTextureColorModFloat :: Ptr SDLTexture -> CFloat -> CFloat -> CFloat -> IO CBool

sdlSetTextureColorModFloat :: SDLTexture -> Float -> Float -> Float -> IO Bool
sdlSetTextureColorModFloat (SDLTexture tex) r g b =
  toBool <$> c_sdlSetTextureColorModFloat tex (realToFrac r) (realToFrac g) (realToFrac b)

foreign import ccall unsafe "SDL_GetTextureColorMod"
  c_sdlGetTextureColorMod :: Ptr SDLTexture -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CBool

sdlGetTextureColorMod :: SDLTexture -> IO (Maybe (Word8, Word8, Word8))
sdlGetTextureColorMod (SDLTexture tex) =
  alloca $ \rPtr ->
  alloca $ \gPtr ->
  alloca $ \bPtr -> do
    success <- c_sdlGetTextureColorMod tex rPtr gPtr bPtr
    if not (toBool success)
      then return Nothing
      else do
        r <- peek rPtr
        g <- peek gPtr
        b <- peek bPtr
        return $ Just (r, g, b)

foreign import ccall unsafe "SDL_GetTextureColorModFloat"
  c_sdlGetTextureColorModFloat :: Ptr SDLTexture -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CBool

sdlGetTextureColorModFloat :: SDLTexture -> IO (Maybe (Float, Float, Float))
sdlGetTextureColorModFloat (SDLTexture tex) =
  alloca $ \rPtr ->
  alloca $ \gPtr ->
  alloca $ \bPtr -> do
    success <- c_sdlGetTextureColorModFloat tex rPtr gPtr bPtr
    if not (toBool success)
      then return Nothing
      else do
        r <- realToFrac <$> peek rPtr
        g <- realToFrac <$> peek gPtr
        b <- realToFrac <$> peek bPtr
        return $ Just (r, g, b)

foreign import ccall unsafe "SDL_SetTextureAlphaMod"
  c_sdlSetTextureAlphaMod :: Ptr SDLTexture -> Word8 -> IO CBool

sdlSetTextureAlphaMod :: SDLTexture -> Word8 -> IO Bool
sdlSetTextureAlphaMod (SDLTexture tex) alpha = toBool <$> c_sdlSetTextureAlphaMod tex alpha

foreign import ccall unsafe "SDL_SetTextureAlphaModFloat"
  c_sdlSetTextureAlphaModFloat :: Ptr SDLTexture -> CFloat -> IO CBool

sdlSetTextureAlphaModFloat :: SDLTexture -> Float -> IO Bool
sdlSetTextureAlphaModFloat (SDLTexture tex) alpha = toBool <$> c_sdlSetTextureAlphaModFloat tex (realToFrac alpha)

foreign import ccall unsafe "SDL_GetTextureAlphaMod"
  c_sdlGetTextureAlphaMod :: Ptr SDLTexture -> Ptr Word8 -> IO CBool

sdlGetTextureAlphaMod :: SDLTexture -> IO (Maybe Word8)
sdlGetTextureAlphaMod (SDLTexture tex) =
  alloca $ \aPtr -> do
    success <- c_sdlGetTextureAlphaMod tex aPtr
    if not (toBool success)
      then return Nothing
      else Just <$> peek aPtr

foreign import ccall unsafe "SDL_GetTextureAlphaModFloat"
  c_sdlGetTextureAlphaModFloat :: Ptr SDLTexture -> Ptr CFloat -> IO CBool

sdlGetTextureAlphaModFloat :: SDLTexture -> IO (Maybe Float)
sdlGetTextureAlphaModFloat (SDLTexture tex) =
  alloca $ \aPtr -> do
    success <- c_sdlGetTextureAlphaModFloat tex aPtr
    if not (toBool success)
      then return Nothing
      else Just . realToFrac <$> peek aPtr

foreign import ccall unsafe "SDL_SetTextureBlendMode"
  c_sdlSetTextureBlendMode :: Ptr SDLTexture -> SDLBlendMode -> IO CBool

sdlSetTextureBlendMode :: SDLTexture -> SDLBlendMode -> IO Bool
sdlSetTextureBlendMode (SDLTexture tex) mode = toBool <$> c_sdlSetTextureBlendMode tex mode

foreign import ccall unsafe "SDL_GetTextureBlendMode"
  c_sdlGetTextureBlendMode :: Ptr SDLTexture -> Ptr SDLBlendMode -> IO CBool

sdlGetTextureBlendMode :: SDLTexture -> IO (Maybe SDLBlendMode)
sdlGetTextureBlendMode (SDLTexture tex) =
  alloca $ \modePtr -> do
    success <- c_sdlGetTextureBlendMode tex modePtr
    if not (toBool success)
      then return Nothing
      else Just <$> peek modePtr

foreign import ccall unsafe "SDL_SetTextureScaleMode"
  c_sdlSetTextureScaleMode :: Ptr SDLTexture -> CInt -> IO CBool

sdlSetTextureScaleMode :: SDLTexture -> SDLScaleMode -> IO Bool
sdlSetTextureScaleMode (SDLTexture tex) mode =
    fromCBool <$> c_sdlSetTextureScaleMode tex (fromIntegral $ fromEnum mode)

foreign import ccall unsafe "SDL_GetTextureScaleMode"
  c_sdlGetTextureScaleMode :: Ptr SDLTexture -> Ptr CInt -> IO CBool

-- | Haskell-friendly wrapper for SDL_RenderDebugTextFormat
foreign import ccall unsafe "wrapper_SDL_RenderDebugTextFormat"
  c_sdlRenderDebugTextFormat :: Ptr SDLRenderer -> CFloat -> CFloat -> CString -> IO ()

sdlRenderDebugTextFormat :: Ptr SDLRenderer -> Float -> Float -> String -> IO ()
sdlRenderDebugTextFormat renderer x y str =
  withCString str $ \cstr ->
    c_sdlRenderDebugTextFormat renderer (realToFrac x) (realToFrac y) cstr

sdlGetTextureScaleMode :: SDLTexture -> IO (Maybe SDLScaleMode)
sdlGetTextureScaleMode (SDLTexture tex) =
  alloca $ \(modePtr :: Ptr CInt) -> do -- Allocate space for CInt
    success <- c_sdlGetTextureScaleMode tex modePtr -- Pass the Ptr CInt
    if not (toBool success)
      then return Nothing
      else do
        modeCInt <- peek modePtr -- Peek the CInt value
        return $ Just (toEnum $ fromIntegral modeCInt)

foreign import ccall unsafe "SDL_UpdateTexture"
  c_sdlUpdateTexture :: Ptr SDLTexture -> Ptr SDLRect -> Ptr () -> CInt -> IO CBool

sdlUpdateTexture :: SDLTexture -> Maybe SDLRect -> Ptr () -> Int -> IO Bool
sdlUpdateTexture (SDLTexture tex) mRect pixels pitch =
  maybeWith with mRect $ \rectPtr ->
    toBool <$> c_sdlUpdateTexture tex rectPtr pixels (fromIntegral pitch)

foreign import ccall unsafe "SDL_UpdateYUVTexture"
  c_sdlUpdateYUVTexture :: Ptr SDLTexture -> Ptr SDLRect -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO CBool

sdlUpdateYUVTexture :: SDLTexture -> Maybe SDLRect -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> IO Bool
sdlUpdateYUVTexture (SDLTexture tex) mRect yPlane yPitch uPlane uPitch vPlane vPitch =
  maybeWith with mRect $ \rectPtr ->
    toBool <$> c_sdlUpdateYUVTexture tex rectPtr yPlane (fromIntegral yPitch) uPlane (fromIntegral uPitch) vPlane (fromIntegral vPitch)

foreign import ccall unsafe "SDL_UpdateNVTexture"
  c_sdlUpdateNVTexture :: Ptr SDLTexture -> Ptr SDLRect -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO CBool

sdlUpdateNVTexture :: SDLTexture -> Maybe SDLRect -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> IO Bool
sdlUpdateNVTexture (SDLTexture tex) mRect yPlane yPitch uvPlane uvPitch =
  maybeWith with mRect $ \rectPtr ->
    toBool <$> c_sdlUpdateNVTexture tex rectPtr yPlane (fromIntegral yPitch) uvPlane (fromIntegral uvPitch)

foreign import ccall unsafe "SDL_LockTexture"
  c_sdlLockTexture :: Ptr SDLTexture -> Ptr SDLRect -> Ptr (Ptr ()) -> Ptr CInt -> IO CBool

sdlLockTexture :: SDLTexture -> Maybe SDLRect -> IO (Maybe (Ptr (), Int))
sdlLockTexture (SDLTexture tex) mRect =
  maybeWith with mRect $ \rectPtr ->
  alloca $ \pixelsPtrPtr ->
  alloca $ \pitchPtr -> do
    success <- c_sdlLockTexture tex rectPtr pixelsPtrPtr pitchPtr
    if not (toBool success)
      then return Nothing
      else do
        pixelsPtr <- peek pixelsPtrPtr
        pitch <- fromIntegral <$> peek pitchPtr
        return $ Just (pixelsPtr, pitch)

foreign import ccall unsafe "SDL_LockTextureToSurface"
  c_sdlLockTextureToSurface :: Ptr SDLTexture -> Ptr SDLRect -> Ptr (Ptr SDLSurface) -> IO CBool

sdlLockTextureToSurface :: SDLTexture -> Maybe SDLRect -> IO (Maybe (Ptr SDLSurface))
sdlLockTextureToSurface (SDLTexture tex) mRect =
  maybeWith with mRect $ \rectPtr ->
  alloca $ \surfacePtrPtr -> do
    success <- c_sdlLockTextureToSurface tex rectPtr surfacePtrPtr
    if not (toBool success)
      then return Nothing
      else Just <$> peek surfacePtrPtr

foreign import ccall unsafe "SDL_UnlockTexture"
  c_sdlUnlockTexture :: Ptr SDLTexture -> IO ()

sdlUnlockTexture :: SDLTexture -> IO ()
sdlUnlockTexture (SDLTexture tex) = c_sdlUnlockTexture tex

-- ** Render Target **
foreign import ccall unsafe "SDL_SetRenderTarget"
  c_sdlSetRenderTarget :: Ptr SDLRenderer -> Ptr SDLTexture -> IO CBool

sdlSetRenderTarget :: SDLRenderer -> Maybe SDLTexture -> IO Bool
sdlSetRenderTarget (SDLRenderer renderer) mTex =
  toBool <$> c_sdlSetRenderTarget renderer (maybe nullPtr (\(SDLTexture p) -> p) mTex)

foreign import ccall unsafe "SDL_GetRenderTarget"
  c_sdlGetRenderTarget :: Ptr SDLRenderer -> IO (Ptr SDLTexture)

sdlGetRenderTarget :: SDLRenderer -> IO (Maybe SDLTexture)
sdlGetRenderTarget (SDLRenderer renderer) = do
  ptr <- c_sdlGetRenderTarget renderer
  if ptr == nullPtr
    then return Nothing
    else return $ Just (SDLTexture ptr)

-- ** Logical Presentation & Coordinates **
foreign import ccall unsafe "SDL_SetRenderLogicalPresentation"
  c_sdlSetRenderLogicalPresentation :: Ptr SDLRenderer -> CInt -> CInt -> SDLRendererLogicalPresentation -> IO CBool

sdlSetRenderLogicalPresentation :: SDLRenderer -> Int -> Int -> SDLRendererLogicalPresentation -> IO Bool
sdlSetRenderLogicalPresentation (SDLRenderer renderer) w h mode =
  toBool <$> c_sdlSetRenderLogicalPresentation renderer (fromIntegral w) (fromIntegral h) mode

foreign import ccall unsafe "SDL_GetRenderLogicalPresentation"
  c_sdlGetRenderLogicalPresentation :: Ptr SDLRenderer -> Ptr CInt -> Ptr CInt -> Ptr SDLRendererLogicalPresentation -> IO CBool

sdlGetRenderLogicalPresentation :: SDLRenderer -> IO (Maybe (Int, Int, SDLRendererLogicalPresentation))
sdlGetRenderLogicalPresentation (SDLRenderer renderer) =
  alloca $ \wPtr ->
  alloca $ \hPtr ->
  alloca $ \modePtr -> do
    success <- c_sdlGetRenderLogicalPresentation renderer wPtr hPtr modePtr
    if not (toBool success)
      then return Nothing
      else do
        w <- fromIntegral <$> peek wPtr
        h <- fromIntegral <$> peek hPtr
        mode <- peek modePtr
        return $ Just (w, h, mode)

foreign import ccall unsafe "SDL_GetRenderLogicalPresentationRect"
  c_sdlGetRenderLogicalPresentationRect :: Ptr SDLRenderer -> Ptr SDLFRect -> IO CBool

sdlGetRenderLogicalPresentationRect :: SDLRenderer -> IO (Maybe SDLFRect)
sdlGetRenderLogicalPresentationRect (SDLRenderer renderer) =
    alloca $ \rectPtr -> do
        success <- c_sdlGetRenderLogicalPresentationRect renderer rectPtr
        if not (toBool success)
            then return Nothing
            else Just <$> peek rectPtr

foreign import ccall unsafe "SDL_RenderCoordinatesFromWindow"
  c_sdlRenderCoordinatesFromWindow :: Ptr SDLRenderer -> CFloat -> CFloat -> Ptr CFloat -> Ptr CFloat -> IO CBool

sdlRenderCoordinatesFromWindow :: SDLRenderer -> Float -> Float -> IO (Maybe (Float, Float))
sdlRenderCoordinatesFromWindow (SDLRenderer renderer) winX winY =
  alloca $ \xPtr ->
  alloca $ \yPtr -> do
    success <- c_sdlRenderCoordinatesFromWindow renderer (realToFrac winX) (realToFrac winY) xPtr yPtr
    if not (toBool success)
      then return Nothing
      else do
        x <- realToFrac <$> peek xPtr
        y <- realToFrac <$> peek yPtr
        return $ Just (x, y)

foreign import ccall unsafe "SDL_RenderCoordinatesToWindow"
  c_sdlRenderCoordinatesToWindow :: Ptr SDLRenderer -> CFloat -> CFloat -> Ptr CFloat -> Ptr CFloat -> IO CBool

sdlRenderCoordinatesToWindow :: SDLRenderer -> Float -> Float -> IO (Maybe (Float, Float))
sdlRenderCoordinatesToWindow (SDLRenderer renderer) x y =
  alloca $ \winXPtr ->
  alloca $ \winYPtr -> do
    success <- c_sdlRenderCoordinatesToWindow renderer (realToFrac x) (realToFrac y) winXPtr winYPtr
    if not (toBool success)
      then return Nothing
      else do
        winX <- realToFrac <$> peek winXPtr
        winY <- realToFrac <$> peek winYPtr
        return $ Just (winX, winY)

foreign import ccall unsafe "SDL_ConvertEventToRenderCoordinates"
  c_sdlConvertEventToRenderCoordinates :: Ptr SDLRenderer -> Ptr SDLEvent -> IO CBool

sdlConvertEventToRenderCoordinates :: SDLRenderer -> Ptr SDLEvent -> IO Bool
sdlConvertEventToRenderCoordinates (SDLRenderer renderer) eventPtr =
    toBool <$> c_sdlConvertEventToRenderCoordinates renderer eventPtr

-- ** Viewport & Clipping **
foreign import ccall unsafe "SDL_SetRenderViewport"
  c_sdlSetRenderViewport :: Ptr SDLRenderer -> Ptr SDLRect -> IO CBool

sdlSetRenderViewport :: SDLRenderer -> Maybe SDLRect -> IO Bool
sdlSetRenderViewport (SDLRenderer renderer) mRect =
  maybeWith with mRect $ \rectPtr ->
    toBool <$> c_sdlSetRenderViewport renderer rectPtr

foreign import ccall unsafe "SDL_GetRenderViewport"
  c_sdlGetRenderViewport :: Ptr SDLRenderer -> Ptr SDLRect -> IO CBool

sdlGetRenderViewport :: SDLRenderer -> IO (Maybe SDLRect)
sdlGetRenderViewport (SDLRenderer renderer) =
  alloca $ \rectPtr -> do
    success <- c_sdlGetRenderViewport renderer rectPtr
    if not (toBool success)
      then return Nothing
      else Just <$> peek rectPtr

foreign import ccall unsafe "SDL_RenderViewportSet"
  c_sdlRenderViewportSet :: Ptr SDLRenderer -> IO CBool

sdlRenderViewportSet :: SDLRenderer -> IO Bool
sdlRenderViewportSet (SDLRenderer renderer) = toBool <$> c_sdlRenderViewportSet renderer

foreign import ccall unsafe "SDL_GetRenderSafeArea"
    c_sdlGetRenderSafeArea :: Ptr SDLRenderer -> Ptr SDLRect -> IO CBool

sdlGetRenderSafeArea :: SDLRenderer -> IO (Maybe SDLRect)
sdlGetRenderSafeArea (SDLRenderer renderer) =
    alloca $ \rectPtr -> do
        success <- c_sdlGetRenderSafeArea renderer rectPtr
        if not (toBool success)
            then return Nothing
            else Just <$> peek rectPtr

foreign import ccall unsafe "SDL_SetRenderClipRect"
  c_sdlSetRenderClipRect :: Ptr SDLRenderer -> Ptr SDLRect -> IO CBool

sdlSetRenderClipRect :: SDLRenderer -> Maybe SDLRect -> IO Bool
sdlSetRenderClipRect (SDLRenderer renderer) mRect =
  maybeWith with mRect $ \rectPtr ->
    toBool <$> c_sdlSetRenderClipRect renderer rectPtr

foreign import ccall unsafe "SDL_GetRenderClipRect"
  c_sdlGetRenderClipRect :: Ptr SDLRenderer -> Ptr SDLRect -> IO CBool

sdlGetRenderClipRect :: SDLRenderer -> IO (Maybe SDLRect)
sdlGetRenderClipRect (SDLRenderer renderer) =
  alloca $ \rectPtr -> do
    success <- c_sdlGetRenderClipRect renderer rectPtr
    if not (toBool success)
      then return Nothing
      else Just <$> peek rectPtr -- Note: Might return empty rect if disabled

foreign import ccall unsafe "SDL_RenderClipEnabled"
  c_sdlRenderClipEnabled :: Ptr SDLRenderer -> IO CBool

sdlRenderClipEnabled :: SDLRenderer -> IO Bool
sdlRenderClipEnabled (SDLRenderer renderer) = toBool <$> c_sdlRenderClipEnabled renderer

-- ** Render State **
foreign import ccall unsafe "SDL_SetRenderScale"
  c_sdlSetRenderScale :: Ptr SDLRenderer -> CFloat -> CFloat -> IO CBool

sdlSetRenderScale :: SDLRenderer -> Float -> Float -> IO Bool
sdlSetRenderScale (SDLRenderer renderer) scaleX scaleY =
  toBool <$> c_sdlSetRenderScale renderer (realToFrac scaleX) (realToFrac scaleY)

foreign import ccall unsafe "SDL_GetRenderScale"
  c_sdlGetRenderScale :: Ptr SDLRenderer -> Ptr CFloat -> Ptr CFloat -> IO CBool

sdlGetRenderScale :: SDLRenderer -> IO (Maybe (Float, Float))
sdlGetRenderScale (SDLRenderer renderer) =
  alloca $ \scaleXPtr ->
  alloca $ \scaleYPtr -> do
    success <- c_sdlGetRenderScale renderer scaleXPtr scaleYPtr
    if not (toBool success)
      then return Nothing
      else do
        scaleX <- realToFrac <$> peek scaleXPtr
        scaleY <- realToFrac <$> peek scaleYPtr
        return $ Just (scaleX, scaleY)

foreign import ccall unsafe "SDL_SetRenderDrawColor"
  c_sdlSetRenderDrawColor :: Ptr SDLRenderer -> Word8 -> Word8 -> Word8 -> Word8 -> IO CBool

sdlSetRenderDrawColor :: SDLRenderer -> Word8 -> Word8 -> Word8 -> Word8 -> IO Bool
sdlSetRenderDrawColor (SDLRenderer renderer) r g b a = toBool <$> c_sdlSetRenderDrawColor renderer r g b a

foreign import ccall unsafe "SDL_SetRenderDrawColorFloat"
  c_sdlSetRenderDrawColorFloat :: Ptr SDLRenderer -> CFloat -> CFloat -> CFloat -> CFloat -> IO CBool

sdlSetRenderDrawColorFloat :: SDLRenderer -> Float -> Float -> Float -> Float -> IO Bool
sdlSetRenderDrawColorFloat (SDLRenderer renderer) r g b a =
    toBool <$> c_sdlSetRenderDrawColorFloat renderer (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)

foreign import ccall unsafe "SDL_GetRenderDrawColor"
  c_sdlGetRenderDrawColor :: Ptr SDLRenderer -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CBool

sdlGetRenderDrawColor :: SDLRenderer -> IO (Maybe (Word8, Word8, Word8, Word8))
sdlGetRenderDrawColor (SDLRenderer renderer) =
  alloca $ \rPtr ->
  alloca $ \gPtr ->
  alloca $ \bPtr ->
  alloca $ \aPtr -> do
    success <- c_sdlGetRenderDrawColor renderer rPtr gPtr bPtr aPtr
    if not (toBool success)
      then return Nothing
      else do
        r <- peek rPtr
        g <- peek gPtr
        b <- peek bPtr
        a <- peek aPtr
        return $ Just (r, g, b, a)

foreign import ccall unsafe "SDL_GetRenderDrawColorFloat"
  c_sdlGetRenderDrawColorFloat :: Ptr SDLRenderer -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO CBool

sdlGetRenderDrawColorFloat :: SDLRenderer -> IO (Maybe (Float, Float, Float, Float))
sdlGetRenderDrawColorFloat (SDLRenderer renderer) =
  alloca $ \rPtr ->
  alloca $ \gPtr ->
  alloca $ \bPtr ->
  alloca $ \aPtr -> do
    success <- c_sdlGetRenderDrawColorFloat renderer rPtr gPtr bPtr aPtr
    if not (toBool success)
      then return Nothing
      else do
        r <- realToFrac <$> peek rPtr
        g <- realToFrac <$> peek gPtr
        b <- realToFrac <$> peek bPtr
        a <- realToFrac <$> peek aPtr
        return $ Just (r, g, b, a)

foreign import ccall unsafe "SDL_SetRenderColorScale"
    c_sdlSetRenderColorScale :: Ptr SDLRenderer -> CFloat -> IO CBool

sdlSetRenderColorScale :: SDLRenderer -> Float -> IO Bool
sdlSetRenderColorScale (SDLRenderer renderer) scale =
    toBool <$> c_sdlSetRenderColorScale renderer (realToFrac scale)

foreign import ccall unsafe "SDL_GetRenderColorScale"
    c_sdlGetRenderColorScale :: Ptr SDLRenderer -> Ptr CFloat -> IO CBool

sdlGetRenderColorScale :: SDLRenderer -> IO (Maybe Float)
sdlGetRenderColorScale (SDLRenderer renderer) =
    alloca $ \scalePtr -> do
        success <- c_sdlGetRenderColorScale renderer scalePtr
        if not (toBool success)
            then return Nothing
            else Just . realToFrac <$> peek scalePtr

foreign import ccall unsafe "SDL_SetRenderDrawBlendMode"
  c_sdlSetRenderDrawBlendMode :: Ptr SDLRenderer -> SDLBlendMode -> IO CBool

sdlSetRenderDrawBlendMode :: SDLRenderer -> SDLBlendMode -> IO Bool
sdlSetRenderDrawBlendMode (SDLRenderer renderer) mode = toBool <$> c_sdlSetRenderDrawBlendMode renderer mode

foreign import ccall unsafe "SDL_GetRenderDrawBlendMode"
  c_sdlGetRenderDrawBlendMode :: Ptr SDLRenderer -> Ptr SDLBlendMode -> IO CBool

sdlGetRenderDrawBlendMode :: SDLRenderer -> IO (Maybe SDLBlendMode)
sdlGetRenderDrawBlendMode (SDLRenderer renderer) =
  alloca $ \modePtr -> do
    success <- c_sdlGetRenderDrawBlendMode renderer modePtr
    if not (toBool success)
      then return Nothing
      else Just <$> peek modePtr

foreign import ccall unsafe "SDL_SetRenderTextureAddressMode"
    c_sdlSetRenderTextureAddressMode :: Ptr SDLRenderer -> SDLTextureAddressMode -> SDLTextureAddressMode -> IO CBool

sdlSetRenderTextureAddressMode :: SDLRenderer -> SDLTextureAddressMode -> SDLTextureAddressMode -> IO Bool
sdlSetRenderTextureAddressMode (SDLRenderer renderer) uMode vMode =
    toBool <$> c_sdlSetRenderTextureAddressMode renderer uMode vMode

foreign import ccall unsafe "SDL_GetRenderTextureAddressMode"
    c_sdlGetRenderTextureAddressMode :: Ptr SDLRenderer -> Ptr SDLTextureAddressMode -> Ptr SDLTextureAddressMode -> IO CBool

sdlGetRenderTextureAddressMode :: SDLRenderer -> IO (Maybe (SDLTextureAddressMode, SDLTextureAddressMode))
sdlGetRenderTextureAddressMode (SDLRenderer renderer) =
    alloca $ \uModePtr ->
    alloca $ \vModePtr -> do
        success <- c_sdlGetRenderTextureAddressMode renderer uModePtr vModePtr
        if not (toBool success)
            then return Nothing
            else do
                uMode <- peek uModePtr
                vMode <- peek vModePtr
                return $ Just (uMode, vMode)

-- ** Clearing **
foreign import ccall unsafe "SDL_RenderClear"
  c_sdlRenderClear :: Ptr SDLRenderer -> IO CBool

sdlRenderClear :: SDLRenderer -> IO Bool
sdlRenderClear (SDLRenderer renderer) = toBool <$> c_sdlRenderClear renderer

-- ** Drawing Primitives **
foreign import ccall unsafe "SDL_RenderPoint"
  c_sdlRenderPoint :: Ptr SDLRenderer -> CFloat -> CFloat -> IO CBool

sdlRenderPoint :: SDLRenderer -> Float -> Float -> IO Bool
sdlRenderPoint (SDLRenderer renderer) x y = toBool <$> c_sdlRenderPoint renderer (realToFrac x) (realToFrac y)

foreign import ccall unsafe "SDL_RenderPoints"
  c_sdlRenderPoints :: Ptr SDLRenderer -> Ptr SDLFPoint -> CInt -> IO CBool

sdlRenderPoints :: SDLRenderer -> [SDLFPoint] -> IO Bool
sdlRenderPoints (SDLRenderer renderer) points =
  withArrayLen points $ \count pointsPtr ->
    toBool <$> c_sdlRenderPoints renderer pointsPtr (fromIntegral count)

foreign import ccall unsafe "SDL_RenderLine"
  c_sdlRenderLine :: Ptr SDLRenderer -> CFloat -> CFloat -> CFloat -> CFloat -> IO CBool

sdlRenderLine :: SDLRenderer -> Float -> Float -> Float -> Float -> IO Bool
sdlRenderLine (SDLRenderer renderer) x1 y1 x2 y2 =
  toBool <$> c_sdlRenderLine renderer (realToFrac x1) (realToFrac y1) (realToFrac x2) (realToFrac y2)

foreign import ccall unsafe "SDL_RenderLines"
  c_sdlRenderLines :: Ptr SDLRenderer -> Ptr SDLFPoint -> CInt -> IO CBool

sdlRenderLines :: SDLRenderer -> [SDLFPoint] -> IO Bool
sdlRenderLines (SDLRenderer renderer) points =
  withArrayLen points $ \count pointsPtr ->
    toBool <$> c_sdlRenderLines renderer pointsPtr (fromIntegral count)

foreign import ccall unsafe "SDL_RenderRect"
  c_sdlRenderRect :: Ptr SDLRenderer -> Ptr SDLFRect -> IO CBool

sdlRenderRect :: SDLRenderer -> Maybe SDLFRect -> IO Bool
sdlRenderRect (SDLRenderer renderer) mRect =
  maybeWith with mRect $ \rectPtr ->
    toBool <$> c_sdlRenderRect renderer rectPtr

foreign import ccall unsafe "SDL_RenderRects"
  c_sdlRenderRects :: Ptr SDLRenderer -> Ptr SDLFRect -> CInt -> IO CBool

sdlRenderRects :: SDLRenderer -> [SDLFRect] -> IO Bool
sdlRenderRects (SDLRenderer renderer) rects =
  withArrayLen rects $ \count rectsPtr ->
    toBool <$> c_sdlRenderRects renderer rectsPtr (fromIntegral count)

foreign import ccall unsafe "SDL_RenderFillRect"
  c_sdlRenderFillRect :: Ptr SDLRenderer -> Ptr SDLFRect -> IO CBool

sdlRenderFillRect :: SDLRenderer -> Maybe SDLFRect -> IO Bool
sdlRenderFillRect (SDLRenderer renderer) mRect =
  maybeWith with mRect $ \rectPtr ->
    toBool <$> c_sdlRenderFillRect renderer rectPtr

foreign import ccall unsafe "SDL_RenderFillRects"
  c_sdlRenderFillRects :: Ptr SDLRenderer -> Ptr SDLFRect -> CInt -> IO CBool

sdlRenderFillRects :: SDLRenderer -> [SDLFRect] -> IO Bool
sdlRenderFillRects (SDLRenderer renderer) rects =
  withArrayLen rects $ \count rectsPtr ->
    toBool <$> c_sdlRenderFillRects renderer rectsPtr (fromIntegral count)

-- ** Drawing Textures **
foreign import ccall unsafe "SDL_RenderTexture"
  c_sdlRenderTexture :: Ptr SDLRenderer -> Ptr SDLTexture -> Ptr SDLFRect -> Ptr SDLFRect -> IO CBool

sdlRenderTexture :: SDLRenderer -> SDLTexture -> Maybe SDLFRect -> Maybe SDLFRect -> IO Bool
sdlRenderTexture (SDLRenderer renderer) (SDLTexture tex) mSrcRect mDstRect =
  maybeWith with mSrcRect $ \srcPtr ->
  maybeWith with mDstRect $ \dstPtr ->
    toBool <$> c_sdlRenderTexture renderer tex srcPtr dstPtr

foreign import ccall unsafe "SDL_RenderTextureRotated"
  c_sdlRenderTextureRotated :: Ptr SDLRenderer -> Ptr SDLTexture -> Ptr SDLFRect -> Ptr SDLFRect -> CDouble -> Ptr SDLFPoint -> CInt -> IO CBool

sdlRenderTextureRotated :: SDLRenderer -> SDLTexture -> Maybe SDLFRect -> Maybe SDLFRect -> Double -> Maybe SDLFPoint -> SDLFlipMode -> IO Bool
sdlRenderTextureRotated (SDLRenderer renderer) (SDLTexture tex) mSrcRect mDstRect angle mCenter flipMode = -- Pass the enum value directly
  maybeWith with mSrcRect $ \srcPtr ->
  maybeWith with mDstRect $ \dstPtr ->
  maybeWith with mCenter $ \centerPtr ->
    fromCBool <$> c_sdlRenderTextureRotated renderer tex srcPtr dstPtr (realToFrac angle) centerPtr (fromIntegral $ fromEnum flipMode)

foreign import ccall unsafe "SDL_RenderTextureAffine"
  c_sdlRenderTextureAffine :: Ptr SDLRenderer -> Ptr SDLTexture -> Ptr SDLFRect -> Ptr SDLFPoint -> Ptr SDLFPoint -> Ptr SDLFPoint -> IO CBool

sdlRenderTextureAffine :: SDLRenderer -> SDLTexture -> Maybe SDLFRect -> Maybe SDLFPoint -> Maybe SDLFPoint -> Maybe SDLFPoint -> IO Bool
sdlRenderTextureAffine (SDLRenderer renderer) (SDLTexture tex) mSrcRect mOrigin mRight mDown =
    maybeWith with mSrcRect $ \srcPtr ->
    maybeWith with mOrigin $ \originPtr ->
    maybeWith with mRight $ \rightPtr ->
    maybeWith with mDown $ \downPtr ->
        toBool <$> c_sdlRenderTextureAffine renderer tex srcPtr originPtr rightPtr downPtr

foreign import ccall unsafe "SDL_RenderTextureTiled"
    c_sdlRenderTextureTiled :: Ptr SDLRenderer -> Ptr SDLTexture -> Ptr SDLFRect -> CFloat -> Ptr SDLFRect -> IO CBool

sdlRenderTextureTiled :: SDLRenderer -> SDLTexture -> Maybe SDLFRect -> Float -> Maybe SDLFRect -> IO Bool
sdlRenderTextureTiled (SDLRenderer renderer) (SDLTexture tex) mSrcRect scale mDstRect =
    maybeWith with mSrcRect $ \srcPtr ->
    maybeWith with mDstRect $ \dstPtr ->
        toBool <$> c_sdlRenderTextureTiled renderer tex srcPtr (realToFrac scale) dstPtr

foreign import ccall unsafe "SDL_RenderTexture9Grid"
    c_sdlRenderTexture9Grid :: Ptr SDLRenderer -> Ptr SDLTexture -> Ptr SDLFRect -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> Ptr SDLFRect -> IO CBool

sdlRenderTexture9Grid :: SDLRenderer -> SDLTexture -> Maybe SDLFRect -> Float -> Float -> Float -> Float -> Float -> Maybe SDLFRect -> IO Bool
sdlRenderTexture9Grid (SDLRenderer renderer) (SDLTexture tex) mSrcRect leftW rightW topH bottomH scale mDstRect =
    maybeWith with mSrcRect $ \srcPtr ->
    maybeWith with mDstRect $ \dstPtr ->
        toBool <$> c_sdlRenderTexture9Grid renderer tex srcPtr
                      (realToFrac leftW) (realToFrac rightW) (realToFrac topH) (realToFrac bottomH) (realToFrac scale)
                      dstPtr

foreign import ccall unsafe "SDL_RenderTexture9GridTiled"
    c_sdlRenderTexture9GridTiled :: Ptr SDLRenderer -> Ptr SDLTexture -> Ptr SDLFRect -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> Ptr SDLFRect -> CFloat -> IO CBool

sdlRenderTexture9GridTiled :: SDLRenderer -> SDLTexture -> Maybe SDLFRect -> Float -> Float -> Float -> Float -> Float -> Maybe SDLFRect -> Float -> IO Bool
sdlRenderTexture9GridTiled (SDLRenderer renderer) (SDLTexture tex) mSrcRect leftW rightW topH bottomH scale mDstRect tileScale =
    maybeWith with mSrcRect $ \srcPtr ->
    maybeWith with mDstRect $ \dstPtr ->
        toBool <$> c_sdlRenderTexture9GridTiled renderer tex srcPtr
                      (realToFrac leftW) (realToFrac rightW) (realToFrac topH) (realToFrac bottomH) (realToFrac scale)
                      dstPtr (realToFrac tileScale)

-- ** Drawing Geometry **
foreign import ccall unsafe "SDL_RenderGeometry"
  c_sdlRenderGeometry :: Ptr SDLRenderer -> Ptr SDLTexture -> Ptr SDLVertex -> CInt -> Ptr CInt -> CInt -> IO CBool

sdlRenderGeometry :: SDLRenderer -> Maybe SDLTexture -> [SDLVertex] -> Maybe [Int] -> IO Bool
sdlRenderGeometry (SDLRenderer renderer) mTex vertices mIndices =
  withArrayLen vertices $ \numVertices verticesPtr -> do -- Process vertices first
    let texPtr = maybe nullPtr (\(SDLTexture p) -> p) mTex

    -- Decide how to handle indices using case
    case mIndices of
      -- No indices: call C function with NULL pointer and 0 count
      Nothing ->
        toBool <$> c_sdlRenderGeometry renderer texPtr verticesPtr (fromIntegral numVertices) nullPtr 0

      -- Indices present: use withArrayLen for indices
      Just indices ->
        withArrayLen (map fromIntegral indices) $ \numIndices' indicesPtr' -> -- Get numIndices' and indicesPtr' here
          -- Call C function with the pointers and counts obtained
          toBool <$> c_sdlRenderGeometry renderer texPtr verticesPtr (fromIntegral numVertices) indicesPtr' (fromIntegral numIndices')

foreign import ccall unsafe "SDL_RenderGeometryRaw"
  c_sdlRenderGeometryRaw :: Ptr SDLRenderer -> Ptr SDLTexture -> Ptr CFloat -> CInt -> Ptr SDLFColor -> CInt -> Ptr CFloat -> CInt -> CInt -> Ptr () -> CInt -> CInt -> IO CBool

sdlRenderGeometryRaw :: SDLRenderer -> Maybe SDLTexture -> Ptr CFloat -> Int -> Ptr SDLFColor -> Int -> Ptr CFloat -> Int -> Int -> Ptr () -> Int -> Int -> IO Bool
sdlRenderGeometryRaw (SDLRenderer renderer) mTex xy xyStride color colorStride uv uvStride numVertices indices numIndices sizeIndices =
  toBool <$> c_sdlRenderGeometryRaw renderer
                                     (maybe nullPtr (\(SDLTexture p) -> p) mTex)
                                     xy (fromIntegral xyStride)
                                     color (fromIntegral colorStride)
                                     uv (fromIntegral uvStride)
                                     (fromIntegral numVertices)
                                     indices (fromIntegral numIndices) (fromIntegral sizeIndices)

-- ** Reading Pixels **
foreign import ccall unsafe "SDL_RenderReadPixels"
  c_sdlRenderReadPixels :: Ptr SDLRenderer -> Ptr SDLRect -> IO (Ptr SDLSurface)

sdlRenderReadPixels :: SDLRenderer -> Maybe SDLRect -> IO (Maybe (Ptr SDLSurface))
sdlRenderReadPixels (SDLRenderer renderer) mRect =
  maybeWith with mRect $ \rectPtr -> do
    ptr <- c_sdlRenderReadPixels renderer rectPtr
    if ptr == nullPtr
      then return Nothing
      else return $ Just ptr -- Returns raw Ptr SDLSurface

-- ** Presentation **
foreign import ccall safe "SDL_RenderPresent" -- Safe due to OS interaction
  c_sdlRenderPresent :: Ptr SDLRenderer -> IO CBool

sdlRenderPresent :: SDLRenderer -> IO Bool
sdlRenderPresent (SDLRenderer renderer) = toBool <$> c_sdlRenderPresent renderer

foreign import ccall unsafe "SDL_SetRenderVSync"
  c_sdlSetRenderVSync :: Ptr SDLRenderer -> CInt -> IO CBool

sdlSetRenderVSync :: SDLRenderer -> Int -> IO Bool
sdlSetRenderVSync (SDLRenderer renderer) vsync = toBool <$> c_sdlSetRenderVSync renderer (fromIntegral vsync)

foreign import ccall unsafe "SDL_GetRenderVSync"
  c_sdlGetRenderVSync :: Ptr SDLRenderer -> Ptr CInt -> IO CBool

sdlGetRenderVSync :: SDLRenderer -> IO (Maybe Int)
sdlGetRenderVSync (SDLRenderer renderer) =
  alloca $ \vsyncPtr -> do
    success <- c_sdlGetRenderVSync renderer vsyncPtr
    if not (toBool success)
      then return Nothing
      else Just . fromIntegral <$> peek vsyncPtr

-- ** Synchronization / Backend Access **
foreign import ccall unsafe "SDL_FlushRenderer"
  c_sdlFlushRenderer :: Ptr SDLRenderer -> IO CBool

sdlFlushRenderer :: SDLRenderer -> IO Bool
sdlFlushRenderer (SDLRenderer renderer) = toBool <$> c_sdlFlushRenderer renderer

-- Note: Metal layer/encoder return Ptr () - user must cast appropriately
foreign import ccall unsafe "SDL_GetRenderMetalLayer"
  c_sdlGetRenderMetalLayer :: Ptr SDLRenderer -> IO (Ptr ())

sdlGetRenderMetalLayer :: SDLRenderer -> IO (Ptr ())
sdlGetRenderMetalLayer (SDLRenderer renderer) = c_sdlGetRenderMetalLayer renderer

foreign import ccall unsafe "SDL_GetRenderMetalCommandEncoder"
  c_sdlGetRenderMetalCommandEncoder :: Ptr SDLRenderer -> IO (Ptr ())

sdlGetRenderMetalCommandEncoder :: SDLRenderer -> IO (Ptr ())
sdlGetRenderMetalCommandEncoder (SDLRenderer renderer) = c_sdlGetRenderMetalCommandEncoder renderer

foreign import ccall unsafe "SDL_AddVulkanRenderSemaphores"
    c_sdlAddVulkanRenderSemaphores :: Ptr SDLRenderer -> Word32 -> Int64 -> Int64 -> IO CBool

sdlAddVulkanRenderSemaphores :: SDLRenderer -> Word32 -> Int64 -> Int64 -> IO Bool
sdlAddVulkanRenderSemaphores (SDLRenderer renderer) waitStageMask waitSemaphore signalSemaphore =
    toBool <$> c_sdlAddVulkanRenderSemaphores renderer waitStageMask waitSemaphore signalSemaphore

-- ** Debugging **
foreign import ccall unsafe "SDL_RenderDebugText"
  c_sdlRenderDebugText :: Ptr SDLRenderer -> CFloat -> CFloat -> CString -> IO CBool

sdlRenderDebugText :: SDLRenderer -> Float -> Float -> String -> IO Bool
sdlRenderDebugText (SDLRenderer renderer) x y str =
  withCString str $ \cStr ->
    toBool <$> c_sdlRenderDebugText renderer (realToFrac x) (realToFrac y) cStr

-- ** Texture Defaults **
foreign import ccall unsafe "SDL_SetDefaultTextureScaleMode"
    c_sdlSetDefaultTextureScaleMode :: Ptr SDLRenderer -> CInt -> IO CBool

sdlSetDefaultTextureScaleMode :: SDLRenderer -> SDLScaleMode -> IO Bool
sdlSetDefaultTextureScaleMode (SDLRenderer renderer) scaleMode = -- Pass the enum value directly
    fromCBool <$> c_sdlSetDefaultTextureScaleMode renderer (fromIntegral $ fromEnum scaleMode)

foreign import ccall unsafe "SDL_GetDefaultTextureScaleMode"
    c_sdlGetDefaultTextureScaleMode :: Ptr SDLRenderer -> Ptr CInt -> IO CBool -- << Must be Ptr CInt

-- Haskell wrapper (Ensure alloca also uses Ptr CInt)
sdlGetDefaultTextureScaleMode :: SDLRenderer -> IO (Maybe SDLScaleMode)
sdlGetDefaultTextureScaleMode (SDLRenderer renderer) =
    alloca $ \(modePtr :: Ptr CInt) -> do -- Allocate space for CInt
        success <- c_sdlGetDefaultTextureScaleMode renderer modePtr -- Pass Ptr CInt
        if not (toBool success)
            then return Nothing
            else do
                modeCInt <- peek modePtr -- Peek CInt
                return $ Just (toEnum $ fromIntegral modeCInt) -- Convert

-- ** Custom GPU State (GPU Renderer) **
data SDLGPURenderStateDesc = SDLGPURenderStateDesc
    { gpuRenderStateDescVersion         :: Word32
    , gpuRenderStateDescFragmentShader  :: SDLGPUShader
    , gpuRenderStateDescSamplerBindings :: [SDLGPUTextureSamplerBinding]
    , gpuRenderStateDescStorageTextures :: [SDLGPUTexture]
    , gpuRenderStateDescStorageBuffers  :: [SDLGPUBuffer]
    } deriving (Show, Eq)

-- Helper for marshalling SDL_GPURenderStateDesc
withGPURenderStateDesc SDLGPURenderStateDesc{..} f =
    withArrayLen gpuRenderStateDescSamplerBindings $ \numSamplers samplersPtr ->
    withArrayLen (map (\(SDLGPUTexture p) -> p) gpuRenderStateDescStorageTextures) $ \numTex texPtrs ->
    withArrayLen (map (\(SDLGPUBuffer p) -> p) gpuRenderStateDescStorageBuffers) $ \numBuf bufPtrs ->
    allocaBytes #{size SDL_GPURenderStateDesc} $ \descPtr -> do
        let (SDLGPUShader fsPtr) = gpuRenderStateDescFragmentShader
        -- Correct the poke for version:
        #{poke SDL_GPURenderStateDesc, version} descPtr (fromIntegral gpuRenderStateDescVersion :: CUInt) -- Convert Word32 to CUInt
        #{poke SDL_GPURenderStateDesc, fragment_shader} descPtr fsPtr
        #{poke SDL_GPURenderStateDesc, num_sampler_bindings} descPtr (fromIntegral numSamplers :: CInt)
        #{poke SDL_GPURenderStateDesc, sampler_bindings} descPtr samplersPtr
        #{poke SDL_GPURenderStateDesc, num_storage_textures} descPtr (fromIntegral numTex :: CInt)
        #{poke SDL_GPURenderStateDesc, storage_textures} descPtr texPtrs
        #{poke SDL_GPURenderStateDesc, num_storage_buffers} descPtr (fromIntegral numBuf :: CInt)
        #{poke SDL_GPURenderStateDesc, storage_buffers} descPtr bufPtrs
        f (castPtr descPtr)

foreign import ccall unsafe "SDL_CreateGPURenderState"
    c_sdlCreateGPURenderState :: Ptr SDLRenderer -> Ptr () -> IO (Ptr SDLGPURenderState)

sdlCreateGPURenderState :: SDLRenderer -> SDLGPURenderStateDesc -> IO (Maybe SDLGPURenderState)
sdlCreateGPURenderState (SDLRenderer renderer) desc =
    withGPURenderStateDesc desc $ \descPtr -> do
        statePtr <- c_sdlCreateGPURenderState renderer descPtr
        if statePtr == nullPtr
            then return Nothing
            else return $ Just (SDLGPURenderState statePtr)

foreign import ccall unsafe "SDL_SetGPURenderStateFragmentUniforms"
    c_sdlSetGPURenderStateFragmentUniforms :: Ptr SDLGPURenderState -> CUInt -> Ptr () -> CUInt -> IO CBool

-- Push raw byte data as GPU state fragment uniform data.
sdlSetGPURenderStateFragmentUniformsRaw :: SDLGPURenderState -> Word32 -> Ptr () -> Word32 -> IO Bool
sdlSetGPURenderStateFragmentUniformsRaw (SDLGPURenderState state) slotIndex dataPtr len =
    toBool <$> c_sdlSetGPURenderStateFragmentUniforms state (fromIntegral slotIndex) dataPtr (fromIntegral len)

-- Convenience function to push a Storable value.
sdlSetGPURenderStateFragmentUniforms :: Storable a => SDLGPURenderState -> Word32 -> a -> IO Bool
sdlSetGPURenderStateFragmentUniforms state slotIndex dat =
    with dat $ \dataPtr ->
        sdlSetGPURenderStateFragmentUniformsRaw state slotIndex (castPtr dataPtr) (fromIntegral (sizeOf dat))

foreign import ccall unsafe "SDL_SetRenderGPUState"
    c_sdlSetRenderGPUState :: Ptr SDLRenderer -> Ptr SDLGPURenderState -> IO CBool

sdlSetRenderGPUState :: SDLRenderer -> Maybe SDLGPURenderState -> IO Bool
sdlSetRenderGPUState (SDLRenderer renderer) mState =
    toBool <$> c_sdlSetRenderGPUState renderer (maybe nullPtr (\(SDLGPURenderState s) -> s) mState)

foreign import ccall unsafe "SDL_DestroyGPURenderState"
    c_sdlDestroyGPURenderState :: Ptr SDLGPURenderState -> IO ()

sdlDestroyGPURenderState :: SDLGPURenderState -> IO ()
sdlDestroyGPURenderState (SDLGPURenderState state) = c_sdlDestroyGPURenderState state


-- Property String Definitions (Moved after types/enums for clarity)
pattern SDL_PROP_RENDERER_CREATE_NAME_STRING :: String
pattern SDL_PROP_RENDERER_CREATE_NAME_STRING                                = "SDL.renderer.create.name"
pattern SDL_PROP_RENDERER_CREATE_WINDOW_POINTER :: String
pattern SDL_PROP_RENDERER_CREATE_WINDOW_POINTER                             = "SDL.renderer.create.window"
pattern SDL_PROP_RENDERER_CREATE_SURFACE_POINTER :: String
pattern SDL_PROP_RENDERER_CREATE_SURFACE_POINTER                            = "SDL.renderer.create.surface"
pattern SDL_PROP_RENDERER_CREATE_OUTPUT_COLORSPACE_NUMBER :: String
pattern SDL_PROP_RENDERER_CREATE_OUTPUT_COLORSPACE_NUMBER                   = "SDL.renderer.create.output_colorspace"
pattern SDL_PROP_RENDERER_CREATE_PRESENT_VSYNC_NUMBER :: String
pattern SDL_PROP_RENDERER_CREATE_PRESENT_VSYNC_NUMBER                       = "SDL.renderer.create.present_vsync"
pattern SDL_PROP_RENDERER_CREATE_GPU_SHADERS_SPIRV_BOOLEAN :: String
pattern SDL_PROP_RENDERER_CREATE_GPU_SHADERS_SPIRV_BOOLEAN                  = "SDL.renderer.create.gpu.shaders_spirv"
pattern SDL_PROP_RENDERER_CREATE_GPU_SHADERS_DXIL_BOOLEAN :: String
pattern SDL_PROP_RENDERER_CREATE_GPU_SHADERS_DXIL_BOOLEAN                   = "SDL.renderer.create.gpu.shaders_dxil"
pattern SDL_PROP_RENDERER_CREATE_GPU_SHADERS_MSL_BOOLEAN :: String
pattern SDL_PROP_RENDERER_CREATE_GPU_SHADERS_MSL_BOOLEAN                    = "SDL.renderer.create.gpu.shaders_msl"
pattern SDL_PROP_RENDERER_CREATE_VULKAN_INSTANCE_POINTER :: String
pattern SDL_PROP_RENDERER_CREATE_VULKAN_INSTANCE_POINTER                    = "SDL.renderer.create.vulkan.instance"
pattern SDL_PROP_RENDERER_CREATE_VULKAN_SURFACE_NUMBER :: String
pattern SDL_PROP_RENDERER_CREATE_VULKAN_SURFACE_NUMBER                      = "SDL.renderer.create.vulkan.surface"
pattern SDL_PROP_RENDERER_CREATE_VULKAN_PHYSICAL_DEVICE_POINTER :: String
pattern SDL_PROP_RENDERER_CREATE_VULKAN_PHYSICAL_DEVICE_POINTER             = "SDL.renderer.create.vulkan.physical_device"
pattern SDL_PROP_RENDERER_CREATE_VULKAN_DEVICE_POINTER :: String
pattern SDL_PROP_RENDERER_CREATE_VULKAN_DEVICE_POINTER                      = "SDL.renderer.create.vulkan.device"
pattern SDL_PROP_RENDERER_CREATE_VULKAN_GRAPHICS_QUEUE_FAMILY_INDEX_NUMBER :: String
pattern SDL_PROP_RENDERER_CREATE_VULKAN_GRAPHICS_QUEUE_FAMILY_INDEX_NUMBER  = "SDL.renderer.create.vulkan.graphics_queue_family_index"
pattern SDL_PROP_RENDERER_CREATE_VULKAN_PRESENT_QUEUE_FAMILY_INDEX_NUMBER :: String
pattern SDL_PROP_RENDERER_CREATE_VULKAN_PRESENT_QUEUE_FAMILY_INDEX_NUMBER   = "SDL.renderer.create.vulkan.present_queue_family_index"

pattern SDL_PROP_RENDERER_NAME_STRING :: String
pattern SDL_PROP_RENDERER_NAME_STRING                               = "SDL.renderer.name"
pattern SDL_PROP_RENDERER_WINDOW_POINTER :: String
pattern SDL_PROP_RENDERER_WINDOW_POINTER                            = "SDL.renderer.window"
pattern SDL_PROP_RENDERER_SURFACE_POINTER :: String
pattern SDL_PROP_RENDERER_SURFACE_POINTER                           = "SDL.renderer.surface"
pattern SDL_PROP_RENDERER_VSYNC_NUMBER :: String
pattern SDL_PROP_RENDERER_VSYNC_NUMBER                              = "SDL.renderer.vsync"
pattern SDL_PROP_RENDERER_MAX_TEXTURE_SIZE_NUMBER :: String
pattern SDL_PROP_RENDERER_MAX_TEXTURE_SIZE_NUMBER                   = "SDL.renderer.max_texture_size"
pattern SDL_PROP_RENDERER_TEXTURE_FORMATS_POINTER :: String
pattern SDL_PROP_RENDERER_TEXTURE_FORMATS_POINTER                   = "SDL.renderer.texture_formats"
pattern SDL_PROP_RENDERER_OUTPUT_COLORSPACE_NUMBER :: String
pattern SDL_PROP_RENDERER_OUTPUT_COLORSPACE_NUMBER                  = "SDL.renderer.output_colorspace"
pattern SDL_PROP_RENDERER_HDR_ENABLED_BOOLEAN :: String
pattern SDL_PROP_RENDERER_HDR_ENABLED_BOOLEAN                       = "SDL.renderer.HDR_enabled"
pattern SDL_PROP_RENDERER_SDR_WHITE_POINT_FLOAT :: String
pattern SDL_PROP_RENDERER_SDR_WHITE_POINT_FLOAT                     = "SDL.renderer.SDR_white_point"
pattern SDL_PROP_RENDERER_HDR_HEADROOM_FLOAT :: String
pattern SDL_PROP_RENDERER_HDR_HEADROOM_FLOAT                        = "SDL.renderer.HDR_headroom"
pattern SDL_PROP_RENDERER_D3D9_DEVICE_POINTER :: String
pattern SDL_PROP_RENDERER_D3D9_DEVICE_POINTER                       = "SDL.renderer.d3d9.device"
pattern SDL_PROP_RENDERER_D3D11_DEVICE_POINTER :: String
pattern SDL_PROP_RENDERER_D3D11_DEVICE_POINTER                      = "SDL.renderer.d3d11.device"
pattern SDL_PROP_RENDERER_D3D11_SWAPCHAIN_POINTER :: String
pattern SDL_PROP_RENDERER_D3D11_SWAPCHAIN_POINTER                   = "SDL.renderer.d3d11.swap_chain"
pattern SDL_PROP_RENDERER_D3D12_DEVICE_POINTER :: String
pattern SDL_PROP_RENDERER_D3D12_DEVICE_POINTER                      = "SDL.renderer.d3d12.device"
pattern SDL_PROP_RENDERER_D3D12_SWAPCHAIN_POINTER :: String
pattern SDL_PROP_RENDERER_D3D12_SWAPCHAIN_POINTER                   = "SDL.renderer.d3d12.swap_chain"
pattern SDL_PROP_RENDERER_D3D12_COMMAND_QUEUE_POINTER :: String
pattern SDL_PROP_RENDERER_D3D12_COMMAND_QUEUE_POINTER               = "SDL.renderer.d3d12.command_queue"
pattern SDL_PROP_RENDERER_VULKAN_INSTANCE_POINTER :: String
pattern SDL_PROP_RENDERER_VULKAN_INSTANCE_POINTER                   = "SDL.renderer.vulkan.instance"
pattern SDL_PROP_RENDERER_VULKAN_SURFACE_NUMBER :: String
pattern SDL_PROP_RENDERER_VULKAN_SURFACE_NUMBER                     = "SDL.renderer.vulkan.surface"
pattern SDL_PROP_RENDERER_VULKAN_PHYSICAL_DEVICE_POINTER :: String
pattern SDL_PROP_RENDERER_VULKAN_PHYSICAL_DEVICE_POINTER            = "SDL.renderer.vulkan.physical_device"
pattern SDL_PROP_RENDERER_VULKAN_DEVICE_POINTER :: String
pattern SDL_PROP_RENDERER_VULKAN_DEVICE_POINTER                     = "SDL.renderer.vulkan.device"
pattern SDL_PROP_RENDERER_VULKAN_GRAPHICS_QUEUE_FAMILY_INDEX_NUMBER :: String
pattern SDL_PROP_RENDERER_VULKAN_GRAPHICS_QUEUE_FAMILY_INDEX_NUMBER = "SDL.renderer.vulkan.graphics_queue_family_index"
pattern SDL_PROP_RENDERER_VULKAN_PRESENT_QUEUE_FAMILY_INDEX_NUMBER :: String
pattern SDL_PROP_RENDERER_VULKAN_PRESENT_QUEUE_FAMILY_INDEX_NUMBER  = "SDL.renderer.vulkan.present_queue_family_index"
pattern SDL_PROP_RENDERER_VULKAN_SWAPCHAIN_IMAGE_COUNT_NUMBER :: String
pattern SDL_PROP_RENDERER_VULKAN_SWAPCHAIN_IMAGE_COUNT_NUMBER       = "SDL.renderer.vulkan.swapchain_image_count"
pattern SDL_PROP_RENDERER_GPU_DEVICE_POINTER :: String
pattern SDL_PROP_RENDERER_GPU_DEVICE_POINTER                        = "SDL.renderer.gpu.device"

pattern SDL_PROP_TEXTURE_CREATE_COLORSPACE_NUMBER :: String
pattern SDL_PROP_TEXTURE_CREATE_COLORSPACE_NUMBER           = "SDL.texture.create.colorspace"
pattern SDL_PROP_TEXTURE_CREATE_FORMAT_NUMBER :: String
pattern SDL_PROP_TEXTURE_CREATE_FORMAT_NUMBER               = "SDL.texture.create.format"
pattern SDL_PROP_TEXTURE_CREATE_ACCESS_NUMBER :: String
pattern SDL_PROP_TEXTURE_CREATE_ACCESS_NUMBER               = "SDL.texture.create.access"
pattern SDL_PROP_TEXTURE_CREATE_WIDTH_NUMBER :: String
pattern SDL_PROP_TEXTURE_CREATE_WIDTH_NUMBER                = "SDL.texture.create.width"
pattern SDL_PROP_TEXTURE_CREATE_HEIGHT_NUMBER :: String
pattern SDL_PROP_TEXTURE_CREATE_HEIGHT_NUMBER               = "SDL.texture.create.height"
pattern SDL_PROP_TEXTURE_CREATE_SDR_WHITE_POINT_FLOAT :: String
pattern SDL_PROP_TEXTURE_CREATE_SDR_WHITE_POINT_FLOAT       = "SDL.texture.create.SDR_white_point"
pattern SDL_PROP_TEXTURE_CREATE_HDR_HEADROOM_FLOAT :: String
pattern SDL_PROP_TEXTURE_CREATE_HDR_HEADROOM_FLOAT          = "SDL.texture.create.HDR_headroom"
pattern SDL_PROP_TEXTURE_CREATE_D3D11_TEXTURE_POINTER :: String
pattern SDL_PROP_TEXTURE_CREATE_D3D11_TEXTURE_POINTER       = "SDL.texture.create.d3d11.texture"
pattern SDL_PROP_TEXTURE_CREATE_D3D11_TEXTURE_U_POINTER :: String
pattern SDL_PROP_TEXTURE_CREATE_D3D11_TEXTURE_U_POINTER     = "SDL.texture.create.d3d11.texture_u"
pattern SDL_PROP_TEXTURE_CREATE_D3D11_TEXTURE_V_POINTER :: String
pattern SDL_PROP_TEXTURE_CREATE_D3D11_TEXTURE_V_POINTER     = "SDL.texture.create.d3d11.texture_v"
pattern SDL_PROP_TEXTURE_CREATE_D3D12_TEXTURE_POINTER :: String
pattern SDL_PROP_TEXTURE_CREATE_D3D12_TEXTURE_POINTER       = "SDL.texture.create.d3d12.texture"
pattern SDL_PROP_TEXTURE_CREATE_D3D12_TEXTURE_U_POINTER :: String
pattern SDL_PROP_TEXTURE_CREATE_D3D12_TEXTURE_U_POINTER     = "SDL.texture.create.d3d12.texture_u"
pattern SDL_PROP_TEXTURE_CREATE_D3D12_TEXTURE_V_POINTER :: String
pattern SDL_PROP_TEXTURE_CREATE_D3D12_TEXTURE_V_POINTER     = "SDL.texture.create.d3d12.texture_v"
pattern SDL_PROP_TEXTURE_CREATE_METAL_PIXELBUFFER_POINTER :: String
pattern SDL_PROP_TEXTURE_CREATE_METAL_PIXELBUFFER_POINTER   = "SDL.texture.create.metal.pixelbuffer"
pattern SDL_PROP_TEXTURE_CREATE_OPENGL_TEXTURE_NUMBER :: String
pattern SDL_PROP_TEXTURE_CREATE_OPENGL_TEXTURE_NUMBER       = "SDL.texture.create.opengl.texture"
pattern SDL_PROP_TEXTURE_CREATE_OPENGL_TEXTURE_UV_NUMBER :: String
pattern SDL_PROP_TEXTURE_CREATE_OPENGL_TEXTURE_UV_NUMBER    = "SDL.texture.create.opengl.texture_uv"
pattern SDL_PROP_TEXTURE_CREATE_OPENGL_TEXTURE_U_NUMBER :: String
pattern SDL_PROP_TEXTURE_CREATE_OPENGL_TEXTURE_U_NUMBER     = "SDL.texture.create.opengl.texture_u"
pattern SDL_PROP_TEXTURE_CREATE_OPENGL_TEXTURE_V_NUMBER :: String
pattern SDL_PROP_TEXTURE_CREATE_OPENGL_TEXTURE_V_NUMBER     = "SDL.texture.create.opengl.texture_v"
pattern SDL_PROP_TEXTURE_CREATE_OPENGLES2_TEXTURE_NUMBER :: String
pattern SDL_PROP_TEXTURE_CREATE_OPENGLES2_TEXTURE_NUMBER    = "SDL.texture.create.opengles2.texture"
pattern SDL_PROP_TEXTURE_CREATE_OPENGLES2_TEXTURE_UV_NUMBER :: String
pattern SDL_PROP_TEXTURE_CREATE_OPENGLES2_TEXTURE_UV_NUMBER = "SDL.texture.create.opengles2.texture_uv"
pattern SDL_PROP_TEXTURE_CREATE_OPENGLES2_TEXTURE_U_NUMBER :: String
pattern SDL_PROP_TEXTURE_CREATE_OPENGLES2_TEXTURE_U_NUMBER  = "SDL.texture.create.opengles2.texture_u"
pattern SDL_PROP_TEXTURE_CREATE_OPENGLES2_TEXTURE_V_NUMBER :: String
pattern SDL_PROP_TEXTURE_CREATE_OPENGLES2_TEXTURE_V_NUMBER  = "SDL.texture.create.opengles2.texture_v"
pattern SDL_PROP_TEXTURE_CREATE_VULKAN_TEXTURE_NUMBER :: String
pattern SDL_PROP_TEXTURE_CREATE_VULKAN_TEXTURE_NUMBER       = "SDL.texture.create.vulkan.texture"

pattern SDL_PROP_TEXTURE_COLORSPACE_NUMBER :: String
pattern SDL_PROP_TEXTURE_COLORSPACE_NUMBER                  = "SDL.texture.colorspace"
pattern SDL_PROP_TEXTURE_FORMAT_NUMBER :: String
pattern SDL_PROP_TEXTURE_FORMAT_NUMBER                      = "SDL.texture.format"
pattern SDL_PROP_TEXTURE_ACCESS_NUMBER :: String
pattern SDL_PROP_TEXTURE_ACCESS_NUMBER                      = "SDL.texture.access"
pattern SDL_PROP_TEXTURE_WIDTH_NUMBER :: String
pattern SDL_PROP_TEXTURE_WIDTH_NUMBER                       = "SDL.texture.width"
pattern SDL_PROP_TEXTURE_HEIGHT_NUMBER :: String
pattern SDL_PROP_TEXTURE_HEIGHT_NUMBER                      = "SDL.texture.height"
pattern SDL_PROP_TEXTURE_SDR_WHITE_POINT_FLOAT :: String
pattern SDL_PROP_TEXTURE_SDR_WHITE_POINT_FLOAT              = "SDL.texture.SDR_white_point"
pattern SDL_PROP_TEXTURE_HDR_HEADROOM_FLOAT :: String
pattern SDL_PROP_TEXTURE_HDR_HEADROOM_FLOAT                 = "SDL.texture.HDR_headroom"
pattern SDL_PROP_TEXTURE_D3D11_TEXTURE_POINTER :: String
pattern SDL_PROP_TEXTURE_D3D11_TEXTURE_POINTER              = "SDL.texture.d3d11.texture"
pattern SDL_PROP_TEXTURE_D3D11_TEXTURE_U_POINTER :: String
pattern SDL_PROP_TEXTURE_D3D11_TEXTURE_U_POINTER            = "SDL.texture.d3d11.texture_u"
pattern SDL_PROP_TEXTURE_D3D11_TEXTURE_V_POINTER :: String
pattern SDL_PROP_TEXTURE_D3D11_TEXTURE_V_POINTER            = "SDL.texture.d3d11.texture_v"
pattern SDL_PROP_TEXTURE_D3D12_TEXTURE_POINTER :: String
pattern SDL_PROP_TEXTURE_D3D12_TEXTURE_POINTER              = "SDL.texture.d3d12.texture"
pattern SDL_PROP_TEXTURE_D3D12_TEXTURE_U_POINTER :: String
pattern SDL_PROP_TEXTURE_D3D12_TEXTURE_U_POINTER            = "SDL.texture.d3d12.texture_u"
pattern SDL_PROP_TEXTURE_D3D12_TEXTURE_V_POINTER :: String
pattern SDL_PROP_TEXTURE_D3D12_TEXTURE_V_POINTER            = "SDL.texture.d3d12.texture_v"
pattern SDL_PROP_TEXTURE_OPENGL_TEXTURE_NUMBER :: String
pattern SDL_PROP_TEXTURE_OPENGL_TEXTURE_NUMBER              = "SDL.texture.opengl.texture"
pattern SDL_PROP_TEXTURE_OPENGL_TEXTURE_UV_NUMBER :: String
pattern SDL_PROP_TEXTURE_OPENGL_TEXTURE_UV_NUMBER           = "SDL.texture.opengl.texture_uv"
pattern SDL_PROP_TEXTURE_OPENGL_TEXTURE_U_NUMBER :: String
pattern SDL_PROP_TEXTURE_OPENGL_TEXTURE_U_NUMBER            = "SDL.texture.opengl.texture_u"
pattern SDL_PROP_TEXTURE_OPENGL_TEXTURE_V_NUMBER :: String
pattern SDL_PROP_TEXTURE_OPENGL_TEXTURE_V_NUMBER            = "SDL.texture.opengl.texture_v"
pattern SDL_PROP_TEXTURE_OPENGL_TEXTURE_TARGET_NUMBER :: String
pattern SDL_PROP_TEXTURE_OPENGL_TEXTURE_TARGET_NUMBER       = "SDL.texture.opengl.target"
pattern SDL_PROP_TEXTURE_OPENGL_TEX_W_FLOAT :: String
pattern SDL_PROP_TEXTURE_OPENGL_TEX_W_FLOAT                 = "SDL.texture.opengl.tex_w"
pattern SDL_PROP_TEXTURE_OPENGL_TEX_H_FLOAT :: String
pattern SDL_PROP_TEXTURE_OPENGL_TEX_H_FLOAT                 = "SDL.texture.opengl.tex_h"
pattern SDL_PROP_TEXTURE_OPENGLES2_TEXTURE_NUMBER :: String
pattern SDL_PROP_TEXTURE_OPENGLES2_TEXTURE_NUMBER           = "SDL.texture.opengles2.texture"
pattern SDL_PROP_TEXTURE_OPENGLES2_TEXTURE_UV_NUMBER :: String
pattern SDL_PROP_TEXTURE_OPENGLES2_TEXTURE_UV_NUMBER        = "SDL.texture.opengles2.texture_uv"
pattern SDL_PROP_TEXTURE_OPENGLES2_TEXTURE_U_NUMBER :: String
pattern SDL_PROP_TEXTURE_OPENGLES2_TEXTURE_U_NUMBER         = "SDL.texture.opengles2.texture_u"
pattern SDL_PROP_TEXTURE_OPENGLES2_TEXTURE_V_NUMBER :: String
pattern SDL_PROP_TEXTURE_OPENGLES2_TEXTURE_V_NUMBER         = "SDL.texture.opengles2.texture_v"
pattern SDL_PROP_TEXTURE_OPENGLES2_TEXTURE_TARGET_NUMBER :: String
pattern SDL_PROP_TEXTURE_OPENGLES2_TEXTURE_TARGET_NUMBER    = "SDL.texture.opengles2.target"
pattern SDL_PROP_TEXTURE_VULKAN_TEXTURE_NUMBER :: String
pattern SDL_PROP_TEXTURE_VULKAN_TEXTURE_NUMBER              = "SDL.texture.vulkan.texture"

fromCBool :: CBool -> Bool
fromCBool = toBool
