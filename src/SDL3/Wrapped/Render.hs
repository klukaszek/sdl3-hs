{-# LANGUAGE NoMonomorphismRestriction #-}

module SDL3.Wrapped.Render
  ( module SDL3.Raw.Render
  , sdlGetNumRenderDrivers
  , sdlGetRenderDriver
  , sdlCreateWindowAndRenderer
  , sdlCreateRenderer
  , sdlCreateRendererWithProperties
  , sdlCreateGPURenderer
  , sdlGetGPURendererDevice
  , sdlCreateSoftwareRenderer
  , sdlDestroyRenderer
  , sdlGetRenderer
  , sdlGetRenderWindow
  , sdlGetRendererName
  , sdlGetRendererProperties
  , sdlGetRenderOutputSize
  , sdlGetCurrentRenderOutputSize
  , sdlCreateTexture
  , sdlCreateTextureFromSurface
  , sdlCreateTextureWithProperties
  , sdlDestroyTexture
  , sdlGetTextureProperties
  , sdlGetRendererFromTexture
  , sdlGetTextureSize
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
  , sdlRenderDebugTextFormat
  , sdlGetTextureScaleMode
  , sdlSetTexturePalette
  , sdlGetTexturePalette
  , sdlUpdateTexture
  , sdlUpdateYUVTexture
  , sdlUpdateNVTexture
  , sdlLockTexture
  , sdlLockTextureToSurface
  , sdlUnlockTexture
  , sdlSetRenderTarget
  , sdlGetRenderTarget
  , sdlSetRenderLogicalPresentation
  , sdlGetRenderLogicalPresentation
  , sdlGetRenderLogicalPresentationRect
  , sdlRenderCoordinatesFromWindow
  , sdlRenderCoordinatesToWindow
  , sdlConvertEventToRenderCoordinates
  , sdlSetRenderViewport
  , sdlGetRenderViewport
  , sdlRenderViewportSet
  , sdlGetRenderSafeArea
  , sdlSetRenderClipRect
  , sdlGetRenderClipRect
  , sdlRenderClipEnabled
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
  , sdlRenderClear
  , sdlRenderPoint
  , sdlRenderPoints
  , sdlRenderLine
  , sdlRenderLines
  , sdlRenderRect
  , sdlRenderRects
  , sdlRenderFillRect
  , sdlRenderFillRects
  , sdlRenderTexture
  , sdlRenderTextureRotated
  , sdlRenderTextureAffine
  , sdlRenderTextureTiled
  , sdlRenderTexture9Grid
  , sdlRenderTexture9GridTiled
  , sdlRenderGeometry
  , sdlRenderGeometryRaw
  , sdlRenderReadPixels
  , sdlRenderPresent
  , sdlSetRenderVSync
  , sdlGetRenderVSync
  , sdlFlushRenderer
  , sdlGetRenderMetalLayer
  , sdlGetRenderMetalCommandEncoder
  , sdlAddVulkanRenderSemaphores
  , sdlRenderDebugText
  , sdlSetDefaultTextureScaleMode
  , sdlGetDefaultTextureScaleMode
  , sdlCreateGPURenderState
  , sdlSetGPURenderStateFragmentUniforms
  , sdlSetGPURenderState
  , sdlDestroyGPURenderState
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import SDL3.Raw.Render hiding
  ( sdlGetNumRenderDrivers
  , sdlGetRenderDriver
  , sdlCreateWindowAndRenderer
  , sdlCreateRenderer
  , sdlCreateRendererWithProperties
  , sdlCreateGPURenderer
  , sdlGetGPURendererDevice
  , sdlCreateSoftwareRenderer
  , sdlDestroyRenderer
  , sdlGetRenderer
  , sdlGetRenderWindow
  , sdlGetRendererName
  , sdlGetRendererProperties
  , sdlGetRenderOutputSize
  , sdlGetCurrentRenderOutputSize
  , sdlCreateTexture
  , sdlCreateTextureFromSurface
  , sdlCreateTextureWithProperties
  , sdlDestroyTexture
  , sdlGetTextureProperties
  , sdlGetRendererFromTexture
  , sdlGetTextureSize
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
  , sdlRenderDebugTextFormat
  , sdlGetTextureScaleMode
  , sdlSetTexturePalette
  , sdlGetTexturePalette
  , sdlUpdateTexture
  , sdlUpdateYUVTexture
  , sdlUpdateNVTexture
  , sdlLockTexture
  , sdlLockTextureToSurface
  , sdlUnlockTexture
  , sdlSetRenderTarget
  , sdlGetRenderTarget
  , sdlSetRenderLogicalPresentation
  , sdlGetRenderLogicalPresentation
  , sdlGetRenderLogicalPresentationRect
  , sdlRenderCoordinatesFromWindow
  , sdlRenderCoordinatesToWindow
  , sdlConvertEventToRenderCoordinates
  , sdlSetRenderViewport
  , sdlGetRenderViewport
  , sdlRenderViewportSet
  , sdlGetRenderSafeArea
  , sdlSetRenderClipRect
  , sdlGetRenderClipRect
  , sdlRenderClipEnabled
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
  , sdlRenderClear
  , sdlRenderPoint
  , sdlRenderPoints
  , sdlRenderLine
  , sdlRenderLines
  , sdlRenderRect
  , sdlRenderRects
  , sdlRenderFillRect
  , sdlRenderFillRects
  , sdlRenderTexture
  , sdlRenderTextureRotated
  , sdlRenderTextureAffine
  , sdlRenderTextureTiled
  , sdlRenderTexture9Grid
  , sdlRenderTexture9GridTiled
  , sdlRenderGeometry
  , sdlRenderGeometryRaw
  , sdlRenderReadPixels
  , sdlRenderPresent
  , sdlSetRenderVSync
  , sdlGetRenderVSync
  , sdlFlushRenderer
  , sdlGetRenderMetalLayer
  , sdlGetRenderMetalCommandEncoder
  , sdlAddVulkanRenderSemaphores
  , sdlRenderDebugText
  , sdlSetDefaultTextureScaleMode
  , sdlGetDefaultTextureScaleMode
  , sdlCreateGPURenderState
  , sdlSetGPURenderStateFragmentUniforms
  , sdlSetGPURenderState
  , sdlDestroyGPURenderState
  )
import qualified SDL3.Raw.Render as Raw



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

liftIO8 :: MonadIO m => (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> IO r) -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> m r
liftIO8 f x1 x2 x3 x4 x5 x6 x7 x8 = liftIO (f x1 x2 x3 x4 x5 x6 x7 x8)

liftIO9 :: MonadIO m => (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> IO r) -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> m r
liftIO9 f x1 x2 x3 x4 x5 x6 x7 x8 x9 = liftIO (f x1 x2 x3 x4 x5 x6 x7 x8 x9)

liftIO10 :: MonadIO m => (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> IO r) -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> m r
liftIO10 f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 = liftIO (f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)

liftIO11 :: MonadIO m => (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> IO r) -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> m r
liftIO11 f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 = liftIO (f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11)

liftIO12 :: MonadIO m => (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> IO r) -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> m r
liftIO12 f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 = liftIO (f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12)

sdlGetNumRenderDrivers = liftIO0 Raw.sdlGetNumRenderDrivers
sdlGetRenderDriver = liftIO1 Raw.sdlGetRenderDriver
sdlCreateWindowAndRenderer = liftIO4 Raw.sdlCreateWindowAndRenderer
sdlCreateRenderer = liftIO2 Raw.sdlCreateRenderer
sdlCreateRendererWithProperties = liftIO1 Raw.sdlCreateRendererWithProperties
sdlCreateGPURenderer = liftIO2 Raw.sdlCreateGPURenderer
sdlGetGPURendererDevice = liftIO1 Raw.sdlGetGPURendererDevice
sdlCreateSoftwareRenderer = liftIO1 Raw.sdlCreateSoftwareRenderer
sdlDestroyRenderer = liftIO1 Raw.sdlDestroyRenderer
sdlGetRenderer = liftIO1 Raw.sdlGetRenderer
sdlGetRenderWindow = liftIO1 Raw.sdlGetRenderWindow
sdlGetRendererName = liftIO1 Raw.sdlGetRendererName
sdlGetRendererProperties = liftIO1 Raw.sdlGetRendererProperties
sdlGetRenderOutputSize = liftIO1 Raw.sdlGetRenderOutputSize
sdlGetCurrentRenderOutputSize = liftIO1 Raw.sdlGetCurrentRenderOutputSize
sdlCreateTexture = liftIO5 Raw.sdlCreateTexture
sdlCreateTextureFromSurface = liftIO2 Raw.sdlCreateTextureFromSurface
sdlCreateTextureWithProperties = liftIO2 Raw.sdlCreateTextureWithProperties
sdlDestroyTexture = liftIO1 Raw.sdlDestroyTexture
sdlGetTextureProperties = liftIO1 Raw.sdlGetTextureProperties
sdlGetRendererFromTexture = liftIO1 Raw.sdlGetRendererFromTexture
sdlGetTextureSize = liftIO1 Raw.sdlGetTextureSize
sdlSetTextureColorMod = liftIO4 Raw.sdlSetTextureColorMod
sdlSetTextureColorModFloat = liftIO4 Raw.sdlSetTextureColorModFloat
sdlGetTextureColorMod = liftIO1 Raw.sdlGetTextureColorMod
sdlGetTextureColorModFloat = liftIO1 Raw.sdlGetTextureColorModFloat
sdlSetTextureAlphaMod = liftIO2 Raw.sdlSetTextureAlphaMod
sdlSetTextureAlphaModFloat = liftIO2 Raw.sdlSetTextureAlphaModFloat
sdlGetTextureAlphaMod = liftIO1 Raw.sdlGetTextureAlphaMod
sdlGetTextureAlphaModFloat = liftIO1 Raw.sdlGetTextureAlphaModFloat
sdlSetTextureBlendMode = liftIO2 Raw.sdlSetTextureBlendMode
sdlGetTextureBlendMode = liftIO1 Raw.sdlGetTextureBlendMode
sdlSetTextureScaleMode = liftIO2 Raw.sdlSetTextureScaleMode
sdlRenderDebugTextFormat = liftIO4 Raw.sdlRenderDebugTextFormat
sdlGetTextureScaleMode = liftIO1 Raw.sdlGetTextureScaleMode
sdlSetTexturePalette = liftIO2 Raw.sdlSetTexturePalette
sdlGetTexturePalette = liftIO1 Raw.sdlGetTexturePalette
sdlUpdateTexture = liftIO4 Raw.sdlUpdateTexture
sdlUpdateYUVTexture = liftIO8 Raw.sdlUpdateYUVTexture
sdlUpdateNVTexture = liftIO6 Raw.sdlUpdateNVTexture
sdlLockTexture = liftIO2 Raw.sdlLockTexture
sdlLockTextureToSurface = liftIO2 Raw.sdlLockTextureToSurface
sdlUnlockTexture = liftIO1 Raw.sdlUnlockTexture
sdlSetRenderTarget = liftIO2 Raw.sdlSetRenderTarget
sdlGetRenderTarget = liftIO1 Raw.sdlGetRenderTarget
sdlSetRenderLogicalPresentation = liftIO4 Raw.sdlSetRenderLogicalPresentation
sdlGetRenderLogicalPresentation = liftIO1 Raw.sdlGetRenderLogicalPresentation
sdlGetRenderLogicalPresentationRect = liftIO1 Raw.sdlGetRenderLogicalPresentationRect
sdlRenderCoordinatesFromWindow = liftIO3 Raw.sdlRenderCoordinatesFromWindow
sdlRenderCoordinatesToWindow = liftIO3 Raw.sdlRenderCoordinatesToWindow
sdlConvertEventToRenderCoordinates = liftIO2 Raw.sdlConvertEventToRenderCoordinates
sdlSetRenderViewport = liftIO2 Raw.sdlSetRenderViewport
sdlGetRenderViewport = liftIO1 Raw.sdlGetRenderViewport
sdlRenderViewportSet = liftIO1 Raw.sdlRenderViewportSet
sdlGetRenderSafeArea = liftIO1 Raw.sdlGetRenderSafeArea
sdlSetRenderClipRect = liftIO2 Raw.sdlSetRenderClipRect
sdlGetRenderClipRect = liftIO1 Raw.sdlGetRenderClipRect
sdlRenderClipEnabled = liftIO1 Raw.sdlRenderClipEnabled
sdlSetRenderScale = liftIO3 Raw.sdlSetRenderScale
sdlGetRenderScale = liftIO1 Raw.sdlGetRenderScale
sdlSetRenderDrawColor = liftIO5 Raw.sdlSetRenderDrawColor
sdlSetRenderDrawColorFloat = liftIO5 Raw.sdlSetRenderDrawColorFloat
sdlGetRenderDrawColor = liftIO1 Raw.sdlGetRenderDrawColor
sdlGetRenderDrawColorFloat = liftIO1 Raw.sdlGetRenderDrawColorFloat
sdlSetRenderColorScale = liftIO2 Raw.sdlSetRenderColorScale
sdlGetRenderColorScale = liftIO1 Raw.sdlGetRenderColorScale
sdlSetRenderDrawBlendMode = liftIO2 Raw.sdlSetRenderDrawBlendMode
sdlGetRenderDrawBlendMode = liftIO1 Raw.sdlGetRenderDrawBlendMode
sdlSetRenderTextureAddressMode = liftIO3 Raw.sdlSetRenderTextureAddressMode
sdlGetRenderTextureAddressMode = liftIO1 Raw.sdlGetRenderTextureAddressMode
sdlRenderClear = liftIO1 Raw.sdlRenderClear
sdlRenderPoint = liftIO3 Raw.sdlRenderPoint
sdlRenderPoints = liftIO2 Raw.sdlRenderPoints
sdlRenderLine = liftIO5 Raw.sdlRenderLine
sdlRenderLines = liftIO2 Raw.sdlRenderLines
sdlRenderRect = liftIO2 Raw.sdlRenderRect
sdlRenderRects = liftIO2 Raw.sdlRenderRects
sdlRenderFillRect = liftIO2 Raw.sdlRenderFillRect
sdlRenderFillRects = liftIO2 Raw.sdlRenderFillRects
sdlRenderTexture = liftIO4 Raw.sdlRenderTexture
sdlRenderTextureRotated = liftIO7 Raw.sdlRenderTextureRotated
sdlRenderTextureAffine = liftIO6 Raw.sdlRenderTextureAffine
sdlRenderTextureTiled = liftIO5 Raw.sdlRenderTextureTiled
sdlRenderTexture9Grid = liftIO9 Raw.sdlRenderTexture9Grid
sdlRenderTexture9GridTiled = liftIO10 Raw.sdlRenderTexture9GridTiled
sdlRenderGeometry = liftIO4 Raw.sdlRenderGeometry
sdlRenderGeometryRaw = liftIO12 Raw.sdlRenderGeometryRaw
sdlRenderReadPixels = liftIO2 Raw.sdlRenderReadPixels
sdlRenderPresent = liftIO1 Raw.sdlRenderPresent
sdlSetRenderVSync = liftIO2 Raw.sdlSetRenderVSync
sdlGetRenderVSync = liftIO1 Raw.sdlGetRenderVSync
sdlFlushRenderer = liftIO1 Raw.sdlFlushRenderer
sdlGetRenderMetalLayer = liftIO1 Raw.sdlGetRenderMetalLayer
sdlGetRenderMetalCommandEncoder = liftIO1 Raw.sdlGetRenderMetalCommandEncoder
sdlAddVulkanRenderSemaphores = liftIO4 Raw.sdlAddVulkanRenderSemaphores
sdlRenderDebugText = liftIO4 Raw.sdlRenderDebugText
sdlSetDefaultTextureScaleMode = liftIO2 Raw.sdlSetDefaultTextureScaleMode
sdlGetDefaultTextureScaleMode = liftIO1 Raw.sdlGetDefaultTextureScaleMode
sdlCreateGPURenderState = liftIO2 Raw.sdlCreateGPURenderState
sdlSetGPURenderStateFragmentUniforms = liftIO3 Raw.sdlSetGPURenderStateFragmentUniforms
sdlSetGPURenderState = liftIO2 Raw.sdlSetGPURenderState
sdlDestroyGPURenderState = liftIO1 Raw.sdlDestroyGPURenderState
