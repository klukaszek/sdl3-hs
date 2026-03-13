module SDL3.Wrapped.Surface
  ( module SDL3.Raw.Surface
  , SDLSurface
  , withSurfacePtr
  , sdlUnsafeFromRawSurface
  , sdlUnsafeToRawSurface
  , sdlGetSurfaceFlags
  , sdlGetSurfaceFormat
  , sdlGetSurfaceWidth
  , sdlGetSurfaceHeight
  , sdlGetSurfaceSize
  , sdlGetSurfacePitch
  , sdlGetSurfacePixels
  , sdlGetSurfaceRefcount
  , withSurfacePixels
  , sdlMustLockSurface
  , sdlCreateSurface
  , sdlCreateSurfaceFrom
  , sdlDestroySurface
  , sdlGetSurfaceProperties
  , sdlSetSurfaceColorspace
  , sdlGetSurfaceColorspace
  , sdlCreateSurfacePalette
  , sdlSetSurfacePalette
  , sdlGetSurfacePalette
  , sdlLockSurface
  , sdlUnlockSurface
  , sdlLoadSurface
  , sdlLoadSurfaceIo
  , sdlLoadBMP
  , sdlSaveBMP
  , sdlLoadPNG
  , sdlLoadPNGIo
  , sdlSavePNGIo
  , sdlSavePNG
  , sdlBlitSurface
  , sdlBlitSurfaceScaled
  , sdlFlipSurface
  , sdlRotateSurface
  , sdlScaleSurface
  , sdlDuplicateSurface
  , sdlMapSurfaceRGB
  , sdlMapSurfaceRGBA
  , sdlReadSurfacePixel
  , sdlWriteSurfacePixel
  , sdlConvertSurface
  , sdlAddSurfaceAlternateImage
  , sdlSurfaceHasAlternateImages
  , sdlGetSurfaceImages
  , sdlRemoveSurfaceAlternateImages
  , sdlLoadBMPIo
  , sdlSaveBMPIo
  , sdlSetSurfaceRLE
  , sdlSurfaceHasRLE
  , sdlSetSurfaceColorKey
  , sdlSurfaceHasColorKey
  , sdlGetSurfaceColorKey
  , sdlSetSurfaceColorMod
  , sdlGetSurfaceColorMod
  , sdlSetSurfaceAlphaMod
  , sdlGetSurfaceAlphaMod
  , sdlSetSurfaceBlendMode
  , sdlGetSurfaceBlendMode
  , sdlSetSurfaceClipRect
  , sdlGetSurfaceClipRect
  , sdlConvertSurfaceAndColorspace
  , sdlConvertPixels
  , sdlConvertPixelsAndColorspace
  , sdlPremultiplyAlpha
  , sdlPremultiplySurfaceAlpha
  , sdlClearSurface
  , sdlFillSurfaceRect
  , sdlFillSurfaceRects
  , sdlBlitSurfaceUnchecked
  , sdlBlitSurfaceUncheckedScaled
  , sdlStretchSurface
  , sdlBlitSurfaceTiled
  , sdlBlitSurfaceTiledWithScale
  , sdlBlitSurface9Grid
  , sdlReadSurfacePixelFloat
  , sdlWriteSurfacePixelFloat
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (mapMaybe)
import Data.Word (Word8, Word32)
import Foreign.C.Types (CBool, CFloat, CInt, CUInt)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import SDL3.Pixels (SDLColorspace(..), SDLPixelFormat, SDLPalette)
import SDL3.Raw.IOStream (SDLIOStream)
import SDL3.Raw.Properties (SDLPropertiesID)
import SDL3.Raw.Rect (SDLRect)
import SDL3.Raw.Surface hiding
  ( SDLSurface(..)
  , sdlMustLockSurface
  , sdlCreateSurface
  , sdlCreateSurfaceFrom
  , sdlDestroySurface
  , sdlGetSurfaceProperties
  , sdlSetSurfaceColorspace
  , sdlGetSurfaceColorspace
  , sdlCreateSurfacePalette
  , sdlSetSurfacePalette
  , sdlGetSurfacePalette
  , sdlLockSurface
  , sdlUnlockSurface
  , sdlLoadSurface
  , sdlLoadSurfaceIo
  , sdlLoadBMP
  , sdlSaveBMP
  , sdlLoadPNG
  , sdlLoadPNGIo
  , sdlSavePNGIo
  , sdlSavePNG
  , sdlBlitSurface
  , sdlBlitSurfaceScaled
  , sdlFlipSurface
  , sdlRotateSurface
  , sdlScaleSurface
  , sdlDuplicateSurface
  , sdlMapSurfaceRGB
  , sdlMapSurfaceRGBA
  , sdlReadSurfacePixel
  , sdlWriteSurfacePixel
  , sdlConvertSurface
  , sdlAddSurfaceAlternateImage
  , sdlSurfaceHasAlternateImages
  , sdlGetSurfaceImages
  , sdlRemoveSurfaceAlternateImages
  , sdlLoadBMPIo
  , sdlSaveBMPIo
  , sdlSetSurfaceRLE
  , sdlSurfaceHasRLE
  , sdlSetSurfaceColorKey
  , sdlSurfaceHasColorKey
  , sdlGetSurfaceColorKey
  , sdlSetSurfaceColorMod
  , sdlGetSurfaceColorMod
  , sdlSetSurfaceAlphaMod
  , sdlGetSurfaceAlphaMod
  , sdlSetSurfaceBlendMode
  , sdlGetSurfaceBlendMode
  , sdlSetSurfaceClipRect
  , sdlGetSurfaceClipRect
  , sdlConvertSurfaceAndColorspace
  , sdlConvertPixels
  , sdlConvertPixelsAndColorspace
  , sdlPremultiplyAlpha
  , sdlPremultiplySurfaceAlpha
  , sdlClearSurface
  , sdlFillSurfaceRect
  , sdlFillSurfaceRects
  , sdlBlitSurfaceUnchecked
  , sdlBlitSurfaceUncheckedScaled
  , sdlStretchSurface
  , sdlBlitSurfaceTiled
  , sdlBlitSurfaceTiledWithScale
  , sdlBlitSurface9Grid
  , sdlReadSurfacePixelFloat
  , sdlWriteSurfacePixelFloat
  )
import qualified SDL3.Raw.Surface as Raw

newtype SDLSurface = SDLSurface (Ptr Raw.SDLSurface)
  deriving (Eq)

instance Show SDLSurface where
  show (SDLSurface surfacePtr) = "SDLSurface " ++ show surfacePtr

sdlUnsafeFromRawSurface :: Ptr Raw.SDLSurface -> Maybe SDLSurface
sdlUnsafeFromRawSurface surfacePtr
  | surfacePtr == nullPtr = Nothing
  | otherwise = Just (SDLSurface surfacePtr)

sdlUnsafeToRawSurface :: SDLSurface -> Ptr Raw.SDLSurface
sdlUnsafeToRawSurface (SDLSurface surfacePtr) = surfacePtr

withSurfacePtr :: MonadIO m => SDLSurface -> (Ptr Raw.SDLSurface -> IO a) -> m a
withSurfacePtr surface action = liftIO $ action (sdlUnsafeToRawSurface surface)

peekSurface :: SDLSurface -> IO Raw.SDLSurface
peekSurface = peek . sdlUnsafeToRawSurface

wrapSurfaceIO :: IO (Maybe (Ptr Raw.SDLSurface)) -> IO (Maybe SDLSurface)
wrapSurfaceIO = fmap (>>= sdlUnsafeFromRawSurface)

sdlGetSurfaceFlags :: MonadIO m => SDLSurface -> m SDLSurfaceFlags
sdlGetSurfaceFlags surface = liftIO $ Raw.surfaceFlags <$> peekSurface surface

sdlGetSurfaceFormat :: MonadIO m => SDLSurface -> m SDLPixelFormat
sdlGetSurfaceFormat surface = liftIO $ Raw.surfaceFormat <$> peekSurface surface

sdlGetSurfaceWidth :: MonadIO m => SDLSurface -> m Int
sdlGetSurfaceWidth surface = liftIO $ Raw.surfaceW <$> peekSurface surface

sdlGetSurfaceHeight :: MonadIO m => SDLSurface -> m Int
sdlGetSurfaceHeight surface = liftIO $ Raw.surfaceH <$> peekSurface surface

sdlGetSurfaceSize :: MonadIO m => SDLSurface -> m (Int, Int)
sdlGetSurfaceSize surface = liftIO $ do
  surfaceData <- peekSurface surface
  pure (Raw.surfaceW surfaceData, Raw.surfaceH surfaceData)

sdlGetSurfacePitch :: MonadIO m => SDLSurface -> m Int
sdlGetSurfacePitch surface = liftIO $ Raw.surfacePitch <$> peekSurface surface

sdlGetSurfacePixels :: MonadIO m => SDLSurface -> m (Maybe (Ptr ()))
sdlGetSurfacePixels surface = liftIO $ do
  pixelsPtr <- Raw.surfacePixels <$> peekSurface surface
  pure $ if pixelsPtr == nullPtr then Nothing else Just pixelsPtr

sdlGetSurfaceRefcount :: MonadIO m => SDLSurface -> m Int
sdlGetSurfaceRefcount surface = liftIO $ Raw.surfaceRefcount <$> peekSurface surface

withSurfacePixels :: MonadIO m => SDLSurface -> (Ptr () -> IO a) -> m (Maybe a)
withSurfacePixels surface action = liftIO $ do
  maybePixels <- sdlGetSurfacePixels surface
  case maybePixels of
    Nothing -> pure Nothing
    Just pixelsPtr -> Just <$> action pixelsPtr

sdlMustLockSurface :: MonadIO m => SDLSurface -> m Bool
sdlMustLockSurface surface = liftIO $ Raw.sdlMustLockSurface (sdlUnsafeToRawSurface surface)

sdlCreateSurface :: MonadIO m => CInt -> CInt -> SDLPixelFormat -> m (Maybe SDLSurface)
sdlCreateSurface w h fmt = liftIO $ wrapSurfaceIO (Raw.sdlCreateSurface w h fmt)

sdlCreateSurfaceFrom :: MonadIO m => CInt -> CInt -> SDLPixelFormat -> Ptr () -> CInt -> m (Maybe SDLSurface)
sdlCreateSurfaceFrom w h fmt pixels pitch = liftIO $ wrapSurfaceIO (Raw.sdlCreateSurfaceFrom w h fmt pixels pitch)

sdlDestroySurface :: MonadIO m => SDLSurface -> m ()
sdlDestroySurface surface = liftIO $ Raw.sdlDestroySurface (sdlUnsafeToRawSurface surface)

sdlGetSurfaceProperties :: MonadIO m => SDLSurface -> m SDLPropertiesID
sdlGetSurfaceProperties surface = liftIO $ Raw.sdlGetSurfaceProperties (sdlUnsafeToRawSurface surface)

sdlSetSurfaceColorspace :: MonadIO m => SDLSurface -> SDLColorspace -> m Bool
sdlSetSurfaceColorspace surface colorspace =
  liftIO $ Raw.sdlSetSurfaceColorspace (sdlUnsafeToRawSurface surface) colorspace

sdlGetSurfaceColorspace :: MonadIO m => SDLSurface -> m SDLColorspace
sdlGetSurfaceColorspace surface = liftIO $ Raw.sdlGetSurfaceColorspace (sdlUnsafeToRawSurface surface)

sdlCreateSurfacePalette :: MonadIO m => SDLSurface -> m (Ptr SDLPalette)
sdlCreateSurfacePalette surface = liftIO $ Raw.sdlCreateSurfacePalette (sdlUnsafeToRawSurface surface)

sdlSetSurfacePalette :: MonadIO m => SDLSurface -> Ptr SDLPalette -> m Bool
sdlSetSurfacePalette surface palette =
  liftIO $ Raw.sdlSetSurfacePalette (sdlUnsafeToRawSurface surface) palette

sdlGetSurfacePalette :: MonadIO m => SDLSurface -> m (Ptr SDLPalette)
sdlGetSurfacePalette surface = liftIO $ Raw.sdlGetSurfacePalette (sdlUnsafeToRawSurface surface)

sdlLockSurface :: MonadIO m => SDLSurface -> m Bool
sdlLockSurface surface = liftIO $ Raw.sdlLockSurface (sdlUnsafeToRawSurface surface)

sdlUnlockSurface :: MonadIO m => SDLSurface -> m ()
sdlUnlockSurface surface = liftIO $ Raw.sdlUnlockSurface (sdlUnsafeToRawSurface surface)

sdlLoadSurface :: MonadIO m => FilePath -> m (Maybe SDLSurface)
sdlLoadSurface path = liftIO $ wrapSurfaceIO (Raw.sdlLoadSurface path)

sdlLoadSurfaceIo :: MonadIO m => SDLIOStream -> Bool -> m (Maybe SDLSurface)
sdlLoadSurfaceIo stream closeIo = liftIO $ wrapSurfaceIO (Raw.sdlLoadSurfaceIo stream closeIo)

sdlLoadBMP :: MonadIO m => FilePath -> m (Maybe SDLSurface)
sdlLoadBMP path = liftIO $ wrapSurfaceIO (Raw.sdlLoadBMP path)

sdlSaveBMP :: MonadIO m => SDLSurface -> FilePath -> m Bool
sdlSaveBMP surface path = liftIO $ Raw.sdlSaveBMP (sdlUnsafeToRawSurface surface) path

sdlLoadPNG :: MonadIO m => FilePath -> m (Maybe SDLSurface)
sdlLoadPNG path = liftIO $ wrapSurfaceIO (Raw.sdlLoadPNG path)

sdlLoadPNGIo :: MonadIO m => SDLIOStream -> Bool -> m (Maybe SDLSurface)
sdlLoadPNGIo stream closeIo = liftIO $ wrapSurfaceIO (Raw.sdlLoadPNGIo stream closeIo)

sdlSavePNGIo :: MonadIO m => SDLSurface -> SDLIOStream -> Bool -> m Bool
sdlSavePNGIo surface stream closeIo =
  liftIO $ Raw.sdlSavePNGIo (sdlUnsafeToRawSurface surface) stream closeIo

sdlSavePNG :: MonadIO m => SDLSurface -> FilePath -> m Bool
sdlSavePNG surface path = liftIO $ Raw.sdlSavePNG (sdlUnsafeToRawSurface surface) path

sdlBlitSurface :: MonadIO m => SDLSurface -> Maybe SDLRect -> SDLSurface -> Maybe SDLRect -> m Bool
sdlBlitSurface src srcRect dst dstRect =
  liftIO $ Raw.sdlBlitSurface (sdlUnsafeToRawSurface src) srcRect (sdlUnsafeToRawSurface dst) dstRect

sdlBlitSurfaceScaled :: MonadIO m => SDLSurface -> Maybe SDLRect -> SDLSurface -> Maybe SDLRect -> SDLScaleMode -> m Bool
sdlBlitSurfaceScaled src srcRect dst dstRect scaleMode =
  liftIO $ Raw.sdlBlitSurfaceScaled (sdlUnsafeToRawSurface src) srcRect (sdlUnsafeToRawSurface dst) dstRect scaleMode

sdlFlipSurface :: MonadIO m => SDLSurface -> SDLFlipMode -> m Bool
sdlFlipSurface surface flipMode = liftIO $ Raw.sdlFlipSurface (sdlUnsafeToRawSurface surface) flipMode

sdlRotateSurface :: MonadIO m => SDLSurface -> Float -> m (Maybe SDLSurface)
sdlRotateSurface surface angle = liftIO $ wrapSurfaceIO (Raw.sdlRotateSurface (sdlUnsafeToRawSurface surface) angle)

sdlScaleSurface :: MonadIO m => SDLSurface -> CInt -> CInt -> SDLScaleMode -> m (Maybe SDLSurface)
sdlScaleSurface surface w h scaleMode =
  liftIO $ wrapSurfaceIO (Raw.sdlScaleSurface (sdlUnsafeToRawSurface surface) w h scaleMode)

sdlDuplicateSurface :: MonadIO m => SDLSurface -> m (Maybe SDLSurface)
sdlDuplicateSurface surface = liftIO $ wrapSurfaceIO (Raw.sdlDuplicateSurface (sdlUnsafeToRawSurface surface))

sdlMapSurfaceRGB :: MonadIO m => SDLSurface -> Word8 -> Word8 -> Word8 -> m Word32
sdlMapSurfaceRGB surface r g b = liftIO $ Raw.sdlMapSurfaceRGB (sdlUnsafeToRawSurface surface) r g b

sdlMapSurfaceRGBA :: MonadIO m => SDLSurface -> Word8 -> Word8 -> Word8 -> Word8 -> m Word32
sdlMapSurfaceRGBA surface r g b a = liftIO $ Raw.sdlMapSurfaceRGBA (sdlUnsafeToRawSurface surface) r g b a

sdlReadSurfacePixel :: MonadIO m => SDLSurface -> CInt -> CInt -> m (Maybe (Word8, Word8, Word8, Word8))
sdlReadSurfacePixel surface x y = liftIO $ Raw.sdlReadSurfacePixel (sdlUnsafeToRawSurface surface) x y

sdlWriteSurfacePixel :: MonadIO m => SDLSurface -> CInt -> CInt -> Word8 -> Word8 -> Word8 -> Word8 -> m Bool
sdlWriteSurfacePixel surface x y r g b a =
  liftIO $ Raw.sdlWriteSurfacePixel (sdlUnsafeToRawSurface surface) x y r g b a

sdlConvertSurface :: MonadIO m => SDLSurface -> SDLPixelFormat -> m (Maybe SDLSurface)
sdlConvertSurface surface formatEnum =
  liftIO $ wrapSurfaceIO (Raw.sdlConvertSurface (sdlUnsafeToRawSurface surface) formatEnum)

sdlAddSurfaceAlternateImage :: MonadIO m => SDLSurface -> SDLSurface -> m Bool
sdlAddSurfaceAlternateImage surface alternate =
  liftIO $ Raw.sdlAddSurfaceAlternateImage (sdlUnsafeToRawSurface surface) (sdlUnsafeToRawSurface alternate)

sdlSurfaceHasAlternateImages :: MonadIO m => SDLSurface -> m Bool
sdlSurfaceHasAlternateImages surface = liftIO $ Raw.sdlSurfaceHasAlternateImages (sdlUnsafeToRawSurface surface)

sdlGetSurfaceImages :: MonadIO m => SDLSurface -> m [SDLSurface]
sdlGetSurfaceImages surface =
  liftIO $ mapMaybe sdlUnsafeFromRawSurface <$> Raw.sdlGetSurfaceImages (sdlUnsafeToRawSurface surface)

sdlRemoveSurfaceAlternateImages :: MonadIO m => SDLSurface -> m ()
sdlRemoveSurfaceAlternateImages surface = liftIO $ Raw.sdlRemoveSurfaceAlternateImages (sdlUnsafeToRawSurface surface)

sdlLoadBMPIo :: MonadIO m => SDLIOStream -> Bool -> m (Maybe SDLSurface)
sdlLoadBMPIo stream closeIo = liftIO $ wrapSurfaceIO (Raw.sdlLoadBMPIo stream closeIo)

sdlSaveBMPIo :: MonadIO m => SDLSurface -> SDLIOStream -> Bool -> m Bool
sdlSaveBMPIo surface stream closeIo =
  liftIO $ Raw.sdlSaveBMPIo (sdlUnsafeToRawSurface surface) stream closeIo

sdlSetSurfaceRLE :: MonadIO m => SDLSurface -> Bool -> m Bool
sdlSetSurfaceRLE surface enabled = liftIO $ Raw.sdlSetSurfaceRLE (sdlUnsafeToRawSurface surface) enabled

sdlSurfaceHasRLE :: MonadIO m => SDLSurface -> m Bool
sdlSurfaceHasRLE surface = liftIO $ Raw.sdlSurfaceHasRLE (sdlUnsafeToRawSurface surface)

sdlSetSurfaceColorKey :: MonadIO m => SDLSurface -> Bool -> Word32 -> m Bool
sdlSetSurfaceColorKey surface enabled key =
  liftIO $ Raw.sdlSetSurfaceColorKey (sdlUnsafeToRawSurface surface) enabled key

sdlSurfaceHasColorKey :: MonadIO m => SDLSurface -> m Bool
sdlSurfaceHasColorKey surface = liftIO $ Raw.sdlSurfaceHasColorKey (sdlUnsafeToRawSurface surface)

sdlGetSurfaceColorKey :: MonadIO m => SDLSurface -> m (Maybe Word32)
sdlGetSurfaceColorKey surface = liftIO $ Raw.sdlGetSurfaceColorKey (sdlUnsafeToRawSurface surface)

sdlSetSurfaceColorMod :: MonadIO m => SDLSurface -> Word8 -> Word8 -> Word8 -> m Bool
sdlSetSurfaceColorMod surface r g b =
  liftIO $ Raw.sdlSetSurfaceColorMod (sdlUnsafeToRawSurface surface) r g b

sdlGetSurfaceColorMod :: MonadIO m => SDLSurface -> m (Maybe (Word8, Word8, Word8))
sdlGetSurfaceColorMod surface = liftIO $ Raw.sdlGetSurfaceColorMod (sdlUnsafeToRawSurface surface)

sdlSetSurfaceAlphaMod :: MonadIO m => SDLSurface -> Word8 -> m Bool
sdlSetSurfaceAlphaMod surface alpha = liftIO $ Raw.sdlSetSurfaceAlphaMod (sdlUnsafeToRawSurface surface) alpha

sdlGetSurfaceAlphaMod :: MonadIO m => SDLSurface -> m (Maybe Word8)
sdlGetSurfaceAlphaMod surface = liftIO $ Raw.sdlGetSurfaceAlphaMod (sdlUnsafeToRawSurface surface)

sdlSetSurfaceBlendMode :: MonadIO m => SDLSurface -> CInt -> m Bool
sdlSetSurfaceBlendMode surface blendMode =
  liftIO $ Raw.sdlSetSurfaceBlendMode (sdlUnsafeToRawSurface surface) blendMode

sdlGetSurfaceBlendMode :: MonadIO m => SDLSurface -> m (Maybe CInt)
sdlGetSurfaceBlendMode surface = liftIO $ Raw.sdlGetSurfaceBlendMode (sdlUnsafeToRawSurface surface)

sdlSetSurfaceClipRect :: MonadIO m => SDLSurface -> Ptr SDLRect -> m Bool
sdlSetSurfaceClipRect surface rectPtr = liftIO $ Raw.sdlSetSurfaceClipRect (sdlUnsafeToRawSurface surface) rectPtr

sdlGetSurfaceClipRect :: MonadIO m => SDLSurface -> Ptr SDLRect -> m Bool
sdlGetSurfaceClipRect surface rectPtr = liftIO $ Raw.sdlGetSurfaceClipRect (sdlUnsafeToRawSurface surface) rectPtr

sdlConvertSurfaceAndColorspace :: MonadIO m => SDLSurface -> CUInt -> Ptr () -> CInt -> CUInt -> m (Maybe SDLSurface)
sdlConvertSurfaceAndColorspace surface format palette colorspace props =
  liftIO $ wrapSurfaceIO (Raw.sdlConvertSurfaceAndColorspace (sdlUnsafeToRawSurface surface) format palette colorspace props)

sdlConvertPixels :: MonadIO m => CInt -> CInt -> CUInt -> Ptr () -> CInt -> CUInt -> Ptr () -> CInt -> m Bool
sdlConvertPixels a b c d e f g h = liftIO $ Raw.sdlConvertPixels a b c d e f g h

sdlConvertPixelsAndColorspace :: MonadIO m => CInt -> CInt -> CUInt -> CInt -> CUInt -> Ptr () -> CInt -> CUInt -> CInt -> CUInt -> Ptr () -> CInt -> m Bool
sdlConvertPixelsAndColorspace a b c d e f g h i j k l =
  liftIO $ Raw.sdlConvertPixelsAndColorspace a b c d e f g h i j k l

sdlPremultiplyAlpha :: MonadIO m => CInt -> CInt -> CUInt -> Ptr () -> CInt -> CUInt -> Ptr () -> CInt -> Bool -> m Bool
sdlPremultiplyAlpha w h srcFmt src srcPitch dstFmt dst dstPitch linear =
  liftIO $ Raw.sdlPremultiplyAlpha w h srcFmt src srcPitch dstFmt dst dstPitch linear

sdlPremultiplySurfaceAlpha :: MonadIO m => SDLSurface -> Bool -> m Bool
sdlPremultiplySurfaceAlpha surface linear =
  liftIO $ Raw.sdlPremultiplySurfaceAlpha (sdlUnsafeToRawSurface surface) linear

sdlClearSurface :: MonadIO m => SDLSurface -> Float -> Float -> Float -> Float -> m Bool
sdlClearSurface surface r g b a =
  liftIO $ Raw.sdlClearSurface (sdlUnsafeToRawSurface surface) r g b a

sdlFillSurfaceRect :: MonadIO m => SDLSurface -> Ptr SDLRect -> Word32 -> m Bool
sdlFillSurfaceRect surface rectPtr color =
  liftIO $ Raw.sdlFillSurfaceRect (sdlUnsafeToRawSurface surface) rectPtr color

sdlFillSurfaceRects :: MonadIO m => SDLSurface -> Ptr SDLRect -> CInt -> Word32 -> m Bool
sdlFillSurfaceRects surface rects count color =
  liftIO $ Raw.sdlFillSurfaceRects (sdlUnsafeToRawSurface surface) rects count color

sdlBlitSurfaceUnchecked :: MonadIO m => SDLSurface -> Ptr SDLRect -> SDLSurface -> Ptr SDLRect -> m Bool
sdlBlitSurfaceUnchecked src srcRect dst dstRect =
  liftIO $ Raw.sdlBlitSurfaceUnchecked (sdlUnsafeToRawSurface src) srcRect (sdlUnsafeToRawSurface dst) dstRect

sdlBlitSurfaceUncheckedScaled :: MonadIO m => SDLSurface -> Ptr SDLRect -> SDLSurface -> Ptr SDLRect -> CInt -> m Bool
sdlBlitSurfaceUncheckedScaled src srcRect dst dstRect scaleMode =
  liftIO $ Raw.sdlBlitSurfaceUncheckedScaled (sdlUnsafeToRawSurface src) srcRect (sdlUnsafeToRawSurface dst) dstRect scaleMode

sdlStretchSurface :: MonadIO m => SDLSurface -> Ptr SDLRect -> SDLSurface -> Ptr SDLRect -> CInt -> m Bool
sdlStretchSurface src srcRect dst dstRect scaleMode =
  liftIO $ Raw.sdlStretchSurface (sdlUnsafeToRawSurface src) srcRect (sdlUnsafeToRawSurface dst) dstRect scaleMode

sdlBlitSurfaceTiled :: MonadIO m => SDLSurface -> Ptr SDLRect -> SDLSurface -> Ptr SDLRect -> m Bool
sdlBlitSurfaceTiled src srcRect dst dstRect =
  liftIO $ Raw.sdlBlitSurfaceTiled (sdlUnsafeToRawSurface src) srcRect (sdlUnsafeToRawSurface dst) dstRect

sdlBlitSurfaceTiledWithScale :: MonadIO m => SDLSurface -> Ptr SDLRect -> Float -> CInt -> SDLSurface -> Ptr SDLRect -> m Bool
sdlBlitSurfaceTiledWithScale src srcRect scale scaleMode dst dstRect =
  liftIO $ Raw.sdlBlitSurfaceTiledWithScale (sdlUnsafeToRawSurface src) srcRect scale scaleMode (sdlUnsafeToRawSurface dst) dstRect

sdlBlitSurface9Grid :: MonadIO m => SDLSurface -> Ptr SDLRect -> CInt -> CInt -> CInt -> CInt -> Float -> CInt -> SDLSurface -> Ptr SDLRect -> m Bool
sdlBlitSurface9Grid src srcRect leftW rightW topH bottomH scale scaleMode dst dstRect =
  liftIO $ Raw.sdlBlitSurface9Grid (sdlUnsafeToRawSurface src) srcRect leftW rightW topH bottomH scale scaleMode (sdlUnsafeToRawSurface dst) dstRect

sdlReadSurfacePixelFloat :: MonadIO m => SDLSurface -> CInt -> CInt -> m (Maybe (Float, Float, Float, Float))
sdlReadSurfacePixelFloat surface x y =
  liftIO $ Raw.sdlReadSurfacePixelFloat (sdlUnsafeToRawSurface surface) x y

sdlWriteSurfacePixelFloat :: MonadIO m => SDLSurface -> CInt -> CInt -> Float -> Float -> Float -> Float -> m Bool
sdlWriteSurfacePixelFloat surface x y r g b a =
  liftIO $ Raw.sdlWriteSurfacePixelFloat (sdlUnsafeToRawSurface surface) x y r g b a
