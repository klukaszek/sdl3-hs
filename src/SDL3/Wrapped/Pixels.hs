module SDL3.Wrapped.Pixels
  ( module SDL3.Raw.Pixels
  , sdlGetPixelFormatName
  , sdlGetMasksForPixelFormat
  , sdlGetPixelFormatForMasks
  , sdlGetPixelFormatDetails
  , sdlCreatePalette
  , sdlSetPaletteColors
  , sdlDestroyPalette
  , sdlMapRGB
  , sdlMapRGBA
  , sdlGetRGB
  , sdlGetRGBA
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word8, Word32)
import SDL3.Raw.Pixels hiding
  ( sdlGetPixelFormatName
  , sdlGetMasksForPixelFormat
  , sdlGetPixelFormatForMasks
  , sdlGetPixelFormatDetails
  , sdlCreatePalette
  , sdlSetPaletteColors
  , sdlDestroyPalette
  , sdlMapRGB
  , sdlMapRGBA
  , sdlGetRGB
  , sdlGetRGBA
  )
import qualified SDL3.Raw.Pixels as Raw

sdlGetPixelFormatName :: MonadIO m => SDLPixelFormat -> m String
sdlGetPixelFormatName = liftIO . Raw.sdlGetPixelFormatName

sdlGetMasksForPixelFormat
  :: MonadIO m
  => SDLPixelFormat
  -> m (Maybe (Int, Word32, Word32, Word32, Word32))
sdlGetMasksForPixelFormat = liftIO . Raw.sdlGetMasksForPixelFormat

sdlGetPixelFormatForMasks
  :: MonadIO m
  => Int
  -> Word32
  -> Word32
  -> Word32
  -> Word32
  -> m SDLPixelFormat
sdlGetPixelFormatForMasks bitsPerPixel rMask gMask bMask aMask =
  liftIO $ Raw.sdlGetPixelFormatForMasks bitsPerPixel rMask gMask bMask aMask

sdlGetPixelFormatDetails
  :: MonadIO m
  => SDLPixelFormat
  -> m (Maybe SDLPixelFormatDetails)
sdlGetPixelFormatDetails = liftIO . Raw.sdlGetPixelFormatDetails

sdlCreatePalette :: MonadIO m => Int -> m (Maybe SDLPalette)
sdlCreatePalette = liftIO . Raw.sdlCreatePalette

sdlSetPaletteColors
  :: MonadIO m
  => SDLPalette
  -> [SDLColor]
  -> Int
  -> m Bool
sdlSetPaletteColors palette colors firstColor =
  liftIO $ Raw.sdlSetPaletteColors palette colors firstColor

sdlDestroyPalette :: MonadIO m => SDLPalette -> m ()
sdlDestroyPalette = liftIO . Raw.sdlDestroyPalette

sdlMapRGB
  :: MonadIO m
  => SDLPixelFormatDetails
  -> Maybe SDLPalette
  -> Word8
  -> Word8
  -> Word8
  -> m Word32
sdlMapRGB formatDetails palette r g b =
  liftIO $ Raw.sdlMapRGB formatDetails palette r g b

sdlMapRGBA
  :: MonadIO m
  => SDLPixelFormatDetails
  -> Maybe SDLPalette
  -> Word8
  -> Word8
  -> Word8
  -> Word8
  -> m Word32
sdlMapRGBA formatDetails palette r g b a =
  liftIO $ Raw.sdlMapRGBA formatDetails palette r g b a

sdlGetRGB
  :: MonadIO m
  => Word32
  -> SDLPixelFormatDetails
  -> Maybe SDLPalette
  -> m (Word8, Word8, Word8)
sdlGetRGB pixelValue formatDetails palette =
  liftIO $ Raw.sdlGetRGB pixelValue formatDetails palette

sdlGetRGBA
  :: MonadIO m
  => Word32
  -> SDLPixelFormatDetails
  -> Maybe SDLPalette
  -> m (Word8, Word8, Word8, Word8)
sdlGetRGBA pixelValue formatDetails palette =
  liftIO $ Raw.sdlGetRGBA pixelValue formatDetails palette
