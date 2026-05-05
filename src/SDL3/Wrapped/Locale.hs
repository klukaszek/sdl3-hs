{-|
Module      : SDL3.Wrapped.Locale
Description : Wrapped SDL locale services
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Ergonomic wrappers over 'SDL3.Raw.Locale'.
-}

module SDL3.Wrapped.Locale
  ( SDLLocale(..)
  , sdlGetPreferredLocales
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek, poke)
import SDL3.Raw.Locale (SDLLocale(..))
import qualified SDL3.Raw.Locale as Raw

sdlGetPreferredLocales :: MonadIO m => m [SDLLocale]
sdlGetPreferredLocales = liftIO $
  alloca $ \countPtr -> do
    poke countPtr (0 :: CInt)
    localePtrs <- Raw.sdlGetPreferredLocalesRaw countPtr
    if localePtrs == nullPtr
      then return []
      else do
        count <- peek countPtr
        locales <- peekArray (fromIntegral count) localePtrs
        mapM peek locales <* free localePtrs
