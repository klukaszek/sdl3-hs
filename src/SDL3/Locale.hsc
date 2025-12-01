{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds#-}

{-|
Module      : SDL.Locale - SDL Locale
Description : SDL locale services
Copyright   : (c) Kyle Lukaszek, 2025
Starting new chunk from module SDL.Log
License     : BSD3
Description : Provides access to user's preferred locales (language and country)
-}

module SDL3.Locale
  ( -- * Types
    SDLLocale(..)

    -- * Functions
  , sdlGetPreferredLocales
  ) where

#include <SDL3/SDL_locale.h>

import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (free, alloca)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Array (peekArray)

-- | Represents a locale with language and optional country code
data SDLLocale = SDLLocale
  { language :: String    -- ^ ISO-639 language code (e.g., "en")
  , country  :: Maybe String -- ^ ISO-3166 country code (e.g., "US"), or Nothing
  } deriving (Eq, Show)

instance Storable SDLLocale where
  sizeOf _ = #size SDL_Locale
  alignment _ = #alignment SDL_Locale
  peek ptr = do
    lang <- peekCString =<< (#peek SDL_Locale, language) ptr
    countryPtr <- (#peek SDL_Locale, country) ptr
    countryVal <- if countryPtr == nullPtr
                 then return Nothing
                 else Just <$> peekCString countryPtr
    return $ SDLLocale lang countryVal
  poke _ _ = error "SDLLocale.poke not implemented" -- Not needed for this use case

-- | Get the user's preferred locales
foreign import ccall "SDL_GetPreferredLocales"
  sdlGetPreferredLocalesRaw :: Ptr CInt -> IO (Ptr (Ptr SDLLocale))

sdlGetPreferredLocales :: IO [SDLLocale]
sdlGetPreferredLocales = alloca $ \countPtr -> do
  poke countPtr 0
  localePtrs <- sdlGetPreferredLocalesRaw countPtr
  if localePtrs == nullPtr
    then return []
    else do
      count <- peek countPtr
      locales <- peekArray (fromIntegral count) localePtrs
      mapM peek locales <* free localePtrs
