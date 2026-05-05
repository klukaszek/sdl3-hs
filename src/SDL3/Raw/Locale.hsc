{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds#-}

{-|
Module      : SDL3.Raw.Locale
Description : Raw SDL locale services
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Low-level locale bindings. Prefer 'SDL3.Wrapped.Locale' or 'SDL3.Locale'
unless you need direct FFI access.
-}

module SDL3.Raw.Locale
  ( SDLLocale(..)
  , sdlGetPreferredLocalesRaw
  ) where

#include <SDL3/SDL_locale.h>

import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))

data SDLLocale = SDLLocale
  { language :: String
  , country  :: Maybe String
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
  poke _ _ = error "SDLLocale.poke not implemented"

foreign import ccall "SDL_GetPreferredLocales"
  sdlGetPreferredLocalesRaw :: Ptr CInt -> IO (Ptr (Ptr SDLLocale))
