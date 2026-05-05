{-# LANGUAGE ForeignFunctionInterface #-}

module SDL3.Raw.Misc
  ( sdlOpenURLRaw
  ) where

import Foreign.C.String (CString)

foreign import ccall "SDL_OpenURL" sdlOpenURLRaw :: CString -> IO Bool
