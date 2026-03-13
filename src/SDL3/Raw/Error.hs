module SDL3.Raw.Error
  ( sdlSetError
  , sdlSetErrorV
  , sdlOutOfMemory
  , sdlGetError
  , sdlClearError
  ) where

import Foreign.C.String (CString)
import Foreign.C.Types (CBool(..))

foreign import ccall "SDL_SetError" sdlSetError :: CString -> IO CBool

foreign import ccall "SDL_SetErrorV" sdlSetErrorV :: CString -> IO CBool

foreign import ccall "SDL_OutOfMemory" sdlOutOfMemory :: IO CBool

foreign import ccall "SDL_GetError" sdlGetError :: IO CString

foreign import ccall "SDL_ClearError" sdlClearError :: IO CBool
