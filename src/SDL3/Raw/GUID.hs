{-# LANGUAGE ForeignFunctionInterface #-}

module SDL3.Raw.GUID
  ( SDLGUID(..)
  , sdlGUIDToStringRaw
  , sdlStringToGUIDRaw
  ) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

newtype SDLGUID = SDLGUID {unSDLGUID :: [Word8]}
  deriving (Show, Eq)

instance Storable SDLGUID where
  sizeOf _ = 16
  alignment _ = alignment (undefined :: Word8)

  peek ptr = do
    bytes <- peekArray 16 (castPtr ptr)
    return $ SDLGUID bytes

  poke ptr (SDLGUID bytes) =
    pokeArray (castPtr ptr) (take 16 $ bytes ++ repeat 0)

foreign import ccall safe "wrapper_SDL_GUIDToString"
  sdlGUIDToStringRaw ::
    Ptr Word8 -> CString -> CInt -> IO ()

foreign import ccall safe "wrapper_SDL_StringToGUID"
  sdlStringToGUIDRaw ::
    CString -> Ptr Word8 -> IO ()
