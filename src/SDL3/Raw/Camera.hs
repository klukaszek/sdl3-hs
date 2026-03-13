module SDL3.Raw.Camera
  ( SDLCamera
  , SDLCameraID
  , SDLCameraSpec(..)
  , SDLCameraPosition(..)
  , sdlGetNumCameraDriversRaw
  , sdlGetCameraDriverRaw
  , sdlGetCurrentCameraDriverRaw
  , sdlGetCamerasRaw
  , sdlGetCameraSupportedFormatsRaw
  , sdlGetCameraNameRaw
  , sdlGetCameraPositionRaw
  , sdlOpenCameraRaw
  , sdlGetCameraPermissionStateRaw
  , sdlGetCameraIDRaw
  , sdlGetCameraPropertiesRaw
  , sdlGetCameraFormatRaw
  , sdlAcquireCameraFrameRaw
  , sdlReleaseCameraFrameRaw
  , sdlCloseCameraRaw
  ) where

import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import SDL3.Raw.Pixels
  ( SDLColorspace(..)
  , SDLPixelFormat(..)
  , cUIntToColorspace
  , cUIntToPixelFormat
  , colorspaceToWord32
  , pixelFormatToCUInt
  )
import SDL3.Raw.Properties (SDLPropertiesID)
import SDL3.Raw.Surface (SDLSurface)

type SDLCameraID = Word32

data SDLCamera = SDLCamera

data SDLCameraSpec = SDLCameraSpec
  { cameraFormat :: SDLPixelFormat
  , cameraColorspace :: SDLColorspace
  , cameraWidth :: Int
  , cameraHeight :: Int
  , cameraFramerateNum :: Int
  , cameraFramerateDenom :: Int
  }
  deriving (Eq, Show)

instance Storable SDLCameraSpec where
  sizeOf _ = 24
  alignment _ = 4
  peek ptr = do
    fmt <- peekByteOff ptr 0 :: IO Word32
    cspace <- peekByteOff ptr 4 :: IO Word32
    w <- peekByteOff ptr 8 :: IO Int32
    h <- peekByteOff ptr 12 :: IO Int32
    num <- peekByteOff ptr 16 :: IO Int32
    denom <- peekByteOff ptr 20 :: IO Int32
    return $
      SDLCameraSpec
        (cUIntToPixelFormat (fromIntegral fmt))
        (cUIntToColorspace (fromIntegral cspace))
        (fromIntegral w)
        (fromIntegral h)
        (fromIntegral num)
        (fromIntegral denom)
  poke ptr (SDLCameraSpec fmt cspace w h num denom) = do
    pokeByteOff ptr 0 (pixelFormatToCUInt fmt)
    pokeByteOff ptr 4 (colorspaceToWord32 cspace)
    pokeByteOff ptr 8 (fromIntegral w :: CInt)
    pokeByteOff ptr 12 (fromIntegral h :: CInt)
    pokeByteOff ptr 16 (fromIntegral num :: CInt)
    pokeByteOff ptr 20 (fromIntegral denom :: CInt)

data SDLCameraPosition
  = SDL_CAMERA_POSITION_UNKNOWN
  | SDL_CAMERA_POSITION_FRONT_FACING
  | SDL_CAMERA_POSITION_BACK_FACING
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

foreign import ccall "SDL_GetNumCameraDrivers"
  sdlGetNumCameraDriversRaw :: IO CInt

foreign import ccall "SDL_GetCameraDriver"
  sdlGetCameraDriverRaw :: CInt -> IO CString

foreign import ccall "SDL_GetCurrentCameraDriver"
  sdlGetCurrentCameraDriverRaw :: IO CString

foreign import ccall "SDL_GetCameras"
  sdlGetCamerasRaw :: Ptr CInt -> IO (Ptr SDLCameraID)

foreign import ccall "SDL_GetCameraSupportedFormats"
  sdlGetCameraSupportedFormatsRaw :: SDLCameraID -> Ptr CInt -> IO (Ptr (Ptr SDLCameraSpec))

foreign import ccall "SDL_GetCameraName"
  sdlGetCameraNameRaw :: SDLCameraID -> IO CString

foreign import ccall "SDL_GetCameraPosition"
  sdlGetCameraPositionRaw :: SDLCameraID -> IO CInt

foreign import ccall "SDL_OpenCamera"
  sdlOpenCameraRaw :: SDLCameraID -> Ptr SDLCameraSpec -> IO (Ptr SDLCamera)

foreign import ccall "SDL_GetCameraPermissionState"
  sdlGetCameraPermissionStateRaw :: Ptr SDLCamera -> IO CInt

foreign import ccall "SDL_GetCameraID"
  sdlGetCameraIDRaw :: Ptr SDLCamera -> IO SDLCameraID

foreign import ccall "SDL_GetCameraProperties"
  sdlGetCameraPropertiesRaw :: Ptr SDLCamera -> IO SDLPropertiesID

foreign import ccall "SDL_GetCameraFormat"
  sdlGetCameraFormatRaw :: Ptr SDLCamera -> Ptr SDLCameraSpec -> IO Bool

foreign import ccall "SDL_AcquireCameraFrame"
  sdlAcquireCameraFrameRaw :: Ptr SDLCamera -> Ptr Word64 -> IO (Ptr SDLSurface)

foreign import ccall "SDL_ReleaseCameraFrame"
  sdlReleaseCameraFrameRaw :: Ptr SDLCamera -> Ptr SDLSurface -> IO ()

foreign import ccall "SDL_CloseCamera"
  sdlCloseCameraRaw :: Ptr SDLCamera -> IO ()
