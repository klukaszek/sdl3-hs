{-|
Module      : SDL.Camera
Description : Video capture and camera management
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

This module provides bindings to the SDL3 camera API, allowing Haskell applications
to interact with video capture devices such as webcams. It supports enumerating,
querying, and opening camera devices, as well as acquiring video frames as
SDL surfaces.

The SDL camera API provides access to video frames but does not handle audio
or full-motion video encoding. Applications can process frames as pixel data
or upload them to SDL textures for rendering.

Note that camera access often requires user permission on many platforms,
which may involve a system prompt. The API provides mechanisms to check
permission status and handle approval or denial events.
-}

module SDL.Camera
  ( -- * Camera Types
    SDL_CameraID
  , SDL_Camera
  , SDL_CameraSpec(..)
  , SDL_CameraPosition(..)

    -- * Camera Driver Functions
  , sdlGetNumCameraDrivers
  , sdlGetCameraDriver
  , sdlGetCurrentCameraDriver

    -- * Camera Device Functions
  , sdlGetCameras
  , sdlGetCameraSupportedFormats
  , sdlGetCameraName
  , sdlGetCameraPosition
  , sdlOpenCamera
  , sdlGetCameraPermissionState
  , sdlGetCameraID
  , sdlGetCameraProperties
  , sdlGetCameraFormat
  , sdlAcquireCameraFrame
  , sdlReleaseCameraFrame
  , sdlCloseCamera
  ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Data.Word
import Data.Int
import SDL.Pixels (SDLPixelFormat(..), pixelFormatToWord32, SDLColorspace(..), colorspaceToWord32)
import SDL.Surface (SDLSurface)

-- | A unique ID for a camera device, valid while connected to the system.
--
-- If the device is disconnected and reconnected, it receives a new ID.
-- The value 0 is invalid.
--
-- @since 3.2.0
type SDL_CameraID = Word32

-- | An opaque structure representing an opened camera device.
--
-- @since 3.2.0
data SDL_Camera = SDL_Camera

-- | Specification of an output format for a camera device.
--
-- @since 3.2.0
data SDL_CameraSpec = SDL_CameraSpec
  { cameraFormat         :: SDLPixelFormat     -- ^ Frame pixel format
  , cameraColorspace     :: SDLColorspace      -- ^ Frame colorspace
  , cameraWidth          :: Int                -- ^ Frame width
  , cameraHeight         :: Int                -- ^ Frame height
  , cameraFramerateNum   :: Int                -- ^ Frame rate numerator (FPS = num / denom)
  , cameraFramerateDenom :: Int                -- ^ Frame rate denominator (duration = denom / num)
  } deriving (Eq, Show)

instance Storable SDL_CameraSpec where
  sizeOf _ = 24  -- sizeof(SDL_PixelFormat) + sizeof(SDL_Colorspace) + 4 * sizeof(int)
  alignment _ = 4
  peek ptr = do
    fmt    <- peekByteOff ptr 0  :: IO Word32
    cspace <- peekByteOff ptr 4  :: IO Word32
    w      <- peekByteOff ptr 8  :: IO Int32  -- Specify Int32
    h      <- peekByteOff ptr 12 :: IO Int32  -- Specify Int32
    num    <- peekByteOff ptr 16 :: IO Int32  -- Specify Int32
    denom  <- peekByteOff ptr 20 :: IO Int32  -- Specify Int32
    return $ SDL_CameraSpec (toEnum $ fromIntegral fmt) (toEnum $ fromIntegral cspace)
                            (fromIntegral w) (fromIntegral h) (fromIntegral num) (fromIntegral denom)
  poke ptr (SDL_CameraSpec fmt cspace w h num denom) = do
    pokeByteOff ptr 0 (pixelFormatToWord32 fmt)
    pokeByteOff ptr 4 (colorspaceToWord32 cspace)
    pokeByteOff ptr 8 (fromIntegral w :: CInt)
    pokeByteOff ptr 12 (fromIntegral h :: CInt)
    pokeByteOff ptr 16 (fromIntegral num :: CInt)
    pokeByteOff ptr 20 (fromIntegral denom :: CInt)

-- | Position of a camera relative to the system device.
--
-- @since 3.2.0
data SDL_CameraPosition
  = SDL_CameraPositionUnknown
  | SDL_CameraPositionFrontFacing
  | SDL_CameraPositionBackFacing
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Get the number of built-in camera drivers.
--
-- Returns a hardcoded number of available drivers, or zero if none are compiled in.
--
-- @since 3.2.0
foreign import ccall "SDL_GetNumCameraDrivers"
  sdlGetNumCameraDrivers :: IO CInt

-- | Get the name of a built-in camera driver by index.
--
-- Driver names are simple ASCII identifiers (e.g., "v4l2", "coremedia").
--
-- @since 3.2.0
foreign import ccall "SDL_GetCameraDriver"
  sdlGetCameraDriverRaw :: CInt -> IO CString

sdlGetCameraDriver :: Int -> IO (Maybe String)
sdlGetCameraDriver index = do
  cstr <- sdlGetCameraDriverRaw (fromIntegral index)
  if cstr == nullPtr
    then return Nothing
    else Just <$> peekCString cstr

-- | Get the name of the current camera driver.
--
-- Returns NULL if no driver is initialized.
--
-- @since 3.2.0
foreign import ccall "SDL_GetCurrentCameraDriver"
  sdlGetCurrentCameraDriverRaw :: IO CString

sdlGetCurrentCameraDriver :: IO (Maybe String)
sdlGetCurrentCameraDriver = do
  cstr <- sdlGetCurrentCameraDriverRaw
  if cstr == nullPtr
    then return Nothing
    else Just <$> peekCString cstr

-- | Get a list of currently connected camera devices.
--
-- @since 3.2.0
foreign import ccall "SDL_GetCameras"
  sdlGetCamerasRaw :: Ptr CInt -> IO (Ptr SDL_CameraID)

sdlGetCameras :: IO [SDL_CameraID]
sdlGetCameras = alloca $ \countPtr -> do
  poke countPtr 0
  idsPtr <- sdlGetCamerasRaw countPtr
  if idsPtr == nullPtr
    then return []
    else do
      count <- peek countPtr
      ids <- peekArray (fromIntegral count) idsPtr
      return ids

-- | Get the list of native formats/sizes a camera supports.
--
-- @since 3.2.0
foreign import ccall "SDL_GetCameraSupportedFormats"
  sdlGetCameraSupportedFormatsRaw :: SDL_CameraID -> Ptr CInt -> IO (Ptr (Ptr SDL_CameraSpec))

sdlGetCameraSupportedFormats :: SDL_CameraID -> IO [SDL_CameraSpec]
sdlGetCameraSupportedFormats instance_id = alloca $ \countPtr -> do
  poke countPtr 0
  specsPtr <- sdlGetCameraSupportedFormatsRaw instance_id countPtr
  if specsPtr == nullPtr
    then return []
    else do
      count <- peek countPtr
      specPtrs <- peekArray (fromIntegral count) specsPtr
      mapM peek specPtrs

-- | Get the human-readable name of a camera device.
--
-- @since 3.2.0
foreign import ccall "SDL_GetCameraName"
  sdlGetCameraNameRaw :: SDL_CameraID -> IO CString

sdlGetCameraName :: SDL_CameraID -> IO (Maybe String)
sdlGetCameraName instance_id = do
  cstr <- sdlGetCameraNameRaw instance_id
  if cstr == nullPtr
    then return Nothing
    else Just <$> peekCString cstr

-- | Get the position of a camera relative to the system.
--
-- @since 3.2.0
foreign import ccall "SDL_GetCameraPosition"
  sdlGetCameraPosition :: SDL_CameraID -> IO CInt

sdlGetCameraPositionEnum :: SDL_CameraID -> IO SDL_CameraPosition
sdlGetCameraPositionEnum instance_id = toEnum . fromIntegral <$> sdlGetCameraPosition instance_id

-- | Open a camera device for video capture.
--
-- @since 3.2.0
foreign import ccall "SDL_OpenCamera"
  sdlOpenCameraRaw :: SDL_CameraID -> Ptr SDL_CameraSpec -> IO (Ptr SDL_Camera)

sdlOpenCamera :: SDL_CameraID -> Maybe SDL_CameraSpec -> IO (Maybe (Ptr SDL_Camera))
sdlOpenCamera instance_id mspec =
  case mspec of
    Just spec -> alloca $ \specPtr -> do
      poke specPtr spec
      camPtr <- sdlOpenCameraRaw instance_id specPtr
      return $ if camPtr == nullPtr then Nothing else Just camPtr
    Nothing -> do
      camPtr <- sdlOpenCameraRaw instance_id nullPtr
      return $ if camPtr == nullPtr then Nothing else Just camPtr


-- | Query if camera access has been approved by the user.
--
-- Returns -1 if denied, 1 if approved, 0 if pending.
--
-- @since 3.2.0
foreign import ccall "SDL_GetCameraPermissionState"
  sdlGetCameraPermissionState :: Ptr SDL_Camera -> IO CInt

-- | Get the instance ID of an opened camera.
--
-- @since 3.2.0
foreign import ccall "SDL_GetCameraID"
  sdlGetCameraID :: Ptr SDL_Camera -> IO SDL_CameraID

-- | Get the properties associated with an opened camera.
--
-- @since 3.2.0
foreign import ccall "SDL_GetCameraProperties"
  sdlGetCameraProperties :: Ptr SDL_Camera -> IO Word32

-- | Get the format specification of an opened camera.
--
-- @since 3.2.0
foreign import ccall "SDL_GetCameraFormat"
  sdlGetCameraFormatRaw :: Ptr SDL_Camera -> Ptr SDL_CameraSpec -> IO Bool

sdlGetCameraFormat :: Ptr SDL_Camera -> IO (Maybe SDL_CameraSpec)
sdlGetCameraFormat camera = alloca $ \specPtr -> do
  success <- sdlGetCameraFormatRaw camera specPtr
  if success
    then Just <$> peek specPtr
    else return Nothing

-- | Acquire a frame from an opened camera.
--
-- @since 3.2.0
foreign import ccall "SDL_AcquireCameraFrame"
  sdlAcquireCameraFrameRaw :: Ptr SDL_Camera -> Ptr Word64 -> IO (Ptr SDLSurface)

sdlAcquireCameraFrame :: Ptr SDL_Camera -> IO (Maybe (Ptr SDLSurface, Word64))
sdlAcquireCameraFrame camera = alloca $ \tsPtr -> do
  surfPtr <- sdlAcquireCameraFrameRaw camera tsPtr
  if surfPtr == nullPtr
    then return Nothing
    else do
      ts <- peek tsPtr
      return $ Just (surfPtr, ts)

-- | Release a frame acquired from a camera.
--
-- @since 3.2.0
foreign import ccall "SDL_ReleaseCameraFrame"
  sdlReleaseCameraFrame :: Ptr SDL_Camera -> Ptr SDLSurface -> IO ()

-- | Close an opened camera device.
--
-- @since 3.2.0
foreign import ccall "SDL_CloseCamera"
  sdlCloseCamera :: Ptr SDL_Camera -> IO ()
