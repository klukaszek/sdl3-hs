module SDL3.Wrapped.Camera
  ( SDLCamera
  , SDLCameraID
  , SDLCameraSpec(..)
  , SDLCameraPosition(..)
  , withCameraPtr
  , sdlUnsafeFromRawCamera
  , sdlUnsafeToRawCamera
  , sdlGetNumCameraDrivers
  , sdlGetCameraDriver
  , sdlGetCurrentCameraDriver
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

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word64)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable
import SDL3.Raw.Camera
  ( SDLCameraID
  , SDLCameraPosition(..)
  , SDLCameraSpec(..)
  )
import SDL3.Raw.Properties (SDLPropertiesID)
import qualified SDL3.Raw.Camera as Raw
import SDL3.Surface (SDLSurface, sdlUnsafeFromRawSurface, sdlUnsafeToRawSurface)

newtype SDLCamera = SDLCamera (Ptr Raw.SDLCamera)
  deriving (Eq)

instance Show SDLCamera where
  show (SDLCamera cameraPtr) = "SDLCamera " ++ show cameraPtr

sdlUnsafeFromRawCamera :: Ptr Raw.SDLCamera -> Maybe SDLCamera
sdlUnsafeFromRawCamera cameraPtr
  | cameraPtr == nullPtr = Nothing
  | otherwise = Just (SDLCamera cameraPtr)

sdlUnsafeToRawCamera :: SDLCamera -> Ptr Raw.SDLCamera
sdlUnsafeToRawCamera (SDLCamera cameraPtr) = cameraPtr

withCameraPtr :: MonadIO m => SDLCamera -> (Ptr Raw.SDLCamera -> IO a) -> m a
withCameraPtr camera action = liftIO $ action (sdlUnsafeToRawCamera camera)

sdlGetNumCameraDrivers :: MonadIO m => m CInt
sdlGetNumCameraDrivers = liftIO Raw.sdlGetNumCameraDriversRaw

sdlGetCameraDriver :: MonadIO m => Int -> m (Maybe String)
sdlGetCameraDriver index = liftIO $ do
  cstr <- Raw.sdlGetCameraDriverRaw (fromIntegral index)
  if cstr == nullPtr
    then return Nothing
    else Just <$> peekCString cstr

sdlGetCurrentCameraDriver :: MonadIO m => m (Maybe String)
sdlGetCurrentCameraDriver = liftIO $ do
  cstr <- Raw.sdlGetCurrentCameraDriverRaw
  if cstr == nullPtr
    then return Nothing
    else Just <$> peekCString cstr

sdlGetCameras :: MonadIO m => m [SDLCameraID]
sdlGetCameras = liftIO $ alloca $ \countPtr -> do
  poke countPtr 0
  idsPtr <- Raw.sdlGetCamerasRaw countPtr
  if idsPtr == nullPtr
    then return []
    else do
      count <- peek countPtr
      peekArray (fromIntegral count) idsPtr

sdlGetCameraSupportedFormats :: MonadIO m => SDLCameraID -> m [SDLCameraSpec]
sdlGetCameraSupportedFormats instanceId = liftIO $ alloca $ \countPtr -> do
  poke countPtr 0
  specsPtr <- Raw.sdlGetCameraSupportedFormatsRaw instanceId countPtr
  if specsPtr == nullPtr
    then return []
    else do
      count <- peek countPtr
      specPtrs <- peekArray (fromIntegral count) specsPtr
      mapM peek specPtrs

sdlGetCameraName :: MonadIO m => SDLCameraID -> m (Maybe String)
sdlGetCameraName instanceId = liftIO $ do
  cstr <- Raw.sdlGetCameraNameRaw instanceId
  if cstr == nullPtr
    then return Nothing
    else Just <$> peekCString cstr

sdlGetCameraPosition :: MonadIO m => SDLCameraID -> m CInt
sdlGetCameraPosition = liftIO . Raw.sdlGetCameraPositionRaw

sdlOpenCamera :: MonadIO m => SDLCameraID -> Maybe SDLCameraSpec -> m (Maybe SDLCamera)
sdlOpenCamera instanceId mspec = liftIO $
  case mspec of
    Just spec -> alloca $ \specPtr -> do
      poke specPtr spec
      camPtr <- Raw.sdlOpenCameraRaw instanceId specPtr
      return $ sdlUnsafeFromRawCamera camPtr
    Nothing -> do
      camPtr <- Raw.sdlOpenCameraRaw instanceId nullPtr
      return $ sdlUnsafeFromRawCamera camPtr

sdlGetCameraPermissionState :: MonadIO m => SDLCamera -> m CInt
sdlGetCameraPermissionState camera =
  liftIO $ Raw.sdlGetCameraPermissionStateRaw (sdlUnsafeToRawCamera camera)

sdlGetCameraID :: MonadIO m => SDLCamera -> m SDLCameraID
sdlGetCameraID camera = liftIO $ Raw.sdlGetCameraIDRaw (sdlUnsafeToRawCamera camera)

sdlGetCameraProperties :: MonadIO m => SDLCamera -> m SDLPropertiesID
sdlGetCameraProperties camera =
  liftIO $ Raw.sdlGetCameraPropertiesRaw (sdlUnsafeToRawCamera camera)

sdlGetCameraFormat :: MonadIO m => SDLCamera -> m (Maybe SDLCameraSpec)
sdlGetCameraFormat camera = liftIO $ alloca $ \specPtr -> do
  success <- Raw.sdlGetCameraFormatRaw (sdlUnsafeToRawCamera camera) specPtr
  if success
    then Just <$> peek specPtr
    else return Nothing

sdlAcquireCameraFrame :: MonadIO m => SDLCamera -> m (Maybe (SDLSurface, Word64))
sdlAcquireCameraFrame camera = liftIO $ alloca $ \tsPtr -> do
  surfPtr <- Raw.sdlAcquireCameraFrameRaw (sdlUnsafeToRawCamera camera) tsPtr
  case sdlUnsafeFromRawSurface surfPtr of
    Nothing -> return Nothing
    Just surface -> do
      ts <- peek tsPtr
      return $ Just (surface, ts)

sdlReleaseCameraFrame :: MonadIO m => SDLCamera -> SDLSurface -> m ()
sdlReleaseCameraFrame camera surface =
  liftIO $ Raw.sdlReleaseCameraFrameRaw (sdlUnsafeToRawCamera camera) (sdlUnsafeToRawSurface surface)

sdlCloseCamera :: MonadIO m => SDLCamera -> m ()
sdlCloseCamera camera = liftIO $ Raw.sdlCloseCameraRaw (sdlUnsafeToRawCamera camera)
