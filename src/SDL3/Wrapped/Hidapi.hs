{-# LANGUAGE PatternSynonyms #-}

module SDL3.Wrapped.Hidapi
  ( SDLHidDevice(..)
  , SDLHidBusType(..)
  , SDLHidDeviceInfo(..)
  , sdlHidInit
  , sdlHidExit
  , sdlHidDeviceChangeCount
  , sdlHidEnumerate
  , sdlHidFreeEnumeration
  , sdlHidOpen
  , sdlHidOpenPath
  , sdlHidWrite
  , sdlHidReadTimeout
  , sdlHidRead
  , sdlHidSetNonblocking
  , sdlHidSendFeatureReport
  , sdlHidGetFeatureReport
  , sdlHidGetInputReport
  , sdlHidClose
  , pattern SDL_PROP_HIDAPI_LIBUSB_DEVICE_HANDLE_POINTER
  , sdlHidGetProperties
  , sdlHidGetManufacturerString
  , sdlHidGetProductString
  , sdlHidGetSerialNumberString
  , sdlHidGetIndexedString
  , sdlHidGetDeviceInfo
  , sdlHidGetReportDescriptor
  , sdlHidBleScan
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign
import Foreign.C.String
import Foreign.C.Types
import SDL3.Raw.Hidapi
  ( SDLHidBusType(..)
  , SDLHidDevice(..)
  , SDLHidDeviceInfo(..)
  , pattern SDL_PROP_HIDAPI_LIBUSB_DEVICE_HANDLE_POINTER
  )
import SDL3.Raw.Properties (SDLPropertiesID)
import qualified SDL3.Raw.Hidapi as Raw

buildHidDeviceInfoList :: Ptr SDLHidDeviceInfo -> IO [SDLHidDeviceInfo]
buildHidDeviceInfoList ptr
  | ptr == nullPtr = return []
  | otherwise = do
      deviceInfo <- peek ptr
      restInfo <- buildHidDeviceInfoList (hidNext deviceInfo)
      return (deviceInfo : restInfo)

sdlHidGetProperties :: MonadIO m => SDLHidDevice -> m SDLPropertiesID
sdlHidGetProperties = liftIO . Raw.sdlHidGetPropertiesRaw

sdlHidInit :: MonadIO m => m CInt
sdlHidInit = liftIO Raw.sdlHidInitRaw

sdlHidExit :: MonadIO m => m CInt
sdlHidExit = liftIO Raw.sdlHidExitRaw

sdlHidDeviceChangeCount :: MonadIO m => m Word32
sdlHidDeviceChangeCount = liftIO Raw.sdlHidDeviceChangeCountRaw

sdlHidEnumerate :: MonadIO m => Word16 -> Word16 -> m (Maybe [SDLHidDeviceInfo], Ptr SDLHidDeviceInfo)
sdlHidEnumerate vendorId productId = liftIO $ do
  devicePtr <- Raw.sdlHidEnumerateRaw vendorId productId
  if devicePtr == nullPtr
    then return (Nothing, nullPtr)
    else do
      devices <- buildHidDeviceInfoList devicePtr
      return (Just devices, devicePtr)

sdlHidFreeEnumeration :: MonadIO m => Ptr SDLHidDeviceInfo -> m ()
sdlHidFreeEnumeration = liftIO . Raw.sdlHidFreeEnumerationRaw

sdlHidOpen :: MonadIO m => Word16 -> Word16 -> Maybe String -> m (Maybe SDLHidDevice)
sdlHidOpen vendorId productId serial = liftIO $ do
  device <- case serial of
    Nothing -> Raw.sdlHidOpenRaw vendorId productId nullPtr
    Just s -> withCWString s $ \serialPtr -> Raw.sdlHidOpenRaw vendorId productId serialPtr
  if device == nullPtr
    then return Nothing
    else return $ Just $ SDLHidDevice device

sdlHidOpenPath :: MonadIO m => String -> m (Maybe SDLHidDevice)
sdlHidOpenPath path = liftIO $ do
  device <- withCString path Raw.sdlHidOpenPathRaw
  if device == nullPtr
    then return Nothing
    else return $ Just $ SDLHidDevice device

sdlHidWrite :: MonadIO m => SDLHidDevice -> [Word8] -> m Int
sdlHidWrite (SDLHidDevice dev) bytes = liftIO $
  withArrayLen bytes $ \len bytesPtr -> do
    result <- Raw.sdlHidWriteRaw dev bytesPtr (fromIntegral len)
    return (fromIntegral result)

sdlHidReadTimeout :: MonadIO m => SDLHidDevice -> Int -> Int -> m (Maybe [Word8])
sdlHidReadTimeout (SDLHidDevice dev) len timeoutMs = liftIO $
  allocaArray len $ \buffer -> do
    bytesRead <- Raw.sdlHidReadTimeoutRaw dev buffer (fromIntegral len) (fromIntegral timeoutMs)
    if bytesRead <= 0
      then return Nothing
      else Just <$> peekArray (fromIntegral bytesRead) buffer

sdlHidRead :: MonadIO m => SDLHidDevice -> Int -> m (Maybe [Word8])
sdlHidRead (SDLHidDevice dev) len = liftIO $
  allocaArray len $ \buffer -> do
    bytesRead <- Raw.sdlHidReadRaw dev buffer (fromIntegral len)
    if bytesRead <= 0
      then return Nothing
      else Just <$> peekArray (fromIntegral bytesRead) buffer

sdlHidSetNonblocking :: MonadIO m => SDLHidDevice -> Bool -> m Bool
sdlHidSetNonblocking (SDLHidDevice dev) nonblock = liftIO $ do
  result <- Raw.sdlHidSetNonblockingRaw dev (if nonblock then 1 else 0)
  return (result == 0)

sdlHidSendFeatureReport :: MonadIO m => SDLHidDevice -> [Word8] -> m Int
sdlHidSendFeatureReport (SDLHidDevice dev) bytes = liftIO $
  withArrayLen bytes $ \len bytesPtr -> do
    result <- Raw.sdlHidSendFeatureReportRaw dev bytesPtr (fromIntegral len)
    return (fromIntegral result)

sdlHidGetFeatureReport :: MonadIO m => SDLHidDevice -> Word8 -> Int -> m (Maybe [Word8])
sdlHidGetFeatureReport (SDLHidDevice dev) reportId len = liftIO $
  allocaArray len $ \buffer -> do
    poke buffer reportId
    bytesRead <- Raw.sdlHidGetFeatureReportRaw dev buffer (fromIntegral len)
    if bytesRead <= 0
      then return Nothing
      else Just <$> peekArray (fromIntegral bytesRead) buffer

sdlHidGetInputReport :: MonadIO m => SDLHidDevice -> Word8 -> Int -> m (Maybe [Word8])
sdlHidGetInputReport (SDLHidDevice dev) reportId len = liftIO $
  allocaArray len $ \buffer -> do
    poke buffer reportId
    bytesRead <- Raw.sdlHidGetInputReportRaw dev buffer (fromIntegral len)
    if bytesRead <= 0
      then return Nothing
      else Just <$> peekArray (fromIntegral bytesRead) buffer

sdlHidClose :: MonadIO m => SDLHidDevice -> m Bool
sdlHidClose (SDLHidDevice dev) = liftIO $ do
  result <- Raw.sdlHidCloseRaw dev
  return (result == 0)

sdlHidGetManufacturerString :: MonadIO m => SDLHidDevice -> m (Maybe String)
sdlHidGetManufacturerString (SDLHidDevice dev) = liftIO $ do
  let bufSize = 256
  allocaArray0 bufSize $ \buffer -> do
    result <- Raw.sdlHidGetManufacturerStringRaw dev buffer (fromIntegral bufSize)
    if result < 0
      then return Nothing
      else Just <$> peekCWString buffer

sdlHidGetProductString :: MonadIO m => SDLHidDevice -> m (Maybe String)
sdlHidGetProductString (SDLHidDevice dev) = liftIO $ do
  let bufSize = 256
  allocaArray0 bufSize $ \buffer -> do
    result <- Raw.sdlHidGetProductStringRaw dev buffer (fromIntegral bufSize)
    if result < 0
      then return Nothing
      else Just <$> peekCWString buffer

sdlHidGetSerialNumberString :: MonadIO m => SDLHidDevice -> m (Maybe String)
sdlHidGetSerialNumberString (SDLHidDevice dev) = liftIO $ do
  let bufSize = 256
  allocaArray0 bufSize $ \buffer -> do
    result <- Raw.sdlHidGetSerialNumberStringRaw dev buffer (fromIntegral bufSize)
    if result < 0
      then return Nothing
      else Just <$> peekCWString buffer

sdlHidGetIndexedString :: MonadIO m => SDLHidDevice -> Int -> m (Maybe String)
sdlHidGetIndexedString (SDLHidDevice dev) stringIndex = liftIO $ do
  let bufSize = 256
  allocaArray0 bufSize $ \buffer -> do
    result <- Raw.sdlHidGetIndexedStringRaw dev (fromIntegral stringIndex) buffer (fromIntegral bufSize)
    if result < 0
      then return Nothing
      else Just <$> peekCWString buffer

sdlHidGetDeviceInfo :: MonadIO m => SDLHidDevice -> m (Maybe SDLHidDeviceInfo)
sdlHidGetDeviceInfo (SDLHidDevice dev) = liftIO $ do
  deviceInfo <- Raw.sdlHidGetDeviceInfoRaw dev
  if deviceInfo == nullPtr
    then return Nothing
    else Just <$> peek deviceInfo

sdlHidGetReportDescriptor :: MonadIO m => SDLHidDevice -> m (Maybe [Word8])
sdlHidGetReportDescriptor (SDLHidDevice dev) = liftIO $ do
  let bufSize = 4096
  allocaArray bufSize $ \buffer -> do
    bytesRead <- Raw.sdlHidGetReportDescriptorRaw dev buffer (fromIntegral bufSize)
    if bytesRead <= 0
      then return Nothing
      else Just <$> peekArray (fromIntegral bytesRead) buffer

sdlHidBleScan :: MonadIO m => Bool -> m ()
sdlHidBleScan = liftIO . Raw.sdlHidBleScanRaw
