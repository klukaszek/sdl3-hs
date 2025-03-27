{-# LANGUAGE ForeignFunctionInterface #-}

{-|
Module      : SDL.Hidapi
Description : SDL HIDAPI functions for interacting with HID devices
Copyright   : Kyle Lukaszek, 2025
License     : BSD3

This module provides bindings to the SDL3 HIDAPI, allowing Haskell applications
to interact with HID (Human Interface Device) devices such as game controllers,
keyboards, mice and other USB, Bluetooth, I2C, or SPI devices.

The HIDAPI allows enumeration, opening, and reading/writing to HID devices,
which is especially useful for accessing specialized hardware or game controllers.

This module is an adaptation of the original HIDAPI interface by Alan Ott.
-}

module SDL.Hidapi
  ( -- * Types
    SDLHidDevice(..)
  , SDLHidBusType(..)
  , SDLHidDeviceInfo(..)
  
    -- * Initialization
  , sdlHidInit
  , sdlHidExit
  
    -- * Device Discovery
  , sdlHidDeviceChangeCount
  , sdlHidEnumerate
  , sdlHidFreeEnumeration
  
    -- * Device Operations
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
  
    -- * Device Information
  , sdlHidGetManufacturerString
  , sdlHidGetProductString
  , sdlHidGetSerialNumberString
  , sdlHidGetIndexedString
  , sdlHidGetDeviceInfo
  , sdlHidGetReportDescriptor
  
    -- * Bluetooth Operations
  , sdlHidBleScan
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.Word
import Data.Int
import Control.Monad

import SDL.Stdinc
import SDL.Error

#include <SDL3/SDL_hidapi.h>

-- | An opaque handle representing an open HID device.
newtype SDLHidDevice = SDLHidDevice { unSDLHidDevice :: Ptr SDLHidDevice }
  deriving (Show, Eq)

-- | HID underlying bus types.
data SDLHidBusType
  = SDL_HID_API_BUS_UNKNOWN    -- ^ Unknown bus type
  | SDL_HID_API_BUS_USB        -- ^ USB bus
  | SDL_HID_API_BUS_BLUETOOTH  -- ^ Bluetooth or Bluetooth LE bus
  | SDL_HID_API_BUS_I2C        -- ^ I2C bus
  | SDL_HID_API_BUS_SPI        -- ^ SPI bus
  deriving (Show, Eq, Enum)

-- | Information about a connected HID device
data SDLHidDeviceInfo = SDLHidDeviceInfo
  { hidPath             :: String          -- ^ Platform-specific device path
  , hidVendorId         :: Word16          -- ^ Device Vendor ID
  , hidProductId        :: Word16          -- ^ Device Product ID
  , hidSerialNumber     :: Maybe String    -- ^ Serial Number (may be Nothing)
  , hidReleaseNumber    :: Word16          -- ^ Device Release Number (BCD)
  , hidManufacturer     :: Maybe String    -- ^ Manufacturer String (may be Nothing)
  , hidProduct          :: Maybe String    -- ^ Product string (may be Nothing)
  , hidUsagePage        :: Word16          -- ^ Usage Page for this Device/Interface
  , hidUsage            :: Word16          -- ^ Usage for this Device/Interface
  , hidInterfaceNumber  :: Int             -- ^ The USB interface number (-1 if not applicable)
  , hidInterfaceClass   :: Int             -- ^ Additional USB interface class info
  , hidInterfaceSubclass:: Int             -- ^ Additional USB interface subclass info
  , hidInterfaceProtocol:: Int             -- ^ Additional USB interface protocol info
  , hidBusType          :: SDLHidBusType   -- ^ Underlying bus type
  , hidNext             :: Ptr SDLHidDeviceInfo -- ^ Next device in list (raw pointer)
  } deriving (Show, Eq)

instance Storable SDLHidDeviceInfo where
  sizeOf _ = #{size SDL_hid_device_info}
  alignment _ = #{alignment SDL_hid_device_info}
  
  peek ptr = do
    pathPtr <- #{peek SDL_hid_device_info, path} ptr
    path <- if pathPtr == nullPtr then return "" else peekCString pathPtr
    
    vendorId <- #{peek SDL_hid_device_info, vendor_id} ptr
    productId <- #{peek SDL_hid_device_info, product_id} ptr
    
    serialNumberPtr <- #{peek SDL_hid_device_info, serial_number} ptr
    serialNumber <- if serialNumberPtr == nullPtr 
                    then return Nothing 
                    else Just <$> peekCWString serialNumberPtr
    
    releaseNumber <- #{peek SDL_hid_device_info, release_number} ptr
    
    manufacturerPtr <- #{peek SDL_hid_device_info, manufacturer_string} ptr
    manufacturer <- if manufacturerPtr == nullPtr 
                    then return Nothing 
                    else Just <$> peekCWString manufacturerPtr
    
    productPtr <- #{peek SDL_hid_device_info, product_string} ptr
    product <- if productPtr == nullPtr 
               then return Nothing 
               else Just <$> peekCWString productPtr
    
    usagePage <- #{peek SDL_hid_device_info, usage_page} ptr
    usage <- #{peek SDL_hid_device_info, usage} ptr
    interfaceNumber <- #{peek SDL_hid_device_info, interface_number} ptr
    interfaceClass <- #{peek SDL_hid_device_info, interface_class} ptr
    interfaceSubclass <- #{peek SDL_hid_device_info, interface_subclass} ptr
    interfaceProtocol <- #{peek SDL_hid_device_info, interface_protocol} ptr
    
    busTypeInt <- #{peek SDL_hid_device_info, bus_type} ptr :: IO CInt
    let busType = toEnum (fromIntegral busTypeInt)
    
    next <- #{peek SDL_hid_device_info, next} ptr
    
    return $ SDLHidDeviceInfo 
      path vendorId productId serialNumber releaseNumber manufacturer product
      usagePage usage interfaceNumber interfaceClass interfaceSubclass
      interfaceProtocol busType next
  
  poke _ _ = error "Poking SDL_hid_device_info not implemented"
  -- We typically don't need to poke since we're only reading this structure

-- | Initialize the HIDAPI library.
--
-- This function initializes the HIDAPI library. Calling it is not strictly
-- necessary, as it will be called automatically by sdlHidEnumerate() and
-- any of the sdlHidOpen_*() functions if it is needed. This function should
-- be called at the beginning of execution however, if there is a chance of
-- HIDAPI handles being opened by different threads simultaneously.
--
-- Each call to this function should have a matching call to sdlHidExit()
foreign import ccall "SDL_hid_init" sdlHidInit :: IO CInt

-- | Finalize the HIDAPI library.
--
-- This function frees all of the static data associated with HIDAPI. It
-- should be called at the end of execution to avoid memory leaks.
foreign import ccall "SDL_hid_exit" sdlHidExit :: IO CInt

-- | Check to see if devices may have been added or removed.
--
-- Enumerating the HID devices is an expensive operation, so you can call this
-- to see if there have been any system device changes since the last call to
-- this function. A change in the counter returned doesn't necessarily mean
-- that anything has changed, but you can call sdlHidEnumerate() to get an
-- updated device list.
--
-- Calling this function for the first time may cause a thread or other system
-- resource to be allocated to track device change notifications.
foreign import ccall "SDL_hid_device_change_count" sdlHidDeviceChangeCount :: IO Word32

-- | Enumerate the HID Devices.
--
-- This function returns a linked list of all the HID devices attached to the
-- system which match vendor_id and product_id. If `vendor_id` is set to 0
-- then any vendor matches. If `product_id` is set to 0 then any product
-- matches. If `vendor_id` and `product_id` are both set to 0, then all HID
-- devices will be returned.
--
-- By default SDL will only enumerate controllers, to reduce risk of hanging
-- or crashing on bad drivers, but SDL_HINT_HIDAPI_ENUMERATE_ONLY_CONTROLLERS
-- can be set to "0" to enumerate all HID devices.
foreign import ccall "SDL_hid_enumerate" sdlHidEnumerateRaw :: 
  Word16 -> Word16 -> IO (Ptr SDLHidDeviceInfo)

-- Convert the C linked list to a Haskell list, keeping original pointers
buildHidDeviceInfoList :: Ptr SDLHidDeviceInfo -> IO [SDLHidDeviceInfo]
buildHidDeviceInfoList ptr
  | ptr == nullPtr = return []
  | otherwise = do
      deviceInfo <- peek ptr
      restInfo <- buildHidDeviceInfoList (hidNext deviceInfo)
      return (deviceInfo : restInfo)

-- Public wrapper function for enumeration
sdlHidEnumerate :: Word16 -> Word16 -> IO (Maybe [SDLHidDeviceInfo], Ptr SDLHidDeviceInfo)
sdlHidEnumerate vendorId productId = do
  devicePtr <- sdlHidEnumerateRaw vendorId productId
  if devicePtr == nullPtr
    then return (Nothing, nullPtr)
    else do
      devices <- buildHidDeviceInfoList devicePtr
      return (Just devices, devicePtr)

-- | Free an enumeration linked list.
--
-- This function frees a linked list created by sdlHidEnumerate().
foreign import ccall "SDL_hid_free_enumeration" sdlHidFreeEnumeration :: 
  Ptr SDLHidDeviceInfo -> IO ()

-- | Open a HID device using a Vendor ID (VID), Product ID (PID) and optionally
-- a serial number.
--
-- If `serial_number` is Nothing, the first device with the specified VID and PID
-- is opened.
foreign import ccall "SDL_hid_open" sdlHidOpenRaw :: 
  Word16 -> Word16 -> Ptr CWchar -> IO (Ptr SDLHidDevice)

-- Wrapper function with Haskell types
sdlHidOpen :: Word16 -> Word16 -> Maybe String -> IO (Maybe SDLHidDevice)
sdlHidOpen vendorId productId serial = do
  device <- case serial of
              Nothing -> sdlHidOpenRaw vendorId productId nullPtr
              Just s -> withCWString s $ \serialPtr -> 
                          sdlHidOpenRaw vendorId productId serialPtr
  if device == nullPtr
    then return Nothing
    else return $ Just $ SDLHidDevice device

-- | Open a HID device by its path name.
--
-- The path name be determined by calling sdlHidEnumerate(), or a
-- platform-specific path name can be used (eg: /dev/hidraw0 on Linux).
foreign import ccall "SDL_hid_open_path" sdlHidOpenPathRaw :: 
  CString -> IO (Ptr SDLHidDevice)

sdlHidOpenPath :: String -> IO (Maybe SDLHidDevice)
sdlHidOpenPath path = do
  device <- withCString path sdlHidOpenPathRaw
  if device == nullPtr
    then return Nothing
    else return $ Just $ SDLHidDevice device

-- | Write an Output report to a HID device.
--
-- The first byte of `data` must contain the Report ID. For devices which only
-- support a single report, this must be set to 0x0. The remaining bytes
-- contain the report data. Since the Report ID is mandatory, calls to
-- sdlHidWrite() will always contain one more byte than the report contains.
foreign import ccall "SDL_hid_write" sdlHidWriteRaw :: 
  Ptr SDLHidDevice -> Ptr Word8 -> CSize -> IO CInt

sdlHidWrite :: SDLHidDevice -> [Word8] -> IO Int
sdlHidWrite (SDLHidDevice dev) bytes = do
  withArrayLen bytes $ \len bytesPtr -> do
    result <- sdlHidWriteRaw dev bytesPtr (fromIntegral len)
    return (fromIntegral result)

-- | Read an Input report from a HID device with timeout.
--
-- Input reports are returned to the host through the INTERRUPT IN endpoint.
-- The first byte will contain the Report number if the device uses numbered
-- reports.
foreign import ccall "SDL_hid_read_timeout" sdlHidReadTimeoutRaw :: 
  Ptr SDLHidDevice -> Ptr Word8 -> CSize -> CInt -> IO CInt

sdlHidReadTimeout :: SDLHidDevice -> Int -> Int -> IO (Maybe [Word8])
sdlHidReadTimeout (SDLHidDevice dev) len timeoutMs = do
  allocaArray len $ \buffer -> do
    bytesRead <- sdlHidReadTimeoutRaw dev buffer (fromIntegral len) (fromIntegral timeoutMs)
    if bytesRead <= 0
      then return Nothing
      else do
        result <- peekArray (fromIntegral bytesRead) buffer
        return $ Just result

-- | Read an Input report from a HID device.
--
-- Input reports are returned to the host through the INTERRUPT IN endpoint.
-- The first byte will contain the Report number if the device uses numbered
-- reports.
foreign import ccall "SDL_hid_read" sdlHidReadRaw :: 
  Ptr SDLHidDevice -> Ptr Word8 -> CSize -> IO CInt

sdlHidRead :: SDLHidDevice -> Int -> IO (Maybe [Word8])
sdlHidRead (SDLHidDevice dev) len = do
  allocaArray len $ \buffer -> do
    bytesRead <- sdlHidReadRaw dev buffer (fromIntegral len)
    if bytesRead <= 0
      then return Nothing
      else do
        result <- peekArray (fromIntegral bytesRead) buffer
        return $ Just result

-- | Set the device handle to be non-blocking.
--
-- In non-blocking mode calls to sdlHidRead() will return immediately with a
-- value of 0 if there is no data to be read. In blocking mode, sdlHidRead()
-- will wait (block) until there is data to read before returning.
foreign import ccall "SDL_hid_set_nonblocking" sdlHidSetNonblockingRaw :: 
  Ptr SDLHidDevice -> CInt -> IO CInt

sdlHidSetNonblocking :: SDLHidDevice -> Bool -> IO Bool
sdlHidSetNonblocking (SDLHidDevice dev) nonblock = do
  result <- sdlHidSetNonblockingRaw dev (if nonblock then 1 else 0)
  return (result == 0)

-- | Send a Feature report to the device.
--
-- Feature reports are sent over the Control endpoint as a Set_Report
-- transfer. The first byte of `data` must contain the Report ID.
foreign import ccall "SDL_hid_send_feature_report" sdlHidSendFeatureReportRaw :: 
  Ptr SDLHidDevice -> Ptr Word8 -> CSize -> IO CInt

sdlHidSendFeatureReport :: SDLHidDevice -> [Word8] -> IO Int
sdlHidSendFeatureReport (SDLHidDevice dev) bytes = do
  withArrayLen bytes $ \len bytesPtr -> do
    result <- sdlHidSendFeatureReportRaw dev bytesPtr (fromIntegral len)
    return (fromIntegral result)

-- | Get a feature report from a HID device.
--
-- Set the first byte of `data` to the Report ID of the report to be read.
foreign import ccall "SDL_hid_get_feature_report" sdlHidGetFeatureReportRaw :: 
  Ptr SDLHidDevice -> Ptr Word8 -> CSize -> IO CInt

sdlHidGetFeatureReport :: SDLHidDevice -> Word8 -> Int -> IO (Maybe [Word8])
sdlHidGetFeatureReport (SDLHidDevice dev) reportId len = do
  allocaArray len $ \buffer -> do
    poke buffer reportId  -- Set first byte to report ID
    bytesRead <- sdlHidGetFeatureReportRaw dev buffer (fromIntegral len)
    if bytesRead <= 0
      then return Nothing
      else do
        result <- peekArray (fromIntegral bytesRead) buffer
        return $ Just result

-- | Get an input report from a HID device.
--
-- Set the first byte of `data` to the Report ID of the report to be read.
foreign import ccall "SDL_hid_get_input_report" sdlHidGetInputReportRaw :: 
  Ptr SDLHidDevice -> Ptr Word8 -> CSize -> IO CInt

sdlHidGetInputReport :: SDLHidDevice -> Word8 -> Int -> IO (Maybe [Word8])
sdlHidGetInputReport (SDLHidDevice dev) reportId len = do
  allocaArray len $ \buffer -> do
    poke buffer reportId  -- Set first byte to report ID
    bytesRead <- sdlHidGetInputReportRaw dev buffer (fromIntegral len)
    if bytesRead <= 0
      then return Nothing
      else do
        result <- peekArray (fromIntegral bytesRead) buffer
        return $ Just result

-- | Close a HID device.
foreign import ccall "SDL_hid_close" sdlHidCloseRaw :: Ptr SDLHidDevice -> IO CInt

sdlHidClose :: SDLHidDevice -> IO Bool
sdlHidClose (SDLHidDevice dev) = do
  result <- sdlHidCloseRaw dev
  return (result == 0)

-- | Get The Manufacturer String from a HID device.
foreign import ccall "SDL_hid_get_manufacturer_string" sdlHidGetManufacturerStringRaw :: 
  Ptr SDLHidDevice -> Ptr CWchar -> CSize -> IO CInt

sdlHidGetManufacturerString :: SDLHidDevice -> IO (Maybe String)
sdlHidGetManufacturerString (SDLHidDevice dev) = do
  let bufSize = 256  -- Arbitrary but reasonable size
  allocaArray0 bufSize $ \buffer -> do
    result <- sdlHidGetManufacturerStringRaw dev buffer (fromIntegral bufSize)
    if result < 0
      then return Nothing
      else Just <$> peekCWString buffer

-- | Get The Product String from a HID device.
foreign import ccall "SDL_hid_get_product_string" sdlHidGetProductStringRaw :: 
  Ptr SDLHidDevice -> Ptr CWchar -> CSize -> IO CInt

sdlHidGetProductString :: SDLHidDevice -> IO (Maybe String)
sdlHidGetProductString (SDLHidDevice dev) = do
  let bufSize = 256  -- Arbitrary but reasonable size
  allocaArray0 bufSize $ \buffer -> do
    result <- sdlHidGetProductStringRaw dev buffer (fromIntegral bufSize)
    if result < 0
      then return Nothing
      else Just <$> peekCWString buffer

-- | Get The Serial Number String from a HID device.
foreign import ccall "SDL_hid_get_serial_number_string" sdlHidGetSerialNumberStringRaw :: 
  Ptr SDLHidDevice -> Ptr CWchar -> CSize -> IO CInt

sdlHidGetSerialNumberString :: SDLHidDevice -> IO (Maybe String)
sdlHidGetSerialNumberString (SDLHidDevice dev) = do
  let bufSize = 256  -- Arbitrary but reasonable size
  allocaArray0 bufSize $ \buffer -> do
    result <- sdlHidGetSerialNumberStringRaw dev buffer (fromIntegral bufSize)
    if result < 0
      then return Nothing
      else Just <$> peekCWString buffer

-- | Get a string from a HID device, based on its string index.
foreign import ccall "SDL_hid_get_indexed_string" sdlHidGetIndexedStringRaw :: 
  Ptr SDLHidDevice -> CInt -> Ptr CWchar -> CSize -> IO CInt

sdlHidGetIndexedString :: SDLHidDevice -> Int -> IO (Maybe String)
sdlHidGetIndexedString (SDLHidDevice dev) stringIndex = do
  let bufSize = 256  -- Arbitrary but reasonable size
  allocaArray0 bufSize $ \buffer -> do
    result <- sdlHidGetIndexedStringRaw dev (fromIntegral stringIndex) buffer (fromIntegral bufSize)
    if result < 0
      then return Nothing
      else Just <$> peekCWString buffer

-- | Get the device info from a HID device.
foreign import ccall "SDL_hid_get_device_info" sdlHidGetDeviceInfoRaw :: 
  Ptr SDLHidDevice -> IO (Ptr SDLHidDeviceInfo)

sdlHidGetDeviceInfo :: SDLHidDevice -> IO (Maybe SDLHidDeviceInfo)
sdlHidGetDeviceInfo (SDLHidDevice dev) = do
  deviceInfo <- sdlHidGetDeviceInfoRaw dev
  if deviceInfo == nullPtr
    then return Nothing
    else Just <$> peek deviceInfo

-- | Get a report descriptor from a HID device.
foreign import ccall "SDL_hid_get_report_descriptor" sdlHidGetReportDescriptorRaw :: 
  Ptr SDLHidDevice -> Ptr Word8 -> CSize -> IO CInt

sdlHidGetReportDescriptor :: SDLHidDevice -> IO (Maybe [Word8])
sdlHidGetReportDescriptor (SDLHidDevice dev) = do
  let bufSize = 4096  -- Recommended size from the API docs
  allocaArray bufSize $ \buffer -> do
    bytesRead <- sdlHidGetReportDescriptorRaw dev buffer (fromIntegral bufSize)
    if bytesRead <= 0
      then return Nothing
      else do
        result <- peekArray (fromIntegral bytesRead) buffer
        return $ Just result

-- | Start or stop a BLE scan on iOS and tvOS to pair Steam Controllers.
foreign import ccall "SDL_hid_ble_scan" sdlHidBleScan :: Bool -> IO ()
