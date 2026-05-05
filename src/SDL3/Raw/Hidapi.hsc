{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}

#include <SDL3/SDL_hidapi.h>

module SDL3.Raw.Hidapi
  ( SDLHidDevice(..)
  , SDLHidBusType(..)
  , SDLHidDeviceInfo(..)
  , pattern SDL_PROP_HIDAPI_LIBUSB_DEVICE_HANDLE_POINTER
  , sdlHidGetPropertiesRaw
  , sdlHidInitRaw
  , sdlHidExitRaw
  , sdlHidDeviceChangeCountRaw
  , sdlHidEnumerateRaw
  , sdlHidFreeEnumerationRaw
  , sdlHidOpenRaw
  , sdlHidOpenPathRaw
  , sdlHidWriteRaw
  , sdlHidReadTimeoutRaw
  , sdlHidReadRaw
  , sdlHidSetNonblockingRaw
  , sdlHidSendFeatureReportRaw
  , sdlHidGetFeatureReportRaw
  , sdlHidGetInputReportRaw
  , sdlHidCloseRaw
  , sdlHidGetManufacturerStringRaw
  , sdlHidGetProductStringRaw
  , sdlHidGetSerialNumberStringRaw
  , sdlHidGetIndexedStringRaw
  , sdlHidGetDeviceInfoRaw
  , sdlHidGetReportDescriptorRaw
  , sdlHidBleScanRaw
  ) where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import SDL3.Raw.Properties (SDLPropertiesID)

pattern SDL_PROP_HIDAPI_LIBUSB_DEVICE_HANDLE_POINTER = #{const_str SDL_PROP_HIDAPI_LIBUSB_DEVICE_HANDLE_POINTER}

newtype SDLHidDevice = SDLHidDevice {unSDLHidDevice :: Ptr SDLHidDevice}
  deriving (Show, Eq)

data SDLHidBusType
  = SDL_HID_API_BUS_UNKNOWN
  | SDL_HID_API_BUS_USB
  | SDL_HID_API_BUS_BLUETOOTH
  | SDL_HID_API_BUS_I2C
  | SDL_HID_API_BUS_SPI
  deriving (Show, Eq, Enum)

data SDLHidDeviceInfo = SDLHidDeviceInfo
  { hidPath :: String
  , hidVendorId :: Word16
  , hidProductId :: Word16
  , hidSerialNumber :: Maybe String
  , hidReleaseNumber :: Word16
  , hidManufacturer :: Maybe String
  , hidProduct :: Maybe String
  , hidUsagePage :: Word16
  , hidUsage :: Word16
  , hidInterfaceNumber :: Int
  , hidInterfaceClass :: Int
  , hidInterfaceSubclass :: Int
  , hidInterfaceProtocol :: Int
  , hidBusType :: SDLHidBusType
  , hidNext :: Ptr SDLHidDeviceInfo
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
    serialNumber <- if serialNumberPtr == nullPtr then return Nothing else Just <$> peekCWString serialNumberPtr
    releaseNumber <- #{peek SDL_hid_device_info, release_number} ptr
    manufacturerPtr <- #{peek SDL_hid_device_info, manufacturer_string} ptr
    manufacturer <- if manufacturerPtr == nullPtr then return Nothing else Just <$> peekCWString manufacturerPtr
    productPtr <- #{peek SDL_hid_device_info, product_string} ptr
    productVal <- if productPtr == nullPtr then return Nothing else Just <$> peekCWString productPtr
    usagePage <- #{peek SDL_hid_device_info, usage_page} ptr
    usage <- #{peek SDL_hid_device_info, usage} ptr
    interfaceNumber <- #{peek SDL_hid_device_info, interface_number} ptr
    interfaceClass <- #{peek SDL_hid_device_info, interface_class} ptr
    interfaceSubclass <- #{peek SDL_hid_device_info, interface_subclass} ptr
    interfaceProtocol <- #{peek SDL_hid_device_info, interface_protocol} ptr
    busTypeInt <- #{peek SDL_hid_device_info, bus_type} ptr :: IO CInt
    let busType = toEnum (fromIntegral busTypeInt)
    next <- #{peek SDL_hid_device_info, next} ptr
    return $
      SDLHidDeviceInfo
        path vendorId productId serialNumber releaseNumber manufacturer productVal
        usagePage usage interfaceNumber interfaceClass interfaceSubclass
        interfaceProtocol busType next

  poke _ _ = error "Poking SDL_hid_device_info not implemented"

foreign import ccall "SDL_hid_get_properties"
  sdlHidGetPropertiesRaw :: SDLHidDevice -> IO SDLPropertiesID

foreign import ccall "SDL_hid_init" sdlHidInitRaw :: IO CInt
foreign import ccall "SDL_hid_exit" sdlHidExitRaw :: IO CInt
foreign import ccall "SDL_hid_device_change_count" sdlHidDeviceChangeCountRaw :: IO Word32
foreign import ccall "SDL_hid_enumerate" sdlHidEnumerateRaw :: Word16 -> Word16 -> IO (Ptr SDLHidDeviceInfo)
foreign import ccall "SDL_hid_free_enumeration" sdlHidFreeEnumerationRaw :: Ptr SDLHidDeviceInfo -> IO ()
foreign import ccall "SDL_hid_open" sdlHidOpenRaw :: Word16 -> Word16 -> Ptr CWchar -> IO (Ptr SDLHidDevice)
foreign import ccall "SDL_hid_open_path" sdlHidOpenPathRaw :: CString -> IO (Ptr SDLHidDevice)
foreign import ccall "SDL_hid_write" sdlHidWriteRaw :: Ptr SDLHidDevice -> Ptr Word8 -> CSize -> IO CInt
foreign import ccall "SDL_hid_read_timeout" sdlHidReadTimeoutRaw :: Ptr SDLHidDevice -> Ptr Word8 -> CSize -> CInt -> IO CInt
foreign import ccall "SDL_hid_read" sdlHidReadRaw :: Ptr SDLHidDevice -> Ptr Word8 -> CSize -> IO CInt
foreign import ccall "SDL_hid_set_nonblocking" sdlHidSetNonblockingRaw :: Ptr SDLHidDevice -> CInt -> IO CInt
foreign import ccall "SDL_hid_send_feature_report" sdlHidSendFeatureReportRaw :: Ptr SDLHidDevice -> Ptr Word8 -> CSize -> IO CInt
foreign import ccall "SDL_hid_get_feature_report" sdlHidGetFeatureReportRaw :: Ptr SDLHidDevice -> Ptr Word8 -> CSize -> IO CInt
foreign import ccall "SDL_hid_get_input_report" sdlHidGetInputReportRaw :: Ptr SDLHidDevice -> Ptr Word8 -> CSize -> IO CInt
foreign import ccall "SDL_hid_close" sdlHidCloseRaw :: Ptr SDLHidDevice -> IO CInt
foreign import ccall "SDL_hid_get_manufacturer_string" sdlHidGetManufacturerStringRaw :: Ptr SDLHidDevice -> Ptr CWchar -> CSize -> IO CInt
foreign import ccall "SDL_hid_get_product_string" sdlHidGetProductStringRaw :: Ptr SDLHidDevice -> Ptr CWchar -> CSize -> IO CInt
foreign import ccall "SDL_hid_get_serial_number_string" sdlHidGetSerialNumberStringRaw :: Ptr SDLHidDevice -> Ptr CWchar -> CSize -> IO CInt
foreign import ccall "SDL_hid_get_indexed_string" sdlHidGetIndexedStringRaw :: Ptr SDLHidDevice -> CInt -> Ptr CWchar -> CSize -> IO CInt
foreign import ccall "SDL_hid_get_device_info" sdlHidGetDeviceInfoRaw :: Ptr SDLHidDevice -> IO (Ptr SDLHidDeviceInfo)
foreign import ccall "SDL_hid_get_report_descriptor" sdlHidGetReportDescriptorRaw :: Ptr SDLHidDevice -> Ptr Word8 -> CSize -> IO CInt
foreign import ccall "SDL_hid_ble_scan" sdlHidBleScanRaw :: Bool -> IO ()
