import SDL
import Foreign.Ptr (nullPtr)

main :: IO ()
main = do
  devices <- sdlGetTouchDevices
  sdlLog $ "Touch Devices: " ++ show devices
  case devices of
    (touchID:_) -> do
      name <- sdlGetTouchDeviceName touchID
      sdlLog $ "Device Name: " ++ show name
      devType <- sdlGetTouchDeviceType touchID
      sdlLog $ "Device Type: " ++ show devType
      fingers <- sdlGetTouchFingers touchID
      sdlLog $ "Active Fingers: " ++ show fingers
    _ -> sdlLog "No touch devices found."
