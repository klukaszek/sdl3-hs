import SDL.Touch
import Foreign.Ptr (nullPtr)

main :: IO ()
main = do
  devices <- sdlGetTouchDevices
  putStrLn $ "Touch Devices: " ++ show devices
  case devices of
    (touchID:_) -> do
      name <- sdlGetTouchDeviceName touchID
      putStrLn $ "Device Name: " ++ show name
      devType <- sdlGetTouchDeviceType touchID
      putStrLn $ "Device Type: " ++ show devType
      fingers <- sdlGetTouchFingers touchID
      putStrLn $ "Active Fingers: " ++ show fingers
    _ -> putStrLn "No touch devices found."
