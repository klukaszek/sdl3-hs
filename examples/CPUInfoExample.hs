import SDL3

-- Really simple test.

main :: IO ()
main = do
  cores <- sdlGetNumLogicalCPUCores
  hasSSE <- sdlHasSSE
  ram <- sdlGetSystemRAM
  sdlLog $ "System has " ++ show cores ++ " logical cores"
  sdlLog $ "SSE support: " ++ show hasSSE
  sdlLog $ "RAM: " ++ show ram ++ " MiB"
