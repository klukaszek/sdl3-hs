import SDL.CPUInfo

-- Really simple test.

main :: IO ()
main = do
  cores <- sdlGetNumLogicalCPUCores
  hasSSE <- sdlHasSSE
  ram <- sdlGetSystemRAM
  putStrLn $ "System has " ++ show cores ++ " logical cores"
  putStrLn $ "SSE support: " ++ show hasSSE
  putStrLn $ "RAM: " ++ show ram ++ " MiB"
