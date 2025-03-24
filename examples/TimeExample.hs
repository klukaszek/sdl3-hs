import SDL.Time

main :: IO ()
main = do
  -- Get current time
  maybeTicks <- sdlGetCurrentTime
  case maybeTicks of
    Just ticks -> do
      putStrLn $ "Current ticks: " ++ show ticks
      maybeDt <- sdlTimeToDateTime ticks True
      putStrLn $ "Local DateTime: " ++ show maybeDt
    Nothing -> putStrLn "Failed to get current time"

  -- Get locale preferences
  maybePrefs <- sdlGetDateTimeLocalePreferences
  putStrLn $ "Locale Preferences: " ++ show maybePrefs

  -- Example date calculation
  days <- sdlGetDaysInMonth 2025 3
  putStrLn $ "Days in March 2025: " ++ show days
