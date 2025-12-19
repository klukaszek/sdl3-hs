import SDL3

main :: IO ()
main = do
  -- Get current time
  maybeTicks <- sdlGetCurrentTime
  case maybeTicks of
    Just ticks -> do
      sdlLog $ "Current ticks: " ++ show ticks
      maybeDt <- sdlTimeToDateTime ticks True
      sdlLog $ "Local DateTime: " ++ show maybeDt
    Nothing -> sdlLog "Failed to get current time"

  -- Get locale preferences
  maybePrefs <- sdlGetDateTimeLocalePreferences
  sdlLog $ "Locale Preferences: " ++ show maybePrefs

  -- Example date calculation
  days <- sdlGetDaysInMonth 2025 3
  sdlLog $ "Days in March 2025: " ++ show days
