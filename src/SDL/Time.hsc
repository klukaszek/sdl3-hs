{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : SDL.Time
Description : SDL real-time clock and date/time routines
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

SDL provides real-time clock and date/time management functionality.

This module offers tools for working with time in two primary forms: 'SDLTime', representing nanoseconds
since a specific epoch, and 'SDLDateTime', which breaks time into human-readable components like years,
months, and days. It includes functions for converting between these formats, querying the current time,
and performing date-related calculations.

These utilities are useful for applications needing precise time tracking or calendar-based operations,
such as scheduling, logging, or time-based game mechanics.
-}

module SDL.Time
  ( -- * Types
    SDLTime
  , SDLDateTime(..)
  , SDLDateFormat(..)
  , SDLTimeFormat(..)

    -- * Locale Preference Functions
  , sdlGetDateTimeLocalePreferences

    -- * Time Query and Conversion Functions
  , sdlGetCurrentTime
  , sdlTimeToDateTime
  , sdlDateTimeToTime
  , sdlTimeToWindows
  , sdlTimeFromWindows

    -- * Date Calculation Functions
  , sdlGetDaysInMonth
  , sdlGetDayOfYear
  , sdlGetDayOfWeek
  ) where

#include <SDL3/SDL_time.h>

import Foreign.C.Types
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable(..))
import Data.Word (Word64, Word32)
import SDL.Stdinc (SDLTime)
import Control.Monad (when)

-- | A structure holding a calendar date and time broken down into its components (SDL_DateTime).
data SDLDateTime = SDLDateTime
  { dtYear        :: Int    -- ^ Year
  , dtMonth       :: Int    -- ^ Month [1-12]
  , dtDay         :: Int    -- ^ Day of the month [1-31]
  , dtHour        :: Int    -- ^ Hour [0-23]
  , dtMinute      :: Int    -- ^ Minute [0-59]
  , dtSecond      :: Int    -- ^ Seconds [0-60]
  , dtNanosecond  :: Int    -- ^ Nanoseconds [0-999999999]
  , dtDayOfWeek   :: Int    -- ^ Day of the week [0-6] (0 being Sunday)
  , dtUtcOffset   :: Int    -- ^ Seconds east of UTC
  } deriving (Eq, Show)

instance Storable SDLDateTime where
  sizeOf _ = #size SDL_DateTime
  alignment _ = #alignment SDL_DateTime
  peek ptr = SDLDateTime
    <$> (#peek SDL_DateTime, year) ptr
    <*> (#peek SDL_DateTime, month) ptr
    <*> (#peek SDL_DateTime, day) ptr
    <*> (#peek SDL_DateTime, hour) ptr
    <*> (#peek SDL_DateTime, minute) ptr
    <*> (#peek SDL_DateTime, second) ptr
    <*> (#peek SDL_DateTime, nanosecond) ptr
    <*> (#peek SDL_DateTime, day_of_week) ptr
    <*> (#peek SDL_DateTime, utc_offset) ptr
  poke ptr (SDLDateTime year month day hour minute second nanosecond day_of_week utc_offset) = do
    (#poke SDL_DateTime, year) ptr year
    (#poke SDL_DateTime, month) ptr month
    (#poke SDL_DateTime, day) ptr day
    (#poke SDL_DateTime, hour) ptr hour
    (#poke SDL_DateTime, minute) ptr minute
    (#poke SDL_DateTime, second) ptr second
    (#poke SDL_DateTime, nanosecond) ptr nanosecond
    (#poke SDL_DateTime, day_of_week) ptr day_of_week
    (#poke SDL_DateTime, utc_offset) ptr utc_offset

-- | The preferred date format of the current system locale (SDL_DateFormat).
data SDLDateFormat
  = SDLDateFormatYYYYMMDD  -- ^ Year/Month/Day
  | SDLDateFormatDDMMYYYY  -- ^ Day/Month/Year
  | SDLDateFormatMMDDYYYY  -- ^ Month/Day/Year
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | The preferred time format of the current system locale (SDL_TimeFormat).
data SDLTimeFormat
  = SDLTimeFormat24HR  -- ^ 24 hour time
  | SDLTimeFormat12HR  -- ^ 12 hour time
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Convert C enum to Haskell SDLDateFormat.
fromCSDLDateFormat :: CInt -> SDLDateFormat
fromCSDLDateFormat 0 = SDLDateFormatYYYYMMDD
fromCSDLDateFormat 1 = SDLDateFormatDDMMYYYY
fromCSDLDateFormat 2 = SDLDateFormatMMDDYYYY
fromCSDLDateFormat _ = SDLDateFormatYYYYMMDD  -- Default fallback

-- | Convert C enum to Haskell SDLTimeFormat.
fromCSDLTimeFormat :: CInt -> SDLTimeFormat
fromCSDLTimeFormat 0 = SDLTimeFormat24HR
fromCSDLTimeFormat 1 = SDLTimeFormat12HR
fromCSDLTimeFormat _ = SDLTimeFormat24HR  -- Default fallback

-- | Gets the current preferred date and time format for the system locale (SDL_GetDateTimeLocalePreferences).
foreign import ccall "SDL_GetDateTimeLocalePreferences"
  sdlGetDateTimeLocalePreferencesRaw :: Ptr CInt -> Ptr CInt -> IO Bool

-- | Haskell wrapper for SDL_GetDateTimeLocalePreferences.
sdlGetDateTimeLocalePreferences :: IO (Maybe (SDLDateFormat, SDLTimeFormat))
sdlGetDateTimeLocalePreferences = alloca $ \datePtr -> alloca $ \timePtr -> do
  success <- sdlGetDateTimeLocalePreferencesRaw datePtr timePtr
  if success
    then do
      date <- fromCSDLDateFormat <$> peek datePtr
      time <- fromCSDLTimeFormat <$> peek timePtr
      return $ Just (date, time)
    else return Nothing

-- | Gets the current value of the system realtime clock (SDL_GetCurrentTime).
foreign import ccall "SDL_GetCurrentTime"
  sdlGetCurrentTimeRaw :: Ptr SDLTime -> IO Bool

-- | Haskell wrapper for SDL_GetCurrentTime.
sdlGetCurrentTime :: IO (Maybe SDLTime)
sdlGetCurrentTime = alloca $ \ticksPtr -> do
  success <- sdlGetCurrentTimeRaw ticksPtr
  if success
    then Just <$> peek ticksPtr
    else return Nothing

-- | Converts an SDL_Time to a calendar time (SDL_TimeToDateTime).
foreign import ccall "SDL_TimeToDateTime"
  sdlTimeToDateTimeRaw :: SDLTime -> Ptr SDLDateTime -> Bool -> IO Bool

-- | Haskell wrapper for SDL_TimeToDateTime.
sdlTimeToDateTime :: SDLTime -> Bool -> IO (Maybe SDLDateTime)
sdlTimeToDateTime ticks localTime = alloca $ \dtPtr -> do
  success <- sdlTimeToDateTimeRaw ticks dtPtr localTime
  if success
    then Just <$> peek dtPtr
    else return Nothing

-- | Converts a calendar time to an SDL_Time (SDL_DateTimeToTime).
foreign import ccall "SDL_DateTimeToTime"
  sdlDateTimeToTimeRaw :: Ptr SDLDateTime -> Ptr SDLTime -> IO Bool

-- | Haskell wrapper for SDL_DateTimeToTime.
sdlDateTimeToTime :: SDLDateTime -> IO (Maybe SDLTime)
sdlDateTimeToTime dt = alloca $ \dtPtr -> alloca $ \ticksPtr -> do
  poke dtPtr dt
  success <- sdlDateTimeToTimeRaw dtPtr ticksPtr
  if success
    then Just <$> peek ticksPtr
    else return Nothing

-- | Converts an SDL time into a Windows FILETIME (SDL_TimeToWindows).
foreign import ccall "SDL_TimeToWindows"
  sdlTimeToWindowsRaw :: SDLTime -> Ptr Word32 -> Ptr Word32 -> IO ()

-- | Haskell wrapper for SDL_TimeToWindows.
sdlTimeToWindows :: SDLTime -> IO (Word32, Word32)
sdlTimeToWindows ticks = alloca $ \lowPtr -> alloca $ \highPtr -> do
  sdlTimeToWindowsRaw ticks lowPtr highPtr
  low <- peek lowPtr
  high <- peek highPtr
  return (low, high)

-- | Converts a Windows FILETIME to an SDL time (SDL_TimeFromWindows).
foreign import ccall "SDL_TimeFromWindows"
  sdlTimeFromWindows :: Word32 -> Word32 -> IO SDLTime

-- | Get the number of days in a month for a given year (SDL_GetDaysInMonth).
foreign import ccall "SDL_GetDaysInMonth"
  sdlGetDaysInMonth :: Int -> Int -> IO Int

-- | Get the day of year for a calendar date (SDL_GetDayOfYear).
foreign import ccall "SDL_GetDayOfYear"
  sdlGetDayOfYear :: Int -> Int -> Int -> IO Int

-- | Get the day of week for a calendar date (SDL_GetDayOfWeek).
foreign import ccall "SDL_GetDayOfWeek"
  sdlGetDayOfWeek :: Int -> Int -> Int -> IO Int
