-- SDL/Time.hsc
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-} -- For SDLDateTime Storable

-- |
-- Module      : SDL.Time
-- Description : SDL real-time clock and date/time routines
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- SDL provides real-time clock and date/time management functionality.
--
-- This module offers tools for working with time in two primary forms: 'SDLTime', representing nanoseconds
-- since a specific epoch, and 'SDLDateTime', which breaks time into human-readable components like years,
-- months, and days. It includes functions for converting between these formats, querying the current time,
-- and performing date-related calculations.
--
-- These utilities are useful for applications needing precise time tracking or calendar-based operations,
-- such as scheduling, logging, or time-based game mechanics.

#include <SDL3/SDL_time.h>

module SDL3.Time
  ( -- * Types
    SDLTime
  , SDLDateTime(..)
  , SDLDateFormat(..)
  , pattern SDL_DATE_FORMAT_YYYYMMDD
  , pattern SDL_DATE_FORMAT_DDMMYYYY
  , pattern SDL_DATE_FORMAT_MMDDYYYY
  , SDLTimeFormat(..)
  , pattern SDL_TIME_FORMAT_24HR
  , pattern SDL_TIME_FORMAT_12HR

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

import Foreign.C.Types
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable(..), peek, poke)
import Data.Word (Word32)
import SDL3.Stdinc (SDLTime)
import Foreign.Marshal.Utils (toBool, fromBool)

-- | A structure holding a calendar date and time broken down into its components (SDL_DateTime).
data SDLDateTime = SDLDateTime
  { dtYear        :: CInt    -- ^ Year (using CInt for direct mapping)
  , dtMonth       :: CInt    -- ^ Month [1-12]
  , dtDay         :: CInt    -- ^ Day of the month [1-31]
  , dtHour        :: CInt    -- ^ Hour [0-23]
  , dtMinute      :: CInt    -- ^ Minute [0-59]
  , dtSecond      :: CInt    -- ^ Seconds [0-60]
  , dtNanosecond  :: CInt    -- ^ Nanoseconds [0-999999999]
  , dtDayOfWeek   :: CInt    -- ^ Day of the week [0-6] (0 being Sunday)
  , dtUtcOffset   :: CInt    -- ^ Seconds east of UTC
  } deriving (Eq, Show)

-- Using RecordWildCards for Storable instance
instance Storable SDLDateTime where
  sizeOf _ = #{size SDL_DateTime}
  alignment _ = #{alignment SDL_DateTime}
  peek ptr = do
    dtYear        <- #{peek SDL_DateTime, year} ptr
    dtMonth       <- #{peek SDL_DateTime, month} ptr
    dtDay         <- #{peek SDL_DateTime, day} ptr
    dtHour        <- #{peek SDL_DateTime, hour} ptr
    dtMinute      <- #{peek SDL_DateTime, minute} ptr
    dtSecond      <- #{peek SDL_DateTime, second} ptr
    dtNanosecond  <- #{peek SDL_DateTime, nanosecond} ptr
    dtDayOfWeek   <- #{peek SDL_DateTime, day_of_week} ptr
    dtUtcOffset   <- #{peek SDL_DateTime, utc_offset} ptr
    return SDLDateTime {..} -- Uses RecordWildCards extension
  poke ptr SDLDateTime{..} = do -- Uses RecordWildCards extension
    #{poke SDL_DateTime, year} ptr dtYear
    #{poke SDL_DateTime, month} ptr dtMonth
    #{poke SDL_DateTime, day} ptr dtDay
    #{poke SDL_DateTime, hour} ptr dtHour
    #{poke SDL_DateTime, minute} ptr dtMinute
    #{poke SDL_DateTime, second} ptr dtSecond
    #{poke SDL_DateTime, nanosecond} ptr dtNanosecond
    #{poke SDL_DateTime, day_of_week} ptr dtDayOfWeek
    #{poke SDL_DateTime, utc_offset} ptr dtUtcOffset

-- | The preferred date format of the current system locale (SDL_DateFormat).
newtype SDLDateFormat = SDLDateFormat CInt
  deriving newtype (Show, Eq, Ord, Storable, Enum)

pattern SDL_DATE_FORMAT_YYYYMMDD :: SDLDateFormat
pattern SDL_DATE_FORMAT_YYYYMMDD = SDLDateFormat #{const SDL_DATE_FORMAT_YYYYMMDD}
pattern SDL_DATE_FORMAT_DDMMYYYY :: SDLDateFormat
pattern SDL_DATE_FORMAT_DDMMYYYY = SDLDateFormat #{const SDL_DATE_FORMAT_DDMMYYYY}
pattern SDL_DATE_FORMAT_MMDDYYYY :: SDLDateFormat
pattern SDL_DATE_FORMAT_MMDDYYYY = SDLDateFormat #{const SDL_DATE_FORMAT_MMDDYYYY}

-- | The preferred time format of the current system locale (SDL_TimeFormat).
newtype SDLTimeFormat = SDLTimeFormat CInt
  deriving newtype (Show, Eq, Ord, Storable, Enum)

pattern SDL_TIME_FORMAT_24HR :: SDLTimeFormat
pattern SDL_TIME_FORMAT_24HR = SDLTimeFormat #{const SDL_TIME_FORMAT_24HR}
pattern SDL_TIME_FORMAT_12HR :: SDLTimeFormat
pattern SDL_TIME_FORMAT_12HR = SDLTimeFormat #{const SDL_TIME_FORMAT_12HR}

-- | Gets the current preferred date and time format for the system locale (SDL_GetDateTimeLocalePreferences).
foreign import ccall unsafe "SDL_GetDateTimeLocalePreferences"
  c_sdlGetDateTimeLocalePreferences :: Ptr CInt -> Ptr CInt -> IO CBool

-- | Haskell wrapper for SDL_GetDateTimeLocalePreferences.
sdlGetDateTimeLocalePreferences :: IO (Maybe (SDLDateFormat, SDLTimeFormat))
sdlGetDateTimeLocalePreferences = alloca $ \datePtr -> alloca $ \timePtr -> do
  success <- c_sdlGetDateTimeLocalePreferences datePtr timePtr
  if toBool success
    then do
      dateCInt <- peek datePtr
      timeCInt <- peek timePtr
      -- Use derived Enum instance's toEnum to convert CInt back to the newtype
      let date = toEnum (fromIntegral dateCInt) :: SDLDateFormat
      let time = toEnum (fromIntegral timeCInt) :: SDLTimeFormat
      return $ Just (date, time)
    else return Nothing

-- | Gets the current value of the system realtime clock (SDL_GetCurrentTime).
foreign import ccall unsafe "SDL_GetCurrentTime"
  c_sdlGetCurrentTime :: Ptr SDLTime -> IO CBool

-- | Haskell wrapper for SDL_GetCurrentTime.
sdlGetCurrentTime :: IO (Maybe SDLTime)
sdlGetCurrentTime = alloca $ \ticksPtr -> do
  success <- c_sdlGetCurrentTime ticksPtr
  if toBool success
    then Just <$> peek ticksPtr
    else return Nothing

-- | Converts an SDL_Time to a calendar time (SDL_TimeToDateTime).
foreign import ccall unsafe "SDL_TimeToDateTime"
  c_sdlTimeToDateTime :: SDLTime -> Ptr SDLDateTime -> CBool -> IO CBool

-- | Haskell wrapper for SDL_TimeToDateTime.
sdlTimeToDateTime :: SDLTime -> Bool -> IO (Maybe SDLDateTime)
sdlTimeToDateTime ticks localTime = alloca $ \dtPtr -> do
  success <- c_sdlTimeToDateTime ticks dtPtr (fromBool localTime)
  if toBool success
    then Just <$> peek dtPtr
    else return Nothing

-- | Converts a calendar time to an SDL_Time (SDL_DateTimeToTime).
foreign import ccall unsafe "SDL_DateTimeToTime"
  c_sdlDateTimeToTime :: Ptr SDLDateTime -> Ptr SDLTime -> IO CBool

-- | Haskell wrapper for SDL_DateTimeToTime.
sdlDateTimeToTime :: SDLDateTime -> IO (Maybe SDLTime)
sdlDateTimeToTime dt = alloca $ \dtPtr -> alloca $ \ticksPtr -> do
  poke dtPtr dt
  success <- c_sdlDateTimeToTime dtPtr ticksPtr
  if toBool success
    then Just <$> peek ticksPtr
    else return Nothing

-- | Converts an SDL time into a Windows FILETIME (SDL_TimeToWindows).
-- Note: Windows FILETIME is a 64-bit value representing 100-nanosecond intervals
-- since January 1, 1601 (UTC). SDL returns it as two 32-bit parts.
foreign import ccall unsafe "SDL_TimeToWindows"
  c_sdlTimeToWindows :: SDLTime -> Ptr Word32 -> Ptr Word32 -> IO ()

-- | Haskell wrapper for SDL_TimeToWindows. Returns (lowPart, highPart).
sdlTimeToWindows :: SDLTime -> IO (Word32, Word32)
sdlTimeToWindows ticks = alloca $ \lowPtr -> alloca $ \highPtr -> do
  c_sdlTimeToWindows ticks lowPtr highPtr
  low <- peek lowPtr
  high <- peek highPtr
  return (low, high)

-- | Converts a Windows FILETIME to an SDL time (SDL_TimeFromWindows).
foreign import ccall unsafe "SDL_TimeFromWindows"
  sdlTimeFromWindows :: Word32 -> Word32 -> IO SDLTime -- No wrapper needed, direct FFI call

-- | Get the number of days in a month for a given year (SDL_GetDaysInMonth).
foreign import ccall unsafe "SDL_GetDaysInMonth"
  c_sdlGetDaysInMonth :: CInt -> CInt -> IO CInt

-- | Haskell wrapper for SDL_GetDaysInMonth, using Int for convenience.
sdlGetDaysInMonth :: Int -> Int -> IO Int
sdlGetDaysInMonth year month = fromIntegral <$> c_sdlGetDaysInMonth (fromIntegral year) (fromIntegral month)

-- | Get the day of year for a calendar date (SDL_GetDayOfYear).
foreign import ccall unsafe "SDL_GetDayOfYear"
  c_sdlGetDayOfYear :: CInt -> CInt -> CInt -> IO CInt

-- | Haskell wrapper for SDL_GetDayOfYear, using Int for convenience.
sdlGetDayOfYear :: Int -> Int -> Int -> IO Int
sdlGetDayOfYear year month day = fromIntegral <$> c_sdlGetDayOfYear (fromIntegral year) (fromIntegral month) (fromIntegral day)

-- | Get the day of week for a calendar date (SDL_GetDayOfWeek).
foreign import ccall unsafe "SDL_GetDayOfWeek"
  c_sdlGetDayOfWeek :: CInt -> CInt -> CInt -> IO CInt

-- | Haskell wrapper for SDL_GetDayOfWeek, using Int for convenience. Returns [0-6] where 0 is Sunday.
sdlGetDayOfWeek :: Int -> Int -> Int -> IO Int
sdlGetDayOfWeek year month day = fromIntegral <$> c_sdlGetDayOfWeek (fromIntegral year) (fromIntegral month) (fromIntegral day)
