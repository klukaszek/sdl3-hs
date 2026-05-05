-- SDL/Raw/Time.hsc
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : SDL3.Raw.Time
-- Description : Raw SDL real-time clock and date/time routines
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Low-level FFI bindings for SDL time routines. This module exposes the raw
-- pointer-oriented API; prefer 'SDL3.Wrapped.Time' or 'SDL3.Time' unless you
-- specifically need direct FFI access.

#include <SDL3/SDL_time.h>

module SDL3.Raw.Time
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

    -- * Raw FFI Bindings
  , sdlGetDateTimeLocalePreferencesRaw
  , sdlGetCurrentTimeRaw
  , sdlTimeToDateTimeRaw
  , sdlDateTimeToTimeRaw
  , sdlTimeToWindowsRaw
  , sdlTimeFromWindows
  , sdlGetDaysInMonthRaw
  , sdlGetDayOfYearRaw
  , sdlGetDayOfWeekRaw
  ) where

import Data.Word (Word32)
import Foreign.C.Types
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import SDL3.Raw.Stdinc (SDLTime)

-- | A structure holding a calendar date and time broken down into its components (SDL_DateTime).
data SDLDateTime = SDLDateTime
  { dtYear        :: CInt
  , dtMonth       :: CInt
  , dtDay         :: CInt
  , dtHour        :: CInt
  , dtMinute      :: CInt
  , dtSecond      :: CInt
  , dtNanosecond  :: CInt
  , dtDayOfWeek   :: CInt
  , dtUtcOffset   :: CInt
  } deriving (Eq, Show)

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
    return SDLDateTime {..}
  poke ptr SDLDateTime{..} = do
    #{poke SDL_DateTime, year} ptr dtYear
    #{poke SDL_DateTime, month} ptr dtMonth
    #{poke SDL_DateTime, day} ptr dtDay
    #{poke SDL_DateTime, hour} ptr dtHour
    #{poke SDL_DateTime, minute} ptr dtMinute
    #{poke SDL_DateTime, second} ptr dtSecond
    #{poke SDL_DateTime, nanosecond} ptr dtNanosecond
    #{poke SDL_DateTime, day_of_week} ptr dtDayOfWeek
    #{poke SDL_DateTime, utc_offset} ptr dtUtcOffset

newtype SDLDateFormat = SDLDateFormat CInt
  deriving newtype (Show, Eq, Ord, Storable, Enum)

pattern SDL_DATE_FORMAT_YYYYMMDD = SDLDateFormat #{const SDL_DATE_FORMAT_YYYYMMDD}
pattern SDL_DATE_FORMAT_DDMMYYYY = SDLDateFormat #{const SDL_DATE_FORMAT_DDMMYYYY}
pattern SDL_DATE_FORMAT_MMDDYYYY = SDLDateFormat #{const SDL_DATE_FORMAT_MMDDYYYY}

newtype SDLTimeFormat = SDLTimeFormat CInt
  deriving newtype (Show, Eq, Ord, Storable, Enum)

pattern SDL_TIME_FORMAT_24HR = SDLTimeFormat #{const SDL_TIME_FORMAT_24HR}
pattern SDL_TIME_FORMAT_12HR = SDLTimeFormat #{const SDL_TIME_FORMAT_12HR}

foreign import ccall unsafe "SDL_GetDateTimeLocalePreferences"
  sdlGetDateTimeLocalePreferencesRaw :: Ptr CInt -> Ptr CInt -> IO CBool

foreign import ccall unsafe "SDL_GetCurrentTime"
  sdlGetCurrentTimeRaw :: Ptr SDLTime -> IO CBool

foreign import ccall unsafe "SDL_TimeToDateTime"
  sdlTimeToDateTimeRaw :: SDLTime -> Ptr SDLDateTime -> CBool -> IO CBool

foreign import ccall unsafe "SDL_DateTimeToTime"
  sdlDateTimeToTimeRaw :: Ptr SDLDateTime -> Ptr SDLTime -> IO CBool

foreign import ccall unsafe "SDL_TimeToWindows"
  sdlTimeToWindowsRaw :: SDLTime -> Ptr Word32 -> Ptr Word32 -> IO ()

foreign import ccall unsafe "SDL_TimeFromWindows"
  sdlTimeFromWindows :: Word32 -> Word32 -> IO SDLTime

foreign import ccall unsafe "SDL_GetDaysInMonth"
  sdlGetDaysInMonthRaw :: CInt -> CInt -> IO CInt

foreign import ccall unsafe "SDL_GetDayOfYear"
  sdlGetDayOfYearRaw :: CInt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "SDL_GetDayOfWeek"
  sdlGetDayOfWeekRaw :: CInt -> CInt -> CInt -> IO CInt
