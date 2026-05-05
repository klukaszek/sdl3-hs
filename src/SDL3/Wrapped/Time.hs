{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : SDL3.Wrapped.Time
-- Description : Wrapped SDL real-time clock and date/time routines
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Ergonomic wrappers over 'SDL3.Raw.Time'. These functions generalize over
-- 'MonadIO' while preserving the existing high-level API.

module SDL3.Wrapped.Time
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

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Storable (peek, poke)
import SDL3.Raw.Time
  ( SDLDateFormat(..)
  , SDLDateTime(..)
  , SDLTime
  , SDLTimeFormat(..)
  , pattern SDL_DATE_FORMAT_DDMMYYYY
  , pattern SDL_DATE_FORMAT_MMDDYYYY
  , pattern SDL_DATE_FORMAT_YYYYMMDD
  , pattern SDL_TIME_FORMAT_12HR
  , pattern SDL_TIME_FORMAT_24HR
  )
import qualified SDL3.Raw.Time as Raw

sdlGetDateTimeLocalePreferences :: MonadIO m => m (Maybe (SDLDateFormat, SDLTimeFormat))
sdlGetDateTimeLocalePreferences = liftIO $
  alloca $ \datePtr -> alloca $ \timePtr -> do
    success <- Raw.sdlGetDateTimeLocalePreferencesRaw datePtr timePtr
    if toBool success
      then do
        dateCInt <- peek datePtr
        timeCInt <- peek timePtr
        let date = toEnum (fromIntegral dateCInt) :: SDLDateFormat
        let time = toEnum (fromIntegral timeCInt) :: SDLTimeFormat
        return $ Just (date, time)
      else return Nothing

sdlGetCurrentTime :: MonadIO m => m (Maybe SDLTime)
sdlGetCurrentTime = liftIO $
  alloca $ \ticksPtr -> do
    success <- Raw.sdlGetCurrentTimeRaw ticksPtr
    if toBool success
      then Just <$> peek ticksPtr
      else return Nothing

sdlTimeToDateTime :: MonadIO m => SDLTime -> Bool -> m (Maybe SDLDateTime)
sdlTimeToDateTime ticks localTime = liftIO $
  alloca $ \dtPtr -> do
    success <- Raw.sdlTimeToDateTimeRaw ticks dtPtr (fromBool localTime)
    if toBool success
      then Just <$> peek dtPtr
      else return Nothing

sdlDateTimeToTime :: MonadIO m => SDLDateTime -> m (Maybe SDLTime)
sdlDateTimeToTime dt = liftIO $
  alloca $ \dtPtr -> alloca $ \ticksPtr -> do
    poke dtPtr dt
    success <- Raw.sdlDateTimeToTimeRaw dtPtr ticksPtr
    if toBool success
      then Just <$> peek ticksPtr
      else return Nothing

sdlTimeToWindows :: MonadIO m => SDLTime -> m (Word32, Word32)
sdlTimeToWindows ticks = liftIO $
  alloca $ \lowPtr -> alloca $ \highPtr -> do
    Raw.sdlTimeToWindowsRaw ticks lowPtr highPtr
    low <- peek lowPtr
    high <- peek highPtr
    return (low, high)

sdlTimeFromWindows :: MonadIO m => Word32 -> Word32 -> m SDLTime
sdlTimeFromWindows lowPart highPart = liftIO $ Raw.sdlTimeFromWindows lowPart highPart

sdlGetDaysInMonth :: MonadIO m => Int -> Int -> m Int
sdlGetDaysInMonth year month =
  liftIO $ fromIntegral <$> Raw.sdlGetDaysInMonthRaw (fromIntegral year) (fromIntegral month)

sdlGetDayOfYear :: MonadIO m => Int -> Int -> Int -> m Int
sdlGetDayOfYear year month day =
  liftIO $ fromIntegral <$> Raw.sdlGetDayOfYearRaw (fromIntegral year) (fromIntegral month) (fromIntegral day)

sdlGetDayOfWeek :: MonadIO m => Int -> Int -> Int -> m Int
sdlGetDayOfWeek year month day =
  liftIO $ fromIntegral <$> Raw.sdlGetDayOfWeekRaw (fromIntegral year) (fromIntegral month) (fromIntegral day)
