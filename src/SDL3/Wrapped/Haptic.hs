module SDL3.Wrapped.Haptic
  ( SDLHaptic
  , SDLHapticID
  , SDLHapticDirection(..)
  , SDLHapticConstant(..)
  , SDLHapticPeriodic(..)
  , SDLHapticCondition(..)
  , SDLHapticRamp(..)
  , SDLHapticLeftRight(..)
  , SDLHapticCustom(..)
  , SDLHapticEffect(..)
  , sdlHapticConstant
  , sdlHapticSine
  , sdlHapticSquare
  , sdlHapticTriangle
  , sdlHapticSawtoothUp
  , sdlHapticSawtoothDown
  , sdlHapticRamp
  , sdlHapticSpring
  , sdlHapticDamper
  , sdlHapticInertia
  , sdlHapticFriction
  , sdlHapticLeftRight
  , sdlHapticCustom
  , sdlHapticGain
  , sdlHapticAutocenter
  , sdlHapticStatus
  , sdlHapticPause
  , sdlHapticPolar
  , sdlHapticCartesian
  , sdlHapticSpherical
  , sdlHapticSteeringAxis
  , sdlHapticInfinity
  , sdlUnsafeFromRawHaptic
  , sdlUnsafeToRawHaptic
  , sdlGetHaptics
  , sdlGetHapticNameForID
  , sdlOpenHaptic
  , sdlGetHapticFromID
  , sdlGetHapticID
  , sdlGetHapticName
  , sdlIsMouseHaptic
  , sdlOpenHapticFromMouse
  , sdlIsJoystickHaptic
  , sdlOpenHapticFromJoystick
  , sdlCloseHaptic
  , sdlGetMaxHapticEffects
  , sdlGetMaxHapticEffectsPlaying
  , sdlGetHapticFeatures
  , sdlGetNumHapticAxes
  , sdlHapticEffectSupported
  , sdlCreateHapticEffect
  , sdlUpdateHapticEffect
  , sdlRunHapticEffect
  , sdlStopHapticEffect
  , sdlDestroyHapticEffect
  , sdlGetHapticEffectStatus
  , sdlSetHapticGain
  , sdlSetHapticAutocenter
  , sdlPauseHaptic
  , sdlResumeHaptic
  , sdlStopHapticEffects
  , sdlHapticRumbleSupported
  , sdlInitHapticRumble
  , sdlPlayHapticRumble
  , sdlStopHapticRumble
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)
import Foreign (Ptr, nullPtr, peek, peekArray, toBool)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CFloat(..), CInt(..))
import Foreign.Marshal.Alloc (alloca, free)
import SDL3.Joystick (SDLJoystick(..))
import SDL3.Raw.Haptic
  ( SDLHaptic
  , SDLHapticID
  , SDLHapticDirection(..)
  , SDLHapticConstant(..)
  , SDLHapticPeriodic(..)
  , SDLHapticCondition(..)
  , SDLHapticRamp(..)
  , SDLHapticLeftRight(..)
  , SDLHapticCustom(..)
  , SDLHapticEffect(..)
  , sdlHapticConstant
  , sdlHapticSine
  , sdlHapticSquare
  , sdlHapticTriangle
  , sdlHapticSawtoothUp
  , sdlHapticSawtoothDown
  , sdlHapticRamp
  , sdlHapticSpring
  , sdlHapticDamper
  , sdlHapticInertia
  , sdlHapticFriction
  , sdlHapticLeftRight
  , sdlHapticCustom
  , sdlHapticGain
  , sdlHapticAutocenter
  , sdlHapticStatus
  , sdlHapticPause
  , sdlHapticPolar
  , sdlHapticCartesian
  , sdlHapticSpherical
  , sdlHapticSteeringAxis
  , sdlHapticInfinity
  )
import qualified SDL3.Raw.Haptic as Raw

peekMaybeCString :: CString -> IO (Maybe String)
peekMaybeCString ptr
  | ptr == nullPtr = pure Nothing
  | otherwise = Just <$> peekCString ptr

sdlUnsafeFromRawHaptic :: Ptr Raw.SDLHaptic -> Maybe SDLHaptic
sdlUnsafeFromRawHaptic hapticPtr
  | hapticPtr == nullPtr = Nothing
  | otherwise = Just (Raw.SDLHaptic hapticPtr)

sdlUnsafeToRawHaptic :: SDLHaptic -> Ptr Raw.SDLHaptic
sdlUnsafeToRawHaptic (Raw.SDLHaptic hapticPtr) = hapticPtr

sdlGetHaptics :: MonadIO m => m [SDLHapticID]
sdlGetHaptics = liftIO $ alloca $ \countPtr -> do
  ptr <- Raw.sdlGetHapticsRaw countPtr
  count <- peek countPtr
  ids <- peekArray (fromIntegral count) ptr
  free ptr
  return ids

sdlGetHapticNameForID :: MonadIO m => SDLHapticID -> m (Maybe String)
sdlGetHapticNameForID hid = liftIO $ Raw.sdlGetHapticNameForIDRaw hid >>= peekMaybeCString

sdlOpenHaptic :: MonadIO m => SDLHapticID -> m (Maybe SDLHaptic)
sdlOpenHaptic hid = liftIO $ sdlUnsafeFromRawHaptic <$> Raw.sdlOpenHapticRaw hid

sdlGetHapticFromID :: MonadIO m => SDLHapticID -> m (Maybe SDLHaptic)
sdlGetHapticFromID hid = liftIO $ sdlUnsafeFromRawHaptic <$> Raw.sdlGetHapticFromIDRaw hid

sdlGetHapticID :: MonadIO m => SDLHaptic -> m SDLHapticID
sdlGetHapticID haptic = liftIO $ Raw.sdlGetHapticIDRaw (sdlUnsafeToRawHaptic haptic)

sdlGetHapticName :: MonadIO m => SDLHaptic -> m (Maybe String)
sdlGetHapticName haptic = liftIO $ Raw.sdlGetHapticNameRaw (sdlUnsafeToRawHaptic haptic) >>= peekMaybeCString

sdlIsMouseHaptic :: MonadIO m => m Bool
sdlIsMouseHaptic = liftIO $ toBool <$> Raw.sdlIsMouseHapticRaw

sdlOpenHapticFromMouse :: MonadIO m => m (Maybe SDLHaptic)
sdlOpenHapticFromMouse = liftIO $ sdlUnsafeFromRawHaptic <$> Raw.sdlOpenHapticFromMouseRaw

sdlIsJoystickHaptic :: MonadIO m => SDLJoystick -> m Bool
sdlIsJoystickHaptic (SDLJoystick joy) = liftIO $ toBool <$> Raw.sdlIsJoystickHapticRaw joy

sdlOpenHapticFromJoystick :: MonadIO m => SDLJoystick -> m (Maybe SDLHaptic)
sdlOpenHapticFromJoystick (SDLJoystick joy) =
  liftIO $ sdlUnsafeFromRawHaptic <$> Raw.sdlOpenHapticFromJoystickRaw joy

sdlCloseHaptic :: MonadIO m => SDLHaptic -> m ()
sdlCloseHaptic haptic = liftIO $ Raw.sdlCloseHapticRaw (sdlUnsafeToRawHaptic haptic)

sdlGetMaxHapticEffects :: MonadIO m => SDLHaptic -> m CInt
sdlGetMaxHapticEffects haptic = liftIO $ Raw.sdlGetMaxHapticEffectsRaw (sdlUnsafeToRawHaptic haptic)

sdlGetMaxHapticEffectsPlaying :: MonadIO m => SDLHaptic -> m CInt
sdlGetMaxHapticEffectsPlaying haptic =
  liftIO $ Raw.sdlGetMaxHapticEffectsPlayingRaw (sdlUnsafeToRawHaptic haptic)

sdlGetHapticFeatures :: MonadIO m => SDLHaptic -> m Word32
sdlGetHapticFeatures haptic = liftIO $ Raw.sdlGetHapticFeaturesRaw (sdlUnsafeToRawHaptic haptic)

sdlGetNumHapticAxes :: MonadIO m => SDLHaptic -> m CInt
sdlGetNumHapticAxes haptic = liftIO $ Raw.sdlGetNumHapticAxesRaw (sdlUnsafeToRawHaptic haptic)

sdlHapticEffectSupported :: MonadIO m => SDLHaptic -> Ptr SDLHapticEffect -> m Bool
sdlHapticEffectSupported haptic effect =
  liftIO $ toBool <$> Raw.sdlHapticEffectSupportedRaw (sdlUnsafeToRawHaptic haptic) effect

sdlCreateHapticEffect :: MonadIO m => SDLHaptic -> Ptr SDLHapticEffect -> m CInt
sdlCreateHapticEffect haptic effect =
  liftIO $ Raw.sdlCreateHapticEffectRaw (sdlUnsafeToRawHaptic haptic) effect

sdlUpdateHapticEffect :: MonadIO m => SDLHaptic -> CInt -> Ptr SDLHapticEffect -> m Bool
sdlUpdateHapticEffect haptic effectId effect =
  liftIO $ toBool <$> Raw.sdlUpdateHapticEffectRaw (sdlUnsafeToRawHaptic haptic) effectId effect

sdlRunHapticEffect :: MonadIO m => SDLHaptic -> CInt -> Word32 -> m Bool
sdlRunHapticEffect haptic effectId iterations =
  liftIO $ toBool <$> Raw.sdlRunHapticEffectRaw (sdlUnsafeToRawHaptic haptic) effectId iterations

sdlStopHapticEffect :: MonadIO m => SDLHaptic -> CInt -> m Bool
sdlStopHapticEffect haptic effectId =
  liftIO $ toBool <$> Raw.sdlStopHapticEffectRaw (sdlUnsafeToRawHaptic haptic) effectId

sdlDestroyHapticEffect :: MonadIO m => SDLHaptic -> CInt -> m ()
sdlDestroyHapticEffect haptic effectId =
  liftIO $ Raw.sdlDestroyHapticEffectRaw (sdlUnsafeToRawHaptic haptic) effectId

sdlGetHapticEffectStatus :: MonadIO m => SDLHaptic -> CInt -> m Bool
sdlGetHapticEffectStatus haptic effectId =
  liftIO $ toBool <$> Raw.sdlGetHapticEffectStatusRaw (sdlUnsafeToRawHaptic haptic) effectId

sdlSetHapticGain :: MonadIO m => SDLHaptic -> CInt -> m Bool
sdlSetHapticGain haptic gain =
  liftIO $ toBool <$> Raw.sdlSetHapticGainRaw (sdlUnsafeToRawHaptic haptic) gain

sdlSetHapticAutocenter :: MonadIO m => SDLHaptic -> CInt -> m Bool
sdlSetHapticAutocenter haptic autocenter =
  liftIO $ toBool <$> Raw.sdlSetHapticAutocenterRaw (sdlUnsafeToRawHaptic haptic) autocenter

sdlPauseHaptic :: MonadIO m => SDLHaptic -> m Bool
sdlPauseHaptic haptic = liftIO $ toBool <$> Raw.sdlPauseHapticRaw (sdlUnsafeToRawHaptic haptic)

sdlResumeHaptic :: MonadIO m => SDLHaptic -> m Bool
sdlResumeHaptic haptic = liftIO $ toBool <$> Raw.sdlResumeHapticRaw (sdlUnsafeToRawHaptic haptic)

sdlStopHapticEffects :: MonadIO m => SDLHaptic -> m Bool
sdlStopHapticEffects haptic =
  liftIO $ toBool <$> Raw.sdlStopHapticEffectsRaw (sdlUnsafeToRawHaptic haptic)

sdlHapticRumbleSupported :: MonadIO m => SDLHaptic -> m Bool
sdlHapticRumbleSupported haptic =
  liftIO $ toBool <$> Raw.sdlHapticRumbleSupportedRaw (sdlUnsafeToRawHaptic haptic)

sdlInitHapticRumble :: MonadIO m => SDLHaptic -> m Bool
sdlInitHapticRumble haptic =
  liftIO $ toBool <$> Raw.sdlInitHapticRumbleRaw (sdlUnsafeToRawHaptic haptic)

sdlPlayHapticRumble :: MonadIO m => SDLHaptic -> CFloat -> Word32 -> m Bool
sdlPlayHapticRumble haptic strength rumLen =
  liftIO $ toBool <$> Raw.sdlPlayHapticRumbleRaw (sdlUnsafeToRawHaptic haptic) strength rumLen

sdlStopHapticRumble :: MonadIO m => SDLHaptic -> m Bool
sdlStopHapticRumble haptic =
  liftIO $ toBool <$> Raw.sdlStopHapticRumbleRaw (sdlUnsafeToRawHaptic haptic)
