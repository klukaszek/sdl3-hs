{-# LANGUAGE ForeignFunctionInterface #-}

#include <SDL3/SDL_haptic.h>

module SDL3.Raw.Haptic
  ( SDLHaptic(..)
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
  , sdlGetHapticsRaw
  , sdlGetHapticNameForIDRaw
  , sdlOpenHapticRaw
  , sdlGetHapticFromIDRaw
  , sdlGetHapticIDRaw
  , sdlGetHapticNameRaw
  , sdlIsMouseHapticRaw
  , sdlOpenHapticFromMouseRaw
  , sdlIsJoystickHapticRaw
  , sdlOpenHapticFromJoystickRaw
  , sdlCloseHapticRaw
  , sdlGetMaxHapticEffectsRaw
  , sdlGetMaxHapticEffectsPlayingRaw
  , sdlGetHapticFeaturesRaw
  , sdlGetNumHapticAxesRaw
  , sdlHapticEffectSupportedRaw
  , sdlCreateHapticEffectRaw
  , sdlUpdateHapticEffectRaw
  , sdlRunHapticEffectRaw
  , sdlStopHapticEffectRaw
  , sdlDestroyHapticEffectRaw
  , sdlGetHapticEffectStatusRaw
  , sdlSetHapticGainRaw
  , sdlSetHapticAutocenterRaw
  , sdlPauseHapticRaw
  , sdlResumeHapticRaw
  , sdlStopHapticEffectsRaw
  , sdlHapticRumbleSupportedRaw
  , sdlInitHapticRumbleRaw
  , sdlPlayHapticRumbleRaw
  , sdlStopHapticRumbleRaw
  ) where

import Data.Int (Int16, Int32)
import Data.Word (Word8, Word16, Word32)
import Foreign (Ptr, castPtr, nullPtr, peekArray, plusPtr, toBool)
import Foreign.C.String (CString)
import Foreign.C.Types (CBool(..), CFloat(..), CInt(..))
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (newArray, peekArray, pokeArray)
import Foreign.Storable (Storable(..))
import SDL3.Raw.Joystick (SDLJoystick(..))

newtype SDLHaptic = SDLHaptic {unSDLHaptic :: Ptr SDLHaptic}
  deriving (Eq, Show)

type SDLHapticID = Word32

data SDLHapticDirection = SDLHapticDirection
  { directionType :: Word8
  , directionDir :: [Int32]
  } deriving (Eq, Show)

instance Storable SDLHapticDirection where
  sizeOf _ = #size SDL_HapticDirection
  alignment _ = #alignment SDL_HapticDirection
  peek ptr = SDLHapticDirection
    <$> (#peek SDL_HapticDirection, type) ptr
    <*> (peekArray 3 $ (#ptr SDL_HapticDirection, dir) ptr)
  poke ptr (SDLHapticDirection typ dir) = do
    (#poke SDL_HapticDirection, type) ptr typ
    pokeArray (castPtr $ (#ptr SDL_HapticDirection, dir) ptr) (take 3 dir)

data SDLHapticConstant = SDLHapticConstant
  { constantType :: Word16
  , constantDirection :: SDLHapticDirection
  , constantLength :: Word32
  , constantDelay :: Word16
  , constantButton :: Word16
  , constantInterval :: Word16
  , constantLevel :: Int16
  , constantAttackLength :: Word16
  , constantAttackLevel :: Word16
  , constantFadeLength :: Word16
  , constantFadeLevel :: Word16
  } deriving (Eq, Show)

instance Storable SDLHapticConstant where
  sizeOf _ = #size SDL_HapticConstant
  alignment _ = #alignment SDL_HapticConstant
  peek ptr = SDLHapticConstant
    <$> (#peek SDL_HapticConstant, type) ptr
    <*> peek (castPtr ptr)
    <*> (#peek SDL_HapticConstant, length) ptr
    <*> (#peek SDL_HapticConstant, delay) ptr
    <*> (#peek SDL_HapticConstant, button) ptr
    <*> (#peek SDL_HapticConstant, interval) ptr
    <*> (#peek SDL_HapticConstant, level) ptr
    <*> (#peek SDL_HapticConstant, attack_length) ptr
    <*> (#peek SDL_HapticConstant, attack_level) ptr
    <*> (#peek SDL_HapticConstant, fade_length) ptr
    <*> (#peek SDL_HapticConstant, fade_level) ptr
  poke ptr (SDLHapticConstant typ dir len dly btn int lvl atkLen atkLvl fadeLen fadeLvl) = do
    (#poke SDL_HapticConstant, type) ptr typ
    poke (castPtr ptr) dir
    (#poke SDL_HapticConstant, length) ptr len
    (#poke SDL_HapticConstant, delay) ptr dly
    (#poke SDL_HapticConstant, button) ptr btn
    (#poke SDL_HapticConstant, interval) ptr int
    (#poke SDL_HapticConstant, level) ptr lvl
    (#poke SDL_HapticConstant, attack_length) ptr atkLen
    (#poke SDL_HapticConstant, attack_level) ptr atkLvl
    (#poke SDL_HapticConstant, fade_length) ptr fadeLen
    (#poke SDL_HapticConstant, fade_level) ptr fadeLvl

data SDLHapticPeriodic = SDLHapticPeriodic
  { periodicType :: Word16
  , periodicDirection :: SDLHapticDirection
  , periodicLength :: Word32
  , periodicDelay :: Word16
  , periodicButton :: Word16
  , periodicInterval :: Word16
  , periodicPeriod :: Word16
  , periodicMagnitude :: Int16
  , periodicOffset :: Int16
  , periodicPhase :: Word16
  , periodicAttackLength :: Word16
  , periodicAttackLevel :: Word16
  , periodicFadeLength :: Word16
  , periodicFadeLevel :: Word16
  } deriving (Eq, Show)

instance Storable SDLHapticPeriodic where
  sizeOf _ = #size SDL_HapticPeriodic
  alignment _ = #alignment SDL_HapticPeriodic
  peek ptr = SDLHapticPeriodic
    <$> (#peek SDL_HapticPeriodic, type) ptr
    <*> peek (castPtr ptr)
    <*> (#peek SDL_HapticPeriodic, length) ptr
    <*> (#peek SDL_HapticPeriodic, delay) ptr
    <*> (#peek SDL_HapticPeriodic, button) ptr
    <*> (#peek SDL_HapticPeriodic, interval) ptr
    <*> (#peek SDL_HapticPeriodic, period) ptr
    <*> (#peek SDL_HapticPeriodic, magnitude) ptr
    <*> (#peek SDL_HapticPeriodic, offset) ptr
    <*> (#peek SDL_HapticPeriodic, phase) ptr
    <*> (#peek SDL_HapticPeriodic, attack_length) ptr
    <*> (#peek SDL_HapticPeriodic, attack_level) ptr
    <*> (#peek SDL_HapticPeriodic, fade_length) ptr
    <*> (#peek SDL_HapticPeriodic, fade_level) ptr
  poke ptr (SDLHapticPeriodic typ dir len dly btn int per mag off pha atkLen atkLvl fadeLen fadeLvl) = do
    (#poke SDL_HapticPeriodic, type) ptr typ
    poke (castPtr ptr) dir
    (#poke SDL_HapticPeriodic, length) ptr len
    (#poke SDL_HapticPeriodic, delay) ptr dly
    (#poke SDL_HapticPeriodic, button) ptr btn
    (#poke SDL_HapticPeriodic, interval) ptr int
    (#poke SDL_HapticPeriodic, period) ptr per
    (#poke SDL_HapticPeriodic, magnitude) ptr mag
    (#poke SDL_HapticPeriodic, offset) ptr off
    (#poke SDL_HapticPeriodic, phase) ptr pha
    (#poke SDL_HapticPeriodic, attack_length) ptr atkLen
    (#poke SDL_HapticPeriodic, attack_level) ptr atkLvl
    (#poke SDL_HapticPeriodic, fade_length) ptr fadeLen
    (#poke SDL_HapticPeriodic, fade_level) ptr fadeLvl

data SDLHapticCondition = SDLHapticCondition
  { conditionType :: Word16
  , conditionDirection :: SDLHapticDirection
  , conditionLength :: Word32
  , conditionDelay :: Word16
  , conditionButton :: Word16
  , conditionInterval :: Word16
  , conditionRightSat :: [Word16]
  , conditionLeftSat :: [Word16]
  , conditionRightCoeff :: [Int16]
  , conditionLeftCoeff :: [Int16]
  , conditionDeadband :: [Word16]
  , conditionCenter :: [Int16]
  } deriving (Eq, Show)

instance Storable SDLHapticCondition where
  sizeOf _ = #size SDL_HapticCondition
  alignment _ = #alignment SDL_HapticCondition
  peek ptr = SDLHapticCondition
    <$> (#peek SDL_HapticCondition, type) ptr
    <*> peek (castPtr ptr)
    <*> (#peek SDL_HapticCondition, length) ptr
    <*> (#peek SDL_HapticCondition, delay) ptr
    <*> (#peek SDL_HapticCondition, button) ptr
    <*> (#peek SDL_HapticCondition, interval) ptr
    <*> (peekArray 3 $ (#ptr SDL_HapticCondition, right_sat) ptr)
    <*> (peekArray 3 $ (#ptr SDL_HapticCondition, left_sat) ptr)
    <*> (peekArray 3 $ (#ptr SDL_HapticCondition, right_coeff) ptr)
    <*> (peekArray 3 $ (#ptr SDL_HapticCondition, left_coeff) ptr)
    <*> (peekArray 3 $ (#ptr SDL_HapticCondition, deadband) ptr)
    <*> (peekArray 3 $ (#ptr SDL_HapticCondition, center) ptr)
  poke ptr (SDLHapticCondition typ dir len dly btn int rsat lsat rcoeff lcoeff db cent) = do
    (#poke SDL_HapticCondition, type) ptr typ
    poke (castPtr ptr) dir
    (#poke SDL_HapticCondition, length) ptr len
    (#poke SDL_HapticCondition, delay) ptr dly
    (#poke SDL_HapticCondition, button) ptr btn
    (#poke SDL_HapticCondition, interval) ptr int
    pokeArray (castPtr $ (#ptr SDL_HapticCondition, right_sat) ptr) (take 3 rsat)
    pokeArray (castPtr $ (#ptr SDL_HapticCondition, left_sat) ptr) (take 3 lsat)
    pokeArray (castPtr $ (#ptr SDL_HapticCondition, right_coeff) ptr) (take 3 rcoeff)
    pokeArray (castPtr $ (#ptr SDL_HapticCondition, left_coeff) ptr) (take 3 lcoeff)
    pokeArray (castPtr $ (#ptr SDL_HapticCondition, deadband) ptr) (take 3 db)
    pokeArray (castPtr $ (#ptr SDL_HapticCondition, center) ptr) (take 3 cent)

data SDLHapticRamp = SDLHapticRamp
  { rampType :: Word16
  , rampDirection :: SDLHapticDirection
  , rampLength :: Word32
  , rampDelay :: Word16
  , rampButton :: Word16
  , rampInterval :: Word16
  , rampStart :: Int16
  , rampEnd :: Int16
  , rampAttackLength :: Word16
  , rampAttackLevel :: Word16
  , rampFadeLength :: Word16
  , rampFadeLevel :: Word16
  } deriving (Eq, Show)

instance Storable SDLHapticRamp where
  sizeOf _ = #size SDL_HapticRamp
  alignment _ = #alignment SDL_HapticRamp
  peek ptr = SDLHapticRamp
    <$> (#peek SDL_HapticRamp, type) ptr
    <*> peek (castPtr ptr)
    <*> (#peek SDL_HapticRamp, length) ptr
    <*> (#peek SDL_HapticRamp, delay) ptr
    <*> (#peek SDL_HapticRamp, button) ptr
    <*> (#peek SDL_HapticRamp, interval) ptr
    <*> (#peek SDL_HapticRamp, start) ptr
    <*> (#peek SDL_HapticRamp, end) ptr
    <*> (#peek SDL_HapticRamp, attack_length) ptr
    <*> (#peek SDL_HapticRamp, attack_level) ptr
    <*> (#peek SDL_HapticRamp, fade_length) ptr
    <*> (#peek SDL_HapticRamp, fade_level) ptr
  poke ptr (SDLHapticRamp typ dir len dly btn int start end atkLen atkLvl fadeLen fadeLvl) = do
    (#poke SDL_HapticRamp, type) ptr typ
    poke (castPtr ptr) dir
    (#poke SDL_HapticRamp, length) ptr len
    (#poke SDL_HapticRamp, delay) ptr dly
    (#poke SDL_HapticRamp, button) ptr btn
    (#poke SDL_HapticRamp, interval) ptr int
    (#poke SDL_HapticRamp, start) ptr start
    (#poke SDL_HapticRamp, end) ptr end
    (#poke SDL_HapticRamp, attack_length) ptr atkLen
    (#poke SDL_HapticRamp, attack_level) ptr atkLvl
    (#poke SDL_HapticRamp, fade_length) ptr fadeLen
    (#poke SDL_HapticRamp, fade_level) ptr fadeLvl

data SDLHapticLeftRight = SDLHapticLeftRight
  { leftRightType :: Word16
  , leftRightLength :: Word32
  , leftRightLargeMag :: Word16
  , leftRightSmallMag :: Word16
  } deriving (Eq, Show)

instance Storable SDLHapticLeftRight where
  sizeOf _ = #size SDL_HapticLeftRight
  alignment _ = #alignment SDL_HapticLeftRight
  peek ptr = SDLHapticLeftRight
    <$> (#peek SDL_HapticLeftRight, type) ptr
    <*> (#peek SDL_HapticLeftRight, length) ptr
    <*> (#peek SDL_HapticLeftRight, large_magnitude) ptr
    <*> (#peek SDL_HapticLeftRight, small_magnitude) ptr
  poke ptr (SDLHapticLeftRight typ len large small) = do
    (#poke SDL_HapticLeftRight, type) ptr typ
    (#poke SDL_HapticLeftRight, length) ptr len
    (#poke SDL_HapticLeftRight, large_magnitude) ptr large
    (#poke SDL_HapticLeftRight, small_magnitude) ptr small

data SDLHapticCustom = SDLHapticCustom
  { customType :: Word16
  , customDirection :: SDLHapticDirection
  , customLength :: Word32
  , customDelay :: Word16
  , customButton :: Word16
  , customInterval :: Word16
  , customChannels :: Word8
  , customPeriod :: Word16
  , customSamples :: Word16
  , customData :: Ptr Word16
  , customAttackLength :: Word16
  , customAttackLevel :: Word16
  , customFadeLength :: Word16
  , customFadeLevel :: Word16
  } deriving (Eq, Show)

instance Storable SDLHapticCustom where
  sizeOf _ = #size SDL_HapticCustom
  alignment _ = #alignment SDL_HapticCustom
  peek ptr = SDLHapticCustom
    <$> (#peek SDL_HapticCustom, type) ptr
    <*> peek (castPtr ptr)
    <*> (#peek SDL_HapticCustom, length) ptr
    <*> (#peek SDL_HapticCustom, delay) ptr
    <*> (#peek SDL_HapticCustom, button) ptr
    <*> (#peek SDL_HapticCustom, interval) ptr
    <*> (#peek SDL_HapticCustom, channels) ptr
    <*> (#peek SDL_HapticCustom, period) ptr
    <*> (#peek SDL_HapticCustom, samples) ptr
    <*> (#peek SDL_HapticCustom, data) ptr
    <*> (#peek SDL_HapticCustom, attack_length) ptr
    <*> (#peek SDL_HapticCustom, attack_level) ptr
    <*> (#peek SDL_HapticCustom, fade_length) ptr
    <*> (#peek SDL_HapticCustom, fade_level) ptr
  poke ptr (SDLHapticCustom typ dir len dly btn int chan per samp dat atkLen atkLvl fadeLen fadeLvl) = do
    (#poke SDL_HapticCustom, type) ptr typ
    poke (castPtr ptr) dir
    (#poke SDL_HapticCustom, length) ptr len
    (#poke SDL_HapticCustom, delay) ptr dly
    (#poke SDL_HapticCustom, button) ptr btn
    (#poke SDL_HapticCustom, interval) ptr int
    (#poke SDL_HapticCustom, channels) ptr chan
    (#poke SDL_HapticCustom, period) ptr per
    (#poke SDL_HapticCustom, samples) ptr samp
    (#poke SDL_HapticCustom, data) ptr dat
    (#poke SDL_HapticCustom, attack_length) ptr atkLen
    (#poke SDL_HapticCustom, attack_level) ptr atkLvl
    (#poke SDL_HapticCustom, fade_length) ptr fadeLen
    (#poke SDL_HapticCustom, fade_level) ptr fadeLvl

data SDLHapticEffect
  = HapticConstant SDLHapticConstant
  | HapticPeriodic SDLHapticPeriodic
  | HapticCondition SDLHapticCondition
  | HapticRamp SDLHapticRamp
  | HapticLeftRight SDLHapticLeftRight
  | HapticCustom SDLHapticCustom
  deriving (Eq, Show)

instance Storable SDLHapticEffect where
  sizeOf _ = #size SDL_HapticEffect
  alignment _ = #alignment SDL_HapticEffect
  peek ptr = do
    typ <- (#peek SDL_HapticEffect, type) ptr
    case typ of
      t | t == sdlHapticConstant -> HapticConstant <$> peek (castPtr ptr)
      t | t == sdlHapticSine || t == sdlHapticSquare || t == sdlHapticTriangle || t == sdlHapticSawtoothUp || t == sdlHapticSawtoothDown -> HapticPeriodic <$> peek (castPtr ptr)
      t | t == sdlHapticSpring || t == sdlHapticDamper || t == sdlHapticInertia || t == sdlHapticFriction -> HapticCondition <$> peek (castPtr ptr)
      t | t == sdlHapticRamp -> HapticRamp <$> peek (castPtr ptr)
      t | t == sdlHapticLeftRight -> HapticLeftRight <$> peek (castPtr ptr)
      t | t == sdlHapticCustom -> HapticCustom <$> peek (castPtr ptr)
      _ -> error $ "Unknown haptic effect type: " ++ show typ
  poke ptr (HapticConstant eff) = poke (castPtr ptr) eff
  poke ptr (HapticPeriodic eff) = poke (castPtr ptr) eff
  poke ptr (HapticCondition eff) = poke (castPtr ptr) eff
  poke ptr (HapticRamp eff) = poke (castPtr ptr) eff
  poke ptr (HapticLeftRight eff) = poke (castPtr ptr) eff
  poke ptr (HapticCustom eff) = poke (castPtr ptr) eff

sdlHapticConstant, sdlHapticSine, sdlHapticSquare, sdlHapticTriangle, sdlHapticSawtoothUp, sdlHapticSawtoothDown, sdlHapticRamp, sdlHapticSpring, sdlHapticDamper, sdlHapticInertia, sdlHapticFriction, sdlHapticLeftRight, sdlHapticCustom :: Word16
sdlHapticConstant = #const SDL_HAPTIC_CONSTANT
sdlHapticSine = #const SDL_HAPTIC_SINE
sdlHapticSquare = #const SDL_HAPTIC_SQUARE
sdlHapticTriangle = #const SDL_HAPTIC_TRIANGLE
sdlHapticSawtoothUp = #const SDL_HAPTIC_SAWTOOTHUP
sdlHapticSawtoothDown = #const SDL_HAPTIC_SAWTOOTHDOWN
sdlHapticRamp = #const SDL_HAPTIC_RAMP
sdlHapticSpring = #const SDL_HAPTIC_SPRING
sdlHapticDamper = #const SDL_HAPTIC_DAMPER
sdlHapticInertia = #const SDL_HAPTIC_INERTIA
sdlHapticFriction = #const SDL_HAPTIC_FRICTION
sdlHapticLeftRight = #const SDL_HAPTIC_LEFTRIGHT
sdlHapticCustom = #const SDL_HAPTIC_CUSTOM

sdlHapticGain, sdlHapticAutocenter, sdlHapticStatus, sdlHapticPause, sdlHapticInfinity :: Word32
sdlHapticGain = #const SDL_HAPTIC_GAIN
sdlHapticAutocenter = #const SDL_HAPTIC_AUTOCENTER
sdlHapticStatus = #const SDL_HAPTIC_STATUS
sdlHapticPause = #const SDL_HAPTIC_PAUSE
sdlHapticInfinity = #const SDL_HAPTIC_INFINITY

sdlHapticPolar, sdlHapticCartesian, sdlHapticSpherical, sdlHapticSteeringAxis :: Word8
sdlHapticPolar = #const SDL_HAPTIC_POLAR
sdlHapticCartesian = #const SDL_HAPTIC_CARTESIAN
sdlHapticSpherical = #const SDL_HAPTIC_SPHERICAL
sdlHapticSteeringAxis = #const SDL_HAPTIC_STEERING_AXIS

foreign import ccall "SDL_GetHaptics"
  sdlGetHapticsRaw :: Ptr CInt -> IO (Ptr Word32)

foreign import ccall "SDL_GetHapticNameForID"
  sdlGetHapticNameForIDRaw :: SDLHapticID -> IO CString

foreign import ccall "SDL_OpenHaptic"
  sdlOpenHapticRaw :: Word32 -> IO (Ptr SDLHaptic)

foreign import ccall "SDL_GetHapticFromID"
  sdlGetHapticFromIDRaw :: SDLHapticID -> IO (Ptr SDLHaptic)

foreign import ccall "SDL_GetHapticID"
  sdlGetHapticIDRaw :: Ptr SDLHaptic -> IO SDLHapticID

foreign import ccall "SDL_GetHapticName"
  sdlGetHapticNameRaw :: Ptr SDLHaptic -> IO CString

foreign import ccall "SDL_IsMouseHaptic"
  sdlIsMouseHapticRaw :: IO CBool

foreign import ccall "SDL_OpenHapticFromMouse"
  sdlOpenHapticFromMouseRaw :: IO (Ptr SDLHaptic)

foreign import ccall "SDL_IsJoystickHaptic"
  sdlIsJoystickHapticRaw :: Ptr SDLJoystick -> IO CBool

foreign import ccall "SDL_OpenHapticFromJoystick"
  sdlOpenHapticFromJoystickRaw :: Ptr SDLJoystick -> IO (Ptr SDLHaptic)

foreign import ccall "SDL_CloseHaptic"
  sdlCloseHapticRaw :: Ptr SDLHaptic -> IO ()

foreign import ccall "SDL_GetMaxHapticEffects"
  sdlGetMaxHapticEffectsRaw :: Ptr SDLHaptic -> IO CInt

foreign import ccall "SDL_GetMaxHapticEffectsPlaying"
  sdlGetMaxHapticEffectsPlayingRaw :: Ptr SDLHaptic -> IO CInt

foreign import ccall "SDL_GetHapticFeatures"
  sdlGetHapticFeaturesRaw :: Ptr SDLHaptic -> IO Word32

foreign import ccall "SDL_GetNumHapticAxes"
  sdlGetNumHapticAxesRaw :: Ptr SDLHaptic -> IO CInt

foreign import ccall "SDL_HapticEffectSupported"
  sdlHapticEffectSupportedRaw :: Ptr SDLHaptic -> Ptr SDLHapticEffect -> IO CBool

foreign import ccall "SDL_CreateHapticEffect"
  sdlCreateHapticEffectRaw :: Ptr SDLHaptic -> Ptr SDLHapticEffect -> IO CInt

foreign import ccall "SDL_UpdateHapticEffect"
  sdlUpdateHapticEffectRaw :: Ptr SDLHaptic -> CInt -> Ptr SDLHapticEffect -> IO CBool

foreign import ccall "SDL_RunHapticEffect"
  sdlRunHapticEffectRaw :: Ptr SDLHaptic -> CInt -> Word32 -> IO CBool

foreign import ccall "SDL_StopHapticEffect"
  sdlStopHapticEffectRaw :: Ptr SDLHaptic -> CInt -> IO CBool

foreign import ccall "SDL_DestroyHapticEffect"
  sdlDestroyHapticEffectRaw :: Ptr SDLHaptic -> CInt -> IO ()

foreign import ccall "SDL_GetHapticEffectStatus"
  sdlGetHapticEffectStatusRaw :: Ptr SDLHaptic -> CInt -> IO CBool

foreign import ccall "SDL_SetHapticGain"
  sdlSetHapticGainRaw :: Ptr SDLHaptic -> CInt -> IO CBool

foreign import ccall "SDL_SetHapticAutocenter"
  sdlSetHapticAutocenterRaw :: Ptr SDLHaptic -> CInt -> IO CBool

foreign import ccall "SDL_PauseHaptic"
  sdlPauseHapticRaw :: Ptr SDLHaptic -> IO CBool

foreign import ccall "SDL_ResumeHaptic"
  sdlResumeHapticRaw :: Ptr SDLHaptic -> IO CBool

foreign import ccall "SDL_StopHapticEffects"
  sdlStopHapticEffectsRaw :: Ptr SDLHaptic -> IO CBool

foreign import ccall "SDL_HapticRumbleSupported"
  sdlHapticRumbleSupportedRaw :: Ptr SDLHaptic -> IO CBool

foreign import ccall "SDL_InitHapticRumble"
  sdlInitHapticRumbleRaw :: Ptr SDLHaptic -> IO CBool

foreign import ccall "SDL_PlayHapticRumble"
  sdlPlayHapticRumbleRaw :: Ptr SDLHaptic -> CFloat -> Word32 -> IO CBool

foreign import ccall "SDL_StopHapticRumble"
  sdlStopHapticRumbleRaw :: Ptr SDLHaptic -> IO CBool
