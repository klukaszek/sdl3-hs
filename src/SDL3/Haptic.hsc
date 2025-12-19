{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : SDL.Haptic
Description : SDL haptic (force feedback) management
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

SDL haptic support provides access to force feedback devices. This module
offers functions to manage haptic devices, create and run effects, and control
rumble features.

Basic usage:
- Initialize the subsystem (INIT_HAPTIC)
- Open a haptic device using 'sdlOpenHaptic' or 'sdlOpenHapticFromJoystick'
- Create effects with 'sdlCreateHapticEffect'
- Run effects with 'sdlRunHapticEffect'
- Close with 'sdlCloseHaptic'
-}

module SDL3.Haptic
  ( -- * Types
    SDLHaptic(..)
  , SDLHapticID()
  , SDLHapticDirection(..)
  , SDLHapticConstant(..)
  , SDLHapticPeriodic(..)
  , SDLHapticCondition(..)
  , SDLHapticRamp(..)
  , SDLHapticLeftRight(..)
  , SDLHapticCustom(..)
  , SDLHapticEffect(..)

    -- * Constants
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

    -- * Haptic Functions
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

#include <SDL3/SDL_haptic.h>

import Foreign (Ptr, nullPtr, castPtr, peekArray, pokeArray, plusPtr, toBool)
import Foreign.C.Types (CBool(..), CInt(..), CFloat(..))
import Foreign.C.String (CString)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (alloca, free)
import Data.Word (Word32, Word16, Word8)
import Data.Int (Int32, Int16)
import SDL3.Joystick (SDLJoystick(..))

-- | Opaque type representing a haptic device
newtype SDLHaptic = SDLHaptic { unSDLHaptic :: Ptr SDLHaptic }
  deriving (Eq, Show)

-- | Unique ID for a haptic device
type SDLHapticID = Word32

-- | Haptic direction encoding
data SDLHapticDirection = SDLHapticDirection
  { directionType :: Word8
  , directionDir  :: [Int32]  -- 3 elements
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

-- | Constant effect
data SDLHapticConstant = SDLHapticConstant
  { constantType         :: Word16
  , constantDirection    :: SDLHapticDirection
  , constantLength       :: Word32
  , constantDelay        :: Word16
  , constantButton       :: Word16
  , constantInterval     :: Word16
  , constantLevel        :: Int16
  , constantAttackLength :: Word16
  , constantAttackLevel  :: Word16
  , constantFadeLength   :: Word16
  , constantFadeLevel    :: Word16
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

-- | Periodic effect
data SDLHapticPeriodic = SDLHapticPeriodic
  { periodicType         :: Word16
  , periodicDirection    :: SDLHapticDirection
  , periodicLength       :: Word32
  , periodicDelay        :: Word16
  , periodicButton       :: Word16
  , periodicInterval     :: Word16
  , periodicPeriod       :: Word16
  , periodicMagnitude    :: Int16
  , periodicOffset       :: Int16
  , periodicPhase        :: Word16
  , periodicAttackLength :: Word16
  , periodicAttackLevel  :: Word16
  , periodicFadeLength   :: Word16
  , periodicFadeLevel    :: Word16
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
    <*> (#peek SDL_HapticPeriodic, fade_length) ptr  -- Use <*> here
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

-- | Condition effect
data SDLHapticCondition = SDLHapticCondition
  { conditionType         :: Word16
  , conditionDirection    :: SDLHapticDirection
  , conditionLength       :: Word32
  , conditionDelay        :: Word16
  , conditionButton       :: Word16
  , conditionInterval     :: Word16
  , conditionRightSat     :: [Word16]  -- 3 elements
  , conditionLeftSat      :: [Word16]  -- 3 elements
  , conditionRightCoeff   :: [Int16]   -- 3 elements
  , conditionLeftCoeff    :: [Int16]   -- 3 elements
  , conditionDeadband     :: [Word16]  -- 3 elements
  , conditionCenter       :: [Int16]   -- 3 elements
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

-- | Ramp effect
data SDLHapticRamp = SDLHapticRamp
  { rampType         :: Word16
  , rampDirection    :: SDLHapticDirection
  , rampLength       :: Word32
  , rampDelay        :: Word16
  , rampButton       :: Word16
  , rampInterval     :: Word16
  , rampStart        :: Int16
  , rampEnd          :: Int16
  , rampAttackLength :: Word16
  , rampAttackLevel  :: Word16
  , rampFadeLength   :: Word16
  , rampFadeLevel    :: Word16
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

-- | Left/Right effect
data SDLHapticLeftRight = SDLHapticLeftRight
  { leftRightType         :: Word16
  , leftRightLength       :: Word32
  , leftRightLargeMag     :: Word16
  , leftRightSmallMag     :: Word16
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

-- | Custom effect
data SDLHapticCustom = SDLHapticCustom
  { customType         :: Word16
  , customDirection    :: SDLHapticDirection
  , customLength       :: Word32
  , customDelay        :: Word16
  , customButton       :: Word16
  , customInterval     :: Word16
  , customChannels     :: Word8
  , customPeriod       :: Word16
  , customSamples      :: Word16
  , customData         :: Ptr Word16
  , customAttackLength :: Word16
  , customAttackLevel  :: Word16
  , customFadeLength   :: Word16
  , customFadeLevel    :: Word16
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

-- | Generic haptic effect union
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

-- | Constants
sdlHapticConstant :: Word16
sdlHapticConstant = #const SDL_HAPTIC_CONSTANT

sdlHapticSine :: Word16
sdlHapticSine = #const SDL_HAPTIC_SINE

sdlHapticSquare :: Word16
sdlHapticSquare = #const SDL_HAPTIC_SQUARE

sdlHapticTriangle :: Word16
sdlHapticTriangle = #const SDL_HAPTIC_TRIANGLE

sdlHapticSawtoothUp :: Word16
sdlHapticSawtoothUp = #const SDL_HAPTIC_SAWTOOTHUP

sdlHapticSawtoothDown :: Word16
sdlHapticSawtoothDown = #const SDL_HAPTIC_SAWTOOTHDOWN

sdlHapticRamp :: Word16
sdlHapticRamp = #const SDL_HAPTIC_RAMP

sdlHapticSpring :: Word16
sdlHapticSpring = #const SDL_HAPTIC_SPRING

sdlHapticDamper :: Word16
sdlHapticDamper = #const SDL_HAPTIC_DAMPER

sdlHapticInertia :: Word16
sdlHapticInertia = #const SDL_HAPTIC_INERTIA

sdlHapticFriction :: Word16
sdlHapticFriction = #const SDL_HAPTIC_FRICTION

sdlHapticLeftRight :: Word16
sdlHapticLeftRight = #const SDL_HAPTIC_LEFTRIGHT

sdlHapticCustom :: Word16
sdlHapticCustom = #const SDL_HAPTIC_CUSTOM

sdlHapticGain :: Word32
sdlHapticGain = #const SDL_HAPTIC_GAIN

sdlHapticAutocenter :: Word32
sdlHapticAutocenter = #const SDL_HAPTIC_AUTOCENTER

sdlHapticStatus :: Word32
sdlHapticStatus = #const SDL_HAPTIC_STATUS

sdlHapticPause :: Word32
sdlHapticPause = #const SDL_HAPTIC_PAUSE

sdlHapticPolar :: Word8
sdlHapticPolar = #const SDL_HAPTIC_POLAR

sdlHapticCartesian :: Word8
sdlHapticCartesian = #const SDL_HAPTIC_CARTESIAN

sdlHapticSpherical :: Word8
sdlHapticSpherical = #const SDL_HAPTIC_SPHERICAL

sdlHapticSteeringAxis :: Word8
sdlHapticSteeringAxis = #const SDL_HAPTIC_STEERING_AXIS

sdlHapticInfinity :: Word32
sdlHapticInfinity = #const SDL_HAPTIC_INFINITY

-- | Get list of connected haptic devices
foreign import ccall "SDL_GetHaptics"
  sdlGetHapticsRaw :: Ptr CInt -> IO (Ptr Word32)

sdlGetHaptics :: IO [SDLHapticID]
sdlGetHaptics = alloca $ \countPtr -> do
  ptr <- sdlGetHapticsRaw countPtr
  count <- peek countPtr
  ids <- peekArray (fromIntegral count) ptr
  free ptr
  return $ ids

-- | Get haptic name by ID
foreign import ccall "SDL_GetHapticNameForID"
  sdlGetHapticNameForID :: SDLHapticID -> IO CString

-- | Open a haptic device
foreign import ccall "SDL_OpenHaptic"
  sdlOpenHapticRaw :: Word32 -> IO (Ptr SDLHaptic)

sdlOpenHaptic :: SDLHapticID -> IO (Maybe (Ptr SDLHaptic))
sdlOpenHaptic hid = do
  ptr <- sdlOpenHapticRaw hid
  return $ if ptr == nullPtr then Nothing else Just ptr

-- | Get haptic from ID
foreign import ccall "SDL_GetHapticFromID"
  sdlGetHapticFromID :: SDLHapticID -> IO (Ptr SDLHaptic)

-- | Get haptic ID
foreign import ccall "SDL_GetHapticID"
  sdlGetHapticID :: Ptr SDLHaptic -> IO SDLHapticID

-- | Get haptic name
foreign import ccall "SDL_GetHapticName"
  sdlGetHapticName :: Ptr SDLHaptic -> IO CString

-- | Check if mouse is haptic
foreign import ccall "SDL_IsMouseHaptic"
  sdlIsMouseHaptic_ :: IO CBool

sdlIsMouseHaptic :: IO Bool
sdlIsMouseHaptic = toBool <$> sdlIsMouseHaptic_

-- | Open haptic from mouse
foreign import ccall "SDL_OpenHapticFromMouse"
  sdlOpenHapticFromMouseRaw :: IO (Ptr SDLHaptic)

sdlOpenHapticFromMouse :: IO (Maybe SDLHaptic)
sdlOpenHapticFromMouse = do
  ptr <- sdlOpenHapticFromMouseRaw
  return $ if ptr == nullPtr then Nothing else Just (SDLHaptic ptr)

-- | Check if joystick is haptic
foreign import ccall "SDL_IsJoystickHaptic"
  sdlIsJoystickHaptic_ :: Ptr SDLJoystick -> IO CBool

sdlIsJoystickHaptic :: Ptr SDLJoystick -> IO Bool
sdlIsJoystickHaptic joy = toBool <$> sdlIsJoystickHaptic_ joy

-- | Open haptic from joystick
foreign import ccall "SDL_OpenHapticFromJoystick"
  sdlOpenHapticFromJoystickRaw :: Ptr SDLJoystick -> IO (Ptr SDLHaptic)

sdlOpenHapticFromJoystick :: SDLJoystick -> IO (Maybe SDLHaptic)
sdlOpenHapticFromJoystick (SDLJoystick joy) = do
  ptr <- sdlOpenHapticFromJoystickRaw joy
  return $ if ptr == nullPtr then Nothing else Just (SDLHaptic ptr)

-- | Close haptic device
foreign import ccall "SDL_CloseHaptic"
  sdlCloseHaptic :: Ptr SDLHaptic -> IO ()

-- | Get max number of effects
foreign import ccall "SDL_GetMaxHapticEffects"
  sdlGetMaxHapticEffects :: Ptr SDLHaptic -> IO CInt

-- | Get max number of effects playing
foreign import ccall "SDL_GetMaxHapticEffectsPlaying"
  sdlGetMaxHapticEffectsPlaying :: Ptr SDLHaptic -> IO CInt

-- | Get haptic features
foreign import ccall "SDL_GetHapticFeatures"
  sdlGetHapticFeatures :: Ptr SDLHaptic -> IO Word32

-- | Get number of haptic axes
foreign import ccall "SDL_GetNumHapticAxes"
  sdlGetNumHapticAxes :: Ptr SDLHaptic -> IO CInt

-- | Check if effect is supported
foreign import ccall "SDL_HapticEffectSupported"
  sdlHapticEffectSupported_ :: Ptr SDLHaptic -> Ptr SDLHapticEffect -> IO CBool

sdlHapticEffectSupported :: Ptr SDLHaptic -> Ptr SDLHapticEffect -> IO Bool
sdlHapticEffectSupported haptic effect = toBool <$> sdlHapticEffectSupported_ haptic effect

-- | Create haptic effect
foreign import ccall "SDL_CreateHapticEffect"
  sdlCreateHapticEffect :: Ptr SDLHaptic -> Ptr SDLHapticEffect -> IO CInt

-- | Update haptic effect
foreign import ccall "SDL_UpdateHapticEffect"
  sdlUpdateHapticEffect_ :: Ptr SDLHaptic -> CInt -> Ptr SDLHapticEffect -> IO CBool

sdlUpdateHapticEffect :: Ptr SDLHaptic -> CInt -> Ptr SDLHapticEffect -> IO Bool
sdlUpdateHapticEffect haptic effectId effect = toBool <$> sdlUpdateHapticEffect_ haptic effectId effect

-- | Run haptic effect
foreign import ccall "SDL_RunHapticEffect"
  sdlRunHapticEffect_ :: Ptr SDLHaptic -> CInt -> Word32 -> IO CBool

sdlRunHapticEffect :: Ptr SDLHaptic -> CInt -> Word32 -> IO Bool
sdlRunHapticEffect haptic effectId iterations = toBool <$> sdlRunHapticEffect_ haptic effectId iterations

-- | Stop haptic effect
foreign import ccall "SDL_StopHapticEffect"
  sdlStopHapticEffect_ :: Ptr SDLHaptic -> CInt -> IO CBool

sdlStopHapticEffect :: Ptr SDLHaptic -> CInt -> IO Bool
sdlStopHapticEffect haptic effectId = toBool <$> sdlStopHapticEffect_ haptic effectId

-- | Destroy haptic effect
foreign import ccall "SDL_DestroyHapticEffect"
  sdlDestroyHapticEffect :: Ptr SDLHaptic -> CInt -> IO ()

-- | Get haptic effect status
foreign import ccall "SDL_GetHapticEffectStatus"
  sdlGetHapticEffectStatus_ :: Ptr SDLHaptic -> CInt -> IO CBool

sdlGetHapticEffectStatus :: Ptr SDLHaptic -> CInt -> IO Bool
sdlGetHapticEffectStatus haptic effectId = toBool <$> sdlGetHapticEffectStatus_ haptic effectId

-- | Set haptic gain
foreign import ccall "SDL_SetHapticGain"
  sdlSetHapticGain_ :: Ptr SDLHaptic -> CInt -> IO CBool

sdlSetHapticGain :: Ptr SDLHaptic -> CInt -> IO Bool
sdlSetHapticGain haptic gain = toBool <$> sdlSetHapticGain_ haptic gain

-- | Set haptic autocenter
foreign import ccall "SDL_SetHapticAutocenter"
  sdlSetHapticAutocenter_ :: Ptr SDLHaptic -> CInt -> IO CBool

sdlSetHapticAutocenter :: Ptr SDLHaptic -> CInt -> IO Bool
sdlSetHapticAutocenter haptic autocenter = toBool <$> sdlSetHapticAutocenter_ haptic autocenter

-- | Pause haptic device
foreign import ccall "SDL_PauseHaptic"
  sdlPauseHaptic_ :: Ptr SDLHaptic -> IO CBool

sdlPauseHaptic :: Ptr SDLHaptic -> IO Bool
sdlPauseHaptic haptic = toBool <$> sdlPauseHaptic_ haptic

-- | Resume haptic device
foreign import ccall "SDL_ResumeHaptic"
  sdlResumeHaptic_ :: Ptr SDLHaptic -> IO CBool

sdlResumeHaptic :: Ptr SDLHaptic -> IO Bool
sdlResumeHaptic haptic = toBool <$> sdlResumeHaptic_ haptic

-- | Stop all haptic effects
foreign import ccall "SDL_StopHapticEffects"
  sdlStopHapticEffects_ :: Ptr SDLHaptic -> IO CBool

sdlStopHapticEffects :: Ptr SDLHaptic -> IO Bool
sdlStopHapticEffects haptic = toBool <$> sdlStopHapticEffects_ haptic

-- | Check if rumble is supported
foreign import ccall "SDL_HapticRumbleSupported"
  sdlHapticRumbleSupported_ :: Ptr SDLHaptic -> IO CBool

sdlHapticRumbleSupported :: Ptr SDLHaptic -> IO Bool
sdlHapticRumbleSupported haptic = toBool <$> sdlHapticRumbleSupported_ haptic

-- | Initialize haptic rumble
foreign import ccall "SDL_InitHapticRumble"
  sdlInitHapticRumble_ :: Ptr SDLHaptic -> IO CBool

sdlInitHapticRumble :: Ptr SDLHaptic -> IO Bool
sdlInitHapticRumble haptic = toBool <$> sdlInitHapticRumble_ haptic

-- | Play haptic rumble
foreign import ccall "SDL_PlayHapticRumble"
  sdlPlayHapticRumble_ :: Ptr SDLHaptic -> CFloat -> Word32 -> IO CBool

sdlPlayHapticRumble :: Ptr SDLHaptic -> CFloat -> Word32 -> IO Bool
sdlPlayHapticRumble haptic strength rumLen = toBool <$> sdlPlayHapticRumble_ haptic strength rumLen

-- | Stop haptic rumble
foreign import ccall "SDL_StopHapticRumble"
  sdlStopHapticRumble_ :: Ptr SDLHaptic -> IO CBool

sdlStopHapticRumble :: Ptr SDLHaptic -> IO Bool
sdlStopHapticRumble haptic = toBool <$> sdlStopHapticRumble_ haptic
