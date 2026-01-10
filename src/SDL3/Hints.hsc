{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds#-}

-- SDL/Hints.hsc
{-|
Module      : SDL.Hints
Description : Hint management functions for SDL3.
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

This module provides Haskell bindings to the SDL3 hint management functionality.
-}

module SDL3.Hints
  (
    -- * Hint Priorities
    SDLHintPriority(..)
  , pattern SDL_HINT_DEFAULT
  , pattern SDL_HINT_NORMAL
  , pattern SDL_HINT_OVERRIDE

    -- * Hint Functions
  , sdlSetHintWithPriority
  , sdlSetHint
  , sdlResetHint
  , sdlResetHints
  , sdlGetHint
  , sdlGetHintBoolean
  , sdlAddHintCallback
  , sdlRemoveHintCallback
    -- * Hint Names
  , sdlHintAllowAltTabWhileGrabbed
  , sdlHintAndroidAllowRecreateActivity
  , sdlHintAndroidBlockOnPause
  , sdlHintAndroidLowLatencyAudio
  , sdlHintAndroidTrapBackButton
  , sdlHintAppId
  , sdlHintAppName
  , sdlHintAppleTvControllerUiEvents
  , sdlHintAppleTvRemoteAllowRotation
  , sdlHintAudioAlsaDefaultDevice
  , sdlHintAudioAlsaDefaultPlaybackDevice
  , sdlHintAudioAlsaDefaultRecordingDevice
  , sdlHintAudioCategory
  , sdlHintAudioChannels
  , sdlHintAudioDeviceAppIconName
  , sdlHintAudioDeviceSampleFrames
  , sdlHintAudioDeviceStreamName
  , sdlHintAudioDeviceStreamRole
  , sdlHintAudioDeviceRawStream
  , sdlHintAudioDiskInputFile
  , sdlHintAudioDiskOutputFile
  , sdlHintAudioDiskTimescale
  , sdlHintAudioDriver
  , sdlHintAudioDummyTimescale
  , sdlHintAudioFormat
  , sdlHintAudioFrequency
  , sdlHintAudioIncludeMonitors
  , sdlHintAutoUpdateJoysticks
  , sdlHintAutoUpdateSensors
  , sdlHintBmpSaveLegacyFormat
  , sdlHintCameraDriver
  , sdlHintCpuFeatureMask
  , sdlHintJoystickDirectinput
  , sdlHintFileDialogDriver
  , sdlHintDisplayUsableBounds
  , sdlHintInvalidParamChecks
  , sdlHintEmscriptenAsyncify
  , sdlHintEmscriptenCanvasSelector
  , sdlHintEmscriptenKeyboardElement
  , sdlHintEnableScreenKeyboard
  , sdlHintEvdevDevices
  , sdlHintEventLogging
  , sdlHintForceRaisewindow
  , sdlHintFramebufferAcceleration
  , sdlHintGamecontrollerconfig
  , sdlHintGamecontrollerconfigFile
  , sdlHintGamecontrollertype
  , sdlHintGamecontrollerIgnoreDevices
  , sdlHintGamecontrollerIgnoreDevicesExcept
  , sdlHintGamecontrollerSensorFusion
  , sdlHintGdkTextinputDefaultText
  , sdlHintGdkTextinputDescription
  , sdlHintGdkTextinputMaxLength
  , sdlHintGdkTextinputScope
  , sdlHintGdkTextinputTitle
  , sdlHintHidapiLibusb
  , sdlHintHidapiLibusbGamecube
  , sdlHintHidapiLibusbWhitelist
  , sdlHintHidapiUdev
  , sdlHintGpuDriver
  , sdlHintHidapiEnumerateOnlyControllers
  , sdlHintHidapiIgnoreDevices
  , sdlHintImeImplementedUi
  , sdlHintIosHideHomeIndicator
  , sdlHintJoystickAllowBackgroundEvents
  , sdlHintJoystickArcadestickDevices
  , sdlHintJoystickArcadestickDevicesExcluded
  , sdlHintJoystickBlacklistDevices
  , sdlHintJoystickBlacklistDevicesExcluded
  , sdlHintJoystickDevice
  , sdlHintJoystickEnhancedReports
  , sdlHintJoystickFlightstickDevices
  , sdlHintJoystickFlightstickDevicesExcluded
  , sdlHintJoystickGameinput
  , sdlHintJoystickGamecubeDevices
  , sdlHintJoystickGamecubeDevicesExcluded
  , sdlHintJoystickHidapi
  , sdlHintJoystickHidapiCombineJoyCons
  , sdlHintJoystickHidapiGamecube
  , sdlHintShutdownDbusOnQuit
  , sdlHintStorageTitleDriver
  , sdlHintStorageUserDriver
  , sdlHintThreadForceRealtimeTimeCritical
  , sdlHintThreadPriorityPolicy
  , sdlHintTimerResolution
  , sdlHintTouchMouseEvents
  , sdlHintTrackpadIsTouchOnly
  , sdlHintTvRemoteAsJoystick
  , sdlHintVideoAllowScreensaver
  , sdlHintVideoDisplayPriority
  , sdlHintVideoDoubleBuffer
  , sdlHintVideoDriver
  , sdlHintVideoDummySaveFrames
  , sdlHintVideoEglAllowGetdisplayFallback
  , sdlHintVideoForceEgl
  , sdlHintVideoMacFullscreenSpaces
  , sdlHintVideoMacFullscreenMenuVisibility
  , sdlHintVideoMatchExclusiveModeOnMove
  , sdlHintVideoMinimizeOnFocusLoss
  , sdlHintVideoOffscreenSaveFrames
  , sdlHintVideoSyncWindowOperations
  , sdlHintVideoWaylandAllowLibdecor
  , sdlHintVideoWaylandModeEmulation
  , sdlHintVideoWaylandModeScaling
  , sdlHintVideoWaylandPreferLibdecor
  , sdlHintVideoWaylandScaleToDisplay
  , sdlHintVideoWinD3dcompiler
  , sdlHintVideoX11ExternalWindowInput
  , sdlHintVideoX11NetWmBypassCompositor
  , sdlHintVideoX11NetWmPing
  -- , sdlHintVideoX11Nodirectcolor
  , sdlHintVideoX11ScalingFactor
  , sdlHintVideoX11Visualid
  , sdlHintVideoX11WindowVisualid
  , sdlHintVideoX11Xrandr
  , sdlHintVitaEnableBackTouch
  , sdlHintVitaEnableFrontTouch
  , sdlHintVitaModulePath
  , sdlHintVitaPvrInit
  , sdlHintVitaResolution
  , sdlHintVitaPvrOpengl
  , sdlHintVitaTouchMouseDevice
  , sdlHintVulkanDisplay
  , sdlHintVulkanLibrary
  , sdlHintWaveFactChunk
  , sdlHintWaveChunkLimit
  , sdlHintWaveRiffChunkSize
  , sdlHintWaveTruncation
  , sdlHintWindowActivateWhenRaised
  , sdlHintWindowActivateWhenShown
  , sdlHintWindowAllowTopmost
  , sdlHintWindowFrameUsableWhileCursorHidden
  , sdlHintWindowsCloseOnAltF4
  , sdlHintWindowsEnableMenuMnemonics
  , sdlHintWindowsEnableMessageloop
  , sdlHintWindowsGameinput
  , sdlHintWindowsRawKeyboard
  , sdlHintWindowsRawKeyboardExcludeHotkeys
  , sdlHintWindowsForceSemaphoreKernel
  , sdlHintWindowsIntresourceIcon
  , sdlHintWindowsIntresourceIconSmall
  , sdlHintWindowsUseD3d9ex
  , sdlHintWindowsEraseBackgroundMode
  , sdlHintX11ForceOverrideRedirect
  , sdlHintX11WindowType
  , sdlHintX11XcbLibrary
  , sdlHintXinputEnabled
  , sdlHintAssert
  , sdlHintPenMouseEvents
  , sdlHintPenTouchEvents
  ) where

#include <SDL3/SDL_hints.h>

import Foreign
import Foreign.C

-- | Enumeration of hint priorities, influencing hint setting behavior.
newtype SDLHintPriority = SDLHintPriority CInt
  deriving newtype (Show, Eq, Ord, Storable, Enum) -- Added Ord, Storable, Enum

pattern SDL_HINT_DEFAULT = SDLHintPriority #{const SDL_HINT_DEFAULT}
pattern SDL_HINT_NORMAL = SDLHintPriority #{const SDL_HINT_NORMAL}
pattern SDL_HINT_OVERRIDE = SDLHintPriority #{const SDL_HINT_OVERRIDE}

-- FFI Imports
foreign import ccall unsafe "SDL_SetHintWithPriority" sdlSetHintWithPriority_ :: CString -> CString -> SDLHintPriority -> IO Bool
foreign import ccall unsafe "SDL_SetHint" sdlSetHint_ :: CString -> CString -> IO Bool
foreign import ccall unsafe "SDL_ResetHint" sdlResetHint_ :: CString -> IO Bool
foreign import ccall unsafe "SDL_ResetHints" sdlResetHints_ :: IO ()
foreign import ccall unsafe "SDL_GetHint" sdlGetHint_ :: CString -> IO CString
foreign import ccall unsafe "SDL_GetHintBoolean" sdlGetHintBoolean_ :: CString -> Bool -> IO Bool
foreign import ccall "wrapper" mkHintCallback :: (Ptr () -> CString -> CString -> CString -> IO ()) -> IO (FunPtr (Ptr () -> CString -> CString -> CString -> IO ()))
foreign import ccall unsafe "SDL_AddHintCallback" sdlAddHintCallback_ :: CString -> FunPtr (Ptr () -> CString -> CString -> CString -> IO ()) -> Ptr () -> IO Bool
foreign import ccall unsafe "SDL_RemoveHintCallback" sdlRemoveHintCallback_ :: CString -> FunPtr (Ptr () -> CString -> CString -> CString -> IO ()) -> Ptr () -> IO ()

-- Haskell Wrappers

-- | Set a hint with a specific priority.
sdlSetHintWithPriority :: String -> String -> SDLHintPriority -> IO Bool
sdlSetHintWithPriority name value priority =
  withCString name $ \namePtr ->
    withCString value $ \valuePtr ->
      sdlSetHintWithPriority_ namePtr valuePtr priority

-- | Set a hint with normal priority.
sdlSetHint :: String -> String -> IO Bool
sdlSetHint name value =
  withCString name $ \namePtr ->
    withCString value $ \valuePtr ->
      sdlSetHint_ namePtr valuePtr

-- | Reset a hint to the default value.
sdlResetHint :: String -> IO Bool
sdlResetHint name =
  withCString name $ \namePtr ->
    sdlResetHint_ namePtr

-- | Reset all hints to the default values.
sdlResetHints :: IO ()
sdlResetHints = sdlResetHints_

-- | Get the value of a hint.
sdlGetHint :: String -> IO (Maybe String)
sdlGetHint name =
  withCString name $ \namePtr -> do
    result <- sdlGetHint_ namePtr
    if result == nullPtr
      then return Nothing
      else Just <$> peekCString result

-- | Get the boolean value of a hint variable.
sdlGetHintBoolean :: String -> Bool -> IO Bool
sdlGetHintBoolean name defaultValue =
  withCString name $ \namePtr ->
    sdlGetHintBoolean_ namePtr defaultValue

-- | Add a function to watch a particular hint.
sdlAddHintCallback :: String -> (Ptr () -> String -> String -> String -> IO ()) -> Ptr () -> IO Bool
sdlAddHintCallback name callback userdata =
  withCString name $ \namePtr -> do
    callbackPtr <- mkHintCallback $ \ud ptrName oldVal newVal -> do
      nameStr <- peekCString ptrName
      oldStr <- peekCString oldVal
      newStr <- peekCString newVal
      callback ud nameStr oldStr newStr
    sdlAddHintCallback_ namePtr callbackPtr userdata

-- | Remove a function watching a particular hint.
sdlRemoveHintCallback :: String -> (Ptr () -> String -> String -> String -> IO ()) -> Ptr () -> IO ()
sdlRemoveHintCallback name callback userdata =
  withCString name $ \namePtr -> do
    callbackPtr <- mkHintCallback $ \ud ptrName oldVal newVal -> do
      nameStr <- peekCString ptrName
      oldStr <- peekCString oldVal
      newStr <- peekCString newVal
      callback ud nameStr oldStr newStr  -- Pass all four arguments
    sdlRemoveHintCallback_ namePtr callbackPtr userdata

-- Hint Names

sdlHintAllowAltTabWhileGrabbed :: String
sdlHintAllowAltTabWhileGrabbed = #{const_str SDL_HINT_ALLOW_ALT_TAB_WHILE_GRABBED}

sdlHintAndroidAllowRecreateActivity :: String
sdlHintAndroidAllowRecreateActivity = #{const_str SDL_HINT_ANDROID_ALLOW_RECREATE_ACTIVITY}

sdlHintAndroidBlockOnPause :: String
sdlHintAndroidBlockOnPause = #{const_str SDL_HINT_ANDROID_BLOCK_ON_PAUSE}

sdlHintAndroidLowLatencyAudio :: String
sdlHintAndroidLowLatencyAudio = #{const_str SDL_HINT_ANDROID_LOW_LATENCY_AUDIO}

sdlHintAndroidTrapBackButton :: String
sdlHintAndroidTrapBackButton = #{const_str SDL_HINT_ANDROID_TRAP_BACK_BUTTON}

sdlHintAppId :: String
sdlHintAppId = #{const_str SDL_HINT_APP_ID}

sdlHintAppName :: String
sdlHintAppName = #{const_str SDL_HINT_APP_NAME}

sdlHintAppleTvControllerUiEvents :: String
sdlHintAppleTvControllerUiEvents = #{const_str SDL_HINT_APPLE_TV_CONTROLLER_UI_EVENTS}

sdlHintAppleTvRemoteAllowRotation :: String
sdlHintAppleTvRemoteAllowRotation = #{const_str SDL_HINT_APPLE_TV_REMOTE_ALLOW_ROTATION}

sdlHintAudioAlsaDefaultDevice :: String
sdlHintAudioAlsaDefaultDevice = #{const_str SDL_HINT_AUDIO_ALSA_DEFAULT_DEVICE}

sdlHintAudioAlsaDefaultPlaybackDevice :: String
sdlHintAudioAlsaDefaultPlaybackDevice = #{const_str SDL_HINT_AUDIO_ALSA_DEFAULT_PLAYBACK_DEVICE}

sdlHintAudioAlsaDefaultRecordingDevice :: String
sdlHintAudioAlsaDefaultRecordingDevice = #{const_str SDL_HINT_AUDIO_ALSA_DEFAULT_RECORDING_DEVICE}

sdlHintAudioCategory :: String
sdlHintAudioCategory = #{const_str SDL_HINT_AUDIO_CATEGORY}

sdlHintAudioChannels :: String
sdlHintAudioChannels = #{const_str SDL_HINT_AUDIO_CHANNELS}

sdlHintAudioDeviceAppIconName :: String
sdlHintAudioDeviceAppIconName = #{const_str SDL_HINT_AUDIO_DEVICE_APP_ICON_NAME}

sdlHintAudioDeviceSampleFrames :: String
sdlHintAudioDeviceSampleFrames = #{const_str SDL_HINT_AUDIO_DEVICE_SAMPLE_FRAMES}

sdlHintAudioDeviceStreamName :: String
sdlHintAudioDeviceStreamName = #{const_str SDL_HINT_AUDIO_DEVICE_STREAM_NAME}

sdlHintAudioDeviceStreamRole :: String
sdlHintAudioDeviceStreamRole = #{const_str SDL_HINT_AUDIO_DEVICE_STREAM_ROLE}

sdlHintAudioDeviceRawStream :: String
sdlHintAudioDeviceRawStream = #{const_str SDL_HINT_AUDIO_DEVICE_RAW_STREAM}

sdlHintAudioDiskInputFile :: String
sdlHintAudioDiskInputFile = #{const_str SDL_HINT_AUDIO_DISK_INPUT_FILE}

sdlHintAudioDiskOutputFile :: String
sdlHintAudioDiskOutputFile = #{const_str SDL_HINT_AUDIO_DISK_OUTPUT_FILE}

sdlHintAudioDiskTimescale :: String
sdlHintAudioDiskTimescale = #{const_str SDL_HINT_AUDIO_DISK_TIMESCALE}

sdlHintAudioDriver :: String
sdlHintAudioDriver = #{const_str SDL_HINT_AUDIO_DRIVER}

sdlHintAudioDummyTimescale :: String
sdlHintAudioDummyTimescale = #{const_str SDL_HINT_AUDIO_DUMMY_TIMESCALE}

sdlHintAudioFormat :: String
sdlHintAudioFormat = #{const_str SDL_HINT_AUDIO_FORMAT}

sdlHintAudioFrequency :: String
sdlHintAudioFrequency = #{const_str SDL_HINT_AUDIO_FREQUENCY}

sdlHintAudioIncludeMonitors :: String
sdlHintAudioIncludeMonitors = #{const_str SDL_HINT_AUDIO_INCLUDE_MONITORS}

sdlHintAutoUpdateJoysticks :: String
sdlHintAutoUpdateJoysticks = #{const_str SDL_HINT_AUTO_UPDATE_JOYSTICKS}

sdlHintAutoUpdateSensors :: String
sdlHintAutoUpdateSensors = #{const_str SDL_HINT_AUTO_UPDATE_SENSORS}

sdlHintBmpSaveLegacyFormat :: String
sdlHintBmpSaveLegacyFormat = #{const_str SDL_HINT_BMP_SAVE_LEGACY_FORMAT}

sdlHintCameraDriver :: String
sdlHintCameraDriver = #{const_str SDL_HINT_CAMERA_DRIVER}

sdlHintCpuFeatureMask :: String
sdlHintCpuFeatureMask = #{const_str SDL_HINT_CPU_FEATURE_MASK}

sdlHintJoystickDirectinput :: String
sdlHintJoystickDirectinput = #{const_str SDL_HINT_JOYSTICK_DIRECTINPUT}

sdlHintFileDialogDriver :: String
sdlHintFileDialogDriver = #{const_str SDL_HINT_FILE_DIALOG_DRIVER}

sdlHintDisplayUsableBounds :: String
sdlHintDisplayUsableBounds = #{const_str SDL_HINT_DISPLAY_USABLE_BOUNDS}

sdlHintInvalidParamChecks :: String
sdlHintInvalidParamChecks = #{const_str SDL_HINT_INVALID_PARAM_CHECKS}

sdlHintEmscriptenAsyncify :: String
sdlHintEmscriptenAsyncify = #{const_str SDL_HINT_EMSCRIPTEN_ASYNCIFY}

sdlHintEmscriptenCanvasSelector :: String
sdlHintEmscriptenCanvasSelector = #{const_str SDL_HINT_EMSCRIPTEN_CANVAS_SELECTOR}

sdlHintEmscriptenKeyboardElement :: String
sdlHintEmscriptenKeyboardElement = #{const_str SDL_HINT_EMSCRIPTEN_KEYBOARD_ELEMENT}

sdlHintEnableScreenKeyboard :: String
sdlHintEnableScreenKeyboard = #{const_str SDL_HINT_ENABLE_SCREEN_KEYBOARD}

sdlHintEvdevDevices :: String
sdlHintEvdevDevices = #{const_str SDL_HINT_EVDEV_DEVICES}

sdlHintEventLogging :: String
sdlHintEventLogging = #{const_str SDL_HINT_EVENT_LOGGING}

sdlHintForceRaisewindow :: String
sdlHintForceRaisewindow = #{const_str SDL_HINT_FORCE_RAISEWINDOW}

sdlHintFramebufferAcceleration :: String
sdlHintFramebufferAcceleration = #{const_str SDL_HINT_FRAMEBUFFER_ACCELERATION}

sdlHintGamecontrollerconfig :: String
sdlHintGamecontrollerconfig = #{const_str SDL_HINT_GAMECONTROLLERCONFIG}

sdlHintGamecontrollerconfigFile :: String
sdlHintGamecontrollerconfigFile = #{const_str SDL_HINT_GAMECONTROLLERCONFIG_FILE}

sdlHintGamecontrollertype :: String
sdlHintGamecontrollertype = #{const_str SDL_HINT_GAMECONTROLLERTYPE}

sdlHintGamecontrollerIgnoreDevices :: String
sdlHintGamecontrollerIgnoreDevices = #{const_str SDL_HINT_GAMECONTROLLER_IGNORE_DEVICES}

sdlHintGamecontrollerIgnoreDevicesExcept :: String
sdlHintGamecontrollerIgnoreDevicesExcept = #{const_str SDL_HINT_GAMECONTROLLER_IGNORE_DEVICES_EXCEPT}

sdlHintGamecontrollerSensorFusion :: String
sdlHintGamecontrollerSensorFusion = #{const_str SDL_HINT_GAMECONTROLLER_SENSOR_FUSION}

sdlHintGdkTextinputDefaultText :: String
sdlHintGdkTextinputDefaultText = #{const_str SDL_HINT_GDK_TEXTINPUT_DEFAULT_TEXT}

sdlHintGdkTextinputDescription :: String
sdlHintGdkTextinputDescription = #{const_str SDL_HINT_GDK_TEXTINPUT_DESCRIPTION}

sdlHintGdkTextinputMaxLength :: String
sdlHintGdkTextinputMaxLength = #{const_str SDL_HINT_GDK_TEXTINPUT_MAX_LENGTH}

sdlHintGdkTextinputScope :: String
sdlHintGdkTextinputScope = #{const_str SDL_HINT_GDK_TEXTINPUT_SCOPE}

sdlHintGdkTextinputTitle :: String
sdlHintGdkTextinputTitle = #{const_str SDL_HINT_GDK_TEXTINPUT_TITLE}

sdlHintHidapiLibusb :: String
sdlHintHidapiLibusb = #{const_str SDL_HINT_HIDAPI_LIBUSB}

sdlHintHidapiLibusbGamecube :: String
sdlHintHidapiLibusbGamecube = #{const_str SDL_HINT_HIDAPI_LIBUSB_GAMECUBE}

sdlHintHidapiLibusbWhitelist :: String
sdlHintHidapiLibusbWhitelist = #{const_str SDL_HINT_HIDAPI_LIBUSB_WHITELIST}

sdlHintHidapiUdev :: String
sdlHintHidapiUdev = #{const_str SDL_HINT_HIDAPI_UDEV}

sdlHintGpuDriver :: String
sdlHintGpuDriver = #{const_str SDL_HINT_GPU_DRIVER}

sdlHintHidapiEnumerateOnlyControllers :: String
sdlHintHidapiEnumerateOnlyControllers = #{const_str SDL_HINT_HIDAPI_ENUMERATE_ONLY_CONTROLLERS}

sdlHintHidapiIgnoreDevices :: String
sdlHintHidapiIgnoreDevices = #{const_str SDL_HINT_HIDAPI_IGNORE_DEVICES}

sdlHintImeImplementedUi :: String
sdlHintImeImplementedUi = #{const_str SDL_HINT_IME_IMPLEMENTED_UI}

sdlHintIosHideHomeIndicator :: String
sdlHintIosHideHomeIndicator = #{const_str SDL_HINT_IOS_HIDE_HOME_INDICATOR}

sdlHintJoystickAllowBackgroundEvents :: String
sdlHintJoystickAllowBackgroundEvents = #{const_str SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS}

sdlHintJoystickArcadestickDevices :: String
sdlHintJoystickArcadestickDevices = #{const_str SDL_HINT_JOYSTICK_ARCADESTICK_DEVICES}

sdlHintJoystickArcadestickDevicesExcluded :: String
sdlHintJoystickArcadestickDevicesExcluded = #{const_str SDL_HINT_JOYSTICK_ARCADESTICK_DEVICES_EXCLUDED}

sdlHintJoystickBlacklistDevices :: String
sdlHintJoystickBlacklistDevices = #{const_str SDL_HINT_JOYSTICK_BLACKLIST_DEVICES}

sdlHintJoystickBlacklistDevicesExcluded :: String
sdlHintJoystickBlacklistDevicesExcluded = #{const_str SDL_HINT_JOYSTICK_BLACKLIST_DEVICES_EXCLUDED}

sdlHintJoystickDevice :: String
sdlHintJoystickDevice = #{const_str SDL_HINT_JOYSTICK_DEVICE}

sdlHintJoystickEnhancedReports :: String
sdlHintJoystickEnhancedReports = #{const_str SDL_HINT_JOYSTICK_ENHANCED_REPORTS}

sdlHintJoystickFlightstickDevices :: String
sdlHintJoystickFlightstickDevices = #{const_str SDL_HINT_JOYSTICK_FLIGHTSTICK_DEVICES}

sdlHintJoystickFlightstickDevicesExcluded :: String
sdlHintJoystickFlightstickDevicesExcluded = #{const_str SDL_HINT_JOYSTICK_FLIGHTSTICK_DEVICES_EXCLUDED}

sdlHintJoystickGameinput :: String
sdlHintJoystickGameinput = #{const_str SDL_HINT_JOYSTICK_GAMEINPUT}

sdlHintJoystickGamecubeDevices :: String
sdlHintJoystickGamecubeDevices = #{const_str SDL_HINT_JOYSTICK_GAMECUBE_DEVICES}

sdlHintJoystickGamecubeDevicesExcluded :: String
sdlHintJoystickGamecubeDevicesExcluded = #{const_str SDL_HINT_JOYSTICK_GAMECUBE_DEVICES_EXCLUDED}

sdlHintJoystickHidapi :: String
sdlHintJoystickHidapi = #{const_str SDL_HINT_JOYSTICK_HIDAPI}

sdlHintJoystickHidapiCombineJoyCons :: String
sdlHintJoystickHidapiCombineJoyCons = #{const_str SDL_HINT_JOYSTICK_HIDAPI_COMBINE_JOY_CONS}

sdlHintJoystickHidapiGamecube :: String
sdlHintJoystickHidapiGamecube = #{const_str SDL_HINT_JOYSTICK_HIDAPI_GAMECUBE}

sdlHintShutdownDbusOnQuit :: String
sdlHintShutdownDbusOnQuit = #{const_str SDL_HINT_SHUTDOWN_DBUS_ON_QUIT}

sdlHintStorageTitleDriver :: String
sdlHintStorageTitleDriver = #{const_str SDL_HINT_STORAGE_TITLE_DRIVER}

sdlHintStorageUserDriver :: String
sdlHintStorageUserDriver = #{const_str SDL_HINT_STORAGE_USER_DRIVER}

sdlHintThreadForceRealtimeTimeCritical :: String
sdlHintThreadForceRealtimeTimeCritical = #{const_str SDL_HINT_THREAD_FORCE_REALTIME_TIME_CRITICAL}

sdlHintThreadPriorityPolicy :: String
sdlHintThreadPriorityPolicy = #{const_str SDL_HINT_THREAD_PRIORITY_POLICY}

sdlHintTimerResolution :: String
sdlHintTimerResolution = #{const_str SDL_HINT_TIMER_RESOLUTION}

sdlHintTouchMouseEvents :: String
sdlHintTouchMouseEvents = #{const_str SDL_HINT_TOUCH_MOUSE_EVENTS}

sdlHintTrackpadIsTouchOnly :: String
sdlHintTrackpadIsTouchOnly = #{const_str SDL_HINT_TRACKPAD_IS_TOUCH_ONLY}

sdlHintTvRemoteAsJoystick :: String
sdlHintTvRemoteAsJoystick = #{const_str SDL_HINT_TV_REMOTE_AS_JOYSTICK}

sdlHintVideoAllowScreensaver :: String
sdlHintVideoAllowScreensaver = #{const_str SDL_HINT_VIDEO_ALLOW_SCREENSAVER}

sdlHintVideoDisplayPriority :: String
sdlHintVideoDisplayPriority = #{const_str SDL_HINT_VIDEO_DISPLAY_PRIORITY}

sdlHintVideoDoubleBuffer :: String
sdlHintVideoDoubleBuffer = #{const_str SDL_HINT_VIDEO_DOUBLE_BUFFER}

sdlHintVideoDriver :: String
sdlHintVideoDriver = #{const_str SDL_HINT_VIDEO_DRIVER}

sdlHintVideoDummySaveFrames :: String
sdlHintVideoDummySaveFrames = #{const_str SDL_HINT_VIDEO_DUMMY_SAVE_FRAMES}

sdlHintVideoEglAllowGetdisplayFallback :: String
sdlHintVideoEglAllowGetdisplayFallback = #{const_str SDL_HINT_VIDEO_EGL_ALLOW_GETDISPLAY_FALLBACK}

sdlHintVideoForceEgl :: String
sdlHintVideoForceEgl = #{const_str SDL_HINT_VIDEO_FORCE_EGL}

sdlHintVideoMacFullscreenSpaces :: String
sdlHintVideoMacFullscreenSpaces = #{const_str SDL_HINT_VIDEO_MAC_FULLSCREEN_SPACES}

sdlHintVideoMacFullscreenMenuVisibility :: String
sdlHintVideoMacFullscreenMenuVisibility = #{const_str SDL_HINT_VIDEO_MAC_FULLSCREEN_MENU_VISIBILITY}

sdlHintVideoMatchExclusiveModeOnMove :: String
sdlHintVideoMatchExclusiveModeOnMove = #{const_str SDL_HINT_VIDEO_MATCH_EXCLUSIVE_MODE_ON_MOVE}

sdlHintVideoMinimizeOnFocusLoss :: String
sdlHintVideoMinimizeOnFocusLoss = #{const_str SDL_HINT_VIDEO_MINIMIZE_ON_FOCUS_LOSS}

sdlHintVideoOffscreenSaveFrames :: String
sdlHintVideoOffscreenSaveFrames = #{const_str SDL_HINT_VIDEO_OFFSCREEN_SAVE_FRAMES}

sdlHintVideoSyncWindowOperations :: String
sdlHintVideoSyncWindowOperations = #{const_str SDL_HINT_VIDEO_SYNC_WINDOW_OPERATIONS}

sdlHintVideoWaylandAllowLibdecor :: String
sdlHintVideoWaylandAllowLibdecor = #{const_str SDL_HINT_VIDEO_WAYLAND_ALLOW_LIBDECOR}

sdlHintVideoWaylandModeEmulation :: String
sdlHintVideoWaylandModeEmulation = #{const_str SDL_HINT_VIDEO_WAYLAND_MODE_EMULATION}

sdlHintVideoWaylandModeScaling :: String
sdlHintVideoWaylandModeScaling = #{const_str SDL_HINT_VIDEO_WAYLAND_MODE_SCALING}

sdlHintVideoWaylandPreferLibdecor :: String
sdlHintVideoWaylandPreferLibdecor = #{const_str SDL_HINT_VIDEO_WAYLAND_PREFER_LIBDECOR}

sdlHintVideoWaylandScaleToDisplay :: String
sdlHintVideoWaylandScaleToDisplay = #{const_str SDL_HINT_VIDEO_WAYLAND_SCALE_TO_DISPLAY}

sdlHintVideoWinD3dcompiler :: String
sdlHintVideoWinD3dcompiler = #{const_str SDL_HINT_VIDEO_WIN_D3DCOMPILER}

sdlHintVideoX11ExternalWindowInput :: String
sdlHintVideoX11ExternalWindowInput = #{const_str SDL_HINT_VIDEO_X11_EXTERNAL_WINDOW_INPUT}

sdlHintVideoX11NetWmBypassCompositor :: String
sdlHintVideoX11NetWmBypassCompositor = #{const_str SDL_HINT_VIDEO_X11_NET_WM_BYPASS_COMPOSITOR}

sdlHintVideoX11NetWmPing :: String
sdlHintVideoX11NetWmPing = #{const_str SDL_HINT_VIDEO_X11_NET_WM_PING}

sdlHintVideoX11Nodirectcolor :: String
sdlHintVideoX11Nodirectcolor = #{const_str SDL_HINT_VIDEO_X11_NODIRECTCOLOR}

sdlHintVideoX11ScalingFactor :: String
sdlHintVideoX11ScalingFactor = #{const_str SDL_HINT_VIDEO_X11_SCALING_FACTOR}

sdlHintVideoX11Visualid :: String
sdlHintVideoX11Visualid = #{const_str SDL_HINT_VIDEO_X11_VISUALID}

sdlHintVideoX11WindowVisualid :: String
sdlHintVideoX11WindowVisualid = #{const_str SDL_HINT_VIDEO_X11_WINDOW_VISUALID}

sdlHintVideoX11Xrandr :: String
sdlHintVideoX11Xrandr = #{const_str SDL_HINT_VIDEO_X11_XRANDR}

sdlHintVitaEnableBackTouch :: String
sdlHintVitaEnableBackTouch = #{const_str SDL_HINT_VITA_ENABLE_BACK_TOUCH}

sdlHintVitaEnableFrontTouch :: String
sdlHintVitaEnableFrontTouch = #{const_str SDL_HINT_VITA_ENABLE_FRONT_TOUCH}

sdlHintVitaModulePath :: String
sdlHintVitaModulePath = #{const_str SDL_HINT_VITA_MODULE_PATH}

sdlHintVitaPvrInit :: String
sdlHintVitaPvrInit = #{const_str SDL_HINT_VITA_PVR_INIT}

sdlHintVitaResolution :: String
sdlHintVitaResolution = #{const_str SDL_HINT_VITA_RESOLUTION}

sdlHintVitaPvrOpengl :: String
sdlHintVitaPvrOpengl = #{const_str SDL_HINT_VITA_PVR_OPENGL}

sdlHintVitaTouchMouseDevice :: String
sdlHintVitaTouchMouseDevice = #{const_str SDL_HINT_VITA_TOUCH_MOUSE_DEVICE}

sdlHintVulkanDisplay :: String
sdlHintVulkanDisplay = #{const_str SDL_HINT_VULKAN_DISPLAY}

sdlHintVulkanLibrary :: String
sdlHintVulkanLibrary = #{const_str SDL_HINT_VULKAN_LIBRARY}

sdlHintWaveFactChunk :: String
sdlHintWaveFactChunk = #{const_str SDL_HINT_WAVE_FACT_CHUNK}

sdlHintWaveChunkLimit :: String
sdlHintWaveChunkLimit = #{const_str SDL_HINT_WAVE_CHUNK_LIMIT}

sdlHintWaveRiffChunkSize :: String
sdlHintWaveRiffChunkSize = #{const_str SDL_HINT_WAVE_RIFF_CHUNK_SIZE}

sdlHintWaveTruncation :: String
sdlHintWaveTruncation = #{const_str SDL_HINT_WAVE_TRUNCATION}

sdlHintWindowActivateWhenRaised :: String
sdlHintWindowActivateWhenRaised = #{const_str SDL_HINT_WINDOW_ACTIVATE_WHEN_RAISED}

sdlHintWindowActivateWhenShown :: String
sdlHintWindowActivateWhenShown = #{const_str SDL_HINT_WINDOW_ACTIVATE_WHEN_SHOWN}

sdlHintWindowAllowTopmost :: String
sdlHintWindowAllowTopmost = #{const_str SDL_HINT_WINDOW_ALLOW_TOPMOST}

sdlHintWindowFrameUsableWhileCursorHidden :: String
sdlHintWindowFrameUsableWhileCursorHidden = #{const_str SDL_HINT_WINDOW_FRAME_USABLE_WHILE_CURSOR_HIDDEN}

sdlHintWindowsCloseOnAltF4 :: String
sdlHintWindowsCloseOnAltF4 = #{const_str SDL_HINT_WINDOWS_CLOSE_ON_ALT_F4}

sdlHintWindowsEnableMenuMnemonics :: String
sdlHintWindowsEnableMenuMnemonics = #{const_str SDL_HINT_WINDOWS_ENABLE_MENU_MNEMONICS}

sdlHintWindowsEnableMessageloop :: String
sdlHintWindowsEnableMessageloop = #{const_str SDL_HINT_WINDOWS_ENABLE_MESSAGELOOP}

sdlHintWindowsGameinput :: String
sdlHintWindowsGameinput = #{const_str SDL_HINT_WINDOWS_GAMEINPUT}

sdlHintWindowsRawKeyboard :: String
sdlHintWindowsRawKeyboard = #{const_str SDL_HINT_WINDOWS_RAW_KEYBOARD}

sdlHintWindowsRawKeyboardExcludeHotkeys :: String
sdlHintWindowsRawKeyboardExcludeHotkeys = #{const_str SDL_HINT_WINDOWS_RAW_KEYBOARD_EXCLUDE_HOTKEYS}

sdlHintWindowsForceSemaphoreKernel :: String
sdlHintWindowsForceSemaphoreKernel = #{const_str SDL_HINT_WINDOWS_FORCE_SEMAPHORE_KERNEL}

sdlHintWindowsIntresourceIcon :: String
sdlHintWindowsIntresourceIcon = #{const_str SDL_HINT_WINDOWS_INTRESOURCE_ICON}

sdlHintWindowsIntresourceIconSmall :: String
sdlHintWindowsIntresourceIconSmall = #{const_str SDL_HINT_WINDOWS_INTRESOURCE_ICON_SMALL}

sdlHintWindowsUseD3d9ex :: String
sdlHintWindowsUseD3d9ex = #{const_str SDL_HINT_WINDOWS_USE_D3D9EX}

sdlHintWindowsEraseBackgroundMode :: String
sdlHintWindowsEraseBackgroundMode = #{const_str SDL_HINT_WINDOWS_ERASE_BACKGROUND_MODE}

sdlHintX11ForceOverrideRedirect :: String
sdlHintX11ForceOverrideRedirect = #{const_str SDL_HINT_X11_FORCE_OVERRIDE_REDIRECT}

sdlHintX11WindowType :: String
sdlHintX11WindowType = #{const_str SDL_HINT_X11_WINDOW_TYPE}

sdlHintX11XcbLibrary :: String
sdlHintX11XcbLibrary = #{const_str SDL_HINT_X11_XCB_LIBRARY}

sdlHintXinputEnabled :: String
sdlHintXinputEnabled = #{const_str SDL_HINT_XINPUT_ENABLED}

sdlHintAssert :: String
sdlHintAssert = #{const_str SDL_HINT_ASSERT}

sdlHintPenMouseEvents :: String
sdlHintPenMouseEvents = #{const_str SDL_HINT_PEN_MOUSE_EVENTS}

sdlHintPenTouchEvents :: String
sdlHintPenTouchEvents = #{const_str SDL_HINT_PEN_TOUCH_EVENTS}
