# SDL3 Haskell Bindings λ

Work in progress bindings for the SDL3 library written in Haskell.

Please report any bugs or suggestions you have for the bindings!

## Setup Instructions

::: {.note}
**NOTE:** These bindings are for SDL version 3.3 as of right now.
:::

Install SDL3 from source: [SDL GitHub](https://github.com/libsdl-org/SDL/)

### Linux
To build SDL3 from source on Linux:

```bash
# Navigate to SDL3 source directory
cd path/to/SDL3/source

# Create and enter build directory
mkdir build
cd build

# Configure with cmake.
# CMAKE_BUILD_TYPE can be whatever you need it to be.
cmake -DCMAKE_BUILD_TYPE=Release -GNinja ..

# Build
cmake --build . --config Release --parallel

# Install (requires sudo)
sudo cmake --install . --config Release
```
This will install SDL3 to your system so that cabal can find and build the sdl3 package.

Find your way back to this project's directory and run `cabal build` to build the package with examples, or `cabal build sdl3` to build just the package.

### Windows

> **Recommendation:** Use the Developer PowerShell or Developer Command Prompt that comes with Visual Studio for best results.

```bash
# Navigate to SDL3 source directory
cd path/to/SDL3/source

# Create and enter build directory
mkdir build
cd build

# Configure with cmake.
# CMAKE_BUILD_TYPE can be whatever you need it to be.
cmake -DCMAKE_BUILD_TYPE=Release -GNinja ..

# Build
cmake --build . --config Release --parallel

# Install (requires sudo)
cmake --install . --config Release
```

Make sure SDL3 compiles a .DLL file, and `pkgconfig` knows that SDL3 exists. (You might have to install `pkgconfig` using your pacman of choice.)

You can do this by setting:
```bash
set PKG_CONFIG_PATH="path\to\SDL3\lib\pkgconfig"
# or
$env:PKG_CONFIG_PATH="path\to\SDL3\lib\pkgconfig\"
```

Ensure the .DLL file is present in your `PATH` environment variable or within the root directory for this project.

You'll also want to add a `cabal.project.local` file to the root directory with:

```
package *
  extra-include-dirs: "path/to/SDL3/include/"
  extra-lib-dirs: "path/to/SDL3/lib/"
```

Once all of this is done, you can find your way into this directory and run `cabal build sdl3`. If you would like to install the examples, run `cabal build` instead.

# Working Examples

## Core Functionality
### Initialization & System
- [Init](examples/InitExample.hs) - Basic SDL initialization
- [System](examples/SystemExample.hs) - System information and capabilities
- [Platform](examples/PlatformExample.hs) - Platform-specific functionality
- [CPU Info](examples/CPUInfoExample.hs) - Processor information
- [Power](examples/PowerExample.hs) - Power state monitoring
- [Hints](examples/HintsExample.hs) - SDL configuration hints

## Window & Rendering
### Rendering
- [Render](examples/RenderExample.hs) - 2D rendering basics
### Misc
- [Rect](examples/RectExample.hs) - Rectangle basics (no rendering here)

### GPU & Graphics
These examples are based off of the original [SDL3 GPU Examples](https://github.com/TheSpydog/SDL_gpu_examples/)
- [GPU Triangle](examples/GPURawTriangleExample.hs) - Raw triangle rendering
- [GPU Clear](examples/GPUClearExample.hs) - Basic screen clearing
- [GPU Vertex Buffer](examples/GPUVertexBufferExample.hs) - Vertex buffer usage (matches `gpu-vbuf`)
- [GPU Textured Quad](examples/GPUTexturedQuadExample.hs) - Texture mapping
- [GPU Custom Sampling](examples/GPUCustomSamplingExample.hs) - Implementing custom texture sampling logic
- [GPU Animated Quad](examples/GPUAnimatedQuadExample.hs) - Animation basics
- [GPU Instanced](examples/GPUInstancedExample.hs) - Instanced rendering
- [GPU Stencil](examples/GPUStencilExample.hs) - Stencil buffer operations
- [GPU Cull](examples/GPUCullExample.hs) - Culling techniques
- [GPU Multi-Window](examples/GPUMultiWindowExample.hs) - Multiple window rendering
- [GPU Draw Indirect](examples/GPUDrawIndirectExample.hs) - Leveraging indirect draw commands for GPU-driven rendering
- [GPU Basic Compute](examples/GPUBasicComputeExample.hs) - Basic compute shader usage
- [GPU Clear 3D Slice](examples/GPUClear3DSliceExample.hs) - Clearing a slice of a 3D texture
- [GPU Compute Uniforms](examples/GPUComputeUniformsExample.hs) - Using uniforms with compute shaders
- [GPU Compute Sampler](examples/GPUComputeSamplerExample.hs) - Using samplers within compute shaders
- [GPU Compute Tonemapping](examples/GPUTonemappingExample.hs) - Using compute pipelines for HDRI texture tonemapping.
- [GPU Copy Consistency](examples/GPUCopyConsistencyExample.hs) - Copy a texture to the GPU, alter it, and draw it to the framebuffer.
- [GPU Copy & Readback](examples/GPUCopyAndReadbackExample.hs) - Write texture to GPU and read it back, verifying data integrity.
- (... More coming ...)

## Input & Interaction
### User Input
- [Events](examples/EventsExample.hs) - Event handling
- [Gamepad](examples/GamepadExample.hs) - Controller input
- [Touch Device](examples/TouchDeviceExample.hs) - Touch input handling (Can't test properly but compiles.)
- [Haptic](examples/HapticExample.hs) - Force feedback (Can't test properly but compiles.)

### User Interface
- [Dialog](examples/DialogExample.hs) - Dialog boxes
- [Message Box](examples/MessageBoxExample.hs) - Simple message dialogs
- [Clipboard](examples/ClipboardExample.hs) - Clipboard manipulation
- [Tray](examples/TrayExample.hs) - System tray integration

## Audio & Media
### Audio
- [Audio](examples/AudioExample.hs) - Audio playback
- [WAV](examples/WAVExample.hs) - WAV file handling

### Camera
- [Camera](examples/CameraExample.hs) - Camera device access (Can't test properly but compiles.)

## System Integration
### File & Data
- [Filesystem](examples/FilesystemExample.hs) - File system operations
- [Storage](examples/StorageExample.hs) - Persistent storage
- [GUID](examples/GUIDExample.hs) - Globally unique identifier handling

### Internationalization
- [Locale](examples/LocaleExample.hs) - Localization support

## Time & Process Management
### Time
- [Time](examples/TimeExample.hs) - Time functions
- [Timer](examples/TimerExample.hs) - Timer functionality

### Process
- [Process](examples/ProcessExample.hs) - Process management

## Sensors
### Hardware
- [Sensor](examples/SensorExample.hs) - Hardware sensor access (Can't test properly but compiles.)

## 📊 Binding Status

*Last updated: 2025-07-03 05:29 UTC*

### Summary
- **Total Modules**: 54
- **Modules with Bindings**: 46
- **Complete Bindings**: 39
- **Missing Functions**: 133
- **Completion Rate**: 84%

### Status by Module

| Module | Status |
|--------|--------|
| `SDL_assert` | ✅ Complete |
| `SDL_asyncio` | ❌ No bindings |
| `SDL_atomic` | ✅ Complete |
| `SDL_audio` | ⚠️ 3 missing |
| `SDL_bits` | ✅ Complete |
| `SDL_blendmode` | ❌ No bindings |
| `SDL_camera` | ✅ Complete |
| `SDL_clipboard` | ✅ Complete |
| `SDL_cpuinfo` | ❌ No bindings |
| `SDL_dialog` | ✅ Complete |
| `SDL_endian` | ✅ Complete |
| `SDL_error` | ✅ Complete |
| `SDL_events` | ⚠️ 1 missing |
| `SDL_filesystem` | ✅ Complete |
| `SDL_gamepad` | ✅ Complete |
| `SDL_gpu` | ❌ No bindings |
| `SDL_guid` | ❌ No bindings |
| `SDL_haptic` | ✅ Complete |
| `SDL_hidapi` | ✅ Complete |
| `SDL_hints` | ✅ Complete |
| `SDL_init` | ✅ Complete |
| `SDL_iostream` | ❌ No bindings |
| `SDL_joystick` | ✅ Complete |
| `SDL_keyboard` | ✅ Complete |
| `SDL_keycode` | ✅ Complete |
| `SDL_loadso` | ❌ No bindings |
| `SDL_locale` | ✅ Complete |
| `SDL_log` | ✅ Complete |
| `SDL_messagebox` | ❌ No bindings |
| `SDL_metal` | ✅ Complete |
| `SDL_misc` | ✅ Complete |
| `SDL_mouse` | ✅ Complete |
| `SDL_mutex` | ✅ Complete |
| `SDL_pen` | ✅ Complete |
| `SDL_pixels` | ✅ Complete |
| `SDL_platform` | ✅ Complete |
| `SDL_power` | ✅ Complete |
| `SDL_process` | ✅ Complete |
| `SDL_properties` | ✅ Complete |
| `SDL_rect` | ✅ Complete |
| `SDL_render` | ⚠️ 1 missing |
| `SDL_scancode` | ✅ Complete |
| `SDL_sensor` | ✅ Complete |
| `SDL_stdinc` | ⚠️ 2 missing |
| `SDL_storage` | ✅ Complete |
| `SDL_surface` | ⚠️ 35 missing |
| `SDL_system` | ✅ Complete |
| `SDL_thread` | ⚠️ 2 missing |
| `SDL_time` | ✅ Complete |
| `SDL_timer` | ✅ Complete |
| `SDL_touch` | ✅ Complete |
| `SDL_tray` | ✅ Complete |
| `SDL_version` | ✅ Complete |
| `SDL_video` | ⚠️ 89 missing |

### Legend
- ✅ **Complete**: All functions from the header are bound
- ⚠️ **X missing**: Header has bindings but X functions are missing
- ❌ **No bindings**: No Haskell bindings exist for this header
- ❓ **Unknown**: Status could not be determined

### Notes
- Status reflects core SDL3 headers (test/internal headers excluded)
- Missing function details are available in the `broken/` directory after running the binding checker
- Use `./check-sdl-bindings -i` for interactive binding status checking
- Some modules may intentionally have no bindings if not applicable to Haskell
