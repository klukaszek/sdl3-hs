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

## SDL.h Header File Support Checklist

🟧 means that the related header file should be implemented for the most part. Some things might be missing or non-functioning, though I have tried to be careful.

| Header File | Status | Notes |
|-------------|--------|-------|
| SDL3/SDL_stdinc.h | 🟧 | |
| SDL3/SDL_assert.h | 🟧 | |
| SDL3/SDL_asyncio.h | 🟧 | |
| SDL3/SDL_atomic.h | 🟧 | |
| SDL3/SDL_audio.h | 🟧 | |
| SDL3/SDL_bits.h | 🟧 | |
| SDL3/SDL_blendmode.h | 🟧 | |
| SDL3/SDL_camera.h | 🟧 | |
| SDL3/SDL_clipboard.h | 🟧 | |
| SDL3/SDL_cpuinfo.h | 🟧 | |
| SDL3/SDL_dialog.h | 🟧 | Example stopped working, must investigate |
| SDL3/SDL_endian.h | 🟧 | |
| SDL3/SDL_error.h | 🟧 | |
| SDL3/SDL_events.h | 🟧 | |
| SDL3/SDL_filesystem.h | 🟧 | |
| SDL3/SDL_gamepad.h | 🟧 | |
| SDL3/SDL_gpu.h | 🟧 | |
| SDL3/SDL_guid.h | 🟧 | |
| SDL3/SDL_haptic.h | 🟧 | |
| SDL3/SDL_hidapi.h | 🟧 | |
| SDL3/SDL_hints.h | 🟧 | |
| SDL3/SDL_init.h | 🟧 | This entire file should be refactored. It was one of the first. |
| SDL3/SDL_iostream.h | 🟧 | |
| SDL3/SDL_joystick.h | 🟧 | |
| SDL3/SDL_keyboard.h | 🟧 | |
| SDL3/SDL_keycode.h | 🟧 | |
| SDL3/SDL_loadso.h | 🟧 | |
| SDL3/SDL_locale.h | 🟧 | |
| SDL3/SDL_log.h | 🟧 | |
| SDL3/SDL_messagebox.h | 🟧 | |
| SDL3/SDL_metal.h | 🟧 | |
| SDL3/SDL_misc.h | 🟧 | |
| SDL3/SDL_mouse.h | 🟧 | |
| SDL3/SDL_mutex.h | 🟧 | |
| SDL3/SDL_pen.h | 🟧 | |
| SDL3/SDL_pixels.h | 🟧 | |
| SDL3/SDL_platform.h | 🟧 | |
| SDL3/SDL_power.h | 🟧 | |
| SDL3/SDL_process.h | 🟧 | |
| SDL3/SDL_properties.h | 🟧 | |
| SDL3/SDL_rect.h | 🟧 | |
| SDL3/SDL_render.h | 🟧 | |
| SDL3/SDL_scancode.h | 🟧 | |
| SDL3/SDL_sensor.h | 🟧 | |
| SDL3/SDL_storage.h | 🟧 | |
| SDL3/SDL_surface.h | 🟧 | |
| SDL3/SDL_system.h | 🟧 | |
| SDL3/SDL_thread.h | 🟧 | |
| SDL3/SDL_time.h | 🟧 | |
| SDL3/SDL_timer.h | 🟧 | |
| SDL3/SDL_tray.h | 🟧 | |
| SDL3/SDL_touch.h | 🟧 | |
| SDL3/SDL_version.h | 🟧 | |
| SDL3/SDL_video.h | 🟧 | |

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
