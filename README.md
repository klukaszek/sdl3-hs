# SDL3 Haskell Bindings λ

Haskell bindings for SDL3.

## Project Status

- Targets SDL3 `3.4.0` headers.
- Includes `59` runnable examples (via Cabal `examples` flag).
- Binding status is auto-generated in this README.

## Quick Start

### 1) Clone

```bash
git clone --recurse-submodules https://github.com/klukaszek/sdl3-hs.git
cd sdl3-hs
```

If you already cloned without submodules:

```bash
git submodule update --init --recursive
```

### 2) Install SDL3 (if needed)

If your system already provides SDL3 development files, use that.
Otherwise build from the bundled `SDL3/` submodule:

```bash
cd SDL3
mkdir -p build && cd build
cmake -DCMAKE_BUILD_TYPE=Release -GNinja ..
cmake --build . --config Release --parallel
sudo cmake --install . --config Release   # omit sudo on Windows
```

On Windows, ensure `SDL3.dll` is available in `PATH` (or next to your exe).
If `pkg-config` cannot find SDL3, set:

```bash
set PKG_CONFIG_PATH=path\to\SDL3\lib\pkgconfig
# or
$env:PKG_CONFIG_PATH="path\to\SDL3\lib\pkgconfig"
```

### 3) Build

```bash
cabal build sdl3
```

## Examples

Build all examples:

```bash
cabal build -fexamples
```

List example executables:

```bash
cabal run -fexamples
```

Run a specific example:

```bash
cabal run -fexamples init
```

## Build Flags

- `examples`: builds all example executables (`cabal build -fexamples`)
- `static-linking`: static SDL linking where supported (`cabal build -fstatic-linking`)
- `pkgconfig` (default on): use `pkg-config` for SDL3 discovery

Static linking is not supported on macOS. See `DISTRIBUTION.md` for full
distribution guidance and platform details.

Refresh the generated binding-status block in this README:

```bash
./.github/update-readme.sh
```

# Working Examples

All current examples are listed below (`59` total), and each one maps to a
buildable executable in `sdl3.cabal`.

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

These examples are based off of the original
[SDL3 GPU Examples](https://github.com/TheSpydog/SDL_gpu_examples/)

- [GPU Triangle](examples/GPURawTriangleExample.hs) - Raw triangle rendering
- [GPU Clear](examples/GPUClearExample.hs) - Basic screen clearing
- [GPU Vertex Buffer](examples/GPUVertexBufferExample.hs) - Vertex buffer usage
  (matches `gpu-vbuf`)
- [GPU Textured Quad](examples/GPUTexturedQuadExample.hs) - Texture mapping
- [GPU Custom Sampling](examples/GPUCustomSamplingExample.hs) - Implementing
  custom texture sampling logic
- [GPU Animated Quad](examples/GPUAnimatedQuadExample.hs) - Animation basics
- [GPU Instanced](examples/GPUInstancedExample.hs) - Instanced rendering
- [GPU Stencil](examples/GPUStencilExample.hs) - Stencil buffer operations
- [GPU Cull](examples/GPUCullExample.hs) - Culling techniques
- [GPU Multi-Window](examples/GPUMultiWindowExample.hs) - Multiple window
  rendering
- [GPU Draw Indirect](examples/GPUDrawIndirectExample.hs) - Leveraging indirect
  draw commands for GPU-driven rendering
- [GPU Basic Compute](examples/GPUBasicComputeExample.hs) - Basic compute shader
  usage
- [GPU Clear 3D Slice](examples/GPUClear3DSliceExample.hs) - Clearing a slice of
  a 3D texture
- [GPU Compute Uniforms](examples/GPUComputeUniformsExample.hs) - Using uniforms
  with compute shaders
- [GPU Compute Sampler](examples/GPUComputeSamplerExample.hs) - Using samplers
  within compute shaders
- [GPU Compute Tonemapping](examples/GPUTonemappingExample.hs) - Using compute
  pipelines for HDRI texture tonemapping.
- [GPU Copy Consistency](examples/GPUCopyConsistencyExample.hs) - Copy a texture
  to the GPU, alter it, and draw it to the framebuffer.
- [GPU Copy & Readback](examples/GPUCopyAndReadbackExample.hs) - Write texture
  to GPU and read it back, verifying data integrity.
- [GPU Texture 2D Array](examples/GPUTexture2DArrayExample.hs) - Rendering with
  2D texture arrays
- [GPU Texture Type Test](examples/GPUTextureTypeTestExample.hs) - Testing
  various texture types and formats
- [GPU Compressed Textures](examples/GPUCompressedTexturesExample.hs) - BCn and
  ASTC texture loading
- [GPU Generate Mipmaps](examples/GPUGenerateMipmapsExample.hs) - On-the-fly
  mipmap generation
- [GPU Blit 2D Array](examples/GPUBlit2DArrayExample.hs) - Blitting 2D texture
  array layers
- [GPU Blit Cube](examples/GPUBlitCubeExample.hs) - Blitting cubemap faces
- [GPU Blit Mirror](examples/GPUBlitMirrorExample.hs) - Mirroring blit
  operations
- [GPU Latency](examples/GPULatencyExample.hs) - Measuring and managing
  input-to-display latency
- [GPU Window Resize](examples/GPUWindowResizeExample.hs) - Efficiently handling
  window resizing with GPU resources
- [GPU Triangle MSAA](examples/GPUTriangleMSAAExample.hs) - Multi-sample
  anti-aliasing basics
- [GPU Cubemap](examples/GPUCubemapExample.hs) - Cubemap rendering and skybox
- [GPU Depth Sampler](examples/GPUDepthSamplerExample.hs) - Depth buffer
  sampling and post-processing
- [GPU Compute Sprite Batch](examples/GPUComputeSpriteBatchExample.hs) -
  GPU-driven sprite batching with compute
- [GPU Pull Sprite Batch](examples/GPUPullSpriteBatchExample.hs) - Pull-based
  vertex shader sprite batching

## Input & Interaction

### User Input

- [Events](examples/EventsExample.hs) - Event handling
- [Gamepad](examples/GamepadExample.hs) - Controller input
- [Touch Device](examples/TouchDeviceExample.hs) - Touch input handling
- [Haptic](examples/HapticExample.hs) - Force feedback

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

- [Camera](examples/CameraExample.hs) - Camera device access (tested on macOS)

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

- [Sensor](examples/SensorExample.hs) - Hardware sensor access

> Note: touch/haptic/sensor examples are harder to validate in CI and may need real hardware for full runtime verification.

<!-- BEGIN_BINDING_STATUS -->

## Binding Status

### Summary
- **Total Modules**: 55
- **Modules with Bindings**: 55
- **Complete Bindings**: 55
- **Missing Functions**: 0
- **Completion Rate**: 100%

### Non-complete Modules
- None

### Notes
- Generated from `src/SDL3` against headers in `SDL3/include/SDL3`.
- Missing function details are written by `binding-checker` to `broken/`.

<!-- END_BINDING_STATUS -->
