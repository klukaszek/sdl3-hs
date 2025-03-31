# SDL3 Haskell Bindings

NOT STABLE! NOT EVERY FUNCTION IS TESTED IN THE EXAMPLES! MAYBE UNIT TESTS SOME DAY.

## Windows:

Make sure SDL3 compiles a .DLL file, and `pkgconfig` knows that SDL3 exists.

You can do this by setting `PKG_CONFIG_PATH="path\to\SDL3\lib\pkgconfig"`

You'll also wanna add a `cabal.project.local` file to the root directory with

```
package *
  extra-include-dirs: "path/to/SDL3/include/"
  extra-lib-dirs: "path/to/SDL3/lib/"
```

## SDL.h Header File Support Checklist
- [x] SDL3/SDL_stdinc.h
- [x] SDL3/SDL_assert.h
- [x] SDL3/SDL_asyncio.h
- [x] SDL3/SDL_atomic.h
- [ ] SDL3/SDL_audio.h
- [x] SDL3/SDL_bits.h
- [x] SDL3/SDL_blendmode.h
- [x] SDL3/SDL_camera.h
- [x] SDL3/SDL_clipboard.h
- [x] SDL3/SDL_cpuinfo.h
- [x] SDL3/SDL_dialog.h
- [x] SDL3/SDL_endian.h
- [x] SDL3/SDL_error.h
- [x] SDL3/SDL_events.h
- [x] SDL3/SDL_filesystem.h
- [x] SDL3/SDL_gamepad.h
- [ ] SDL3/SDL_gpu.h
- [x] SDL3/SDL_guid.h
- [x] SDL3/SDL_haptic.h
- [x] SDL3/SDL_hidapi.h
- [x] SDL3/SDL_hints.h
- [x] SDL3/SDL_init.h
- [x] SDL3/SDL_iostream.h
- [x] SDL3/SDL_joystick.h
- [x] SDL3/SDL_keyboard.h
- [x] SDL3/SDL_keycode.h
- [x] SDL3/SDL_loadso.h
- [x] SDL3/SDL_locale.h
- [x] SDL3/SDL_log.h
- [x] SDL3/SDL_messagebox.h
- [x] SDL3/SDL_metal.h
- [x] SDL3/SDL_misc.h
- [x] SDL3/SDL_mouse.h
- [x] SDL3/SDL_mutex.h
- [x] SDL3/SDL_pen.h
- [x] SDL3/SDL_pixels.h
- [x] SDL3/SDL_platform.h
- [x] SDL3/SDL_power.h
- [x] SDL3/SDL_process.h
- [x] SDL3/SDL_properties.h
- [x] SDL3/SDL_rect.h
- [ ] SDL3/SDL_render.h
- [x] SDL3/SDL_scancode.h
- [x] SDL3/SDL_sensor.h
- [x] SDL3/SDL_storage.h
- [x] SDL3/SDL_surface.h
- [x] SDL3/SDL_system.h
- [x] SDL3/SDL_thread.h
- [x] SDL3/SDL_time.h
- [x] SDL3/SDL_timer.h
- [x] SDL3/SDL_tray.h
- [x] SDL3/SDL_touch.h
- [x] SDL3/SDL_version.h
- [x] SDL3/SDL_video.h
