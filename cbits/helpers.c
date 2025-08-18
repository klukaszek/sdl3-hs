#include "../include/helpers.h"
#include <SDL3/SDL_video.h>
#include <stdlib.h>
#include <string.h>

void wrapper_SDL_GUIDToString(Uint8 *guid_data, char *pszGUID, int cbGUID) {
  SDL_GUID guid;
  memcpy(guid.data, guid_data, 16);
  SDL_GUIDToString(guid, pszGUID, cbGUID);
}

size_t wrapper_SDL_IOprintf(SDL_IOStream *context, const char *str) {
    return SDL_IOprintf(context, "%s", str);
}

void wrapper_SDL_StringToGUID(const char *pchGUID, Uint8 *guid_data) {
  SDL_GUID guid = SDL_StringToGUID(pchGUID);
  memcpy(guid_data, guid.data, 16);
}

// Haskell-friendly wrapper for SDL_RenderDebugTextFormat
void wrapper_SDL_RenderDebugTextFormat(SDL_Renderer *renderer, float x, float y, const char *str) {
    SDL_RenderDebugTextFormat(renderer, x, y, "%s", str);
}

// Thread creation wrappers for Haskell FFI
#include <SDL3/SDL_thread.h>

SDL_Thread* wrapper_SDL_CreateThread(SDL_ThreadFunction fn, const char *name, void *data) {
    return SDL_CreateThread(fn, name, data);
}

SDL_Thread* wrapper_SDL_CreateThreadWithProperties(SDL_PropertiesID props) {
    return SDL_CreateThreadWithProperties(props);
}

// Wide-char formatting wrappers for Haskell FFI
#include <stdarg.h>
#include <wchar.h>

int wrapper_SDL_swprintf(wchar_t *dst, size_t maxlen, const wchar_t *fmt, ...) {
    va_list ap;
    int result;
    va_start(ap, fmt);
    result = SDL_vswprintf(dst, maxlen, fmt, ap);
    va_end(ap);
    return result;
}

int wrapper_SDL_vswprintf(wchar_t *dst, size_t maxlen, const wchar_t *fmt, va_list ap) {
    return SDL_vswprintf(dst, maxlen, fmt, ap);
}

// Haskell-friendly wrapper for SDL_GetWindowICCProfile
// Allocates a buffer, copies the ICC profile, and returns it with the size.
void* wrapper_SDL_GetWindowICCProfile(SDL_Window *window, size_t *size) {
    void *profile = SDL_GetWindowICCProfile(window, size);
    if (!profile || !size || *size == 0) {
        return NULL;
    }
    void *copy = malloc(*size);
    if (!copy) {
        return NULL;
    }
    memcpy(copy, profile, *size);
    // SDL docs: the returned pointer from SDL_GetWindowICCProfile must be freed with SDL_free
    SDL_free(profile);
    return copy;
}

// Wrappers for macros in SDL_Atomic
void wrapper_SDL_CompilerBarrier(void) { SDL_CompilerBarrier(); }
void wrapper_SDL_MemoryBarrierAcquire(void) { SDL_MemoryBarrierAcquire(); }
void wrapper_SDL_MemoryBarrierRelease(void) { SDL_MemoryBarrierRelease(); }
void wrapper_SDL_CPUPauseInstruction(void) { SDL_CPUPauseInstruction(); }
