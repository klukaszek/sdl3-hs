#include <SDL3/SDL.h>

void wrapper_SDL_GUIDToString(Uint8 *guid_data, char *pszGUID, int cbGUID); 
void wrapper_SDL_StringToGUID(const char *pchGUID, Uint8 *guid_data);

int wrapper_SDL_swprintf(wchar_t *dst, size_t maxlen, const wchar_t *fmt, ...);
int wrapper_SDL_vswprintf(wchar_t *dst, size_t maxlen, const wchar_t *fmt, va_list ap);


void wrapper_SDL_RenderDebugTextFormat(SDL_Renderer *renderer, float x, float y, const char *str);

SDL_Thread* wrapper_SDL_CreateThread(SDL_ThreadFunction fn, const char *name, void *data);
SDL_Thread* wrapper_SDL_CreateThreadWithProperties(SDL_PropertiesID props);

size_t wrapper_SDL_IOprintf(SDL_IOStream *context, const char *str);

// Haskell-friendly wrapper for SDL_GetWindowICCProfile
void* wrapper_SDL_GetWindowICCProfile(SDL_Window *window, size_t *size);
