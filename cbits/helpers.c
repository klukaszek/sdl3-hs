#include "../include/helpers.h"

void wrapper_SDL_GUIDToString(Uint8 *guid_data, char *pszGUID, int cbGUID) {
  SDL_GUID guid;
  memcpy(guid.data, guid_data, 16);
  SDL_GUIDToString(guid, pszGUID, cbGUID);
}

void wrapper_SDL_StringToGUID(const char *pchGUID, Uint8 *guid_data) {
  SDL_GUID guid = SDL_StringToGUID(pchGUID);
  memcpy(guid_data, guid.data, 16);
}
