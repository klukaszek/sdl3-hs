// cbits/stb_image_wrapper.c
#include "../include/helpers.h"
#define STB_IMAGE_IMPLEMENTATION
#define STBI_MALLOC SDL_malloc
#define STBI_REALLOC SDL_realloc
#define STBI_FREE SDL_free
#define STBI_ONLY_HDR 
#include "../include/stb_image.h"

// Wrapper function that Haskell will call
// It takes the full path directly.
float* hs_stbi_loadf_wrapper(const char* fullPath, int* pWidth, int* pHeight, int* pChannelsInFile, int desiredChannels) {
    return stbi_loadf(fullPath, pWidth, pHeight, pChannelsInFile, desiredChannels);
}
