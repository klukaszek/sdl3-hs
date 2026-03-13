{-# LANGUAGE ForeignFunctionInterface #-}

module SDL3.Raw.Vulkan
  ( VkInstance
  , VkSurfaceKHR
  , VkAllocationCallbacks
  , sdlVulkanLoadLibraryRaw
  , sdlVulkanGetVkGetInstanceProcAddrRaw
  , sdlVulkanUnloadLibraryRaw
  , sdlVulkanGetInstanceExtensionsRaw
  , sdlVulkanCreateSurfaceRaw
  ) where

import Foreign (Ptr, Word32, Word64)
import Foreign.C (CBool(..))
import Foreign.C.String (CString)
import SDL3.Raw.LoadSO (SDLFunctionPointer)
import SDL3.Raw.Video (SDLWindow)

type VkInstance = Ptr ()

type VkSurfaceKHR = Word64

data VkAllocationCallbacks

foreign import ccall unsafe "SDL_Vulkan_LoadLibrary"
  sdlVulkanLoadLibraryRaw :: CString -> IO CBool

foreign import ccall unsafe "SDL_Vulkan_GetVkGetInstanceProcAddr"
  sdlVulkanGetVkGetInstanceProcAddrRaw :: IO SDLFunctionPointer

foreign import ccall unsafe "SDL_Vulkan_UnloadLibrary"
  sdlVulkanUnloadLibraryRaw :: IO ()

foreign import ccall unsafe "SDL_Vulkan_GetInstanceExtensions"
  sdlVulkanGetInstanceExtensionsRaw :: Ptr Word32 -> IO (Ptr CString)

foreign import ccall unsafe "SDL_Vulkan_CreateSurface"
  sdlVulkanCreateSurfaceRaw ::
    Ptr SDLWindow ->
    VkInstance ->
    Ptr VkAllocationCallbacks ->
    Ptr VkSurfaceKHR ->
    IO CBool
