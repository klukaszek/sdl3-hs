-- SDL/Vulkan.hsc
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BlockArguments #-}

{-|
Module      : SDL.Vulkan
Description : Bindings to Vulkan surfaces in SDL
License     : BSD3

This module provides bindings for creating Vulkan surfaces on SDL windows.
-}
#include <SDL3/SDL_vulkan.h>

module SDL.Vulkan
  ( -- * Functions
    -- ** Surface Creation and Destruction
    sdlVulkanLoadLibrary
  , sdlVulkanUnloadLibrary
  , sdlVulkanGetInstanceExtensions
  , sdlVulkanCreateSurface
  ) where

import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C (CBool(..))
import Foreign.Marshal.Utils (toBool)
import Foreign.Ptr (FunPtr, castFunPtr)
import Foreign (Ptr, Word32, Word64, nullFunPtr, alloca, peek, peekArray)
import SDL.LoadSO (SDLFunctionPointer)
import SDL.Video (SDLWindow(..))
import Data.ByteString (ByteString, packCString)

foreign import ccall unsafe "SDL_Vulkan_LoadLibrary"
  c_sdlVulkanLoadLibrary :: CString -> IO CBool

-- | Dynamically load the Vulkan loader library.
sdlVulkanLoadLibrary :: String -> IO Bool
sdlVulkanLoadLibrary path = withCString path \cstr ->
  fromCBool <$> c_sdlVulkanLoadLibrary cstr

type VkInstance = Ptr ()
type VkSurfaceKHR = Word64
data VkAllocationCallbacks

foreign import ccall unsafe "SDL_Vulkan_UnloadLibrary"
  c_sdlVulkanUnloadLibrary :: IO ()

-- | Unload the Vulkan library previously loaded by sdlVulkanLoadLibrary.
sdlVulkanUnloadLibrary :: IO ()
sdlVulkanUnloadLibrary = c_sdlVulkanUnloadLibrary

foreign import ccall unsafe "SDL_Vulkan_GetInstanceExtensions"
  c_sdlVulkanGetInstanceExtensions :: Ptr Word32 -> IO (Ptr CString)

-- | Get the Vulkan instance extensions needed for vkCreateInstance.
sdlVulkanGetInstanceExtensions :: IO [ByteString]
sdlVulkanGetInstanceExtensions = do
  alloca \pCount -> do
    pArr <- c_sdlVulkanGetInstanceExtensions pCount
    n    <- fromIntegral <$> peek pCount
    peekArray n pArr >>= traverse packCString

foreign import ccall unsafe "SDL_Vulkan_CreateSurface"
  c_sdlVulkanCreateSurface :: Ptr SDLWindow -> VkInstance -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO CBool

-- | Create a Vulkan surface for a SDLWindow.
sdlVulkanCreateSurface :: SDLWindow -> VkInstance -> Ptr VkAllocationCallbacks -> IO (Maybe VkSurfaceKHR)
sdlVulkanCreateSurface (SDLWindow winPtr) inst pAllocationCallbacks = do
  alloca \ptrSurfaceKHR -> do
    success <- toBool <$> c_sdlVulkanCreateSurface winPtr inst pAllocationCallbacks ptrSurfaceKHR
    if success
    then Just <$> peek ptrSurfaceKHR
    else pure Nothing

fromCBool :: CBool -> Bool
fromCBool = toBool
