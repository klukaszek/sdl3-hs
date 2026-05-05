module SDL3.Wrapped.Vulkan
  ( VkInstance
  , VkSurfaceKHR
  , VkAllocationCallbacks
  , sdlVulkanLoadLibrary
  , sdlVulkanGetVkGetInstanceProcAddr
  , sdlVulkanUnloadLibrary
  , sdlVulkanGetInstanceExtensions
  , sdlVulkanCreateSurface
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString, packCString)
import Foreign (Ptr, alloca, peek, peekArray)
import Foreign.C.String (withCString)
import Foreign.Marshal.Utils (toBool)
import SDL3.LoadSO (SDLFunctionPointer)
import SDL3.Raw.Vulkan (VkAllocationCallbacks, VkInstance, VkSurfaceKHR)
import qualified SDL3.Raw.Vulkan as Raw
import SDL3.Video (SDLWindow(..))

sdlVulkanLoadLibrary :: MonadIO m => String -> m Bool
sdlVulkanLoadLibrary path =
  liftIO $ withCString path (fmap toBool . Raw.sdlVulkanLoadLibraryRaw)

sdlVulkanGetVkGetInstanceProcAddr :: MonadIO m => m SDLFunctionPointer
sdlVulkanGetVkGetInstanceProcAddr = liftIO Raw.sdlVulkanGetVkGetInstanceProcAddrRaw

sdlVulkanUnloadLibrary :: MonadIO m => m ()
sdlVulkanUnloadLibrary = liftIO Raw.sdlVulkanUnloadLibraryRaw

sdlVulkanGetInstanceExtensions :: MonadIO m => m [ByteString]
sdlVulkanGetInstanceExtensions = liftIO $ alloca $ \pCount -> do
  pArr <- Raw.sdlVulkanGetInstanceExtensionsRaw pCount
  n <- fromIntegral <$> peek pCount
  peekArray n pArr >>= traverse packCString

sdlVulkanCreateSurface ::
  MonadIO m =>
  SDLWindow ->
  VkInstance ->
  Ptr VkAllocationCallbacks ->
  m (Maybe VkSurfaceKHR)
sdlVulkanCreateSurface (SDLWindow winPtr) inst pAllocationCallbacks = liftIO $
  alloca $ \ptrSurfaceKHR -> do
    success <- toBool <$> Raw.sdlVulkanCreateSurfaceRaw winPtr inst pAllocationCallbacks ptrSurfaceKHR
    if success
      then Just <$> peek ptrSurfaceKHR
      else pure Nothing
