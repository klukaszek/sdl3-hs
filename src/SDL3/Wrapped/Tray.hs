module SDL3.Wrapped.Tray
  ( module SDL3.Raw.Tray
  , sdlCreateTray
  , sdlSetTrayIcon
  , sdlSetTrayEntryCallback
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import System.IO.Unsafe (unsafePerformIO)
import SDL3.Raw.Tray hiding (mkTrayCallback, sdlCreateTray, sdlSetTrayIcon, sdlSetTrayEntryCallback)
import qualified SDL3.Raw.Tray as Raw
import SDL3.Surface (SDLSurface, sdlUnsafeToRawSurface)

trayCallbackRefs :: IORef [(SDLTrayEntry, FunPtr Raw.SDLTrayCallback)]
trayCallbackRefs = unsafePerformIO (newIORef [])
{-# NOINLINE trayCallbackRefs #-}

replaceTrayCallback :: SDLTrayEntry -> Maybe (FunPtr Raw.SDLTrayCallback) -> IO [FunPtr Raw.SDLTrayCallback]
replaceTrayCallback entry newCallback =
  atomicModifyIORef' trayCallbackRefs $ \entries ->
    let (matches, rest) = foldr partition ([], []) entries
        updated = maybe rest (\callbackPtr -> (entry, callbackPtr) : rest) newCallback
    in (updated, map snd matches)
  where
    partition pair@(currentEntry, _) (hits, misses)
      | currentEntry == entry = (pair : hits, misses)
      | otherwise = (hits, pair : misses)

sdlCreateTray :: MonadIO m => Maybe SDLSurface -> Maybe String -> m (Maybe SDLTray)
sdlCreateTray maybeSurface tooltip =
  liftIO $ Raw.sdlCreateTray (fmap sdlUnsafeToRawSurface maybeSurface) tooltip

sdlSetTrayIcon :: MonadIO m => SDLTray -> Maybe SDLSurface -> m ()
sdlSetTrayIcon tray maybeSurface =
  liftIO $ Raw.sdlSetTrayIcon tray (fmap sdlUnsafeToRawSurface maybeSurface)

sdlSetTrayEntryCallback :: MonadIO m => SDLTrayEntry -> Maybe (SDLTrayCallback, Ptr ()) -> m ()
sdlSetTrayEntryCallback entry Nothing =
  liftIO $ do
    Raw.sdlSetTrayEntryCallback entry nullFunPtr nullPtr
    oldCallbacks <- replaceTrayCallback entry Nothing
    mapM_ freeHaskellFunPtr oldCallbacks
sdlSetTrayEntryCallback entry (Just (callback, userdata)) =
  liftIO $ do
    callbackFunPtr <- Raw.mkTrayCallback callback
    Raw.sdlSetTrayEntryCallback entry callbackFunPtr userdata
    oldCallbacks <- replaceTrayCallback entry (Just callbackFunPtr)
    mapM_ freeHaskellFunPtr oldCallbacks
