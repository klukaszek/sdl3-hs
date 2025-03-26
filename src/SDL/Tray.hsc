{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SDL.Tray
  ( -- * Types
    SDLTray(..),
    SDLTrayMenu(..),
    SDLTrayEntry(..),
    SDLTrayEntryFlags(..),
    SDLTrayCallback,

    -- * Tray Management
    sdlCreateTray,
    sdlSetTrayIcon,
    sdlSetTrayTooltip,
    sdlDestroyTray,
    sdlUpdateTrays,

    -- * Menu Management
    sdlCreateTrayMenu,
    sdlCreateTraySubmenu,
    sdlGetTrayMenu,
    sdlGetTraySubmenu,
    sdlGetTrayEntries,
    sdlGetTrayEntryParent,
    sdlGetTrayMenuParentEntry,
    sdlGetTrayMenuParentTray,

    -- * Entry Management
    sdlInsertTrayEntryAt,
    sdlRemoveTrayEntry,
    sdlSetTrayEntryLabel,
    sdlGetTrayEntryLabel,
    sdlSetTrayEntryChecked,
    sdlGetTrayEntryChecked,
    sdlSetTrayEntryEnabled,
    sdlGetTrayEntryEnabled,
    sdlSetTrayEntryCallback,
    sdlClickTrayEntry
  ) where

#include <SDL3/SDL_tray.h>

import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types
import Foreign.Ptr (Ptr, nullPtr, FunPtr, nullFunPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray0)
import Foreign.Storable (peek)
import Data.Bits (Bits)
import SDL.Surface (SDLSurface)  -- For type reference, not used directly

newtype SDLTray = SDLTray (Ptr SDLTray) deriving (Show, Eq)
newtype SDLTrayMenu = SDLTrayMenu (Ptr SDLTrayMenu) deriving (Show, Eq)
newtype SDLTrayEntry = SDLTrayEntry (Ptr SDLTrayEntry) deriving (Show, Eq)

newtype SDLTrayEntryFlags = SDLTrayEntryFlags { unSDLTrayEntryFlags :: CUInt }
  deriving (Show, Eq, Bits)

#{enum SDLTrayEntryFlags, SDLTrayEntryFlags
 , sdlTrayEntryButton     = SDL_TRAYENTRY_BUTTON
 , sdlTrayEntryCheckbox   = SDL_TRAYENTRY_CHECKBOX
 , sdlTrayEntrySubmenu    = SDL_TRAYENTRY_SUBMENU
 , sdlTrayEntryDisabled   = SDL_TRAYENTRY_DISABLED
 , sdlTrayEntryChecked    = SDL_TRAYENTRY_CHECKED
 }

type SDLTrayCallback = Ptr () -> SDLTrayEntry -> IO ()

cToBool :: CInt -> Bool
cToBool 0 = False
cToBool _ = True

foreign import ccall "SDL_CreateTray"
  sdlCreateTray_c :: Ptr SDLSurface -> CString -> IO (Ptr SDLTray)

sdlCreateTray :: Maybe (Ptr SDLSurface) -> Maybe String -> IO (Maybe SDLTray)
sdlCreateTray icon tooltip = do
  let iconPtr = maybe nullPtr id icon
  case tooltip of
    Nothing -> do
      ptr <- sdlCreateTray_c iconPtr nullPtr
      return $ if ptr == nullPtr then Nothing else Just (SDLTray ptr)
    Just tt -> withCString tt $ \cTooltip -> do
      ptr <- sdlCreateTray_c iconPtr cTooltip
      return $ if ptr == nullPtr then Nothing else Just (SDLTray ptr)

foreign import ccall "SDL_SetTrayIcon"
  sdlSetTrayIcon_c :: Ptr SDLTray -> Ptr SDLSurface -> IO ()

sdlSetTrayIcon :: SDLTray -> Maybe (Ptr SDLSurface) -> IO ()
sdlSetTrayIcon (SDLTray tray) icon =
  sdlSetTrayIcon_c tray (maybe nullPtr id icon)

foreign import ccall "SDL_SetTrayTooltip"
  sdlSetTrayTooltip_c :: Ptr SDLTray -> CString -> IO ()

sdlSetTrayTooltip :: SDLTray -> Maybe String -> IO ()
sdlSetTrayTooltip (SDLTray tray) tooltip =
  case tooltip of
    Nothing -> sdlSetTrayTooltip_c tray nullPtr
    Just tt -> withCString tt $ sdlSetTrayTooltip_c tray

foreign import ccall "SDL_DestroyTray"
  sdlDestroyTray_c :: Ptr SDLTray -> IO ()

sdlDestroyTray :: SDLTray -> IO ()
sdlDestroyTray (SDLTray tray) = sdlDestroyTray_c tray

foreign import ccall "SDL_CreateTrayMenu"
  sdlCreateTrayMenu_c :: Ptr SDLTray -> IO (Ptr SDLTrayMenu)

sdlCreateTrayMenu :: SDLTray -> IO (Maybe SDLTrayMenu)
sdlCreateTrayMenu (SDLTray tray) = do
  ptr <- sdlCreateTrayMenu_c tray
  return $ if ptr == nullPtr then Nothing else Just (SDLTrayMenu ptr)

foreign import ccall "SDL_CreateTraySubmenu"
  sdlCreateTraySubmenu_c :: Ptr SDLTrayEntry -> IO (Ptr SDLTrayMenu)

sdlCreateTraySubmenu :: SDLTrayEntry -> IO (Maybe SDLTrayMenu)
sdlCreateTraySubmenu (SDLTrayEntry entry) = do
  ptr <- sdlCreateTraySubmenu_c entry
  return $ if ptr == nullPtr then Nothing else Just (SDLTrayMenu ptr)

foreign import ccall "SDL_GetTrayMenu"
  sdlGetTrayMenu_c :: Ptr SDLTray -> IO (Ptr SDLTrayMenu)

sdlGetTrayMenu :: SDLTray -> IO (Maybe SDLTrayMenu)
sdlGetTrayMenu (SDLTray tray) = do
  ptr <- sdlGetTrayMenu_c tray
  return $ if ptr == nullPtr then Nothing else Just (SDLTrayMenu ptr)

foreign import ccall "SDL_GetTraySubmenu"
  sdlGetTraySubmenu_c :: Ptr SDLTrayEntry -> IO (Ptr SDLTrayMenu)

sdlGetTraySubmenu :: SDLTrayEntry -> IO (Maybe SDLTrayMenu)
sdlGetTraySubmenu (SDLTrayEntry entry) = do
  ptr <- sdlGetTraySubmenu_c entry
  return $ if ptr == nullPtr then Nothing else Just (SDLTrayMenu ptr)

foreign import ccall "SDL_GetTrayEntries"
  sdlGetTrayEntries_c :: Ptr SDLTrayMenu -> Ptr CInt -> IO (Ptr (Ptr SDLTrayEntry))

sdlGetTrayEntries :: SDLTrayMenu -> IO [SDLTrayEntry]
sdlGetTrayEntries (SDLTrayMenu menu) = alloca $ \countPtr -> do
  ptr <- sdlGetTrayEntries_c menu countPtr
  count <- fromIntegral <$> peek countPtr
  if ptr == nullPtr
    then return []
    else do
      entries <- peekArray0 nullPtr ptr
      return $ map SDLTrayEntry entries

foreign import ccall "SDL_InsertTrayEntryAt"
  sdlInsertTrayEntryAt_c :: Ptr SDLTrayMenu -> CInt -> CString -> CUInt -> IO (Ptr SDLTrayEntry)

sdlInsertTrayEntryAt :: SDLTrayMenu -> Int -> Maybe String -> SDLTrayEntryFlags -> IO (Maybe SDLTrayEntry)
sdlInsertTrayEntryAt (SDLTrayMenu menu) pos label (SDLTrayEntryFlags flags) =
  case label of
    Nothing -> do
      ptr <- sdlInsertTrayEntryAt_c menu (fromIntegral pos) nullPtr flags
      return $ if ptr == nullPtr then Nothing else Just (SDLTrayEntry ptr)
    Just lbl -> withCString lbl $ \cLabel -> do
      ptr <- sdlInsertTrayEntryAt_c menu (fromIntegral pos) cLabel flags
      return $ if ptr == nullPtr then Nothing else Just (SDLTrayEntry ptr)

foreign import ccall "SDL_RemoveTrayEntry"
  sdlRemoveTrayEntry_c :: Ptr SDLTrayEntry -> IO ()

sdlRemoveTrayEntry :: SDLTrayEntry -> IO ()
sdlRemoveTrayEntry (SDLTrayEntry entry) = sdlRemoveTrayEntry_c entry

foreign import ccall "SDL_SetTrayEntryLabel"
  sdlSetTrayEntryLabel_c :: Ptr SDLTrayEntry -> CString -> IO ()

sdlSetTrayEntryLabel :: SDLTrayEntry -> Maybe String -> IO ()
sdlSetTrayEntryLabel (SDLTrayEntry entry) label =
  case label of
    Nothing -> sdlSetTrayEntryLabel_c entry nullPtr
    Just lbl -> withCString lbl $ sdlSetTrayEntryLabel_c entry

foreign import ccall "SDL_GetTrayEntryLabel"
  sdlGetTrayEntryLabel_c :: Ptr SDLTrayEntry -> IO CString

sdlGetTrayEntryLabel :: SDLTrayEntry -> IO (Maybe String)
sdlGetTrayEntryLabel (SDLTrayEntry entry) = do
  cstr <- sdlGetTrayEntryLabel_c entry
  if cstr == nullPtr
    then return Nothing
    else Just <$> peekCString cstr

foreign import ccall "SDL_SetTrayEntryChecked"
  sdlSetTrayEntryChecked_c :: Ptr SDLTrayEntry -> CInt -> IO ()

sdlSetTrayEntryChecked :: SDLTrayEntry -> Bool -> IO ()
sdlSetTrayEntryChecked (SDLTrayEntry entry) checked =
  sdlSetTrayEntryChecked_c entry (if checked then 1 else 0)

foreign import ccall "SDL_GetTrayEntryChecked"
  sdlGetTrayEntryChecked_c :: Ptr SDLTrayEntry -> IO CInt

sdlGetTrayEntryChecked :: SDLTrayEntry -> IO Bool
sdlGetTrayEntryChecked (SDLTrayEntry entry) = cToBool <$> sdlGetTrayEntryChecked_c entry

foreign import ccall "SDL_SetTrayEntryEnabled"
  sdlSetTrayEntryEnabled_c :: Ptr SDLTrayEntry -> CInt -> IO ()

sdlSetTrayEntryEnabled :: SDLTrayEntry -> Bool -> IO ()
sdlSetTrayEntryEnabled (SDLTrayEntry entry) enabled =
  sdlSetTrayEntryEnabled_c entry (if enabled then 1 else 0)

foreign import ccall "SDL_GetTrayEntryEnabled"
  sdlGetTrayEntryEnabled_c :: Ptr SDLTrayEntry -> IO CInt

sdlGetTrayEntryEnabled :: SDLTrayEntry -> IO Bool
sdlGetTrayEntryEnabled (SDLTrayEntry entry) = cToBool <$> sdlGetTrayEntryEnabled_c entry

foreign import ccall "wrapper"
  mkTrayCallback :: SDLTrayCallback -> IO (FunPtr SDLTrayCallback)

foreign import ccall "SDL_SetTrayEntryCallback"
  sdlSetTrayEntryCallback_c :: Ptr SDLTrayEntry -> FunPtr SDLTrayCallback -> Ptr () -> IO ()

sdlSetTrayEntryCallback :: SDLTrayEntry -> Maybe (SDLTrayCallback, Ptr ()) -> IO ()
sdlSetTrayEntryCallback (SDLTrayEntry entry) Nothing =
  sdlSetTrayEntryCallback_c entry nullFunPtr nullPtr
sdlSetTrayEntryCallback (SDLTrayEntry entry) (Just (cb, userdata)) = do
  cbPtr <- mkTrayCallback cb
  sdlSetTrayEntryCallback_c entry cbPtr userdata

foreign import ccall "SDL_ClickTrayEntry"
  sdlClickTrayEntry_c :: Ptr SDLTrayEntry -> IO ()

sdlClickTrayEntry :: SDLTrayEntry -> IO ()
sdlClickTrayEntry (SDLTrayEntry entry) = sdlClickTrayEntry_c entry

foreign import ccall "SDL_GetTrayEntryParent"
  sdlGetTrayEntryParent_c :: Ptr SDLTrayEntry -> IO (Ptr SDLTrayMenu)

sdlGetTrayEntryParent :: SDLTrayEntry -> IO (Maybe SDLTrayMenu)
sdlGetTrayEntryParent (SDLTrayEntry entry) = do
  ptr <- sdlGetTrayEntryParent_c entry
  return $ if ptr == nullPtr then Nothing else Just (SDLTrayMenu ptr)

foreign import ccall "SDL_GetTrayMenuParentEntry"
  sdlGetTrayMenuParentEntry_c :: Ptr SDLTrayMenu -> IO (Ptr SDLTrayEntry)

sdlGetTrayMenuParentEntry :: SDLTrayMenu -> IO (Maybe SDLTrayEntry)
sdlGetTrayMenuParentEntry (SDLTrayMenu menu) = do
  ptr <- sdlGetTrayMenuParentEntry_c menu
  return $ if ptr == nullPtr then Nothing else Just (SDLTrayEntry ptr)

foreign import ccall "SDL_GetTrayMenuParentTray"
  sdlGetTrayMenuParentTray_c :: Ptr SDLTrayMenu -> IO (Ptr SDLTray)

sdlGetTrayMenuParentTray :: SDLTrayMenu -> IO (Maybe SDLTray)
sdlGetTrayMenuParentTray (SDLTrayMenu menu) = do
  ptr <- sdlGetTrayMenuParentTray_c menu
  return $ if ptr == nullPtr then Nothing else Just (SDLTray ptr)

foreign import ccall "SDL_UpdateTrays"
  sdlUpdateTrays_c :: IO ()

sdlUpdateTrays :: IO ()
sdlUpdateTrays = sdlUpdateTrays_c
