-- SDL/Tray.hsc
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : SDL.Tray
-- Description : SDL system tray notification area functions
-- Copyright   : Kyle Lukaszek, 2025
-- License     : BSD3
--
-- This module provides bindings to the SDL3 system tray API, allowing applications
-- to create and manage icons and menus in the system notification area (often called the "system tray").

module SDL.Tray
  ( -- * Types
    SDLTray(..)
  , SDLTrayMenu(..)
  , SDLTrayEntry(..)
  , SDLTrayEntryFlags(..)
  , SDLTrayCallback

    -- * Patterns / Constants
  , pattern SDL_TRAYENTRY_BUTTON
  , pattern SDL_TRAYENTRY_CHECKBOX
  , pattern SDL_TRAYENTRY_SUBMENU
  , pattern SDL_TRAYENTRY_DISABLED
  , pattern SDL_TRAYENTRY_CHECKED

    -- * Tray Management
  , sdlCreateTray
  , sdlSetTrayIcon
  , sdlSetTrayTooltip
  , sdlDestroyTray
  , sdlUpdateTrays

    -- * Menu Management
  , sdlCreateTrayMenu
  , sdlCreateTraySubmenu
  , sdlGetTrayMenu
  , sdlGetTraySubmenu
  , sdlGetTrayEntries
  , sdlGetTrayEntryParent
  , sdlGetTrayMenuParentEntry
  , sdlGetTrayMenuParentTray

    -- * Entry Management
  , sdlInsertTrayEntryAt
  , sdlRemoveTrayEntry
  , sdlSetTrayEntryLabel
  , sdlGetTrayEntryLabel
  , sdlSetTrayEntryChecked
  , sdlGetTrayEntryChecked
  , sdlSetTrayEntryEnabled
  , sdlGetTrayEntryEnabled
  , sdlSetTrayEntryCallback
  , sdlClickTrayEntry
  , sdlIsTraySupported

  ) where

#include <SDL3/SDL_tray.h>

import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types
import Foreign.Ptr (castPtr, Ptr, nullPtr, FunPtr, nullFunPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray) -- Use peekArray instead of peekArray0 for counted arrays
import Foreign.Storable (Storable, peek)
import Foreign.Marshal.Utils (maybeWith, with, toBool) -- Added toBool
import Data.Bits (Bits, (.|.), zeroBits) -- Added zeroBits
import SDL.Surface (SDLSurface(..)) -- SDLSurface is the data type, we need Ptr SDLSurface

-- Opaque C struct types
data SDL_Tray
data SDL_TrayMenu
data SDL_TrayEntry
data SDL_Surface

-- | Opaque handle for a system tray icon instance.
newtype SDLTray = SDLTray (Ptr SDL_Tray) deriving (Show, Eq)
-- | Opaque handle for a menu associated with a tray icon or submenu entry.
newtype SDLTrayMenu = SDLTrayMenu (Ptr SDL_TrayMenu) deriving (Show, Eq)
-- | Opaque handle for an entry within a system tray menu.
newtype SDLTrayEntry = SDLTrayEntry (Ptr SDL_TrayEntry) deriving (Show, Eq)

-- | Flags defining the type and state of a tray menu entry.
newtype SDLTrayEntryFlags = SDLTrayEntryFlags CUInt
  deriving newtype (Show, Eq, Bits, Num, Storable) -- Added Num for zeroBits

pattern SDL_TRAYENTRY_BUTTON :: SDLTrayEntryFlags
pattern SDL_TRAYENTRY_BUTTON   = SDLTrayEntryFlags #{const SDL_TRAYENTRY_BUTTON}
pattern SDL_TRAYENTRY_CHECKBOX :: SDLTrayEntryFlags
pattern SDL_TRAYENTRY_CHECKBOX = SDLTrayEntryFlags #{const SDL_TRAYENTRY_CHECKBOX}
pattern SDL_TRAYENTRY_SUBMENU :: SDLTrayEntryFlags
pattern SDL_TRAYENTRY_SUBMENU  = SDLTrayEntryFlags #{const SDL_TRAYENTRY_SUBMENU}
pattern SDL_TRAYENTRY_DISABLED :: SDLTrayEntryFlags
pattern SDL_TRAYENTRY_DISABLED = SDLTrayEntryFlags #{const SDL_TRAYENTRY_DISABLED}
pattern SDL_TRAYENTRY_CHECKED :: SDLTrayEntryFlags
pattern SDL_TRAYENTRY_CHECKED  = SDLTrayEntryFlags #{const SDL_TRAYENTRY_CHECKED}

-- | Callback function prototype for when a tray menu entry is clicked.
--   Arguments: userdata pointer, SDLTrayEntry handle.
type SDLTrayCallback = Ptr () -> SDLTrayEntry -> IO ()

-- Wrapper for C boolean results (assuming 1=true, 0=false)
cIntToBool :: CInt -> Bool
cIntToBool 0 = False
cIntToBool _ = True

-- * Tray Management

-- | Returns True if system tray is supported on this platform.
foreign import ccall unsafe "SDL_IsTraySupported"
  c_sdlIsTraySupported :: IO CBool

sdlIsTraySupported :: IO Bool
sdlIsTraySupported = toBool <$> c_sdlIsTraySupported

-- | Creates a new system tray icon instance.
foreign import ccall unsafe "SDL_CreateTray"
  c_sdlCreateTray :: Ptr SDL_Surface -> CString -> IO (Ptr SDL_Tray)

sdlCreateTray :: Maybe (Ptr SDLSurface) -> Maybe String -> IO (Maybe SDLTray)
sdlCreateTray mIcon mTooltip = do
  ptr <- case mIcon of
           Nothing -> maybeWith withCString mTooltip $ \cTooltip ->
                        c_sdlCreateTray nullPtr cTooltip
           Just iconPtr -> maybeWith withCString mTooltip $ \cTooltip ->
                             -- Cast the Ptr SDLSurface to Ptr SDL_Surface for C function
                             c_sdlCreateTray (castPtr iconPtr) cTooltip
  pure $ if ptr == nullPtr then Nothing else Just (SDLTray ptr)


-- | Sets the icon for a system tray instance. Pass Nothing to remove the icon.
foreign import ccall unsafe "SDL_SetTrayIcon"
  c_sdlSetTrayIcon :: Ptr SDL_Tray -> Ptr SDL_Surface -> IO ()

sdlSetTrayIcon :: SDLTray -> Maybe (Ptr SDLSurface) -> IO ()
sdlSetTrayIcon (SDLTray trayPtr) mIcon =
  case mIcon of
    Nothing -> c_sdlSetTrayIcon trayPtr nullPtr
    Just iconPtr -> -- Cast the Ptr SDLSurface to Ptr SDL_Surface for C function
                    c_sdlSetTrayIcon trayPtr (castPtr iconPtr)

-- | Sets the tooltip (hover text) for a system tray icon. Pass Nothing to remove tooltip.
foreign import ccall unsafe "SDL_SetTrayTooltip"
  c_sdlSetTrayTooltip :: Ptr SDL_Tray -> CString -> IO ()

sdlSetTrayTooltip :: SDLTray -> Maybe String -> IO ()
sdlSetTrayTooltip (SDLTray trayPtr) mTooltip =
  maybeWith withCString mTooltip $ \cTooltip ->
    c_sdlSetTrayTooltip trayPtr cTooltip

-- | Destroys a system tray icon instance and its associated menus/entries.
foreign import ccall unsafe "SDL_DestroyTray"
  c_sdlDestroyTray :: Ptr SDL_Tray -> IO ()

sdlDestroyTray :: SDLTray -> IO ()
sdlDestroyTray (SDLTray trayPtr) = c_sdlDestroyTray trayPtr

-- | Update system tray items. Required after making changes on some platforms.
foreign import ccall unsafe "SDL_UpdateTrays"
  c_sdlUpdateTrays :: IO ()

sdlUpdateTrays :: IO ()
sdlUpdateTrays = c_sdlUpdateTrays

-- * Menu Management

-- | Creates the main menu for a system tray icon. Only one main menu per tray.
foreign import ccall unsafe "SDL_CreateTrayMenu"
  c_sdlCreateTrayMenu :: Ptr SDL_Tray -> IO (Ptr SDL_TrayMenu)

sdlCreateTrayMenu :: SDLTray -> IO (Maybe SDLTrayMenu)
sdlCreateTrayMenu (SDLTray trayPtr) = do
  ptr <- c_sdlCreateTrayMenu trayPtr
  pure $ if ptr == nullPtr then Nothing else Just (SDLTrayMenu ptr)

-- | Creates a submenu attached to a specific menu entry (must have SDL_TRAYENTRY_SUBMENU flag).
foreign import ccall unsafe "SDL_CreateTraySubmenu"
  c_sdlCreateTraySubmenu :: Ptr SDL_TrayEntry -> IO (Ptr SDL_TrayMenu)

sdlCreateTraySubmenu :: SDLTrayEntry -> IO (Maybe SDLTrayMenu)
sdlCreateTraySubmenu (SDLTrayEntry entryPtr) = do
  ptr <- c_sdlCreateTraySubmenu entryPtr
  pure $ if ptr == nullPtr then Nothing else Just (SDLTrayMenu ptr)

-- | Gets the main menu associated with a system tray icon.
foreign import ccall unsafe "SDL_GetTrayMenu"
  c_sdlGetTrayMenu :: Ptr SDL_Tray -> IO (Ptr SDL_TrayMenu)

sdlGetTrayMenu :: SDLTray -> IO (Maybe SDLTrayMenu)
sdlGetTrayMenu (SDLTray trayPtr) = do
  ptr <- c_sdlGetTrayMenu trayPtr
  pure $ if ptr == nullPtr then Nothing else Just (SDLTrayMenu ptr)

-- | Gets the submenu associated with a menu entry (if it has one).
foreign import ccall unsafe "SDL_GetTraySubmenu"
  c_sdlGetTraySubmenu :: Ptr SDL_TrayEntry -> IO (Ptr SDL_TrayMenu)

sdlGetTraySubmenu :: SDLTrayEntry -> IO (Maybe SDLTrayMenu)
sdlGetTraySubmenu (SDLTrayEntry entryPtr) = do
  ptr <- c_sdlGetTraySubmenu entryPtr
  pure $ if ptr == nullPtr then Nothing else Just (SDLTrayMenu ptr)

-- | Gets a list of all entries currently in a menu.
foreign import ccall unsafe "SDL_GetTrayEntries"
  c_sdlGetTrayEntries :: Ptr SDL_TrayMenu -> Ptr CInt -> IO (Ptr (Ptr SDL_TrayEntry))

sdlGetTrayEntries :: SDLTrayMenu -> IO [SDLTrayEntry]
sdlGetTrayEntries (SDLTrayMenu menuPtr) = alloca $ \countPtr -> do
  entryPtrArray <- c_sdlGetTrayEntries menuPtr countPtr
  count <- fromIntegral <$> peek countPtr
  if entryPtrArray == nullPtr || count == 0
    then return []
    else do
      -- Use peekArray which takes the count directly
      entryPtrs <- peekArray count entryPtrArray
      -- Wrap pointers in the newtype
      return $ map SDLTrayEntry entryPtrs

-- | Gets the parent menu that contains a specific menu entry.
foreign import ccall unsafe "SDL_GetTrayEntryParent"
  c_sdlGetTrayEntryParent :: Ptr SDL_TrayEntry -> IO (Ptr SDL_TrayMenu)

sdlGetTrayEntryParent :: SDLTrayEntry -> IO (Maybe SDLTrayMenu)
sdlGetTrayEntryParent (SDLTrayEntry entryPtr) = do
  ptr <- c_sdlGetTrayEntryParent entryPtr
  pure $ if ptr == nullPtr then Nothing else Just (SDLTrayMenu ptr)

-- | Gets the parent menu entry that owns a specific submenu.
foreign import ccall unsafe "SDL_GetTrayMenuParentEntry"
  c_sdlGetTrayMenuParentEntry :: Ptr SDL_TrayMenu -> IO (Ptr SDL_TrayEntry)

sdlGetTrayMenuParentEntry :: SDLTrayMenu -> IO (Maybe SDLTrayEntry)
sdlGetTrayMenuParentEntry (SDLTrayMenu menuPtr) = do
  ptr <- c_sdlGetTrayMenuParentEntry menuPtr
  pure $ if ptr == nullPtr then Nothing else Just (SDLTrayEntry ptr)

-- | Gets the parent tray icon instance that owns a menu (either main or submenu).
foreign import ccall unsafe "SDL_GetTrayMenuParentTray"
  c_sdlGetTrayMenuParentTray :: Ptr SDL_TrayMenu -> IO (Ptr SDL_Tray)

sdlGetTrayMenuParentTray :: SDLTrayMenu -> IO (Maybe SDLTray)
sdlGetTrayMenuParentTray (SDLTrayMenu menuPtr) = do
  ptr <- c_sdlGetTrayMenuParentTray menuPtr
  pure $ if ptr == nullPtr then Nothing else Just (SDLTray ptr)

-- * Entry Management

-- | Inserts a new entry into a menu at a specific position.
foreign import ccall unsafe "SDL_InsertTrayEntryAt"
  c_sdlInsertTrayEntryAt :: Ptr SDL_TrayMenu -> CInt -> CString -> CUInt -> IO (Ptr SDL_TrayEntry)

sdlInsertTrayEntryAt :: SDLTrayMenu -> Int -> Maybe String -> SDLTrayEntryFlags -> IO (Maybe SDLTrayEntry)
sdlInsertTrayEntryAt (SDLTrayMenu menuPtr) pos mLabel (SDLTrayEntryFlags flags) = do
  ptr <- maybeWith withCString mLabel $ \cLabel ->
           c_sdlInsertTrayEntryAt menuPtr (fromIntegral pos) cLabel flags
  pure $ if ptr == nullPtr then Nothing else Just (SDLTrayEntry ptr)

-- | Removes a specific entry from its parent menu.
foreign import ccall unsafe "SDL_RemoveTrayEntry"
  c_sdlRemoveTrayEntry :: Ptr SDL_TrayEntry -> IO ()

sdlRemoveTrayEntry :: SDLTrayEntry -> IO ()
sdlRemoveTrayEntry (SDLTrayEntry entryPtr) = c_sdlRemoveTrayEntry entryPtr

-- | Sets the text label for a menu entry. Pass Nothing to remove label.
foreign import ccall unsafe "SDL_SetTrayEntryLabel"
  c_sdlSetTrayEntryLabel :: Ptr SDL_TrayEntry -> CString -> IO ()

sdlSetTrayEntryLabel :: SDLTrayEntry -> Maybe String -> IO ()
sdlSetTrayEntryLabel (SDLTrayEntry entryPtr) mLabel =
  maybeWith withCString mLabel $ \cLabel ->
    c_sdlSetTrayEntryLabel entryPtr cLabel

-- | Gets the text label of a menu entry.
foreign import ccall unsafe "SDL_GetTrayEntryLabel"
  c_sdlGetTrayEntryLabel :: Ptr SDL_TrayEntry -> IO CString

sdlGetTrayEntryLabel :: SDLTrayEntry -> IO (Maybe String)
sdlGetTrayEntryLabel (SDLTrayEntry entryPtr) = do
  cStr <- c_sdlGetTrayEntryLabel entryPtr
  if cStr == nullPtr
    then return Nothing
    else Just <$> peekCString cStr

-- | Sets the checked state for a checkbox menu entry.
foreign import ccall unsafe "SDL_SetTrayEntryChecked"
  c_sdlSetTrayEntryChecked :: Ptr SDL_TrayEntry -> CBool -> IO ()

sdlSetTrayEntryChecked :: SDLTrayEntry -> Bool -> IO ()
sdlSetTrayEntryChecked (SDLTrayEntry entryPtr) checked =
  -- Use CBool for clarity if available, otherwise CInt
  c_sdlSetTrayEntryChecked entryPtr (fromIntegral $ fromEnum checked)

-- | Gets the checked state of a checkbox menu entry.
foreign import ccall unsafe "SDL_GetTrayEntryChecked"
  c_sdlGetTrayEntryChecked :: Ptr SDL_TrayEntry -> IO CBool

sdlGetTrayEntryChecked :: SDLTrayEntry -> IO Bool
sdlGetTrayEntryChecked (SDLTrayEntry entryPtr) = toBool <$> c_sdlGetTrayEntryChecked entryPtr

-- | Sets the enabled/disabled state of a menu entry.
foreign import ccall unsafe "SDL_SetTrayEntryEnabled"
  c_sdlSetTrayEntryEnabled :: Ptr SDL_TrayEntry -> CBool -> IO ()

sdlSetTrayEntryEnabled :: SDLTrayEntry -> Bool -> IO ()
sdlSetTrayEntryEnabled (SDLTrayEntry entryPtr) enabled =
  c_sdlSetTrayEntryEnabled entryPtr (fromIntegral $ fromEnum enabled)

-- | Gets the enabled/disabled state of a menu entry.
foreign import ccall unsafe "SDL_GetTrayEntryEnabled"
  c_sdlGetTrayEntryEnabled :: Ptr SDL_TrayEntry -> IO CBool

sdlGetTrayEntryEnabled :: SDLTrayEntry -> IO Bool
sdlGetTrayEntryEnabled (SDLTrayEntry entryPtr) = toBool <$> c_sdlGetTrayEntryEnabled entryPtr

-- | Dynamically create a wrapper for the Haskell callback function.
foreign import ccall "wrapper"
  mkTrayCallback :: SDLTrayCallback -> IO (FunPtr SDLTrayCallback)

-- | Set the callback function and userdata for a menu entry.
foreign import ccall unsafe "SDL_SetTrayEntryCallback"
  c_sdlSetTrayEntryCallback :: Ptr SDL_TrayEntry -> FunPtr SDLTrayCallback -> Ptr () -> IO ()

sdlSetTrayEntryCallback :: SDLTrayEntry -> Maybe (SDLTrayCallback, Ptr ()) -> IO ()
sdlSetTrayEntryCallback (SDLTrayEntry entryPtr) Nothing =
  c_sdlSetTrayEntryCallback entryPtr nullFunPtr nullPtr
sdlSetTrayEntryCallback (SDLTrayEntry entryPtr) (Just (callback, userdata)) = do
  -- Create the function pointer from the Haskell function
  callbackFunPtr <- mkTrayCallback callback
  c_sdlSetTrayEntryCallback entryPtr callbackFunPtr userdata

-- | Programmatically clicks a menu entry, triggering its callback.
foreign import ccall unsafe "SDL_ClickTrayEntry"
  c_sdlClickTrayEntry :: Ptr SDL_TrayEntry -> IO ()

sdlClickTrayEntry :: SDLTrayEntry -> IO ()
sdlClickTrayEntry (SDLTrayEntry entryPtr) = c_sdlClickTrayEntry entryPtr
