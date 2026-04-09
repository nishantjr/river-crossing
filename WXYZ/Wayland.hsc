{-# LANGUAGE CApiFFI #-}

module WXYZ.Wayland
    ( wlDisplayConnect
    , awaitRegistry
    , initEventQueue
    , next_event
    , Event(..)
    , getRiverWM
    , riverWMAddEventListeners
    )
  where

import           Data.Word
import           Foreign.C.String (CString)
import           Foreign.C.Types (CBool(..))
import           Foreign.Marshal.Alloc (free)
import           Foreign.Ptr (Ptr, nullPtr)
import           Foreign.Storable

data CWlDisplay
type WlDisplay = Ptr CWlDisplay

wlDisplayConnect :: IO (Maybe WlDisplay)
-- ^ Connect to the wayland display with default socket paths.
-- We do not yet allow for specifying the path to the socket.
wlDisplayConnect = do
    display <- _wl_display_connect nullPtr
    if (display == nullPtr)
    then pure Nothing
    else pure $ Just display

foreign import capi "wayland-client-core.h wl_display_connect"
    _wl_display_connect :: CString -> IO WlDisplay


awaitRegistry :: WlDisplay -> IO Bool
-- ^ Given a connection to a Wayland display, obtain handles to the river
-- protocols needed. These are stored as globals on the C-side, since we want a
-- higher-level representation on the Haskell side---i.e. as an event stream rather
-- than callbacks.

awaitRegistry dpy =
    do cbool <- _await_registry dpy
       putStrLn $ show cbool
       pure $ cbool /= 0

foreign import capi "cbits/river.h await_registry"
    _await_registry :: WlDisplay -> IO CBool

data CRiverWM;      type RiverWM     = Ptr CRiverWM     -- ^ river_window_manager_v1
data CRiverWindow;  type RiverWindow = Ptr CRiverWindow -- ^ river_window_v1
data CRiverOutput;  type RiverOutput = Ptr CRiverOutput -- ^ river_output_v1
data CRiverSeat;    type RiverSeat   = Ptr CRiverSeat   -- ^ river_seat_v1

data Event = WMUnavailable
           | WMFinished
           | WMManageStart
           | WMRenderStart
           | WMSessionLocked
           | WMSessionUnlocked
           | WMWindow RiverWindow
           | WMOutput RiverOutput
           | WMSeat RiverSeat
    deriving Show


#include "cbits/river.h"

foreign import capi "cbits/river.h init_event_queue"
    initEventQueue :: IO ()

foreign import capi "cbits/river.h get_river_window_manager"
    getRiverWM :: IO RiverWM

foreign import capi "cbits/river.h river_wm_add_event_listeners"
    riverWMAddEventListeners :: RiverWM -> IO ()

foreign import capi "cbits/river.h wxyz_next_event"
    _wxyz_next_event :: WlDisplay -> IO (Ptr Event)
next_event :: WlDisplay -> IO (Maybe Event)
next_event display =
  do ptr <- _wxyz_next_event display
     if (ptr == nullPtr)
     then pure Nothing
     else do ty <- #{peek struct wxyz_event, type} ptr
             unparsed <- unparse ty ptr
             free ptr
             print unparsed
             pure unparsed
  where
    unparse :: Word8 -> Ptr Event -> IO (Maybe Event)
    unparse #{const WM_UNAVAILABLE} _      = pure $ Just WMUnavailable
    unparse #{const WM_FINISHED} _         = pure $ Just WMFinished
    unparse #{const WM_MANAGE_START} _     = pure $ Just WMManageStart
    unparse #{const WM_RENDER_START} _     = pure $ Just WMRenderStart
    unparse #{const WM_SESSION_LOCKED} _   = pure $ Just WMSessionLocked
    unparse #{const WM_SESSION_UNLOCKED} _ = pure $ Just WMSessionUnlocked

    unparse #{const WM_WINDOW} ptr
        = do window <- (#{peek struct wxyz_event, wm_window.window} ptr)
             pure $ Just (WMWindow window)
    unparse #{const WM_OUTPUT} ptr
        = do output <- (#{peek struct wxyz_event, wm_output.output} ptr)
             pure $ Just (WMOutput output)
    unparse #{const WM_SEAT} ptr
        = do seat <- (#{peek struct wxyz_event, wm_seat.seat} ptr)
             pure $ Just (WMSeat seat)

    unparse e _
        = error $ "Unknown event type: " ++ (show e)

