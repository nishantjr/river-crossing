{-# LANGUAGE CApiFFI #-}

module WXYZ.Wayland
    ( wlDisplayConnect
    , awaitRegistry
    , initEventQueue
    , next_event
    , sendRequest
    , getRiverWM
    , riverWMAddEventListeners
    , WlDisplay
    , Event(..)
    , Request(..)
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

-- | Events from the river_window_management_v1 protocol
data Event = WMUnavailable RiverWM
           | WMFinished RiverWM
           | WMManageStart RiverWM
           | WMRenderStart RiverWM
           | WMSessionLocked RiverWM
           | WMSessionUnlocked RiverWM
           | WMWindow RiverWM RiverWindow
           | WMOutput RiverWM RiverOutput
           | WMSeat RiverWM RiverSeat
    deriving Show

-- | Requests from the river_window_management_v1 protocol
data Request = WMManageFinish RiverWM
             | WMManageDirty RiverWM
             | WMRenderFinish RiverWM

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
    unparse #{const WM_UNAVAILABLE} ptr
        = do wm <- (#{peek struct wxyz_event, wm_unavailable.river_wm} ptr)
             pure $ Just (WMUnavailable wm)
    unparse #{const WM_FINISHED} ptr
        = do wm <- (#{peek struct wxyz_event, wm_finished.river_wm} ptr)
             pure $ Just (WMFinished wm)
    unparse #{const WM_MANAGE_START} ptr
        = do wm <- (#{peek struct wxyz_event, wm_manage_start.river_wm} ptr)
             pure $ Just (WMManageStart wm)
    unparse #{const WM_RENDER_START} ptr
        = do wm <- (#{peek struct wxyz_event, wm_render_start.river_wm} ptr)
             pure $ Just (WMRenderStart wm)
    unparse #{const WM_SESSION_LOCKED} ptr
        = do wm <- (#{peek struct wxyz_event, wm_session_locked.river_wm} ptr)
             pure $ Just (WMSessionLocked wm)
    unparse #{const WM_SESSION_UNLOCKED} ptr
        = do wm <- (#{peek struct wxyz_event, wm_session_unlocked.river_wm} ptr)
             pure $ Just (WMSessionUnlocked wm)
    unparse #{const WM_WINDOW} ptr
        = do wm <- (#{peek struct wxyz_event, wm_window.river_wm} ptr)
             window <- (#{peek struct wxyz_event, wm_window.window} ptr)
             pure $ Just (WMWindow wm window)
    unparse #{const WM_OUTPUT} ptr
        = do wm <- (#{peek struct wxyz_event, wm_output.river_wm} ptr)
             output <- (#{peek struct wxyz_event, wm_output.output} ptr)
             pure $ Just (WMOutput wm output)
    unparse #{const WM_SEAT} ptr
        = do wm <- (#{peek struct wxyz_event, wm_seat.river_wm} ptr)
             seat <- (#{peek struct wxyz_event, wm_seat.seat} ptr)
             pure $ Just (WMSeat wm seat)

    unparse e _
        = error $ "Unknown event type: " ++ (show e)


foreign import capi "river-window-management-v1-client.h river_window_manager_v1_manage_finish"
    _river_window_manager_v1_manage_finish :: RiverWM -> IO ()
foreign import capi "river-window-management-v1-client.h river_window_manager_v1_manage_dirty"
    _river_window_manager_v1_manage_dirty :: RiverWM -> IO ()
foreign import capi "river-window-management-v1-client.h river_window_manager_v1_render_finish"
    _river_window_manager_v1_render_finish :: RiverWM -> IO ()



sendRequest :: WlDisplay -> Request -> IO ()
-- ^ Send a request to River
sendRequest dpy request = case request of
    (WMManageFinish rwm)    -> _river_window_manager_v1_manage_finish rwm
    (WMManageDirty rwm)     -> _river_window_manager_v1_manage_dirty rwm
    (WMRenderFinish rwm)    -> _river_window_manager_v1_render_finish rwm

