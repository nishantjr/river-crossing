{-# LANGUAGE CApiFFI #-}

module WXYZ.Protocol
    ( wlDisplayConnect
    , awaitRegistry
    , initEventQueue
    , next_event
    , sendRequest
    , getRiverWM
    , riverWMAddEventListeners
    , riverWindowGetNode
    , WlDisplay
    , Event(..)
    , Request(..)
    , RiverWM
    , RiverWindow
    , RiverNode
    , RiverOutput
    , RiverSeat
    )
  where

import           Data.Word
import           Data.Int (Int32)
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
data CRiverNode;    type RiverNode   = Ptr CRiverNode   -- ^ river_node_v1
data CRiverOutput;  type RiverOutput = Ptr CRiverOutput -- ^ river_output_v1
data CRiverSeat;    type RiverSeat   = Ptr CRiverSeat   -- ^ river_seat_v1

-- | Events from the river_window_management_v1 protocol
data Event = WMUnavailable                  RiverWM
           | WMFinished                     RiverWM
           | WMManageStart                  RiverWM
           | WMRenderStart                  RiverWM
           | WMSessionLocked                RiverWM
           | WMSessionUnlocked              RiverWM
           | WMWindow                       RiverWM RiverWindow
           | WMOutput                       RiverWM RiverOutput
           | WMSeat                         RiverWM RiverSeat

           | WindowClosed                   RiverWindow
           | WindowDimensionsHint           RiverWindow
           | WindowDimensions               RiverWindow
           | WindowAppId                    RiverWindow
           | WindowTitle                    RiverWindow
           | WindowParent                   RiverWindow
           | WindowDecorationHint           RiverWindow
           | WindowPointerMoveRequested     RiverWindow
           | WindowPointerResizeRequested   RiverWindow
           | WindowShowWindowMenuRequested  RiverWindow
           | WindowMaximizeRequested        RiverWindow
           | WindowUnmaximizeRequested      RiverWindow
           | WindowFullscreenRequested      RiverWindow
           | WindowExitFullscreenRequested  RiverWindow
           | WindowMinimizeRequested        RiverWindow
           | WindowUnreliablePid            RiverWindow
           | WindowPresentationHint         RiverWindow
           | WindowIdentifier               RiverWindow

           | OutputRemoved                  RiverOutput
           | OutputWlOutput                 RiverOutput Word32
           | OutputPosition                 RiverOutput Int32 Int32
           | OutputDimensions               RiverOutput Int32 Int32

           | SeatRemoved                    RiverSeat

    deriving Show

-- | Requests from the river_window_management_v1 protocol
data Request = WMManageFinish RiverWM
             | WMManageDirty RiverWM
             | WMRenderFinish RiverWM

             | NodeSetPosition RiverNode Word32 Word32

             | WindowProposeDimensions RiverWindow Word32 Word32

#include "cbits/river.h"

foreign import capi "cbits/river.h init_event_queue"
    initEventQueue :: IO ()

foreign import capi "cbits/river.h get_river_window_manager"
    getRiverWM :: IO RiverWM

foreign import capi "cbits/river.h river_wm_add_event_listeners"
    riverWMAddEventListeners :: RiverWM -> IO ()

data CEvent
foreign import capi "cbits/river.h wxyz_next_event"
    _wxyz_next_event :: WlDisplay -> IO (Ptr CEvent)
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
    unparse :: Word8 -> Ptr CEvent -> IO (Maybe Event)
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

    unparse #{const WINDOW_CLOSED} ptr
        = do window <- (#{peek struct wxyz_event, window_closed.window} ptr)
             pure $ Just (WindowClosed window)
    unparse #{const WINDOW_DIMENSIONS_HINT} ptr
        = do window <- (#{peek struct wxyz_event, window_dimensions_hint.window} ptr)
             pure $ Just (WindowDimensionsHint window)
    unparse #{const WINDOW_DIMENSIONS} ptr
        = do window <- (#{peek struct wxyz_event, window_dimensions.window} ptr)
             pure $ Just (WindowDimensions window)
    unparse #{const WINDOW_APP_ID} ptr
        = do window <- (#{peek struct wxyz_event, window_app_id.window} ptr)
             pure $ Just (WindowAppId window)
    unparse #{const WINDOW_TITLE} ptr
        = do window <- (#{peek struct wxyz_event, window_title.window} ptr)
             pure $ Just (WindowTitle window)
    unparse #{const WINDOW_PARENT} ptr
        = do window <- (#{peek struct wxyz_event, window_parent.window} ptr)
             pure $ Just (WindowParent window)
    unparse #{const WINDOW_DECORATION_HINT} ptr
        = do window <- (#{peek struct wxyz_event, window_decoration_hint.window} ptr)
             pure $ Just (WindowDecorationHint window)
    unparse #{const WINDOW_POINTER_MOVE_REQUESTED} ptr
        = do window <- (#{peek struct wxyz_event, window_pointer_move_requested.window} ptr)
             pure $ Just (WindowPointerMoveRequested window)
    unparse #{const WINDOW_POINTER_RESIZE_REQUESTED} ptr
        = do window <- (#{peek struct wxyz_event, window_pointer_resize_requested.window} ptr)
             pure $ Just (WindowPointerResizeRequested window)
    unparse #{const WINDOW_SHOW_WINDOW_MENU_REQUESTED} ptr
        = do window <- (#{peek struct wxyz_event, window_show_window_menu_requested.window} ptr)
             pure $ Just (WindowShowWindowMenuRequested window)
    unparse #{const WINDOW_MAXIMIZE_REQUESTED} ptr
        = do window <- (#{peek struct wxyz_event, window_maximize_requested.window} ptr)
             pure $ Just (WindowMaximizeRequested window)
    unparse #{const WINDOW_UNMAXIMIZE_REQUESTED} ptr
        = do window <- (#{peek struct wxyz_event, window_unmaximize_requested.window} ptr)
             pure $ Just (WindowUnmaximizeRequested window)
    unparse #{const WINDOW_FULLSCREEN_REQUESTED} ptr
        = do window <- (#{peek struct wxyz_event, window_fullscreen_requested.window} ptr)
             pure $ Just (WindowFullscreenRequested window)
    unparse #{const WINDOW_EXIT_FULLSCREEN_REQUESTED} ptr
        = do window <- (#{peek struct wxyz_event, window_exit_fullscreen_requested.window} ptr)
             pure $ Just (WindowExitFullscreenRequested window)
    unparse #{const WINDOW_MINIMIZE_REQUESTED} ptr
        = do window <- (#{peek struct wxyz_event, window_minimize_requested.window} ptr)
             pure $ Just (WindowMinimizeRequested window)
    unparse #{const WINDOW_UNRELIABLE_PID} ptr
        = do window <- (#{peek struct wxyz_event, window_unreliable_pid.window} ptr)
             pure $ Just (WindowUnreliablePid window)
    unparse #{const WINDOW_PRESENTATION_HINT} ptr
        = do window <- (#{peek struct wxyz_event, window_presentation_hint.window} ptr)
             pure $ Just (WindowPresentationHint window)
    unparse #{const WINDOW_IDENTIFIER} ptr
        = do window <- (#{peek struct wxyz_event, window_identifier.window} ptr)
             pure $ Just (WindowIdentifier window)

    unparse #{const OUTPUT_REMOVED} ptr
        = do output <- (#{peek struct wxyz_event, output_removed.output} ptr)
             pure $ Just (OutputRemoved output)
    unparse #{const OUTPUT_WL_OUTPUT} ptr
        = do output <- (#{peek struct wxyz_event, output_wl_output.output} ptr)
             name <- (#{peek struct wxyz_event, output_wl_output.name} ptr)
             pure $ Just (OutputWlOutput output name)
    unparse #{const OUTPUT_POSITION} ptr
        = do output <- (#{peek struct wxyz_event, output_position.output} ptr)
             x <- (#{peek struct wxyz_event, output_position.x} ptr)
             y <- (#{peek struct wxyz_event, output_position.y} ptr)
             pure $ Just (OutputPosition output x y)
    unparse #{const OUTPUT_DIMENSIONS} ptr
        = do output <- (#{peek struct wxyz_event, output_dimensions.output} ptr)
             width <- (#{peek struct wxyz_event, output_dimensions.width} ptr)
             height <- (#{peek struct wxyz_event, output_dimensions.height} ptr)
             pure $ Just (OutputDimensions output width height)

    unparse e _
        = error $ "Unknown event type: " ++ (show e)


foreign import capi "river-window-management-v1-client.h river_window_manager_v1_manage_finish"
    _river_window_manager_v1_manage_finish :: RiverWM -> IO ()
foreign import capi "river-window-management-v1-client.h river_window_manager_v1_manage_dirty"
    _river_window_manager_v1_manage_dirty :: RiverWM -> IO ()
foreign import capi "river-window-management-v1-client.h river_window_manager_v1_render_finish"
    _river_window_manager_v1_render_finish :: RiverWM -> IO ()

foreign import capi "river-window-management-v1-client.h river_node_v1_set_position"
    _river_node_v1_set_position :: RiverNode -> Word32 -> Word32 -> IO ()
foreign import capi "river-window-management-v1-client.h river_window_v1_propose_dimensions"
    _river_window_v1_propose_dimensions:: RiverWindow -> Word32 -> Word32 -> IO ()

sendRequest :: WlDisplay -> Request -> IO ()
sendRequest _dpy request = case request of
    (WMManageFinish rwm)    -> _river_window_manager_v1_manage_finish rwm
    (WMManageDirty rwm)     -> _river_window_manager_v1_manage_dirty rwm
    (WMRenderFinish rwm)    -> _river_window_manager_v1_render_finish rwm

    (NodeSetPosition node x y) ->  _river_node_v1_set_position node x y

    (WindowProposeDimensions window w h) ->  _river_window_v1_propose_dimensions window w h


-- | Requests that have `new_id` are a bit weird, because their not purely a request.
-- They also need to generate a fresh id on the client before sending the request.
-- it is therefore stateful. We'll need a Haskell scanner to generate a clean
-- interface for this.
foreign import capi "river-window-management-v1-client.h river_window_v1_get_node"
    riverWindowGetNode :: RiverWindow -> RiverNode

