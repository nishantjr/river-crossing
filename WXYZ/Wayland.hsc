{-# LANGUAGE CApiFFI #-}

module WXYZ.Wayland
    (wlDisplayConnect, awaitRiverProtocols)
  where

import           Foreign.C.String (CString)
import           Foreign.C.Types (CBool(..))
import           Foreign.Ptr (Ptr, nullPtr)

data CWlDisplay
type WlDisplay = Ptr CWlDisplay

wlDisplayConnect :: IO WlDisplay
-- ^ Connect to the wayland display with default socket paths.
-- We do not yet allow for specifying the path to the socket.
wlDisplayConnect = _wl_display_connect nullPtr

foreign import capi "wayland-client-core.h wl_display_connect"
    _wl_display_connect :: CString -> IO WlDisplay


awaitRiverProtocols :: WlDisplay -> IO Bool
-- ^ Given a connection to a Wayland display, obtain handles to the river
-- protocols needed. These are stored as globals on the C-side, since we want a
-- higher-level representation on the Haskell side---i.e. as an event stream rather
-- than callbacks.

awaitRiverProtocols dpy = do cbool <- _await_river_protocols dpy
                             putStrLn $ show cbool
                             pure $ cbool /= 0

foreign import capi "cbits/river.h await_river_protocols"
    _await_river_protocols :: WlDisplay -> IO CBool
