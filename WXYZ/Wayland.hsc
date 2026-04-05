{-# LANGUAGE CApiFFI #-}

module WXYZ.Wayland
    (wlDisplayConnect)
  where

import           Foreign.C.String (CString)
import           Foreign.Ptr (Ptr, nullPtr)

data CWlDisplay
type WlDisplay = Ptr CWlDisplay

wlDisplayConnect :: IO WlDisplay
-- ^ Connect to the wayland display with default socket paths.
-- We do not yet allow for specifying the path to the socket.
wlDisplayConnect = _wl_display_connect nullPtr

foreign import capi "wayland-client-core.h wl_display_connect"
    _wl_display_connect :: CString -> IO WlDisplay

