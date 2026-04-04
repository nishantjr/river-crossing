{-# LANGUAGE CApiFFI #-}

module WXYZ.Wayland
    (wlDisplayConnect)
  where

import           Foreign.C.String (CString)
import           Foreign.Ptr (Ptr, nullPtr)

data CWlDisplay
type WlDisplay = Ptr CWlDisplay

foreign import capi "wayland-client-core.h wl_display_connect"
    _wl_display_connect :: CString -> IO WlDisplay

wlDisplayConnect :: IO WlDisplay
wlDisplayConnect = _wl_display_connect nullPtr
