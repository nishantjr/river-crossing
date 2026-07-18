module WXYZ.XKB
  where

import           Data.Word (Word32)

type KeySym = Word32

#include "xkbcommon/xkbcommon-keysyms.h"

xkb_key_t :: KeySym
xkb_key_t =  #{const XKB_KEY_t}

xkb_key_d :: KeySym
xkb_key_d =  #{const XKB_KEY_d}
