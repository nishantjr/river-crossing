module WXYZ.XKB
  where

import           Data.Word (Word32)

type KeySym = Word32

#include "xkbcommon/xkbcommon-keysyms.h"

xkb_key_h :: KeySym
xkb_key_h =  #{const XKB_KEY_h}

xkb_key_j :: KeySym
xkb_key_j =  #{const XKB_KEY_j}

xkb_key_k :: KeySym
xkb_key_k =  #{const XKB_KEY_k}

xkb_key_t :: KeySym
xkb_key_t =  #{const XKB_KEY_t}

xkb_key_d :: KeySym
xkb_key_d =  #{const XKB_KEY_d}

xkb_key_f :: KeySym
xkb_key_f =  #{const XKB_KEY_f}

xkb_key_Tab :: KeySym
xkb_key_Tab =  #{const XKB_KEY_Tab}
