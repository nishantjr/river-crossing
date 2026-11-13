module Main (main) where

import qualified Data.Map as M
import Data.Bits ((.|.))

import WXYZ.River
    ( focusNext, focusPrev, withFocused
    , toggleInformFullscreen, sendMessage, shell
    , wxyz , WXYZConfig(..)
    )
import WXYZ.Layout.Core as Core
import WXYZ.Protocol (mod_mod1, mod_shift)
import WXYZ.XKB

main :: IO ()
main = wxyz $ WXYZConfig {
                   layout = myLayout
                 , onStartup =    shell "swaybg -c 123123"
                               >> shell "waybar"
                 , keyBindings
                 }
  where
    keyBindings = M.fromList
        [ ((a_, xkb_key_t),     const $ shell "alacritty")
        , ((a_, xkb_key_d),     const $ shell "bemenu-run")

        , ((a_, xkb_key_j),     focusNext)
        , ((a_, xkb_key_k),     focusPrev)

        , ((a_, xkb_key_f),     const $ sendMessage Core.NextLayout)
        , ((as, xkb_key_f),     withFocused toggleInformFullscreen)
        ]
    _s = mod_shift
    a_ = mod_mod1
    as = a_ .|. _s
    myLayout = tiled ||| Core.full
      where
        tiled   = Core.Tall nmaster delta ratio
        nmaster = 1      -- Default number of windows in the master pane
        ratio   = 1/2    -- Default proportion of screen occupied by master pane
        delta   = 3/100  -- Percent of screen to increment by when resizing panes

