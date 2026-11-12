module Main (main) where

import qualified Data.Map as M

import WXYZ.River
    ( focusNext, focusPrev, withFocused
    , toggleInformFullscreen, sendMessage, shell
    , wxyz , WXYZConfig(..)
    )
import WXYZ.Layout.Core as Core
import WXYZ.Protocol (mod_mod1)
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
        [ ((a,  xkb_key_t),      const $ shell "alacritty")
        , ((a,  xkb_key_d),      const $ shell "bemenu-run")

        , ((a,  xkb_key_Tab),    const $ sendMessage Core.NextLayout)
        , ((a,  xkb_key_j),      focusNext)
        , ((a,  xkb_key_k),      focusPrev)

        , ((a,  xkb_key_f),      withFocused toggleInformFullscreen)
        ]
    a = mod_mod1
    myLayout = tiled ||| Core.Mirror tiled ||| Core.full
      where
        tiled   = Core.Tall nmaster delta ratio
        nmaster = 1      -- Default number of windows in the master pane
        ratio   = 1/2    -- Default proportion of screen occupied by master pane
        delta   = 3/100  -- Percent of screen to increment by when resizing panes

