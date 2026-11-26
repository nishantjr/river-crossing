module Main (main) where

import qualified Data.Map as M
import Data.Bits ((.|.))

import Crossing.River
    ( focusNext, focusPrev, withFocused
    , toggleInformFullscreen, sendMessage, shell
    , crossing , CrossingConfig(..)
    , Rectangle(..)
    )
import Crossing.Layout.Core as Core
import Crossing.Layout.Float (float, toggleFloating)
import Crossing.Layout.Workspaces (shift, view, workspaces)
import Crossing.Protocol (mod_mod1, mod_shift)
import Crossing.XKB

main :: IO ()
main = crossing $ CrossingConfig {
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

        , ((a_, xkb_key_space), withFocused $ toggleFloating (Rectangle 0 0 100 100))

        , ((a_, xkb_key_1),     view 1)
        , ((a_, xkb_key_2),     view 2)
        , ((a_, xkb_key_3),     view 3)

        , ((as, xkb_key_1),     shift 1)
        , ((as, xkb_key_2),     shift 2)
        , ((as, xkb_key_3),     shift 3)

        ]
    _s = mod_shift
    a_ = mod_mod1
    as = a_ .|. _s
    myLayout = float $ workspaces [1..3] (tiled ||| Core.full)
    tiled   = Core.Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes


