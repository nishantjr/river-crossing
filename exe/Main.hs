{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS -Wno-unused-top-binds #-} -- Record fields

module Main (main) where

import qualified Data.Map as M

import           WXYZ.River (shell , wxyz , WXYZConfig(..))
import           WXYZ.Protocol (mod_mod1)
import           WXYZ.XKB

import qualified XMonad.Layout as L
import           XMonad.Layout ((|||))

main :: IO ()
main = wxyz $ WXYZConfig {
                   layout = myLayout
                 , onStartup =    shell "swaybg -c 123123"
                               >> shell "waybar"
                 , keyBindings
                 }
  where
    keyBindings = M.fromList
        [ ((mod_mod1, xkb_key_t),      shell "alacritty")
        , ((mod_mod1, xkb_key_d),      shell "bemenu-run")
        ]
    myLayout = tiled ---  ||| L.Mirror tiled ||| L.Full
      where
        tiled   = L.Tall nmaster delta ratio
        nmaster = 1      -- Default number of windows in the master pane
        ratio   = 1/2    -- Default proportion of screen occupied by master pane
        delta   = 3/100  -- Percent of screen to increment by when resizing panes

