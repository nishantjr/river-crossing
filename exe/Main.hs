{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS -Wno-unused-top-binds #-} -- Record fields

module Main (main) where

import qualified Data.Map as M
import           WXYZ.River ( cacheRiverState
                            , handleBinding
                            , manageAndRender
                            , shell
                            , wxyz
                            , WXYZConfig(..)
                            , (<||>)
                            )
import           WXYZ.Protocol (mod_mod1)
import           WXYZ.XKB


main :: IO ()
main = wxyz $ WXYZConfig {
                   onRiverEvent = (cacheRiverState <||> manageAndRender <||> handleBinding)
                 , onStartup =    shell "swaybg -c 123123"
                               >> shell "waybar"
                 , keyBindings
                 }
  where
    keyBindings = M.fromList
        [ ((mod_mod1, xkb_key_t),      shell "alacritty")
        ]
