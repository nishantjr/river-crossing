module Main (main) where

import WXYZ.Wayland

main :: IO ()
main = do display <- wlDisplayConnect
          putStrLn "Connected to the Display."
          success <- awaitRiverProtocols display
          case success of True  -> putStrLn "Good to go."
                          False -> putStrLn "Uh oh. Couldn't obtain river Protocols."
