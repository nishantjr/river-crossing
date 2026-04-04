module Main (main) where

import WXYZ.Wayland

main :: IO ()
main = do display <- wlDisplayConnect
          print display
          putStrLn "Hello World!"
