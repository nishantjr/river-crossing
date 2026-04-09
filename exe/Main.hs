module Main (main) where

import WXYZ.Wayland

main :: IO ()
main = do Just display <- wlDisplayConnect -- TODO: This should print an
                                           -- error message.
          initEventQueue
          putStrLn "Connected to the Display."
          True <- awaitRegistry display -- This prints a log message internally,
                                        -- and isn't likely to happen except for
                                        -- defective window managers. So it is
                                        -- OK to rely on matching failure.
          riverWM <- getRiverWM
          riverWMAddEventListeners riverWM
          event <- next_event display
          print (Just event)
