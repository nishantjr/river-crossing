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
          eventLoop display

eventLoop :: WlDisplay -> IO ()
eventLoop display =
    do e <- next_event display
       case e of
         Nothing -> pure ()
         Just e' -> do requests <- handleEvent e'
                       _ <- mapM (sendRequest display) requests
                       eventLoop display

handleEvent :: Event -> IO [Request]
handleEvent (WMManageStart wm) = pure [(WMManageFinish wm)]
handleEvent (WMRenderStart wm) = pure [(WMRenderFinish wm)]

handleEvent e
    = do putStrLn $ "unhandled event: " ++ (show e)
         pure []
