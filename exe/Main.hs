module Main (main) where

import           WXYZ.Protocol

import           Control.Monad (void)
import           Control.Monad.State

data Window = Window RiverWindow RiverNode

-- Cache of River's Window Management state
data RiverState = RiverState { windows :: [Window] }

type WXYZ a = StateT RiverState IO a

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
          void $ runStateT (eventLoop display) (RiverState [])

eventLoop :: WlDisplay -> WXYZ ()
eventLoop display =
    do e <- liftIO $ next_event display
       case e of
         Nothing -> pure ()
         Just e' -> do requests <- handleEvent e'
                       _ <- liftIO $ mapM (sendRequest display) requests
                       eventLoop display

handleEvent :: Event -> WXYZ [Request]
handleEvent (WMManageStart wm)
    = pure [(WMManageFinish wm)]
handleEvent (WMRenderStart wm) = pure [(WMRenderFinish wm)]

handleEvent (WMWindow _wm win)
    = do st <- get
         let node = riverWindowGetNode win
         put (RiverState $ (windows st) ++ [Window win $ riverWindowGetNode win])
         pure [ (NodeSetPosition node 0 0)
              , (WindowProposeDimensions win 0 0)
              ]

handleEvent e
    = do liftIO $ putStrLn $ "unhandled event: " ++ (show e)
         pure []
