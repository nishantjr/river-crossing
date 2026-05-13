module Main (main) where

import           WXYZ.Protocol

import           Control.Monad (void)
import           Control.Monad.State
import           Control.Monad.Extra (fromMaybeM)
import           Control.Monad.Trans.Maybe (runMaybeT)
import           Data.Map (Map)
import qualified Data.Map as M


data Window = Window { window :: RiverWindow
                     , node   :: RiverNode
                     }

-- Cache of River's Window Management state
data RiverState = RiverState { windows :: Map RiverWindow Window }

-- immutable configuration.
data WXYZConfig = WXYZConfig {
        handleRiverEvent :: Event -> WXYZ (Maybe [Request])
    }

type WXYZ a = StateT RiverState IO a


runWXYZ :: WXYZConfig -> IO ()
runWXYZ config
    = do Just display <- wlDisplayConnect -- TODO: This should print an
                                          -- error message.
         initEventQueue
         putStrLn "Connected to the Display."
         True <- awaitRegistry display -- This prints a log message internally,
                                       -- and isn't likely to happen except for
                                       -- defective window managers. So it is
                                       -- OK to rely on matching failure.
         riverWM <- getRiverWM
         riverWMAddEventListeners riverWM
         void $ runStateT (eventLoop display) (RiverState M.empty)
  where
    eventLoop :: WlDisplay -> WXYZ ()
    eventLoop display =
        do e <- liftIO $ next_event display
           case e of
             Nothing -> pure ()
             Just e' -> do requests <- fromMaybeM (unhandledEvent e')
                                                  (handleRiverEvent config e')
                           _ <- liftIO $ mapM (sendRequest display) requests
                           st <- get
                           liftIO $ putStrLn (show $ length $ windows st)
                           eventLoop display

    unhandledEvent e
        = do liftIO $ putStrLn $ "unhandled event: " ++ (show e)
             pure []


(<||>) ::    (Event -> WXYZ (Maybe [Request]))
          -> (Event -> WXYZ (Maybe [Request]))
          ->  Event -> WXYZ (Maybe [Request])
(<||>) h1 h2 e = do r1 <- h1 e
                    case r1 of Nothing  -> h2 e
                               Just _   -> pure r1

---

manageAndRender :: Event -> WXYZ (Maybe [Request])
manageAndRender (WMManageStart wm) = runMaybeT $ pure [(WMManageFinish wm)]
manageAndRender (WMRenderStart wm) = runMaybeT $ pure [(WMRenderFinish wm)]
manageAndRender _ = pure Nothing

cacheRiverState :: Event -> WXYZ (Maybe [Request])
cacheRiverState (WMWindow _wm win) = runMaybeT $
    do st <- get
       let node = riverWindowGetNode win
       put $ st { windows = M.insert win (Window win node) (windows st) }
       pure [ (NodeSetPosition node 0 0)
            , (WindowProposeDimensions win 0 0)
            ]
cacheRiverState (WindowClosed win) = runMaybeT $
    do st <- get
       put $ st { windows = M.delete win (windows st) }
       pure []
cacheRiverState _ = pure Nothing

---

main :: IO ()
main = runWXYZ (WXYZConfig (cacheRiverState <||> manageAndRender))


