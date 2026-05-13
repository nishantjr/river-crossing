{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS -Wno-unused-top-binds #-} -- Record fields

module Main (main) where

import           WXYZ.Protocol

import           Control.Monad (void)
import           Control.Monad.State
import           Control.Monad.Extra (fromMaybeM)
import           Control.Monad.Trans.Maybe (runMaybeT)
import           Data.Int (Int32)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Word


data Window = Window { handle :: RiverWindow
                     , node   :: RiverNode
                     }

data Position   = Position { x :: Int32, y :: Int32 }
data Dimensions = Dimensions { width :: Int32, height :: Int32 }
data Output = Output { handle :: RiverOutput
                     , wlOutput :: Maybe Word32
                     , position :: Maybe Position
                     , dimensions :: Maybe Dimensions
                     }

-- Cache of River's Window Management state
data RiverState = RiverState { windows :: Map RiverWindow Window
                             , outputs :: Map RiverOutput Output
                             }

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
         void $ runStateT (eventLoop display) (RiverState M.empty M.empty)
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

cacheRiverState (WMOutput _wm output) = runMaybeT $
    do st <- get
       put $ st { outputs = M.insert output (Output output Nothing Nothing Nothing) (outputs st) }
       pure [ ]
cacheRiverState (OutputRemoved output) = runMaybeT $
    do st <- get
       put $ st { outputs = M.delete output (outputs st) }
       pure []
cacheRiverState (OutputPosition output x y) = runMaybeT $
    do st <- get
       put $ st { outputs = M.adjust (\o -> o { position = Just $ Position x y}) output (outputs st) }
       pure []
cacheRiverState (OutputDimensions output width height) = runMaybeT $
    do st <- get
       put $ st { outputs = M.adjust (\o -> o { dimensions = Just $ Dimensions width height}) output (outputs st) }
       pure []


cacheRiverState _ = pure Nothing

---

main :: IO ()
main = runWXYZ (WXYZConfig (cacheRiverState <||> manageAndRender))


