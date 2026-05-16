{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS -Wno-unused-top-binds #-} -- Record fields

module Main (main) where

import           WXYZ.Protocol
import           WXYZ.XKB

import           Control.Monad (void)
import           Control.Monad.State
import           Control.Monad.Extra (fromMaybeM)
import           Control.Monad.Trans.Maybe (runMaybeT)
import           Data.Int (Int32)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Word
import qualified System.Process as P

-- Managing River's State
-- ======================

data Window = Window { handle :: RiverWindow
                     , node   :: RiverNode
                     }
    deriving (Show)

data Position   = Position { x :: Int32, y :: Int32 }
    deriving (Show)
data Dimensions = Dimensions { width :: Int32, height :: Int32 }
    deriving (Show)
data Output = Output { handle :: RiverOutput
                     , wlOutput :: Maybe Word32
                     , position :: Maybe Position
                     , dimensions :: Maybe Dimensions
                     }
    deriving (Show)

data Seat = Seat { handle :: RiverSeat }
    deriving (Show)

-- Cache of River's Window Management state
data RiverState = RiverState { windows :: Map RiverWindow Window
                             , outputs :: Map RiverOutput Output
                             , seats   :: Map RiverSeat   Seat
                             }
    deriving (Show)



manageAndRender :: Event -> WXYZ (Maybe [Request])
manageAndRender (WMManageStart wm) = runMaybeT $
    do wins <- windows <$> get
       firstOutput:_ <- (M.elems . outputs) <$> get -- Maybe Monad fails if no output available.
       let width =  (fromMaybe (Dimensions 0 0) firstOutput.dimensions).width
       let height = (fromMaybe (Dimensions 0 0) firstOutput.dimensions).height
       let dims = map (winProposeDimensions width height (fromIntegral $ length wins))  (M.keys wins)
       pure $ dims ++ [ (WMManageFinish wm) ]
  where
    winProposeDimensions outputWidth outputHeight numWins handle
        = (WindowProposeDimensions handle (outputWidth `div` numWins) outputHeight)

manageAndRender (WMRenderStart wm) = runMaybeT $
    do wins <- windows <$> get
       firstOutput:_ <- (M.elems . outputs) <$> get -- Maybe Monad fails if no output available.
       let width =  (fromMaybe (Dimensions 0 0) firstOutput.dimensions).width
       let positions = map (winSetPosition width (fromIntegral $ length wins))  (zip [0..] $ M.elems wins)
       pure $ positions ++ [ (WMRenderFinish wm) ]
  where
    winSetPosition outputWidth numWins (index, win)
        = (NodeSetPosition (win.node) (index * outputWidth `div` numWins) 0)

manageAndRender _ = pure Nothing

cacheRiverState :: Event -> WXYZ (Maybe [Request])
cacheRiverState (WMWindow _wm win) = runMaybeT $
    do st <- get
       let node = riverWindowGetNode win
       put $ st { windows = M.insert win (Window win node) (windows st) }
       pure []
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
cacheRiverState (OutputWlOutput output wl) = runMaybeT $
    do st <- get
       put $ st { outputs = M.adjust (\o -> o { wlOutput = Just wl}) output (outputs st) }
       pure []
cacheRiverState (OutputPosition output x y) = runMaybeT $
    do st <- get
       put $ st { outputs = M.adjust (\o -> o { position = Just $ Position x y}) output (outputs st) }
       pure []
cacheRiverState (OutputDimensions output width height) = runMaybeT $
    do st <- get
       put $ st { outputs = M.adjust (\o -> o { dimensions = Just $ Dimensions width height}) output (outputs st) }
       pure []

cacheRiverState (WMSeat _wm seat) = runMaybeT $
    do st <- get
       put $ st { seats = M.insert seat (Seat seat) (seats st) }
       pure [ ]
cacheRiverState (SeatRemoved seat) = runMaybeT $
    do st <- get
       put $ st { seats = M.delete seat (seats st) }
       pure []

cacheRiverState _ = pure Nothing

-- Main Loop
-- =========

-- immutable configuration.
data WXYZConfig = WXYZConfig { onRiverEvent :: Event -> WXYZ (Maybe [Request])
                             , onStartup    :: WXYZ ()
                             , keyBindings  :: M.Map (Modifier,KeySym) (WXYZ ())
                             }

type WXYZ a = StateT RiverState IO a

runWXYZ :: WXYZ () -> RiverState -> IO ((), RiverState)
runWXYZ = runStateT

-- | Entry point for the window manager.
wxyz :: WXYZConfig -> IO ()
wxyz config
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
         xkb <- getRiverXKBBindings

         void $ runWXYZ
            (onStartup config >> eventLoop display)
            (RiverState M.empty M.empty M.empty)
  where
    eventLoop :: WlDisplay -> WXYZ ()
    eventLoop display =
        do e <- liftIO $ next_event display
           case e of
             Nothing -> pure ()
             Just e' -> do liftIO $ putStrLn $ "====" ++ (show e')
                           requests <- fromMaybeM (unhandledEvent e')
                                                  (onRiverEvent config e')
                           _ <- liftIO $ mapM (sendRequest display) requests
                           st <- get
                           liftIO $ putStrLn $ (show st) ++ "\n\n"
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


-- High-level Operations
-- =====================

shell :: String -> WXYZ ()
shell cmd = liftIO $ do _ <- P.createProcess $ P.shell cmd
                        pure ()


-- User Configuration
-- ==================

main :: IO ()
main = wxyz $ WXYZConfig {
                   onRiverEvent = (cacheRiverState <||> manageAndRender)
                 , onStartup = (  shell "alacritty"
                               >> shell "alacritty"
                               )
                 , keyBindings
                 }
  where
    keyBindings = M.fromList
        [ ((mod_mod1, xkb_key_t),      shell "alacritty")
        ]
