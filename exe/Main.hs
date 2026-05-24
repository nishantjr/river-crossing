{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS -Wno-unused-top-binds #-} -- Record fields

module Main (main) where

import           Prelude hiding (mod)

import           WXYZ.Protocol
import           WXYZ.XKB

import           Control.Monad (void)
import           Control.Monad.State
import           Control.Monad.Extra (fromMaybeM)
import           Control.Monad.Trans.Reader (ask, runReaderT, ReaderT)
import           Control.Monad.Trans.Maybe (runMaybeT)
import           Data.Int (Int32)
import qualified Data.List as L
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

data Binding = Binding { action :: WXYZ ()
                       }

-- Cache of River's Window Management state
data RiverState = RiverState { windows  :: Map RiverWindow Window
                             , outputs  :: Map RiverOutput Output
                             , seats    :: Map RiverSeat   Seat
                             , bindings :: Map RiverXKBBinding Binding

                             -- Seats registered since last manage block.
                             -- These are needed so that we can set up bindings.
                             -- Setting up bindings in non-idemopotent, so
                             -- unlike in the case of window layouts, we can't
                             -- just re-do everything from scratch and expect
                             -- it to be ok.
                             , newSeats :: [RiverSeat]
                             }

handleBinding :: Event -> WXYZ (Maybe [Request])
handleBinding (XKBBindingPressed binding) = runMaybeT $
    do st <- get
       lift $ fromMaybe (unknownBinding binding) $ fmap action $ M.lookup binding (st.bindings)
       pure []
  where
    unknownBinding b = do
        liftIO $ putStrLn $ "unknown binding: " ++ (show b)


handleBinding _ = pure Nothing


manageAndRender :: Event -> WXYZ (Maybe [Request])
manageAndRender (WMManageStart wm) = runMaybeT $
    do st <- get
       firstOutput:_ <- (M.elems . outputs) <$> get -- Maybe Monad fails if no output available.
       let width =  (fromMaybe (Dimensions 0 0) firstOutput.dimensions).width
       let height = (fromMaybe (Dimensions 0 0) firstOutput.dimensions).height
       let dims = map (winProposeDimensions width height (fromIntegral $ length st.windows))  (M.keys st.windows)

       config <- lift ask
       xkb <- liftIO getRiverXKBBindings
       binds <- lift $ mapM (uncurry $ bindSeat xkb)
                        [(o, (mod,sym,act)) |
                            o <- st.newSeats,
                            ((mod, sym), act) <- M.toList config.keyBindings
                        ]

       pure $ dims ++ (concat binds) ++ [ (WMManageFinish wm) ]
  where
    winProposeDimensions outputWidth outputHeight numWins handle
        = (WindowProposeDimensions handle (outputWidth `div` numWins) outputHeight)
    bindSeat :: RiverXKBBindings -> RiverSeat -> (Modifier, KeySym, WXYZ ()) -> WXYZ [Request]
    bindSeat xkb seat (mod, sym, act) =
      do binding <- liftIO $ riverXKBGetBinding xkb seat sym mod
         st <- get
         put st { bindings = M.insert binding (Binding act) st.bindings
                , newSeats = L.delete seat st.newSeats
                }
         liftIO $ riverXKBBindingAddEventListeners binding
         pure [(XKBBindingEnable binding)]


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
       put $ st { outputs = M.insert output (Output output Nothing Nothing Nothing) (st.outputs) }
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
       put $ st { seats = M.insert seat (Seat seat) (seats st)
                , newSeats = st.newSeats ++ [seat]
                }
       pure []
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

type WXYZ a = ReaderT WXYZConfig (StateT RiverState IO) a

runWXYZ :: WXYZConfig -> RiverState -> WXYZ () -> IO ((), RiverState)
runWXYZ c st act = runStateT (runReaderT act c) st

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
         void $ runWXYZ
            config
            (RiverState M.empty M.empty M.empty M.empty [])
            (onStartup config >> eventLoop display)
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
                   onRiverEvent = (cacheRiverState <||> manageAndRender <||> handleBinding)
                 , onStartup =    shell "swaybg -c 123123"
                               >> shell "waybar"
                 , keyBindings
                 }
  where
    keyBindings = M.fromList
        [ ((mod_mod1, xkb_key_t),      shell "alacritty")
        ]
