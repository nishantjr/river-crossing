{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS -Wno-unused-top-binds #-} -- Record fields

module Main (main) where

import           Control.Monad.State
import           Control.Monad.Trans.Maybe (runMaybeT)
import           Control.Monad.Trans.Reader (ask, runReaderT, ReaderT)
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, listToMaybe)

import           WXYZ.River
                    ( shell
                    , wxyz
                    , Binding(..)
                    , Dimensions(..)
                    , Output(..)
                    , Position(..)
                    , RiverState(..)
                    , Seat(..)
                    , Window(..)
                    , WXYZConfig(..)
                    , (<||>)
                    )
import           WXYZ.Protocol
                    ( mod_mod1
                    , riverXKBGetBinding
                    , riverWindowGetNode
                    , riverXKBBindingAddEventListeners
                    , _river_window_v1_focus_window
                    , getRiverXKBBindings
                    , Request(..)
                    , Event(..)
                    , RiverXKBBindings
                    , RiverSeat
                    , RiverWindow
                    , Modifier
                    )
import           WXYZ.XKB

import           XMonad.Core (WXYZ, Layout, LayoutClass)
import qualified XMonad.Layout as L
import           XMonad.Layout ((|||))

handleBinding :: Event -> WXYZ (Maybe [Request])
handleBinding (XKBBindingPressed binding) = runMaybeT $
    do st <- get
       lift $ fromMaybe (unknownBinding binding) $ fmap action $ M.lookup binding (st.bindings)
       pure []
  where
    unknownBinding b = do
        liftIO $ putStrLn $ "unknown binding: " ++ (show b)


handleBinding _ = pure Nothing


manageAndRender :: LayoutClass layout a => layout a -> Event -> WXYZ (Maybe [Request])
manageAndRender _ (WMManageStart wm) = runMaybeT $
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

       case (listToMaybe $ reverse $ M.keys st.windows, listToMaybe $ M.keys st.seats) of
         (Just lastWin, Just firstSeat) -> liftIO $ do putStrLn $ "------- " ++ (show (lastWin, firstSeat))
                                                       putStrLn $ show (M.keys st.outputs, M.keys st.windows)
                                                       _river_window_v1_focus_window firstSeat lastWin
         _ -> pure ()

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


manageAndRender _ (WMRenderStart wm) = runMaybeT $
    do wins <- windows <$> get
       firstOutput:_ <- (M.elems . outputs) <$> get -- Maybe Monad fails if no output available.
       let width =  (fromMaybe (Dimensions 0 0) firstOutput.dimensions).width
       let positions = map (winSetPosition width (fromIntegral $ length wins))  (zip [0..] $ M.elems wins)
       pure $ positions ++ [ (WMRenderFinish wm) ]
  where
    winSetPosition outputWidth numWins (index, win)
        = (NodeSetPosition (win.node) (index * outputWidth `div` numWins) 0)

manageAndRender _ _ = pure Nothing

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


main :: IO ()
main = wxyz $ WXYZConfig {
                   onRiverEvent = (cacheRiverState <||> manageAndRender myLayout <||> handleBinding)
                 , onStartup =    shell "swaybg -c 123123"
                               >> shell "waybar"
                 , keyBindings
                 }
  where
    keyBindings = M.fromList
        [ ((mod_mod1, xkb_key_t),      shell "alacritty")
        ]
    myLayout = tiled ||| L.Mirror tiled ||| L.Full
      where
        tiled   = L.Tall nmaster delta ratio
        nmaster = 1      -- Default number of windows in the master pane
        ratio   = 1/2    -- Default proportion of screen occupied by master pane
        delta   = 3/100  -- Percent of screen to increment by when resizing panes

