{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS -Wno-unused-top-binds #-} -- Record fields

module Main (main) where

import           Control.Monad.State
import           Control.Monad.Trans.Maybe (runMaybeT)
import           Control.Monad.Trans.Reader (ask)
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, listToMaybe)

import           WXYZ.River
                    ( shell
                    , wxyz
                    , Binding(..)
                    , Dimensions(..)
                    , LayoutClass(..)
                    , Output(..)
                    , Position(..)
                    , Rectangle(..)
                    , RiverState(..)
                    , Seat(..)
                    , WXYZ
                    , WXYZConfig(..)
                    , Window(..)
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
                    , Modifier
                    )
import           WXYZ.XKB

import           XMonad.StackSet (Stack(..))
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

-- | Get a rectangle for a layout.
outputRect :: Output -> (Maybe Rectangle)
outputRect o =
    do dims <- o.dimensions
       pos <- o.position
       pure (Rectangle pos.x pos.y dims.width dims.height)

-- | Generate an XMonad Stack from the River's state.
-- XXX Eventually, we should use the Seat information to get the currently
-- focused windows.
toStack :: [Window] -> Maybe (Stack Window)
toStack (w:ws) = Just $ Stack w [] ws
toStack []     = Nothing

manageAndRender :: LayoutClass layout Window => layout Window -> Event -> WXYZ (Maybe [Request])
manageAndRender l (WMManageStart wm) = runMaybeT $
    do st <- get
       firstOutput:_ <- (M.elems . outputs) <$> get -- Maybe Monad fails if no output available.
       let rect = fromMaybe (Rectangle 0 0 1920 1080) (outputRect firstOutput)
       (winrects, newLayout) <- lift $
            maybe (emptyLayout l rect) (doLayout l rect) (toStack $ M.elems st.windows)
       let dims = map mkProposeDim winrects

       config <- lift ask
       xkb <- liftIO getRiverXKBBindings
       binds <- lift $ mapM (uncurry $ bindSeat xkb)
                        [(o, (modifier,sym,act)) |
                            o <- st.newSeats,
                            ((modifier,sym),act) <- M.toList config.keyBindings
                        ]

       case (listToMaybe $ reverse $ M.keys st.windows, listToMaybe $ M.keys st.seats) of
         (Just lastWin, Just firstSeat) -> liftIO $ do putStrLn $ "------- " ++ (show (lastWin, firstSeat))
                                                       putStrLn $ show (M.keys st.outputs, M.keys st.windows)
                                                       _river_window_v1_focus_window firstSeat lastWin
         _ -> pure ()

       pure $ dims ++ (concat binds) ++ [ (WMManageFinish wm) ]
  where
    mkProposeDim (win, (Rectangle _x _y w h))
        = (WindowProposeDimensions win.handle w h)
    bindSeat :: RiverXKBBindings -> RiverSeat -> (Modifier, KeySym, WXYZ ()) -> WXYZ [Request]
    bindSeat xkb seat (modifier, sym, act) =
      do binding <- liftIO $ riverXKBGetBinding xkb seat sym modifier
         st <- get
         put st { bindings = M.insert binding (Binding act) st.bindings
                , newSeats = L.delete seat st.newSeats
                }
         liftIO $ riverXKBBindingAddEventListeners binding
         pure [(XKBBindingEnable binding)]


manageAndRender l (WMRenderStart wm) = runMaybeT $
    do st <- get
       firstOutput:_ <- (M.elems . outputs) <$> get -- Maybe Monad fails if no output available.
       let rect = fromMaybe (Rectangle 0 0 1920 1080) (outputRect firstOutput)

       (winrects, newLayout) <- lift $
            maybe (emptyLayout l rect) (doLayout l rect) (toStack $ M.elems st.windows)
       let positions = map mkPositions winrects
       pure $ positions ++ [ (WMRenderFinish wm) ]
  where
    mkPositions (win, (Rectangle x y _w _h))
        = (NodeSetPosition win.node x y)

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
        , ((mod_mod1, xkb_key_d),      shell "bemenu-run")
        ]
    myLayout = tiled ---  ||| L.Mirror tiled ||| L.Full
      where
        tiled   = L.Tall nmaster delta ratio
        nmaster = 1      -- Default number of windows in the master pane
        ratio   = 1/2    -- Default proportion of screen occupied by master pane
        delta   = 3/100  -- Percent of screen to increment by when resizing panes

