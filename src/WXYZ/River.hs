{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS -Wno-unused-top-binds #-} -- Record fields

module WXYZ.River (
      wxyz

    , Dimensions(..), Position(..), Rectangle(..)

    , Layout, LayoutClass(..)
    , Message, SomeMessage(..), LayoutMessages(..), fromMessage, sendMessage

    , Binding(..), Output(..), Seat(..), Window(..)

    , WXYZState(..)
    , WXYZConfig(..), WXYZ, runWXYZ
    , shell
    , withFocused, focusWindow, focusNext, focusPrev
    , informFullscreen, informNotFullscreen, toggleInformFullscreen
  ) where

import           Prelude hiding (mod)

import           WXYZ.Protocol
import           WXYZ.XKB
import           XMonad.StackSet hiding (modify, focusWindow)

import           Control.Applicative ((<|>))
import           Control.Monad (forM_, void, when)
import           Control.Monad.State
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Control.Monad.Extra (fromMaybeM, whenJust, ifM)
import           Control.Monad.Trans.Reader (ask, runReaderT, ReaderT)
import           Data.Int (Int32)
import qualified Data.List as L
import           Data.List ((\\))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (isNothing, fromMaybe, listToMaybe)
import           Data.Typeable
import           Data.Word
import qualified System.Process as P



-- Dimensions and Positions
-- ========================

data Position = Position { x :: Int32, y :: Int32 }
    deriving (Eq, Read, Show)

-- XXX For some reason, River/Wayland dimensions are signed.
-- Document why this is.
type Dimension     = Int32
data Dimensions = Dimensions { width :: Dimension, height :: Dimension }
    deriving (Eq, Read, Show)

-- capatibility for XMonad
data Rectangle = Rectangle {
        rect_x      :: !Int32,
        rect_y      :: !Int32,
        rect_width  :: !Dimension,
        rect_height :: !Dimension
        }
    deriving (Eq, Read, Show)

-- Managing State
-- ==============

data Output = Output { handle :: OutputHandle
                     , wlOutput :: Maybe Word32
                     , position :: Maybe Position
                     , dimensions :: Maybe Dimensions
                     } deriving (Show)

data Window = Window { handle             :: WindowHandle
                     , node               :: RiverNode
                     , informedFullscreen :: Bool
                     } deriving (Show)


data Seat = Seat { handle :: SeatHandle
                 , focused :: Maybe WindowHandle
                 }
    deriving (Show)

data Binding = Binding { action :: WXYZ () }

-- | Virtual workspace indices
type WorkspaceId = String


-- Cache of River's Window Management state
data WXYZState = WXYZState { windows  :: Map WindowHandle Window
                           , outputs  :: Map OutputHandle Output
                           , seats    :: Map SeatHandle   Seat
                           , bindings :: Map RiverXKBBinding Binding

                           -- Seats registered since last manage block.
                           -- These are needed so that we can set up bindings.
                           -- Setting up bindings in non-idemopotent, so
                           -- unlike in the case of window layouts, we can't
                           -- just re-do everything from scratch and expect
                           -- it to be ok.
                           , newSeats :: [SeatHandle]

                           , currLayout :: Layout WindowHandle
                           }

-- Immutable configuration
data WXYZConfig l = WXYZConfig
        { layout       :: l WindowHandle
        , onStartup    :: WXYZ ()
        , keyBindings  :: M.Map (Modifier,KeySym) (SeatHandle -> WXYZ ())
        }

type WXYZ = ReaderT (WXYZConfig Layout) (StateT WXYZState IO)

--- | Run the 'WXYZ'' monad, given a chunk of 'WXYZ' monad code, and an initial
--  state. Return the result, and final state
runWXYZ :: WXYZConfig Layout -> WXYZState -> WXYZ a -> IO (a, WXYZState)
runWXYZ c st act = runStateT (runReaderT act c) st


-- Low-level handling of River events
-- ==================================

handleBinding :: Event -> WXYZ (Maybe [Request])
handleBinding (XKBBindingPressed binding) = runMaybeT $
    do st <- get
       lift $ fromMaybe (unknownBinding binding) $ fmap (.action) $ M.lookup binding st.bindings
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
toStack :: [WindowHandle] -> Maybe (Stack WindowHandle)
toStack (w:ws) = Just $ Stack w [] ws
toStack []     = Nothing

manageAndRender :: Event -> WXYZ (Maybe [Request])
manageAndRender (WMManageStart wm) = runMaybeT $
    do st <- get
       config <- lift ask
       firstOutput:_ <- ((M.elems . (.outputs)) <$> get) -- Maybe Monad fails if no output available.
       let rect = fromMaybe (Rectangle 0 0 1920 1080) (outputRect firstOutput)

       (winrects, newLayout) <- lift $
            maybe (emptyLayout st.currLayout rect)
                  (doLayout st.currLayout rect)
                  (toStack $ M.keys st.windows)
       let shown = map WindowShow (map fst winrects)
       let hidden = map WindowHide (M.keys st.windows \\ (map fst winrects))
       let dims = map mkProposeDim winrects

       xkb <- liftIO getRiverXKBBindings
       binds <- lift $ mapM (uncurry $ bindSeat xkb)
                        [(o, (modifier,sym,act)) |
                            o <- st.newSeats,
                            ((modifier,sym),act) <- M.toList config.keyBindings
                        ]
       case newLayout of
           Just l' -> put st{currLayout=l'}
           Nothing -> pure ()
       pure $ shown ++ hidden ++ dims ++ (concat binds) ++ [ (WMManageFinish wm) ]
  where
    mkProposeDim (win, (Rectangle _x _y w h))
        = (WindowProposeDimensions win w h)
    bindSeat :: RiverXKBBindings -> SeatHandle -> (Modifier, KeySym, SeatHandle -> WXYZ ()) -> WXYZ [Request]
    bindSeat xkb seat (modifier, sym, act) =
      do binding <- liftIO $ riverXKBGetBinding xkb seat sym modifier
         st <- get
         put st { bindings = M.insert binding (Binding $ act seat) st.bindings
                , newSeats = L.delete seat st.newSeats
                }
         liftIO $ riverXKBBindingAddEventListeners binding
         pure [(XKBBindingEnable binding)]


manageAndRender (WMRenderStart wm) = runMaybeT $
    do st <- get
       firstOutput:_ <- ((M.elems . (.outputs)) <$> get) -- Maybe Monad fails if no output available.
       let rect = fromMaybe (Rectangle 0 0 1920 1080) (outputRect firstOutput)

       (winrects, newLayout) <- lift $
            maybe (emptyLayout st.currLayout rect)
                  (doLayout st.currLayout rect)
                  (toStack $ M.keys st.windows)
       positions <- mapM mkPositions winrects

       case newLayout of
           Just l' -> put st{currLayout=l'}
           Nothing -> pure ()
       pure $ positions ++ [ (WMRenderFinish wm) ]
  where
    mkPositions :: (WindowHandle, Rectangle) -> MaybeT WXYZ Request
    mkPositions (winHandle, (Rectangle x y _w _h)) =
        do st <- get
           Just win <- lift $ pure $ M.lookup winHandle st.windows
           pure (NodeSetPosition win.node x y)

manageAndRender _ = pure Nothing

cacheRiverState :: Event -> WXYZ (Maybe [Request])
cacheRiverState (WMWindow _wm win) = runMaybeT $
    do st <- get
       let node = riverWindowGetNode win
       put $ st { windows = M.insert win (Window win node False) st.windows }
       -- Focus newly created window. This could be obtusive--perhaps
       -- we want to this to happen for particular types of windows, or
       -- make it user configurable?
       forM_ (M.elems st.seats) $ \s -> do
          when (isNothing s.focused) (lift $ focusWindow s.handle win)
       pure []
cacheRiverState (WindowClosed win) = runMaybeT $
    do st <- get
       put $ st { windows = M.delete win st.windows }
       -- focus next window if closed window is currently focused.
       forM_ (M.elems st.seats) $ \s -> do
          when ((Just win) == s.focused) (lift $ focusNext s.handle)
       pure []

cacheRiverState (WMOutput _wm output) = runMaybeT $
    do st <- get
       put $ st { outputs = M.insert output (Output output Nothing Nothing Nothing) (st.outputs) }
       pure [ ]
cacheRiverState (OutputRemoved output) = runMaybeT $
    do st <- get
       put $ st { outputs = M.delete output st.outputs }
       pure []
cacheRiverState (OutputWlOutput output wl) = runMaybeT $
    do st <- get
       put $ st { outputs = M.adjust (\o -> o { wlOutput = Just wl}) output st.outputs }
       pure []
cacheRiverState (OutputPosition output x y) = runMaybeT $
    do st <- get
       put $ st { outputs = M.adjust (\o -> o { position = Just $ Position x y}) output st.outputs }
       pure []
cacheRiverState (OutputDimensions output width height) = runMaybeT $
    do st <- get
       put $ st { outputs = M.adjust (\o -> o { dimensions = Just $ Dimensions width height}) output st.outputs }
       pure []

cacheRiverState (WMSeat _wm seat) = runMaybeT $
    do st <- get
       put $ st { seats = M.insert seat (Seat seat Nothing) st.seats
                , newSeats = st.newSeats ++ [seat]
                }
       pure []
cacheRiverState (SeatRemoved seat) = runMaybeT $
    do st <- get
       put $ st { seats = M.delete seat st.seats }
       pure []

cacheRiverState _ = pure Nothing



-- Layouts
-- =======

-- | An existential type that can hold any object that is in 'Read'
--   and 'LayoutClass'.
data Layout a = forall l. (LayoutClass l a) => Layout (l a)

-- | Every layout must be an instance of 'LayoutClass', which defines
-- the basic layout operations along with a sensible default for each.
--
-- All of the methods have default implementations, so there is no
-- minimal complete definition.  They do, however, have a dependency
-- structure by default; this is something to be aware of should you
-- choose to implement one of these methods.  Here is how a minimal
-- complete definition would look like if we did not provide any default
-- implementations:
--
-- * (('doLayout' || 'pureLayout') && 'emptyLayout')
--
-- * 'handleMessage' || 'pureMessage'
--
-- * 'description'
--
-- Note that any code which /uses/ 'LayoutClass' methods should only
-- ever call 'doLayout', 'handleMessage', and 'description'!  In
-- other words, the only calls to 'pureMessage', and other
-- such methods should be from the default implementations of
-- 'doLayout', 'handleMessage', and so on.  This ensures that the
-- proper methods will be used, regardless of the particular methods
-- that any 'LayoutClass' instance chooses to define.
class (Show (layout a), Typeable layout) => LayoutClass layout a where

    -- | Given a 'Rectangle' in which to place the windows, and a 'Stack'
    -- of windows, return a list of windows and their corresponding
    -- Rectangles.  If an element is not given a Rectangle by
    -- 'doLayout', then it is not shown on screen.  The order of
    -- windows in this list should be the desired stacking order.
    --
    -- Also possibly return a modified layout (by returning @Just
    -- newLayout@), if this layout needs to be modified (e.g. if it
    -- keeps track of some sort of state).  Return @Nothing@ if the
    -- layout does not need to be modified.
    --
    -- Layouts which do not need access to the 'WXYZ' monad ('IO', window
    -- manager state, or configuration) and do not keep track of their
    -- own state should implement 'pureLayout' instead of 'doLayout'.
    doLayout    :: layout a -> Rectangle -> Stack a
                -> WXYZ ([(a, Rectangle)], Maybe (layout a))
    doLayout l r s   = return (pureLayout l r s, Nothing)

    -- | This is a pure version of 'doLayout', for cases where we
    -- don't need access to the 'WXYZ' monad to determine how to lay out
    -- the windows, and we don't need to modify the layout itself.
    pureLayout  :: layout a -> Rectangle -> Stack a -> [(a, Rectangle)]
    pureLayout _ r s = [(s.focus, r)]

    -- | 'emptyLayout' is called when there are no windows.
    emptyLayout :: layout a -> Rectangle -> WXYZ ([(a, Rectangle)], Maybe (layout a))
    emptyLayout _ _ = return ([], Nothing)

    -- | 'handleMessage' performs message handling.  If
    -- 'handleMessage' returns @Nothing@, then the layout did not
    -- respond to the message and the screen is not refreshed.
    -- Otherwise, 'handleMessage' returns an updated layout and the
    -- screen is refreshed.
    --
    -- Layouts which do not need access to the 'WXYZ' monad to decide how
    -- to handle messages should implement 'pureMessage' instead of
    -- 'handleMessage' (this restricts the risk of error, and makes
    -- testing much easier).
    handleMessage :: layout a -> SomeMessage -> WXYZ (Maybe (layout a))
    handleMessage l  = return . pureMessage l

    -- | Respond to a message by (possibly) changing our layout, but
    -- taking no other action.  If the layout changes, the screen will
    -- be refreshed.
    pureMessage :: layout a -> SomeMessage -> Maybe (layout a)
    pureMessage _ _  = Nothing

    -- | This should be a human-readable string that is used when
    -- selecting layouts by name.  The default implementation is
    -- 'show', which is in some cases a poor default.
    description :: layout a -> String
    description      = show

instance LayoutClass Layout WindowHandle where
    doLayout (Layout l) r s  = fmap (fmap Layout) `fmap` doLayout l r s
    emptyLayout (Layout l) r = fmap (fmap Layout) `fmap` emptyLayout l r
    handleMessage (Layout l) = fmap (fmap Layout) . handleMessage l
    description (Layout l)   = description l

instance Show (Layout a) where show (Layout l) = show l

-- | Based on ideas in /An Extensible Dynamically-Typed Hierarchy of
-- Exceptions/, Simon Marlow, 2006. Use extensible messages to the
-- 'handleMessage' handler.
--
-- User-extensible messages must be a member of this class.
--
class Typeable a => Message a

-- |
-- A wrapped value of some type in the 'Message' class.
--
data SomeMessage = forall a. Message a => SomeMessage a

-- |
-- And now, unwrap a given, unknown 'Message' type, performing a (dynamic)
-- type check on the result.
--
fromMessage :: Message m => SomeMessage -> Maybe m
fromMessage (SomeMessage m) = cast m

-- | 'LayoutMessages' are core messages that all layouts (especially stateful
-- layouts) should consider handling.
data LayoutMessages = Hide              -- ^ sent when a layout becomes non-visible
    deriving Eq

instance Message LayoutMessages

-- | Throw a message to the current 'LayoutClass' possibly modifying how we
-- layout the windows.
-- XXX force a ManageDirty?
sendMessage :: Message a => a -> WXYZ ()
sendMessage a = do
    st <- get
    layout' <- handleMessage st.currLayout (SomeMessage a)
    case layout' of
        Just l' -> (put st{currLayout=l'}) *>
                   (liftIO $ putStrLn (description l'))
        Nothing -> pure ()
    pure ()

-- Main Loop
-- =========

-- | Entry point for the window manager.
wxyz :: (LayoutClass l WindowHandle) => WXYZConfig l -> IO ()
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
         let config' = config{layout=(Layout config.layout)} :: WXYZConfig Layout
         void $ runWXYZ
            config'
            (WXYZState{ windows=M.empty
                      , outputs=M.empty
                      , seats=M.empty
                      , bindings=M.empty
                      , newSeats=[]
                      , currLayout=config'.layout
                      })
            (config.onStartup >> eventLoop display)
  where
    eventLoop :: WlDisplay -> WXYZ ()
    eventLoop display =
        do e <- liftIO $ next_event display
           case e of
             Nothing -> pure ()
             Just e' -> do liftIO $ putStrLn $ "====" ++ (show e')
                           requests <- fromMaybeM (unhandledEvent e')
                                                  (onRiverEvent e')
                           _ <- liftIO $ mapM (sendRequest display) requests
                           eventLoop display
    onRiverEvent =      cacheRiverState
                   <||> manageAndRender
                   <||> handleBinding
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

-- focus
-- -----

-- | Set the focused window for a seat
focusWindow :: SeatHandle -> WindowHandle -> WXYZ ()
focusWindow s w = do st <- get
                     put st{seats=M.adjust (\seat -> seat{focused=Just w}) s st.seats}
                     liftIO $ _river_window_v1_focus_window s w

-- | Focus the next window in the window list.
-- Note that this uses the window managers internal list of windows.
-- This is a fairly arbitrary ordering--it doesn't take into account outputs,
-- and includes windows that are hidden etc.
focusNext :: SeatHandle -> WXYZ ()
focusNext s = do st <- get
                 focusNextInList (M.keys st.windows) s

-- | Focus the previous window in the window list.
focusPrev :: SeatHandle -> WXYZ ()
focusPrev s = do st <- get
                 focusNextInList (reverse $ M.keys st.windows) s

focusNextInList :: [WindowHandle] -> SeatHandle -> WXYZ ()
focusNextInList ws s = do
    st <- get
    let mw = (M.lookup s st.seats >>= (.focused) >>= (flip nextItem ws))
               <|> listToMaybe ws
    whenJust mw $ \w -> focusWindow s w

withFocused :: (WindowHandle -> WXYZ ()) -> SeatHandle -> WXYZ ()
withFocused f s = do
    st <- get
    let mw = (M.lookup s st.seats >>= (.focused))
    whenJust mw $ \w -> f w

-- Inform fullscreen
-- -----------------

-- | Inform the client that it is fullscreen without actually making it
-- fullscreen. This is useful with a user wants the application to adjust
-- its UI to a more minimal version, hiding menus etc. e.g. VLC, Firefox.
informFullscreen :: WindowHandle -> WXYZ ()
informFullscreen wh = do
    st <- get
    put st{windows=M.adjust (\w -> w{informedFullscreen=True}) wh st.windows}
    liftIO $ _river_window_v1_inform_fullscreen wh

-- | Inform the client that it is not fullscreen.
informNotFullscreen :: WindowHandle -> WXYZ ()
informNotFullscreen wh = do
    st <- get
    put st{windows=M.adjust (\w -> w{informedFullscreen=False}) wh st.windows}
    liftIO $ _river_window_v1_inform_not_fullscreen wh

toggleInformFullscreen :: WindowHandle -> WXYZ ()
toggleInformFullscreen w = void $ runMaybeT $
    ifM ((.informedFullscreen) <$> getWindow w)
        (lift $ informNotFullscreen w)
        (lift $ informFullscreen w)

-- Utilities
-- =========

-- | Get the next item in a list, wrapping at the end
nextItem :: Eq a => a -> [a] -> Maybe a
nextItem _ [] = Nothing
nextItem a lst@(x:xs) = lookup a (zip lst (xs ++ [x]))

-- | Get window from a handle
getWindow :: WindowHandle -> MaybeT WXYZ Window
getWindow h = do st <- lift get
                 MaybeT $ pure $ M.lookup h st.windows

