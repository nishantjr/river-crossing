{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS -Wno-unused-top-binds #-} -- Record fields

module WXYZ.River (
      wxyz

    , Dimensions(..), Position(..), Rectangle(..)

    , Layout, LayoutClass(..)
    , Message, SomeMessage(..), LayoutMessages(..), fromMessage

    , Binding(..), Output(..), Seat(..), Window(..)

    , RiverState(..)
    , WXYZConfig(..), WXYZ, runWXYZ
    , shell
    , (<||>)
  ) where

import           Prelude hiding (mod)

import           WXYZ.Protocol
import           WXYZ.XKB
import           XMonad.StackSet hiding (modify)

import           Control.Monad (void)
import           Control.Monad.State
import           Control.Monad.Extra (fromMaybeM)
import           Control.Monad.Trans.Reader (runReaderT, ReaderT)
import           Data.Int (Int32)
import           Data.Map (Map)
import qualified Data.Map as M
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

-- Managing River's State
-- ======================

data Output = Output { handle :: RiverOutput
                     , wlOutput :: Maybe Word32
                     , position :: Maybe Position
                     , dimensions :: Maybe Dimensions
                     } deriving (Show)

data Window = Window { handle :: RiverWindow
                     , node   :: RiverNode
                     } deriving (Show)


data Seat = Seat { handle :: RiverSeat }
    deriving (Show)

data Binding = Binding { action :: WXYZ () }

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

-- Immutable configuration
data WXYZConfig = WXYZConfig { onRiverEvent :: Event -> WXYZ (Maybe [Request])
                             , onStartup    :: WXYZ ()
                             , keyBindings  :: M.Map (Modifier,KeySym) (WXYZ ())
                             }

type WXYZ a = ReaderT WXYZConfig (StateT RiverState IO) a

--- | Run the 'WXYZ'' monad, given a chunk of 'WXYZ' monad code, and an initial
--  state. Return the result, and final state
runWXYZ :: WXYZConfig -> RiverState -> WXYZ a -> IO (a, RiverState)
runWXYZ c st act = runStateT (runReaderT act c) st


-- Layouts
-- =======

-- | Virtual workspace indices
type WorkspaceId = String

------------------------------------------------------------------------
-- LayoutClass handling. See particular instances in Operations.hs

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
-- * 'runLayout' || (('doLayout' || 'pureLayout') && 'emptyLayout')
--
-- * 'handleMessage' || 'pureMessage'
--
-- * 'description'
--
-- Note that any code which /uses/ 'LayoutClass' methods should only
-- ever call 'runLayout', 'handleMessage', and 'description'!  In
-- other words, the only calls to 'doLayout', 'pureMessage', and other
-- such methods should be from the default implementations of
-- 'runLayout', 'handleMessage', and so on.  This ensures that the
-- proper methods will be used, regardless of the particular methods
-- that any 'LayoutClass' instance chooses to define.
class (Show (layout a), Typeable layout) => LayoutClass layout a where

    -- | By default, 'runLayout' calls 'doLayout' if there are any
    --   windows to be laid out, and 'emptyLayout' otherwise.  Most
    --   instances of 'LayoutClass' probably do not need to implement
    --   'runLayout'; it is only useful for layouts which wish to make
    --   use of more of the 'Workspace' information (for example,
    --   "XMonad.Layout.PerWorkspace").
    runLayout :: Workspace WorkspaceId (layout a) a
              -> Rectangle
              -> WXYZ ([(a, Rectangle)], Maybe (layout a))
    runLayout (Workspace _ l ms) r = maybe (emptyLayout l r) (doLayout l r) ms

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
    pureLayout _ r s = [(focus s, r)]

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

instance LayoutClass Layout RiverWindow where
    runLayout (Workspace i (Layout l) ms) r = fmap (fmap Layout) `fmap` runLayout (Workspace i l ms) r
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

-- ---------------------------------------------------------------------
-- General utilities

-- | Lift an 'IO' action into the 'WXYZ' monad
io :: MonadIO m => IO a -> m a
io = liftIO

-- Main Loop
-- =========

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

