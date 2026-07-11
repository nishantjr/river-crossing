{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS -Wno-unused-top-binds #-} -- Record fields

module WXYZ.River (
      wxyz
    , Binding(..)
    , Dimensions(..)
    , Output(..)
    , Position(..)
    , RiverState(..)
    , Seat(..)
    , WXYZ
    , WXYZConfig(..)
    , Window(..)
    , runWXYZ
    , shell
    , (<||>)
  ) where

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
import           Data.Maybe (fromMaybe, listToMaybe)
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

-- Main Loop
-- =========

-- immutable configuration.
data WXYZConfig = WXYZConfig { onRiverEvent :: Event -> WXYZ (Maybe [Request])
                             , onStartup    :: WXYZ ()
                             , keyBindings  :: M.Map (Modifier,KeySym) (WXYZ ())
                             }

type WXYZ a = ReaderT WXYZConfig (StateT RiverState IO) a

--- | Run the 'WXYZ'' monad, given a chunk of 'WXYZ' monad code, and an initial
--  state. Return the result, and final state
runWXYZ :: WXYZConfig -> RiverState -> WXYZ a -> IO (a, RiverState)
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

