module WXYZ.Layout.Workspaces (workspaces) where

import           Control.Applicative
import           Control.Monad (forM)
import           Control.Monad.State
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT, hoistMaybe)
import qualified Data.Bimap as B
import           Data.Functor
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import           Data.Typeable

import           WXYZ.River hiding (WXYZConfig(..), WXYZState(outputs, seats))
import qualified WXYZ.River as State (WXYZState(..))

-- Partitions the list of windows, and assigns them to workspaces.
-- Each output is assigned a workspace. These are the 'visible' workspaces.

workspaces :: [Int] -> l a -> Workspaces l a
workspaces tags l = Workspaces
    { seats = M.empty
    , outputs = B.empty
    , workspaces = M.fromList [(t,Workspace l []) | t <- tags]
    , unassignedSeats = []
    }

data Workspaces l a = Workspaces
    { seats      :: M.Map SeatHandle (OutputHandle)
    , outputs    :: B.Bimap OutputHandle Int
    , workspaces :: M.Map Int (Workspace l a)

    , unassignedSeats :: [SeatHandle] -- seats that haven't been assigned an
    }
deriving instance (Show a, Show (l a)) => Show (Workspaces l a)

data Workspace l a = Workspace { layout :: l a
                               , wins   :: [a]
                               }
deriving instance (Show a, Show (l a)) => Show (Workspace l a)

instance (Eq a, Show a, Typeable a, LayoutClass l a)
            => LayoutClass (Workspaces l) a
  where
    doLayout l _r wins = do
        ls <- forM (M.toList l.workspaces) eachWorkspace
        pure ( concatMap fst ls
             , Just $ l { workspaces = M.fromList (fmap snd ls) }
             )
      where
        -- workspace is visible
        eachWorkspace (tag, ws) | Just oh <- (B.lookupR tag l.outputs) = do
            st <- get
            let mo = M.lookup oh st.outputs
                rect = fromMaybe (Rectangle 0 0 1920 1080) (mo >>= outputRect)
            (rects, l') <- doLayout ws.layout rect (intersect ws.wins wins)
            pure (rects, (tag, ws { layout = (fromMaybe ws.layout l') }))
        -- workspace is hidden
        eachWorkspace (tag, ws) = pure ([], (tag, ws))

    handleMessage l m | Just (WindowCreated a) <- fromMessage m = runMaybeT $ do
        -- Use the workspace of the current seat if it has an output,
        -- or the first output's workspace or else the first workspace.
        mseat <- lift currentSeat
        tag <- hoistMaybe $   (mseat <&> (.handle)
                                 >>= flip M.lookup l.seats
                                 >>= flip B.lookup l.outputs)
                          <|> (listToMaybe $ B.elems l.outputs)
                          <|> (listToMaybe $ M.keys l.workspaces)
        ws <- hoistMaybe $ M.lookup tag l.workspaces
        pure l { workspaces = M.insert tag
                                       ws { wins = a:ws.wins }
                                       l.workspaces
               }
    handleMessage l m | Just (WindowDestroyed a) <- fromMessage m
        = pure . Just $ l { workspaces = fmap (deleteWin a) l.workspaces }
      where
        deleteWin a ws = ws { wins = delete a ws.wins }

    handleMessage l m | Just (OutputCreated o) <- fromMessage @(WindowMessages a) m = runMaybeT $ do
        unassigned <- hoistMaybe $ listToMaybe $
                           M.keys l.workspaces \\  B.elems l.outputs
        pure l { outputs = B.insert o unassigned l.outputs }

    handleMessage l m | Just (OutputDestroyed o) <- fromMessage @(WindowMessages a) m = pure $ Just $
        l { outputs = B.delete o l.outputs
          , unassignedSeats = [s | (s, o') <- M.toList l.seats, o == o']
          }

    handleMessage l m | Just (SeatCreated s) <- fromMessage @(WindowMessages a) m =
     do firstOutput <- listToMaybe . M.keys . (.outputs) <$> get
        case firstOutput of
          Just o  -> pure $ Just $ l { seats = M.insert s o l.seats }
          Nothing -> pure $ Just $ l { unassignedSeats = s:l.unassignedSeats }

    handleMessage l m | Just (SeatDestroyed s) <- fromMessage @(WindowMessages a) m =
        pure $ Just $ l { seats = M.delete s l.seats
                        , unassignedSeats = delete s l.unassignedSeats
                        }

    handleMessage this m = runMaybeT $ do
        seat <- MaybeT currentSeat
        oh <- hoistMaybe $ M.lookup seat.handle this.seats
        tag <- hoistMaybe $ B.lookup oh this.outputs
        ws <- hoistMaybe $ M.lookup tag this.workspaces
        l' <- MaybeT $ handleMessage ws.layout m
        pure $ this { workspaces = M.insert tag (ws { layout = l'}) this.workspaces }
