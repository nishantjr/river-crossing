module WXYZ.Layout.Workspaces (
        workspaces,
        view, shift, WorkspaceMessages(..)
    ) where

import           Control.Applicative
import           Control.Monad (forM)
import           Control.Monad.State
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT, hoistMaybe)
import           Control.Monad.Extra (whenMaybe)
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

type Tag = Int

workspaces :: [Tag] -> l a -> Workspaces l a
workspaces tags l = Workspaces
    { seats = M.empty
    , outputs = B.empty
    , workspaces = M.fromList [(t,Workspace l []) | t <- tags]
    , unassignedSeats = []
    }

data Workspaces l a = Workspaces
    { seats      :: M.Map SeatHandle OutputHandle
    , outputs    :: B.Bimap OutputHandle Tag
    , workspaces :: M.Map Tag (Workspace l a)

    , unassignedSeats :: [SeatHandle] -- seats that haven't been assigned an
    }
deriving instance (Show a, Show (l a)) => Show (Workspaces l a)

data Workspace l a = Workspace { layout :: l a
                               , wins   :: [a]
                               }
deriving instance (Show a, Show (l a)) => Show (Workspace l a)

data WorkspaceMessages a
  = FocusOutput SeatHandle OutputHandle
  -- | View the tagged workspace on that output. If the workspace is already
  -- visible, do nothing.
  | WorkspaceForOutput OutputHandle Tag
  | MoveWindowToWorkspace a Tag
  | View       SeatHandle Tag
  -- TODO: GreedyView?

deriving instance (Show a) => Show (WorkspaceMessages a)
instance (Typeable a) => Message (WorkspaceMessages a)

toWorkspaceMessage :: forall a. Message (WorkspaceMessages a)
    => SomeMessage -> Maybe (WorkspaceMessages a)
toWorkspaceMessage = fromMessage @(WorkspaceMessages a)

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

    -- WindowManagement messages ----------------------------------------------
    handleMessage l m | Just (WindowCreated a) <- fromMessage m = runMaybeT $ do
        -- Use the workspace of the current seat if it has an output,
        -- or the first output's workspace or else the first workspace.
        mseat <- lift currentSeat'
        tag <- hoistMaybe $   (mseat
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

    -- Workspace Management ---------------------------------------------------
    handleMessage l m | Just (FocusOutput s o) <- toWorkspaceMessage @a m =
        whenMaybe (M.member s l.seats && B.member o l.outputs) $
            pure $ l { seats = M.insert s o l.seats }
    handleMessage l m | Just (WorkspaceForOutput o t) <- toWorkspaceMessage @a m =
        whenMaybe (M.member t l.workspaces && B.member o l.outputs) $
            pure $ l { outputs = B.insert o t l.outputs } --- XXX
    handleMessage l m | Just (MoveWindowToWorkspace w t) <- toWorkspaceMessage @a m =
        whenMaybe (M.member t l.workspaces) $ pure $ l { workspaces =
            M.mapWithKey (\t' ws -> if (t == t') then ws { wins = w:(delete w ws.wins) }
                                                 else ws { wins = delete w ws.wins     }
                         ) l.workspaces }

    handleMessage l m | Just (View s t) <- toWorkspaceMessage @a m = runMaybeT $ do
        o <- hoistMaybe $ M.lookup s l.seats
        l' <- MaybeT (handleMessage l $ SomeMessage $ WorkspaceForOutput @a o t)
        -- If workspace was already assigned, o and o' may differ.
        o' <- hoistMaybe $ B.lookupR t l'.outputs
        MaybeT $ handleMessage l' $ SomeMessage $ FocusOutput @a s o'

    -- Fall back --------------------------------------------------------------
    handleMessage this m = runMaybeT $ do
        seat <- MaybeT currentSeat'
        oh <- hoistMaybe $ M.lookup seat this.seats
        tag <- hoistMaybe $ B.lookup oh this.outputs
        ws <- hoistMaybe $ M.lookup tag this.workspaces
        l' <- MaybeT $ handleMessage ws.layout m
        pure $ this { workspaces = M.insert tag (ws { layout = l'}) this.workspaces }


view :: Tag -> SeatHandle -> WXYZ ()
view t s = sendMessage (View @WindowHandle s t)

shift :: Tag -> SeatHandle -> WXYZ ()
shift t s = void $ runMaybeT $ (MaybeT $ focusedWindow s)
                           >>= (\w -> lift $ sendMessage (MoveWindowToWorkspace w t))
