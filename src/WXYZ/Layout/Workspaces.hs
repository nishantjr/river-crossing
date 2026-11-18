module WXYZ.Layout.Workspaces (workspaces) where

import           Control.Monad (forM)
import           Control.Monad.State
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT, hoistMaybe)
import           Data.List (intersect)
import qualified Data.Map as M
import qualified Data.Bimap as B
import           Data.Maybe

import           WXYZ.River hiding (WXYZConfig(..))

-- Partitions the list of windows, and assigns them to workspaces.
-- Each output is assigned a workspace. These are the 'visible' workspaces.

workspaces :: [Int] -> l a -> Workspaces l a
workspaces tags l = Workspaces
    { seats = M.empty
    , outputs = B.empty
    , workspaces = M.fromList [(t,Workspace l []) | t <- tags]
    }

data Workspaces l a = Workspaces
    { seats      :: M.Map SeatHandle (OutputHandle)
    , outputs    :: B.Bimap OutputHandle Int
    , workspaces :: M.Map Int (Workspace l a)
    }
deriving instance (Show a, Show (l a)) => Show (Workspaces l a)

data Workspace l a = Workspace { layout :: l a
                               , wins   :: [a]
                               }
deriving instance (Show a, Show (l a)) => Show (Workspace l a)

instance (Eq a, Show a, LayoutClass l a) => LayoutClass (Workspaces l) a      where
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

    handleMessage this m = runMaybeT $ do
        seat <- MaybeT currentSeat
        oh <- hoistMaybe $ M.lookup seat.handle this.seats
        tag <- hoistMaybe $ B.lookup oh this.outputs
        ws <- hoistMaybe $ M.lookup tag this.workspaces
        l' <- MaybeT $ handleMessage ws.layout m
        pure $ this { workspaces = M.insert tag (ws { layout = l'}) this.workspaces }
