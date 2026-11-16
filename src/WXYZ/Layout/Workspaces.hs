module WXYZ.Layout.Workspaces (workspaces) where

import           Control.Monad (forM)
import           Control.Monad.State
import           Data.List (intersect)
import qualified Data.Map as M
import           Data.Maybe

import           WXYZ.River hiding (WXYZConfig(..))

-- Partitions the list of windows, and assigns them to workspaces.
-- Each output is assigned a workspace. These are the 'visible' workspaces.

workspaces :: [Int] -> l a -> Workspaces l a
workspaces tags l = Workspaces M.empty [Workspace t l [] | t <- tags]

data Workspaces l a = Workspaces
    { visible  :: M.Map OutputHandle (Workspace l a)
    , hidden   :: [Workspace l a]
    }
deriving instance (Show a, Show (l a)) => Show (Workspaces l a)

data Workspace l a = Workspace { tag    :: Int
                               , layout :: l a
                               , wins   :: [a]
                               }
deriving instance (Show a, Show (l a)) => Show (Workspace l a)

instance (Eq a, Show a, LayoutClass l a) => LayoutClass (Workspaces l) a      where
    doLayout l _r wins = do
        ls :: [([(a, Rectangle)], (OutputHandle, Workspace l a))] <- forM (M.toList l.visible) eachOutput
        pure ( concatMap fst ls
             , Just $ l { visible = M.fromList $ fmap snd ls }
             )
      where
        eachOutput (oh, ws) = do
            st <- get
            let mo = M.lookup oh st.outputs
                rect = fromMaybe (Rectangle 0 0 1920 1080) (mo >>= outputRect)
            (rects, l') <- doLayout ws.layout rect (intersect ws.wins wins)
            pure (rects, (oh, ws { layout = (fromMaybe ws.layout l') }))

