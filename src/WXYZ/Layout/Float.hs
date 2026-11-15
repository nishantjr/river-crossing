module WXYZ.Layout.Float (float, FloatMessages(..), toggleFloating) where

import           Prelude hiding (Float, Floating)

import           Control.Arrow (first, second)
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Data.Typeable (Typeable)
import qualified Data.Map as M


import           WXYZ.River

-- | Construct the floating layout.
float :: l a -> Floating l a
float l = Floating l M.empty

-- | Send a message toggling floating status for a window.
toggleFloating :: Rectangle -> WindowHandle -> WXYZ ()
toggleFloating r w = sendMessage $ ToggleFloat w r


data Floating l a =
    Floating { sublayout :: l a
             , floating :: M.Map a Rectangle
             }
deriving instance (Show a, Show (l a)) => Show (Floating l a)

data FloatMessages a = Float a Rectangle
                     | Sink a
                     | ToggleFloat a Rectangle
instance (Typeable a) => Message (FloatMessages a)

instance (Ord a, Show a, Typeable a, Show (l a), LayoutClass l a)
    => LayoutClass (Floating l) a      where
    doLayout l r ws = do
        let (fRects, rest) = partitionMaybe (flip M.lookup l.floating) ws
        (restRects, sublayout') <- doLayout l.sublayout r rest
        pure ( fRects ++ restRects
             , Floating <$> sublayout' <*> (Just l.floating)
             )

    handleMessage l m | Just (Float a r) <- fromMessage m = pure $ Just $
        Floating l.sublayout (M.insert a r l.floating)
    handleMessage l m | Just (Sink a) <- fromMessage m = pure $ Just $
        Floating l.sublayout (M.delete a l.floating)

    handleMessage l m | Just (ToggleFloat a r) <- fromMessage m =
        if (M.member a l.floating) then handleMessage l (SomeMessage $ Sink  a)
                                   else handleMessage l (SomeMessage $ Float a r)

    handleMessage l m = runMaybeT $
        Floating <$> (MaybeT $ handleMessage l.sublayout m)
                 <*> (pure l.floating)

partitionMaybe :: (a -> Maybe b) -> [a] -> ([(a, b)], [a])
partitionMaybe f = loop
  where
    loop [] = ([], [])
    loop (a:as) = case f a of
      Just b  -> first  ((a, b) :) $ loop as
      Nothing -> second ( a     :) $ loop as

