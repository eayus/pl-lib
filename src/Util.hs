module Util where

import Data.Traversable
import Polysemy
import Polysemy.State


enumerate :: Traversable f => f a -> f (Int, a)
enumerate x = run $ evalState 0 $ for x \a -> do
    n <- get
    modify (+1)
    pure (n, a)