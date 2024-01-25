module Polysemy.BiTC.Binding where

import Control.Comonad.Cofree
import Data.Proxy
import Polysemy
import Polysemy.BiTC
import Polysemy.Error
import Util


data Binding v t m a where
    LookupVar :: v -> Binding v t m t
    Extend :: v -> t -> m a -> Binding v t m a

makeSem ''Binding


subst :: (Rules f t '[Binding v t, Error err]) => Proxy err -> Cofree f t -> v -> Cofree f t -> Cofree f t
subst _ arg v (t :< x) = t :< do
    _