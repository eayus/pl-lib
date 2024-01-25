module Example.LC where

import Control.Monad
import Polysemy
import Polysemy.BiTC
import Polysemy.Error
import Polysemy.Reader


type Name = String


data Type
    = Base
    | Fun Type Type
    deriving Eq


type Ctxt = [(Name, Type)]


data ExprF r
    = Var Name
    | Lam Name r
    | App r r
    | Ano r Type
    deriving (Functor, Foldable, Traversable)


inf :: Members '[Typing a Type, Reader Ctxt, Error ()] r => ExprF a -> Sem r Type
inf = \case

    Var s -> asks (lookup s) >>= \case
        Just t -> pure t
        Nothing -> throw ()

    Lam _ _ -> throw ()

    App x y -> infer x >>= \case
        Fun t u -> do
            check y t
            pure u
        Base -> throw ()

    Ano x t -> do
        check x t
        pure t


chk :: Members '[Typing a Type, Reader Ctxt, Error ()] r => ExprF a -> Type -> Sem r ()
chk = \case

    Lam s x -> \case
        Fun t u -> local ((s, t) :) $ check x u
        Base -> throw ()

    x -> \t -> do
        u <- inf x
        unless (t == u) $ throw ()


instance Members '[Reader Ctxt, Error ()] r => Rules ExprF Type r where
    inference = inf
    checking = chk