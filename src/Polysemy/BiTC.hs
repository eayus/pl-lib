module Polysemy.BiTC where

import Control.Comonad
import Control.Comonad.Cofree
import Data.Fix
import Data.Traversable
import Polysemy
import Polysemy.Writer
import Util


-- Weird type checkers might check or infer something twice...


data Typing e t m a where
    Infer :: e -> Typing e t m t
    Check :: e -> t -> Typing e t m ()

makeSem ''Typing


class Rules f t r where
    inference :: forall a. f a -> Sem (Typing a t ': r) t
    checking :: forall a. f a -> t -> Sem (Typing a t ': r) ()


typecheck :: (Traversable f, Rules f t r) => Fix f -> Maybe t -> Sem r (Cofree f t)
typecheck (Fix x) t = do
    let x' = enumerate x
    (xs, u) <- runWriter $ runTyping $ applyRule x' t
    x'' <- for x' \(i, e) -> case lookup i xs of
        Just v -> pure v
        Nothing -> typecheck e Nothing
    pure $ u :< x''


runTyping :: (Traversable f, Rules f t r) => Sem (Typing (Int, Fix f) t ': r) a -> Sem (Writer [(Int, Cofree f t)] ': r) a
runTyping = reinterpret \case
    Infer (i, e) -> do
        e' <- raise $ typecheck e Nothing
        tell [(i, e')]
        pure $ extract e'
    Check (i, e) t -> do
        e' <- raise $ typecheck e $ Just t
        tell [(i, e')]
        pure ()


applyRule :: Rules f t r => f a -> Maybe t -> Sem (Typing a t ': r) t
applyRule e = \case
    Just t -> t <$ checking e t
    Nothing -> inference e