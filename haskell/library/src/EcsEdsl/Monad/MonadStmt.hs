{-# LANGUAGE RankNTypes #-}

module EcsEdsl.Monad.MonadStmt where

import EcsEdsl.Monad.MonadEntitySystem
import EcsEdsl.Expr

class Monad m => MonadStmt m where
  eval :: forall a. Expr a -> m a
  withEntitySystem :: forall m2 a. (MonadEntitySystem m2) => m2 a -> m a
