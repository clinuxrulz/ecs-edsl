{-# LANGUAGE RankNTypes #-}
module EcsEdsl.Monad.MonadEntitySystem where

import EcsEdsl.Monad.MonadEntity
import EcsEdsl.Types

class Monad m => MonadEntitySystem m where
  createEntity :: List Component -> m Entity
  destroyEntity :: Entity -> m ()
  withEntity :: forall m2 a. (MonadEntity m) => Entity -> m2 a -> m a
