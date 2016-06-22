{-# LANGUAGE RankNTypes #-}

module EcsEdsl.Monad.MonadEntitySystem where

import EcsEdsl.Monad.MonadEntity
import EcsEdsl.Types

class Monad m => MonadEntitySystem m where
  createEntity :: m Entity
  destroyEntity :: Entity -> m ()
  withEntity :: forall a. Entity -> (forall m2 r. (MonadEntity m2 => m2 a -> r) -> r) -> m a
