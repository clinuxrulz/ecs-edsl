{-# LANGUAGE RankNTypes #-}

module EcsEdsl.Monad.MonadEntitySystem where

import EcsEdsl.Monad.MonadEntity
import EcsEdsl.EntityEff
import EcsEdsl.Types

class Monad m => MonadEntitySystem m where
  createEntity :: m Entity
  destroyEntity :: Entity -> m ()
  withEntity :: forall a. Entity -> EntityEff a -> m a
