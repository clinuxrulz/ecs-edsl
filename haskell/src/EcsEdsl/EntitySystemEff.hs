{-# LANGUAGE RankNTypes, DeriveFunctor, ScopedTypeVariables #-}

module EcsEdsl.EntitySystemEff where

import EcsEdsl.Monad.MonadEntitySystem
import EcsEdsl.Monad.MonadEntity
import EcsEdsl.EntityEff
import EcsEdsl.Types

import Control.Monad.Free

data EntitySystemEffF next
  = CreateEntity (TList Component) (Entity -> next)
  | DestroyEntity Entity next
  | WithEntity Entity (EntityEff next)
  deriving Functor

newtype EntitySystemEff a = EntitySystemEff (Free EntitySystemEffF a)

unEntitySystemEff :: forall a. EntitySystemEff a -> Free EntitySystemEffF a
unEntitySystemEff (EntitySystemEff a) = a

instance Functor EntitySystemEff where
  fmap f (EntitySystemEff m) = EntitySystemEff (fmap f m)

instance Applicative EntitySystemEff where
  pure a = EntitySystemEff $ pure a
  (EntitySystemEff mf) <*> (EntitySystemEff ma) = EntitySystemEff (mf <*> ma)

instance Monad EntitySystemEff where
  (EntitySystemEff m) >>= f = EntitySystemEff $ m >>= (unEntitySystemEff . f)

instance MonadEntitySystem EntitySystemEff where
  createEntity cs = EntitySystemEff $ liftF $ CreateEntity cs id
  destroyEntity e = EntitySystemEff $ liftF $ DestroyEntity e ()
  withEntity e m = EntitySystemEff $ liftF $ WithEntity e m
