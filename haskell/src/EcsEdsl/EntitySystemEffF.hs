{-# LANGUAGE RankNTypes, DeriveFunctor #-}

module EcsEdsl.EntitySystemEffF where

import EcsEdsl.Monad.MonadEntitySystem
import EcsEdsl.Monad.MonadEntity
import EcsEdsl.Types

import Control.Monad.Free

data EntitySystemEffF next
  = CreateEntity (List Component) (Entity -> next)
  | DestroyEntity Entity next
  | WithEntity (forall r. (forall m a. (MonadEntity m) => WithEntityF m a next -> r) -> r)

data WithEntityF m a next = WithEntityF Entity (m a) (a -> next)

instance Functor (EntitySystemEffF) where
  fmap f (CreateEntity cs k) = CreateEntity cs (f . k)
  fmap f (DestroyEntity e n) = DestroyEntity e (f n)
  fmap f (WithEntity c) =
    c (\x ->
      case x of
        WithEntityF e ma k -> WithEntity (\k2 -> k2 $ WithEntityF e ma (f . k))
    )

instance Functor (WithEntityF m a) where
  fmap f (WithEntityF e ma k) = WithEntityF e ma (f . k)

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
  withEntity e m = EntitySystemEff $ liftF $ WithEntity (\k -> k $ WithEntityF e m id)
