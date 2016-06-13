{-# LANGUAGE RankNTypes, DeriveFunctor #-}

module EcsEdsl.EntityEff where

import EcsEdsl.Monad.MonadEntity
import EcsEdsl.Types

import Control.Monad.Free

data EntityEffF next
  = SetComponent Component next
  | UnsetComponent ComponentType next
  | LookupComponent ComponentType (TMaybe Component -> next)
  deriving Functor

newtype EntityEff a = EntityEff (Free EntityEffF a)

unEntityEff :: forall a. EntityEff a -> Free EntityEffF a
unEntityEff (EntityEff a) = a

instance Functor EntityEff where
  fmap f (EntityEff a) = EntityEff $ fmap f a

instance Applicative EntityEff where
  pure a = EntityEff $ pure a
  (EntityEff mf) <*> (EntityEff ma) = EntityEff $ mf <*> ma

instance Monad EntityEff where
  (EntityEff ma) >>= f = EntityEff $ ma >>= (unEntityEff . f)

instance MonadEntity EntityEff where
  setComponent c = EntityEff $ liftF $ SetComponent c ()
  unsetComponent ct = EntityEff $ liftF $ UnsetComponent ct ()
  lookupComponent ct = EntityEff $ liftF $ LookupComponent ct id
