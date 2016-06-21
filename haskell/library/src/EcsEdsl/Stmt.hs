{-# LANGUAGE RankNTypes #-}

module EcsEdsl.Stmt where

import EcsEdsl.Expr
import EcsEdsl.EntitySystemEff

import Control.Monad.Free

data StmtF next
  = Eval (forall r. (forall a. (Expr a, a -> next) -> r) -> r)
  | WithEntitySystem (EntitySystemEff ()) next

newtype Stmt a = Stmt (Free StmtF a)

unStmt :: forall a. Stmt a -> Free StmtF a
unStmt (Stmt a) = a

{- TODO: Define Functor instance for StmtF
instance Functor Stmt where
  fmap f (Stmt m) = Stmt $ fmap f m

instance Applicative Stmt where
  pure a = Stmt $ pure a
  (Stmt mf) <*> (Stmt ma) = Stmt $ mf <*> ma

instance Monad Stmt where
  (Stmt m) >>= f = Stmt $ m >>= (unStmt . f)
-}