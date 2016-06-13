{-# LANGUAGE RankNTypes #-}

module EcsEdsl.Stmt where

import EcsEdsl.Expr
import EcsEdsl.EntitySystemEff

data Stmt next
  = Eval (forall r. (forall a. (Expr a, a -> next) -> r) -> r)
  | LaterWithEntitySystem (EntitySystemEff ()) next
