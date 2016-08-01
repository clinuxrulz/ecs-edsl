module EcsEdsl.ExprNode where

import EcsEdsl.MathNode

data ExprNode
  = LitBool Bool
  | LitInt Int
  | LitDouble Double
  | LitString String
  | Var Int
  | Math MathNode
