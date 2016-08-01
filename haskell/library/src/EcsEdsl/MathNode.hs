module EcsEdsl.MathNode where

data MathNode
  = Not Int
  | And Int Int
  | Or Int Int
  | AddInt Int Int
  | SubInt Int Int
  | MultInt Int Int
  | DivInt Int Int
  | ModInt Int Int
  | AddDouble Int Int
  | SubDouble Int Int
  | MultDouble Int Int
  | DivDouble Int Int
