{-# LANGUAGE GADTs #-}

module EcsEdsl.Expr where

import EcsEdsl.Types

data Expr a where
  LitInt :: Int -> Expr TInt
  LitDouble :: Double -> Expr TDouble
  LitString :: String -> Expr TString
  AddInt :: Expr TInt -> Expr TInt -> Expr TInt
  SubInt :: Expr TInt -> Expr TInt -> Expr TInt
  MultInt :: Expr TInt -> Expr TInt -> Expr TInt
  DivInt :: Expr TInt -> Expr TInt -> Expr TInt
  ModInt :: Expr TInt -> Expr TInt -> Expr TInt
  AddDouble :: Expr TDouble -> Expr TDouble -> Expr TDouble
  SubDouble :: Expr TDouble -> Expr TDouble -> Expr TDouble
  TimesDouble :: Expr TDouble -> Expr TDouble -> Expr TDouble
  DivDouble :: Expr TDouble -> Expr TDouble -> Expr TDouble
