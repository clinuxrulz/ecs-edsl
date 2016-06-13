{-# LANGUAGE RankNTypes #-}

module EcsEdsl.Declaration where

data Declaration
  = DeclComponentType ComponentTypeDecl
  | DeclEntity EntityDecl

data ComponentTypeDecl
  = ComponentTypeDecl
      String      -- ^ Component Type Name
      [ParamDecl] -- ^ Parameters

data EntityDecl

data ParamDecl
  = ParamDecl
      String    -- ^ Name
      TypeDecl  -- ^ Type

data TypeDecl
  = TDInt
  | TDDouble
  | TDString
  | TDMaybe TypeDecl
  | TDArray TypeDecl
