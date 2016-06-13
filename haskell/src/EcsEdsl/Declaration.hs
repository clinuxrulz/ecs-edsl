{-# LANGUAGE RankNTypes #-}

module EcsEdsl.Declaration where

import EcsEdsl.Expr

data Declaration
  = DeclComponentType ComponentTypeDecl
  | DeclEntity EntityDecl

data EntityDecl
  = EntityDecl
      [ComponentDecl] -- ^ Components

data ComponentTypeDecl
  = ComponentTypeDecl
      String          -- ^ Component Type Name
      [ParamTypeDecl] -- ^ Parameter Types

data ComponentDecl
  = ComponentDecl
      String      -- ^ Component Type Name
      [ParamDecl] -- ^ Parameter Values

data ParamDecl
  = ParamDecl
      String        -- ^ Name
      (forall r.
        (forall t.
          Expr t    -- ^ Value
          -> r
        ) -> r
      )

data ParamTypeDecl
  = ParamTypeDecl
      String    -- ^ Name
      TypeDecl  -- ^ Type

data TypeDecl
  = TDInt
  | TDDouble
  | TDString
  | TDMaybe TypeDecl
  | TDArray TypeDecl
