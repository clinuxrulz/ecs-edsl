{-# LANGUAGE RankNTypes #-}

module EcsEdsl.Declaration where

import EcsEdsl.Expr
import EcsEdsl.Stmt
import EcsEdsl.Types

data Declaration
  = DeclComponentType ComponentTypeDecl
  | DeclEntity EntityDecl
  | DeclSystem SystemDecl

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
      String        -- ^ Parameter Name
      (forall r.
        (forall t.
          Expr t    -- ^ Parameter Value
          -> r
        ) -> r
      )

data ParamTypeDecl
  = ParamTypeDecl
      String    -- ^ Parameter Name
      TypeDecl  -- ^ Parameter Type

data TypeDecl
  = TDInt
  | TDDouble
  | TDString
  | TDMaybe TypeDecl
  | TDArray TypeDecl

data SystemDecl
  = SystemDecl
      String                               -- ^ System Name
      [ComponentTypeDecl]                  -- ^ Listens to Component Types
      (Stmt ())                            -- ^ On Tick Handler
      (Entity -> Stmt ())                  -- ^ Entity Added Handler
      (Entity -> Stmt ())                  -- ^ Entity Removed Handler
      (Entity -> Component -> Stmt ())     -- ^ Component Set Handler
      (Entity -> ComponentType -> Stmt ()) -- ^ Component Unset Handler
