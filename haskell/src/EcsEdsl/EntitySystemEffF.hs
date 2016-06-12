{-# LANGUAGE GADTs #-}

module EcsEdsl.Language.EntitySystemEffF where

import EcsEdsl.Types

data EntitySystemEffF next where
  CreateEntity :: List Component -> (Entity -> next) -> EntitySystemEffF Entity
