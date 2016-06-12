{-# LANGUAGE GADTs #-}

module EcsEdsl.EntitySystemEffF where

import EcsEdsl.Types

data EntitySystemEffF next where
  CreateEntity :: List Component -> (Entity -> next) -> EntitySystemEffF Entity
