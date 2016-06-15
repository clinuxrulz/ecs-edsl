module Main where

import EcsEdsl.Program
import EcsEdsl.Declaration

program =
  Program
    -- Component Types
    [
      ComponentTypeDecl
        "Axes"
        [
          ParamTypeDecl "origin" TDVec2,
          ParamTypeDecl "orient" TDComplex
        ],
      ComponentTypeDecl
        "Velocity"
        [
          ParamTypeDecl "velocity" TDVec2
        ],
      ComponentTypeDecl "Gravity" [],
      ComponentTypeDecl "Player" []
    ]
    -- Systems
    [
    ]
    -- Initial Entities
    []

main = putStrLn "Test goes here."