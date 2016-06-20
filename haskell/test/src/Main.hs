module Main where

import EcsEdsl.Program
import EcsEdsl.Declaration
import EcsEdsl.CompileError
import EcsEdsl.CodeGen.C

import Data.Foldable (traverse_)

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

main = do
  either
    (\err ->
      putStrLn $ "Error: " ++
        (case err of
          TypeNotSupportedYet t -> "Type not supported yet."
        )
    )
    (traverse_ putStrLn)
    (compileToC program)
