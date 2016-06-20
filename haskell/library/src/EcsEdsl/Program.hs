module EcsEdsl.Program where

import EcsEdsl.Declaration

data Program
  = Program
      [ComponentTypeDecl]
      [SystemDecl]
      [EntityDecl]

getComponentTypeDecls :: Program -> [ComponentTypeDecl]
getComponentTypeDecls (Program a _ _) = a
