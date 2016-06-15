module EcsEdsl.Program where

import EcsEdsl.Declaration

data Program
  = Program
      [ComponentTypeDecl]
      [SystemDecl]
      [EntityDecl]
