module EcsEdsl.CompileError where

import EcsEdsl.Declaration

data CompileError =
  TypeNotSupportedYet TypeDecl
