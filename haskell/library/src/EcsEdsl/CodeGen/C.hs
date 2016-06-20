module EcsEdsl.CodeGen.C where

import EcsEdsl.Program
import EcsEdsl.Declaration
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as M

data CodeGenState =
  CodeGenState
    Int                              -- ^ Fresh variable id
    [String]                         -- ^ Current lines of code (backwards)
    (M.Map String ComponentTypeDecl) -- ^ Avaliable Component Types

newtype CodeGenT m a = CodeGenT (ReaderT Program (StateT CodeGenState m) a)
