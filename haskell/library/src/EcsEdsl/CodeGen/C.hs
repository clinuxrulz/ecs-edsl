{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module EcsEdsl.CodeGen.C where

import EcsEdsl.Program
import qualified EcsEdsl.Program as Program
import EcsEdsl.CompileError
import EcsEdsl.Declaration

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error.Class
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M

data CodeGenState =
  CodeGenState
    Int                              -- ^ Fresh variable id
    [String]                         -- ^ Current lines of code (backwards)
    (M.Map String ComponentTypeDecl) -- ^ Avaliable Component Types

stateInit :: CodeGenState
stateInit =
  CodeGenState
    0
    []
    M.empty

newtype CodeGenT m a = CodeGenT (ReaderT Program (StateT CodeGenState m) a)

unCodeGenT :: forall m a. CodeGenT m a -> ReaderT Program (StateT CodeGenState m) a
unCodeGenT (CodeGenT a) = a

instance (Monad m) => Functor (CodeGenT m) where
  fmap f (CodeGenT m) = CodeGenT $ fmap f m

instance (Monad m) => Applicative (CodeGenT m) where
  pure a = CodeGenT $ pure a
  (CodeGenT mf) <*> (CodeGenT ma) = CodeGenT $ mf <*> ma

instance (Monad m) => Monad (CodeGenT m) where
  (CodeGenT m) >>= f = CodeGenT $ m >>= (unCodeGenT . f)

stateGetFreshId :: CodeGenState -> Int
stateGetFreshId (CodeGenState freshId _ _) = freshId

stateSetFreshId :: Int -> CodeGenState -> CodeGenState
stateSetFreshId freshId (CodeGenState _ lines componentTypes) = CodeGenState freshId lines componentTypes

stateGetLines :: CodeGenState -> [String]
stateGetLines (CodeGenState _ lines _) = lines

stateModLines :: ([String] -> [String]) -> CodeGenState -> CodeGenState
stateModLines f (CodeGenState freshId lines componentTypes) = CodeGenState freshId (f lines) componentTypes

addLine :: forall m. (Monad m) => String -> CodeGenT m ()
addLine line = CodeGenT $ modify $ stateModLines (line :)

addLines :: forall m f. (Monad m, Foldable f) => f String -> CodeGenT m ()
addLines lines = traverse_ addLine lines

freshVarId :: forall m. (Monad m) => CodeGenT m Int
freshVarId = do
  freshId <- CodeGenT $ gets stateGetFreshId
  let freshId' = freshId + 1
  CodeGenT $ modify $ stateSetFreshId freshId'
  return freshId

compileToC :: forall m. (MonadError CompileError m) => Program -> m [String]
compileToC program = do
  lines <- evalStateT (runReaderT (unCodeGenT $ compile >> (CodeGenT $ gets stateGetLines)) program) stateInit
  return $ reverse lines

compile :: forall m. (MonadError CompileError m) => CodeGenT m ()
compile = do
  writeComponentTypeStructs

writeComponentTypeStructs :: forall m. (MonadError CompileError m) => CodeGenT m ()
writeComponentTypeStructs = do
  (CodeGenT $ asks Program.getComponentTypeDecls) >>= traverse_ writeComponentTypeStruct

writeComponentTypeStruct :: forall m. (MonadError CompileError m) => ComponentTypeDecl -> CodeGenT m ()
writeComponentTypeStruct (ComponentTypeDecl typeName paramTypes) = do
  addLine $ "struct " ++ typeName ++ " {"
  traverse_ writeParamType paramTypes
  addLine "};"

writeParamType :: forall m. (MonadError CompileError m) => ParamTypeDecl -> CodeGenT m ()
writeParamType (ParamTypeDecl n t) = do
  case t of
    TDInt -> addLine $ "int " ++ n ++ ";"
    TDDouble -> addLine $ "float " ++ n ++ ";"
    TDString -> addLine $ "std::string " ++ n ++ ";"
    _ -> CodeGenT $ throwError $ TypeNotSupportedYet t
