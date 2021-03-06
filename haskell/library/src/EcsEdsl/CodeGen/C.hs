{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module EcsEdsl.CodeGen.C where

import EcsEdsl.Program
import qualified EcsEdsl.Program as Program
import EcsEdsl.CompileError
import EcsEdsl.Declaration

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error.Class
import Data.Foldable (sequence_, traverse_)
import Data.List (intersperse)
import qualified Data.Map.Strict as M

data CodeGenState =
  CodeGenState
    Int                              -- ^ Fresh variable id
    [String]                         -- ^ Current lines of code (backwards)
    Int                              -- ^ Indent level
    (M.Map String ComponentTypeDecl) -- ^ Avaliable Component Types

stateInit :: CodeGenState
stateInit =
  CodeGenState
    0
    []
    0
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
stateGetFreshId (CodeGenState freshId _ _ _) = freshId

stateSetFreshId :: Int -> CodeGenState -> CodeGenState
stateSetFreshId freshId (CodeGenState _ lines indent componentTypes) = CodeGenState freshId lines indent componentTypes

stateGetLines :: CodeGenState -> [String]
stateGetLines (CodeGenState _ lines _ _) = lines

stateModLines :: ([String] -> [String]) -> CodeGenState -> CodeGenState
stateModLines f (CodeGenState freshId lines indent componentTypes) = CodeGenState freshId (f lines) indent componentTypes

stateGetIndent :: CodeGenState -> Int
stateGetIndent (CodeGenState _ _ indent _) = indent

stateModIndent :: (Int -> Int) -> CodeGenState -> CodeGenState
stateModIndent f (CodeGenState freshId lines indent componentTypes) = CodeGenState freshId lines (f indent) componentTypes

getIndentStr :: forall m. (Monad m) => CodeGenT m String
getIndentStr = do
  indent <- CodeGenT $ gets stateGetIndent
  return $ indentStr indent
  where
    indentStr 0 = ""
    indentStr n = "  " ++ indentStr (n - 1)

indentBlock :: forall m a. (Monad m) => CodeGenT m a -> CodeGenT m a
indentBlock block = do
  CodeGenT $ modify $ stateModIndent (\x -> x + 1)
  r <- block
  CodeGenT $ modify $ stateModIndent (\x -> x - 1)
  return r

addLine :: forall m. (Monad m) => String -> CodeGenT m ()
addLine line = do
  indentStr <- getIndentStr
  CodeGenT $ modify $ stateModLines ((indentStr ++ line) :)

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
  writePrelude
  addLine ""
  writeComponentTypeStructs

writePrelude :: forall m. (Monad m) => CodeGenT m ()
writePrelude = do
  addLines
    [
      "struct vec2 {",
      "  float x, y;",
      "};",
      "",
      "struct complex {",
      "  float real, imag;",
      "};"
    ]

writeComponentTypeStructs :: forall m. (MonadError CompileError m) => CodeGenT m ()
writeComponentTypeStructs = do
  types <- (CodeGenT $ asks Program.getComponentTypeDecls)
  (sequence_ $ intersperse (addLine "") (writeComponentTypeStruct <$> types))

writeComponentTypeStruct :: forall m. (MonadError CompileError m) => ComponentTypeDecl -> CodeGenT m ()
writeComponentTypeStruct (ComponentTypeDecl typeName paramTypes) = do
  addLine $ "struct " ++ typeName ++ " {"
  indentBlock
    (traverse_ writeParamType paramTypes)
  addLine "};"

writeParamType :: forall m. (MonadError CompileError m) => ParamTypeDecl -> CodeGenT m ()
writeParamType (ParamTypeDecl n t) = do
  case t of
    TDInt -> addLine $ "int " ++ n ++ ";"
    TDDouble -> addLine $ "float " ++ n ++ ";"
    TDString -> addLine $ "std::string " ++ n ++ ";"
    TDVec2 -> addLine $ "vec2 " ++ n ++ ";"
    TDComplex -> addLine $ "complex " ++ n ++ ";"
    _ -> CodeGenT $ throwError $ TypeNotSupportedYet t
