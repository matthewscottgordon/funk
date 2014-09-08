{-  Copyright 2013 Matthew Gordon.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express orimplied.
See the License for the specific language governing permissions and
limitations under the License.
-}

{-# LANGUAGE FlexibleContexts #-}

module Funk.Renamer
       ( rename,
         collect
       ) where

import qualified Funk.AST as AST
import qualified Funk.Module as Module
import Funk.Names (UnresolvedName(..),
                   ResolvedName(..),
                   Location(..))
import Funk.Scope (Scope, addNameToScope, createGlobalScope,
                   createScope)
import qualified Funk.Scope

import           Control.Monad.Error
import           Data.List (foldl')


rename :: MonadError String m => AST.Module UnresolvedName -> m (AST.Module ResolvedName)
rename (AST.Module decls defs) = do
  defs' <- mapM (renameDef moduleScope) defs
  decls' <- mapM (renameDecl moduleScope) decls
  return (AST.Module decls' defs')
  where
    moduleScope = foldl' addDefToScope createGlobalScope defs


collect :: MonadError String m => AST.Module ResolvedName -> m (Module.Module ResolvedName)
collect (AST.Module decls defs) = do
  let module' = foldr Module.addDecl Module.empty decls
  return $ foldr Module.addDef module' defs


addDefToScope :: Scope -> AST.Def UnresolvedName -> Scope
addDefToScope s (AST.Def n _ _) = Funk.Scope.addNameToScope s n


findName :: MonadError a m => Scope -> String -> m ResolvedName
findName scope n = case Funk.Scope.findName scope n of
  Just n' -> return n'
  Nothing -> fail ("Undefined: \"" ++ n ++ "\"")


renameDecl :: MonadError String m =>
              Scope -> AST.Decl UnresolvedName ->
                       m (AST.Decl ResolvedName)
renameDecl scope (AST.Decl (UnresolvedName n) t) = do
  n' <- findName scope n
  return (AST.Decl n' t)


renameDef :: MonadError String m => Scope -> AST.Def UnresolvedName ->
             m (AST.Def ResolvedName)
renameDef scope (AST.Def (UnresolvedName fName) ps fBody) = do
  let location = FunctionParamRef fName
      ps' = map paramToRef ps
      scope' = foldl' addParamToScope (createScope scope location) ps
  fName' <- findName scope fName
  fBody' <- renameExpr scope' fBody
  return (AST.Def fName' ps' fBody')
  where
    addParamToScope s n = addNameToScope s n
    paramToRef (UnresolvedName n) =
      ResolvedName n (FunctionParamRef fName)

renameExpr :: MonadError a m => Scope -> AST.Expr UnresolvedName ->
              m (AST.Expr ResolvedName)
renameExpr _ (AST.FloatLiteral v) = return (AST.FloatLiteral v)
renameExpr s (AST.Call (UnresolvedName n) exprs) = do
  n' <- findName s n
  exprs' <- mapM (renameExpr s) exprs
  return (AST.Call n' exprs')
renameExpr s (AST.Op (UnresolvedName n) left right) = do
  n' <- findName s n
  left' <- renameExpr s left
  right' <- renameExpr s right
  return (AST.Op n' left' right')
renameExpr s (AST.VarRef (UnresolvedName n)) = do
  n' <- findName s n
  return (AST.VarRef n')
