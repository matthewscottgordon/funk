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
       ( rename
       ) where

import Funk.AST
import Funk.Names (RawName,
                   UnresolvedName(..),
                   ResolvedName(..),
                   Location(..))
import Funk.Scope (addNameToScope, createGlobalScope, createScope)
import qualified Funk.Scope

import Data.List (foldl')
import Control.Monad.Error


type Scope = Funk.Scope.Scope ()

rename :: MonadError String m => Module RawName -> m (Module (ResolvedName ()))
rename (Module decls defs) = do
  defs' <- mapM (renameDef moduleScope) defs
  let decls' = []
  return (Module decls' defs')
  where
    moduleScope = foldl' addDefToScope createGlobalScope defs


addDefToScope :: Scope -> Def RawName -> Scope
addDefToScope s (Def n _ _) = Funk.Scope.addNameToScope s n


findName :: MonadError a m => Scope -> String -> m (ResolvedName ())
findName scope n = case Funk.Scope.findName scope n of
  Just n' -> return n'
  Nothing -> fail ("Undefined: \"" ++ n ++ "\"")

renameDef :: MonadError String m => Scope -> Def RawName ->
             m (Def (ResolvedName ()))
renameDef scope (Def (UnresolvedName fName _) ps fBody) = do
  let location = FunctionParamRef fName
      ps' = map paramToRef ps
      scope' = foldl' addParamToScope (createScope scope location) ps
  fName' <- findName scope fName
  fBody' <- renameExpr scope' fBody
  return (Def fName' ps' fBody')
  where
    addParamToScope s n = addNameToScope s n
    paramToRef (UnresolvedName n _) =
      ResolvedName n () (FunctionParamRef fName)

renameExpr :: MonadError a m => Scope -> Expr RawName ->
              m (Expr (ResolvedName()))
renameExpr _ (FloatLiteral v) = return (FloatLiteral v)
renameExpr s (Call (UnresolvedName n _bb) exprs) = do
  n' <- findName s n
  exprs' <- mapM (renameExpr s) exprs
  return (Call n' exprs')
renameExpr s (Op (UnresolvedName n _) left right) = do
  n' <- findName s n
  left' <- renameExpr s left
  right' <- renameExpr s right
  return (Op n' left' right')
renameExpr s (VarRef (UnresolvedName n _)) = do
  n' <- findName s n
  return (VarRef n')
