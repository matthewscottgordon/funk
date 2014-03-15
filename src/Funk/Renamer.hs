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


data RenameError = RenameError String
type RenameEither a = Either RenameError a

instance Error RenameError where
  strMsg = RenameError

type Scope = Funk.Scope.Scope ()

rename :: Module RawName -> RenameEither (Module (ResolvedName ()))
rename (Module defs fdefs) = do
  defs' <- mapM (renameDef moduleScope) defs
  return (Module defs' fdefs')
  where
    fdefs' = undefined
    moduleScope = foldl' addDefToScope createGlobalScope defs


addDefToScope :: Scope -> Def RawName -> Scope
addDefToScope s (Def n _ _) = Funk.Scope.addNameToScope s n


findName :: Scope -> String -> Either RenameError (ResolvedName ())
findName scope n = case Funk.Scope.findName scope n of
  Just n' -> Right n'
  Nothing -> Left (RenameError n)

renameDef :: Scope -> Def RawName ->
             RenameEither (Def (ResolvedName ()))
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

renameExpr :: Scope -> Expr RawName ->
              RenameEither (Expr (ResolvedName()))
renameExpr _ (FloatLiteral v) = Right (FloatLiteral v)
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
