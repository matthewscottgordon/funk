{-  Copyright 2014 Matthew Gordon.

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

module Funk.Module
    ( Module,
      empty,
      addDecl,
      addDef,
      getFunctions
    ) where

import qualified Data.Map.Strict as Map

import qualified Funk.AST as AST

data Module name = Module (Map.Map name (ModuleEntry name))

data ModuleEntry name =
    ModuleEntry (Maybe (AST.Decl name)) [AST.Def name]


empty :: Module name
empty = Module Map.empty


addDecl :: Ord name => AST.Decl name -> Module name -> Module name
addDecl d@(AST.Decl n _) (Module m) =
    Module $ Map.insert n (ModuleEntry (Just d) []) m


addDef :: Ord name => AST.Def name -> Module name -> Module name
addDef def@(AST.Def n _ _) (Module m) =
    Module $ Map.alter addDef' n m
        where
          addDef' (Just (ModuleEntry decl defs))
              = Just (ModuleEntry decl (def:defs))
          addDef' Nothing
              = Just (ModuleEntry Nothing [def])

-- Just returns the first equation for each function,
-- with no type information. This will want to be updated
-- once renaming and type checking are farther along.
getFunctions :: Module name -> [AST.Def name]
getFunctions (Module es) = map toDef (Map.elems es)
    where
      toDef (ModuleEntry _ ds) = head ds
