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
      getDecl,
      getDecls,
      addDef,
      importDecls,
      getDefs
    ) where

import qualified Data.Foldable
import           Data.List (concatMap)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)

import           Control.Monad ((<=<))

import qualified Funk.AST as AST


data Module name = Module (Map.Map name (ModuleEntry name))

data ModuleEntry name = ModuleEntry {
      moduleEntryDecl :: Maybe (AST.Decl name),
      moduleEntryDefs :: [AST.Def name]
}

foldrModule :: (ModuleEntry name -> a -> a) -> a -> Module name -> a
foldrModule f a (Module m) = Data.Foldable.foldr f a m

mapModule :: (ModuleEntry name -> a) -> Module name -> [a]
mapModule f = foldrModule ((:) . f) []


getEntry :: Ord name => Module name -> name -> Maybe (ModuleEntry name)
getEntry (Module m) n = Map.lookup n m


-- |Create an empty Module.
empty :: Module name
empty = Module Map.empty

-- |Add a new declaration to the module.
-- If the declaration is already in the module, it is replaced. If
-- there is already a definition in the module but no declaration,
-- the given declaration is attached to the definition.
addDecl :: Ord name => AST.Decl name -> Module name -> Module name
addDecl d@(AST.Decl n _) (Module m) =
    Module $ Map.insert n (ModuleEntry (Just d) []) m

-- |Get a declaration from the module by name
-- Returns Nothing if the given name does not occur in the module
-- /or/ if the name is defined but has no type declaration.
getDecl ::  Ord name => Module name -> name -> Maybe (AST.Decl name)
getDecl m = moduleEntryDecl <=< getEntry m

-- |Get a list of all declaration in the module
-- Names with definitions but no type declarations are
-- not returned in this list.
getDecls :: Module name -> [AST.Decl name]
getDecls = catMaybes . mapModule moduleEntryDecl

-- |Add a new definition to a module
-- If the name is not already in the module, it is added. If the name
-- has a declaration, the definition is attached to that declaration.
-- If the name already has a definition or definitions, the new
-- definition is added to the list of definitions.
addDef :: Ord name => AST.Def name -> Module name -> Module name
addDef def@(AST.Def n _ _) (Module m) =
    Module $ Map.alter addDef' n m
        where
          addDef' (Just (ModuleEntry decl defs))
              = Just (ModuleEntry decl (def:defs))
          addDef' Nothing
              = Just (ModuleEntry Nothing [def])

-- |Add the declarations from one module to the namespace of another.
-- The definitions are not imported, only the declarations. If the
-- same name occurs in both module, the new name will override the
-- old one. The intention is that the definition of the name type
-- (specifically it's Ord instance) should be used to prevent or
-- control these collision as required.
importDecls :: Ord name => Module name -> Module name -> Module name
importDecls src dest = foldr addDecl dest (getDecls src)

-- Just returns all definitions for all functions as one big list
-- with no type information. This will want to be updated
-- once renaming and type checking are farther along.
getDefs :: Module name -> [AST.Def name]
getDefs (Module es) = concatMap moduleEntryDefs (Map.elems es)

