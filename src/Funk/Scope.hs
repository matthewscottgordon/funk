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

{-# LANGUAGE MultiParamTypeClasses #-}

module Funk.Scope
       ( Scope,
         createGlobalScope,
         createScope,
         addNameToScope,
         findName
       ) where

import Funk.Names (Location(..),
                   ResolvedName(..),
                   UnresolvedName(..))

import Data.List (find)

data Scope = Scope Location [UnresolvedName] (Scope)
             | GlobalScope [UnresolvedName]


createGlobalScope :: Scope
createGlobalScope = GlobalScope []


createScope :: Scope -> Location  -> Scope
createScope s l = Scope l [] s


addNameToScope :: Scope -> UnresolvedName -> Scope
addNameToScope (GlobalScope ns) n= GlobalScope (n:ns)
addNameToScope (Scope l ns s') n = Scope l (n:ns) s'


findName :: Scope -> String -> Maybe ResolvedName
findName (Scope l ns s) n = case find (hasName n) ns of
  Just (UnresolvedName n') -> Just (ResolvedName n' l)
  Nothing -> findName s n
findName (GlobalScope ns) n = case find (hasName n) ns of
  Just (UnresolvedName n') -> Just (ResolvedName n' GlobalRef)
  Nothing -> Nothing


hasName :: String -> UnresolvedName -> Bool
hasName s (UnresolvedName s') = s == s'
