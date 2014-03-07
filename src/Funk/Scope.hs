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

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Funk.Scope
       ( ScopedName(..),
         Scope,
         createGlobalScope,
         createScope,
         addNameToScope,
         findName
       ) where

class ScopedName n l | n -> l where
  name :: n -> String
  location :: n -> l
  scopedName :: String -> l -> n


data Scope n = Scope [n] (Scope n)
             | GlobalScope [n]


createGlobalScope :: ScopedName n l => Scope n
createGlobalScope = undefined


createScope :: ScopedName n l => Scope n -> l -> Scope n
createScope = undefined


addNameToScope :: ScopedName n l => Scope n -> String -> Scope n
addNameToScope = undefined


findName :: ScopedName n l => Scope n -> String -> Maybe n
findName = undefined
