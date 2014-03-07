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

module Funk.Names
       ( RawName(..),
         ResolvedName(..),
         Location(..)
       ) where

import Funk.Scope (ScopedName(..))

data RawName = RawName String
             deriving (Eq, Show)



data ResolvedName = ResolvedName String Location

data Location = ModuleRef String
              | FunctionParamRef String

instance ScopedName ResolvedName Location where
  name (ResolvedName s _) = s
  location (ResolvedName _ l) = l
  scopedName = ResolvedName
