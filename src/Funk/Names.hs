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

module Funk.Names
       ( UnresolvedName(..),
         ResolvedName(..),
         Location(..)
       ) where


data UnresolvedName = UnresolvedName String
                      deriving (Show, Eq)
data ResolvedName = ResolvedName String Location
                    deriving (Show, Eq)

instance Ord ResolvedName where
    compare (ResolvedName n1 l1) (ResolvedName n2 l2)
        = case compare n1 n2 of
            LT -> LT
            EQ -> compare l1 l2
            GT -> GT

data Location = ModuleRef String
              | FunctionParamRef String
              | GlobalRef
              deriving (Show, Eq)

instance Ord Location where
    compare (FunctionParamRef n1) (FunctionParamRef n2) = compare n1 n2
    compare (FunctionParamRef _) _                      = GT
    compare (ModuleRef s1) (ModuleRef s2)               = compare s1 s2
    compare (ModuleRef _) _                             = GT
    compare GlobalRef GlobalRef                         = EQ
    compare GlobalRef _                                 = GT
