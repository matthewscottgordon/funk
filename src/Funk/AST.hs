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

module Funk.AST where

data Module = Module [Def]
            deriving Show

data Name = Name String
          deriving (Eq, Show)

data Def = Def Name [Name] Expr
         deriving (Eq, Show)

data Expr = FloatLiteral Double
          | Call Name [Expr]
          | VarRef Name
          deriving (Eq, Show)
                   
