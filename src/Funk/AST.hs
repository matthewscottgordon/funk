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

data Module name = Module [Decl name] [Def name]
                 deriving Show

data Decl name = Decl name Type
                 deriving (Eq, Show)

data Type = TypeName String
          | FuncType Type Type
            deriving (Eq, Show)

data Def name = Def (name) [name] (Expr name)
                deriving (Eq, Show)

data Expr name = FloatLiteral Double
               | Call (name) [Expr name]
               | Op (name) (Expr name) (Expr name)
               | VarRef (name)
                 deriving (Eq, Show)


