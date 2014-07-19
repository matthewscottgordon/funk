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

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Funk.CodeGen.GenIR.BuiltIns
       (getBuiltIn
       ) where

import Prelude hiding (div)
import Funk.CodeGen.GenIR.Types (Name,
                                 ExprResult(..),
                                 GenM,
                                 newName,
                                 Operand(..))

import qualified LLVM.General.AST as L
import qualified Funk.Names as F
import LLVM.General.AST (Named((:=)))

import qualified Data.Map.Strict as Map

instance Ord Name where
  compare (F.ResolvedName ln ll) (F.ResolvedName rn rl) =
    if ln > rn then GT
    else if ln < rn then LT
           else locationOrder ll rl
    where
      locationOrder F.GlobalRef      F.GlobalRef      = EQ
      locationOrder F.GlobalRef      _                = LT
      locationOrder _                F.GlobalRef      = GT
      locationOrder (F.ModuleRef lm) (F.ModuleRef rm) = compare lm rm
      locationOrder (F.ModuleRef _)  _                = LT
      locationOrder _                (F.ModuleRef _)  = GT
      locationOrder _                _                = EQ
  

getBuiltIn ::
  Name ->
  Maybe ([ExprResult] -> GenM (L.Named L.Instruction, ExprResult))
getBuiltIn = flip Map.lookup builtIns

builtIns :: Map.Map Name
            ([ExprResult] -> GenM (L.Named L.Instruction, ExprResult))
builtIns = Map.fromList [ (F.ResolvedName "+" F.GlobalRef, add),
                          (F.ResolvedName "-" F.GlobalRef, sub),
                          (F.ResolvedName "*" F.GlobalRef, mul),
                          (F.ResolvedName "/" F.GlobalRef, div) ]


add :: [ExprResult] -> GenM (L.Named L.Instruction, ExprResult)
add (left:right:[]) = do
  name <- newName
  return (name := L.FAdd L.NoFastMathFlags (mkOperand left) (mkOperand right) [],
          NamedResult name)


sub :: [ExprResult] -> GenM (L.Named L.Instruction, ExprResult)
sub (left:right:[]) = do
  name <- newName
  return (name := L.FSub L.NoFastMathFlags (mkOperand left) (mkOperand right) [],
          NamedResult name)


mul :: [ExprResult] -> GenM (L.Named L.Instruction, ExprResult)
mul (left:right:[]) = do
  name <- newName
  return (name := L.FMul L.NoFastMathFlags (mkOperand left) (mkOperand right) [],
          NamedResult name)


div :: [ExprResult] -> GenM (L.Named L.Instruction, ExprResult)
div (left:right:[]) = do
  name <- newName
  return (name := L.FDiv L.NoFastMathFlags (mkOperand left) (mkOperand right) [],
          NamedResult name)

