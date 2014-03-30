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

module Funk.CodeGen.GenIR.Types
       (ExprResult(..),
        Name,
        GenState(..),
        GenM,
        newName,
        Operand(..)
       ) where

import qualified LLVM.General.AST as L
import qualified LLVM.General.AST.Constant as L
import qualified LLVM.General.AST.Float as L

import qualified Funk.Names as F

import Data.Word
import Control.Monad.State

data ExprResult = DoubleResult Double | NamedResult L.Name

type Name = F.ResolvedName ()

data GenState = GenState Word
type GenM a = State GenState a

newName :: GenM L.Name
newName = do
  (GenState i) <- get
  put (GenState (i+1))
  return $ L.UnName i

class Operand a where
  mkOperand :: a -> L.Operand

instance Operand Name where
  mkOperand (F.ResolvedName n _ l) =
    ((if l == F.GlobalRef
      then L.ConstantOperand . L.GlobalReference
      else L.LocalReference ) . L.Name) n

instance Operand ExprResult where
  mkOperand (DoubleResult v) =
    (L.ConstantOperand . L.Float . L.Double) v
  mkOperand (NamedResult n) = L.LocalReference n
