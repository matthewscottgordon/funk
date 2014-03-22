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

module Funk.CodeGen.GenIR
       (genIR,
        showLLVM
       ) where

import LLVM.General.AST (Named((:=)))
import qualified LLVM.General.AST as L
import qualified LLVM.General.AST.Constant as L
import qualified LLVM.General.AST.Float as L
import qualified LLVM.General.AST.CallingConvention as LLVMCallingConv
import LLVM.General.PrettyPrint (showPretty)
import qualified LLVM.General.AST.Linkage as Linkage
import qualified LLVM.General.AST.Visibility as Visibility
import qualified LLVM.General.AST.CallingConvention
  as CallingConvention

import qualified Funk.AST as F
import qualified Funk.Names as F

import Control.Monad.State
import Control.Applicative ((<$>))
import Data.Word

type Name = F.ResolvedName ()

showLLVM :: F.Module (F.ResolvedName ()) -> String
showLLVM = showPretty . genIR

genIR :: F.Module (F.ResolvedName ()) -> L.Module
genIR (F.Module defs _) =
  L.Module "*Module*" Nothing Nothing ds
  where
    ds :: [L.Definition]
    ds = runGen $ do
      llvmDefs <- mapM defIR defs
      return $ map L.GlobalDefinition llvmDefs

data GenState = GenState Word
type GenM a = State GenState a

runGen :: GenM a -> a
runGen act = evalState act (GenState 0)

newName :: GenM L.Name
newName = do
  (GenState i) <- get
  put (GenState (i+1))
  return $ L.UnName i
  
defIR :: F.Def (F.ResolvedName ()) -> GenM L.Global
defIR (F.Def (F.ResolvedName n () _)  ps body) = do
  body' <- bodyIR body
  return $ L.Function Linkage.External
                Visibility.Default
                CallingConvention.C
                []
                (L.FloatingPointType 64 L.IEEE)
                (L.Name n)
                (params, False)
                []
                Nothing
                0
                Nothing
                body'
  where params = map param ps
        param (F.ResolvedName n' _ _) = L.Parameter
                  (L.FloatingPointType 64 L.IEEE)
                  (L.Name n')
                  []

bodyIR :: F.Expr (F.ResolvedName ()) -> GenM [L.BasicBlock]
bodyIR body = do
  blockName <- newName
  (b, r) <- exprIR body
  return [L.BasicBlock blockName b (mkResult r) ]
  where
    mkResult (DoubleResult v) =
      (L.Do(L.Ret(Just(L.ConstantOperand(L.Float(L.Double v)))) [] ))
    mkResult (NamedResult n) =
      (L.Do (L.Ret (Just (L.LocalReference n)) [] ))

data ExprResult = DoubleResult Double | NamedResult L.Name


exprIR :: F.Expr (F.ResolvedName ())
          -> GenM ([L.Named L.Instruction], ExprResult)
exprIR (F.FloatLiteral v) = return ([], DoubleResult v)
exprIR (F.Call n argExprs) = callIR n argExprs
exprIR (F.Op n lArgExpr rArgExpr) = callIR n [lArgExpr, rArgExpr]
exprIR (F.VarRef (F.ResolvedName n _ _)) =
  return ([], NamedResult (L.Name n))

callIR :: F.ResolvedName () -> [F.Expr (F.ResolvedName ())]
          -> GenM ([L.Named L.Instruction], ExprResult)
callIR n argExprs = do
  (argInstructions, args) <- unzip <$> mapM exprIR argExprs
  (callInstr, callResult) <- mkCall n args
  let instructions = (concat argInstructions) ++ [callInstr]
  return (instructions, callResult)

mkCall :: Name -> [ExprResult] ->
          GenM (L.Named L.Instruction, ExprResult)
mkCall n args = do
  name <- newName
  let args' = map (\arg -> (mkOperand arg, [])) args
  return (name := instr args', NamedResult name)
  where
    tailCall     = False
    callingConv  = LLVMCallingConv.C
    returnAttr   = []
    function     = mkOperand n
    functionAttr = []
    metadata     = []
    instr as   = L.Call tailCall callingConv returnAttr
                 (Right function)
                 as functionAttr metadata

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

