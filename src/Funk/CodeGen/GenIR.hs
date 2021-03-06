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

import           LLVM.General.AST (Named((:=)))
import qualified LLVM.General.AST as L
import qualified LLVM.General.AST.Constant as L
import qualified LLVM.General.AST.Float as L
import qualified LLVM.General.AST.CallingConvention as LLVMCallingConv
import           LLVM.General.PrettyPrint (showPretty)
import qualified LLVM.General.AST.Linkage as Linkage
import qualified LLVM.General.AST.Visibility as Visibility
import qualified LLVM.General.AST.CallingConvention
  as CallingConvention

import qualified Funk.AST as F
import qualified Funk.Names as F
import qualified Funk.Module as Module

import           Control.Applicative ((<$>))
import           Control.Monad.State
import           Data.Maybe (fromMaybe)

import           Funk.CodeGen.GenIR.Types
import           Funk.CodeGen.GenIR.BuiltIns (getBuiltIn)


showLLVM :: Module.Module F.ResolvedName -> String
showLLVM = showPretty . genIR

genIR :: Module.Module F.ResolvedName -> L.Module
genIR m =
  L.Module "*Module*" Nothing Nothing ds
  where
    ds :: [L.Definition]
    ds = runGen $ do
      llvmDefs <- mapM defIR (Module.getDefs m)
      return $ map L.GlobalDefinition llvmDefs

runGen :: GenM a -> a
runGen act = evalState act (GenState 0)
  
defIR :: F.Def F.ResolvedName -> GenM L.Global
defIR (F.Def (F.ResolvedName n _)  ps body) = do
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
        param (F.ResolvedName n' _) = L.Parameter
                  (L.FloatingPointType 64 L.IEEE)
                  (L.Name n')
                  []

bodyIR :: F.Expr F.ResolvedName -> GenM [L.BasicBlock]
bodyIR body = do
  blockName <- newName
  (b, r) <- exprIR body
  return [L.BasicBlock blockName b (mkResult r) ]
  where
    mkResult (DoubleResult v) =
      L.Do (L.Ret (Just (L.ConstantOperand (L.Float (L.Double v)))) [])
    mkResult (NamedResult n) =
      L.Do (L.Ret (Just (L.LocalReference 
                              (L.FloatingPointType 64 L.IEEE) n)) [])


exprIR :: F.Expr F.ResolvedName
          -> GenM ([L.Named L.Instruction], ExprResult)
exprIR (F.FloatLiteral v) = return ([], DoubleResult v)
exprIR (F.Call n argExprs) = callIR n argExprs
exprIR (F.Op n lArgExpr rArgExpr) = callIR n [lArgExpr, rArgExpr]
exprIR (F.VarRef (F.ResolvedName n _)) =
  return ([], NamedResult (L.Name n))

callIR :: F.ResolvedName -> [F.Expr F.ResolvedName]
          -> GenM ([L.Named L.Instruction], ExprResult)
callIR n argExprs = do
  (argInstructions, args) <- unzip <$> mapM exprIR argExprs
  (callInstr, callResult) <- mkCall n args
  let instructions = concat argInstructions ++ [callInstr]
  return (instructions, callResult)

mkCall :: Name -> [ExprResult] ->
          GenM (L.Named L.Instruction, ExprResult)
mkCall n = fromMaybe (mkCall' n) (getBuiltIn n)

mkCall' :: Name -> [ExprResult] ->
           GenM (L.Named L.Instruction, ExprResult)
mkCall' n args = do
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



