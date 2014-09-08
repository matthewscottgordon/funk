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

module Funk.CodeGen
       (genIR,
        showLLVM
       ) where

import Funk.CodeGen.GenIR (genIR)
import Funk.Names (ResolvedName(..))
import Funk.Module

import LLVM.General (moduleLLVMAssembly, withModuleFromAST)
import LLVM.General.Context (withContext, Context)

import Control.Monad.Error


showLLVM :: Module ResolvedName -> ErrorT String IO String
showLLVM m = do
  let ir = genIR m
  withContext' $ \c ->
    withModuleFromAST c ir moduleLLVMAssembly

withContext' :: (Context -> ErrorT String IO a) -> ErrorT String IO a
withContext' f = ErrorT . withContext $ runErrorT . f
