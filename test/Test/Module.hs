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

module Test.Module (tests) where

import Test.HUnit
import Test.Framework (testGroup)
import qualified Test.Framework
import Test.Framework.Providers.HUnit

import Funk.Module
import Funk.AST (Def(..),Expr(..))
import Funk.Names

tests :: Test.Framework.Test
tests = testGroup "Module Tests"  [
          testCase "Add one definition" testAddOneDef,
          testCase "Add multiple definitions" testAddDefs
        ]

    
testAddOneDef :: Assertion
testAddOneDef = assertEqual "Error storing def."
                  [def] (getFunctions (addDef def empty))
    where
      def = Def (ResolvedName "foo" GlobalRef) [] (FloatLiteral 1) 


testAddDefs :: Assertion
testAddDefs = assertEqual "Error storing defs."
                  defs (getFunctions (foldr addDef empty defs))
    where
      defs = [Def (ResolvedName "bar" GlobalRef) [] (FloatLiteral 1),
              Def (ResolvedName "foo" GlobalRef) [] (FloatLiteral 5.2)]
                            
