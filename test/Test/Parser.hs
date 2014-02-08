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

module Test.Parser (tests) where

import Test.HUnit
import Test.Framework (testGroup)
import qualified Test.Framework
import Test.Framework.Providers.HUnit

import Funk.Parser
import Funk.AST

import Control.Monad (forM_)

tests :: Test.Framework.Test
tests = testGroup "Parser Tests"  [
          testCase "Basic" testParserBasic
        ]

checkDefs :: [Def] -> [Def] -> Assertion
checkDefs ds ds' = do
  assertEqual "Wrong number of Defs" (length ds) (length ds')
  forM_ (zip3 ds ds' [1..]) $ \(d, d', l) ->
    assertEqual ("For Def " ++ (show l) ++ ":") d d'


testParserBasic :: Assertion
testParserBasic = do
  case Funk.Parser.parse "<testdata>" input of
    Left e -> assertFailure (show e)
    Right (Module defs) -> checkDefs expectedOutput defs
  where
    input = "foo a b = add a b 3\n\
            \bar = foo 1 2\n\
            \baz = foo bar bar\n"
    expectedOutput = [
      Def (Name "foo") [Name "a", Name "b"]
        (Call (Name "add")
           [VarRef (Name "a"), VarRef (Name "b"), FloatLiteral 3]),
      Def (Name "bar") []
        (Call (Name "foo")
          [FloatLiteral 1, FloatLiteral 3]),
      Def (Name "baz") []
        (Call (Name "foo")
           [VarRef (Name "bar"), VarRef (Name "bar")]) ]

