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
          testCase "Basic" testParserBasic,
          testCase "Prefix Ops" testPrefixOp,
          testCase "Foreign Functions" testForeignFunctions,
          testCase "More Functions" testMixedForeignAndRegularFunc,
          testCase "Basic Ops" testOpsBasic,
          testCase "Combined Ops and Functions" testOpsAndFunctions
        ]

checkList :: (Show a, Eq a) => String -> [a] -> [a] -> Assertion
checkList s ds ds' = do
  assertEqual ("Wrong number of " ++ s) (length ds) (length ds')
  forM_ (zip3 ds ds' [1..]) $ \(d, d', l) ->
    assertEqual ("For " ++ s ++ " "++ (show l) ++ ":") d d'


parseAndCheck :: String -> Module -> Assertion
parseAndCheck input (Module expectedDefs expectedFDefs) = do
  case Funk.Parser.parse "<testdata>" input of
    Left e -> assertFailure (show e)
    Right (Module defs fdefs) -> do
      checkList "Def" expectedDefs defs
      checkList "ForeignDef" expectedFDefs fdefs

    
testParserBasic :: Assertion
testParserBasic = parseAndCheck input (Module expectedDefs [])
  where
    input = "foo a b = add a b 3\n\
            \bar = foo 1 2\n\
            \baz = foo bar bar\n"
    expectedDefs = [
      Def (Name "foo") [Name "a", Name "b"]
        (Call (Name "add")
           [VarRef (Name "a"), VarRef (Name "b"), FloatLiteral 3]),
      Def (Name "bar") []
        (Call (Name "foo")
          [FloatLiteral 1, FloatLiteral 2]),
      Def (Name "baz") []
        (Call (Name "foo")
           [VarRef (Name "bar"), VarRef (Name "bar")]) ]

testPrefixOp :: Assertion
testPrefixOp = parseAndCheck input (Module expectedDefs [])
  where
    input = "foo a b = (+) a b\n\
            \bar = (<$>) 1 2\n"
    expectedDefs = [
      Def (Name "foo") [Name "a", Name "b"]
        (Call (Name "+")
           [VarRef (Name "a"), VarRef (Name "b")]),
      Def (Name "bar") []
        (Call (Name "<$>")
          [FloatLiteral 1, FloatLiteral 2]) ]

testForeignFunctions :: Assertion
testForeignFunctions = parseAndCheck input (Module [] expectedFDefs)
  where
    input = "foreign sin theta = sin\n\
            \foreign arcTan2 y x = atan2\n"
    expectedFDefs = [
      ForeignDef (Name "sin") [Name "theta"] (Name "sin"),
      ForeignDef (Name "arcTan2") [Name "y", Name "x"] (Name "atan2")]

testMixedForeignAndRegularFunc :: Assertion
testMixedForeignAndRegularFunc = parseAndCheck input (Module defs fdefs)
  where
    input = "foo a b = add a b 3\n\
            \bar = foo 1 2\n\
            \baz = foo bar bar\n\
            \foreign sin theta = sin\n\
            \foreign arcTan2 y x = atan2\n\
            \foo a b = (+) a b\n\
            \bar = (<$>) 1 2\n"
    defs = [
      Def (Name "foo") [Name "a", Name "b"]
        (Call (Name "add")
           [VarRef (Name "a"), VarRef (Name "b"), FloatLiteral 3]),
      Def (Name "bar") []
        (Call (Name "foo")
          [FloatLiteral 1, FloatLiteral 2]),
      Def (Name "baz") []
        (Call (Name "foo")
           [VarRef (Name "bar"), VarRef (Name "bar")]),
      Def (Name "foo") [Name "a", Name "b"]
        (Call (Name "+")
           [VarRef (Name "a"), VarRef (Name "b")]),
      Def (Name "bar") []
        (Call (Name "<$>")
          [FloatLiteral 1, FloatLiteral 2]) ]
    fdefs = [
      ForeignDef (Name "sin") [Name "theta"] (Name "sin"),
      ForeignDef (Name "arcTan2") [Name "y", Name "x"] (Name "atan2")]

testOpsBasic :: Assertion
testOpsBasic = parseAndCheck input (Module defs [])
  where
    input = "bind a b = a >>= b\n\
            \equal3 one two three = one == two == three\n\
            \foo qw er = er + 1 * 2 / qw - 1234.56\n"
    defs = [
      Def (Name "bind") [Name "a", Name "b"]
        (Op (Name ">>=")
          (VarRef (Name "a"))
          (VarRef (Name "b"))),
      Def (Name "equal3") [Name "one", Name "two", Name "three"]
        (Op (Name "==")
          (Op (Name "==")
             (VarRef (Name "one"))
             (VarRef (Name "two")))
          (VarRef (Name "three"))),
      Def (Name "foo") [Name "qw", Name "er"]
        (Op (Name "-")
          (Op (Name "+")
            (VarRef (Name "er"))
            (Op (Name "/")
              (Op (Name "*")
                (FloatLiteral 1)
                (FloatLiteral 2))
              (VarRef (Name "qw"))))
          (FloatLiteral 1234.56))]


testOpsAndFunctions :: Assertion
testOpsAndFunctions = parseAndCheck input (Module defs [])
  where
    input = "f = g a * b\n\
            \f' = g' a (*) b\n"
    defs = [
      Def (Name "f") []
        (Op (Name "*")
          (Call (Name "g") [VarRef (Name "a")])
          (VarRef (Name "b"))),
      Def (Name "f'") [] (Call (Name "g'")
        [VarRef (Name "a"), VarRef (Name "*"), VarRef (Name "b")])]
        
