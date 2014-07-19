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
import Funk.Names

import Control.Monad (forM_)

tests :: Test.Framework.Test
tests = testGroup "Parser Tests"  [
          testCase "Basic" testParserBasic,
          testCase "Prefix Ops" testPrefixOp,
          testCase "More Functions" testFunc,
          testCase "Basic Ops" testOpsBasic,
          testCase "Combined Ops and Functions" testOpsAndFunctions,
          testCase "Declarations" testDeclarations
        ]

checkList :: (Show a, Eq a) => String -> [a] -> [a] -> Assertion
checkList s ds ds' = do
  assertEqual ("Wrong number of " ++ s) (length ds) (length ds')
  forM_ (zip3 ds ds' [1..]) $ \(d, d', l) ->
    assertEqual ("For " ++ s ++ " "++ (show l) ++ ":") d d'


parseAndCheck :: String -> (Module UnresolvedName) -> Assertion
parseAndCheck input (Module expectedDecls expectedDefs) = do
  case Funk.Parser.parse "<testdata>" input of
    Left e -> assertFailure (show e)
    Right (Module decls defs) -> do
      checkList "Def" expectedDefs defs
      checkList "Decl" expectedDecls decls

    
testParserBasic :: Assertion
testParserBasic = parseAndCheck input (Module [] expectedDefs)
  where
    input = "foo a b = add a b 3\n\
            \bar = foo 1 2\n\
            \baz = foo bar bar\n"
    expectedDefs = [
      Def (UnresolvedName "foo") [UnresolvedName "a", UnresolvedName "b"]
       (Call (UnresolvedName "add")
        [VarRef (UnresolvedName "a"), VarRef (UnresolvedName "b"), FloatLiteral 3]),
      Def (UnresolvedName "bar") []
       (Call (UnresolvedName "foo")
        [FloatLiteral 1, FloatLiteral 2]),
      Def (UnresolvedName "baz") []
       (Call (UnresolvedName "foo")
        [VarRef (UnresolvedName "bar"), VarRef (UnresolvedName "bar")]) ]

testPrefixOp :: Assertion
testPrefixOp = parseAndCheck input (Module [] expectedDefs)
  where
    input = "foo a b = (+) a b\n\
            \bar = (<$>) 1 2\n"
    expectedDefs = [
      Def (UnresolvedName "foo") [UnresolvedName "a", UnresolvedName "b"]
        (Call (UnresolvedName "+")
           [VarRef (UnresolvedName "a"), VarRef (UnresolvedName "b")]),
      Def (UnresolvedName "bar") []
        (Call (UnresolvedName "<$>")
          [FloatLiteral 1, FloatLiteral 2]) ]

testFunc :: Assertion
testFunc = parseAndCheck input (Module [] defs)
  where
    input = "foo a b = add a b 3\n\
            \bar = foo 1 2\n\
            \baz = foo bar bar\n\
            \foo a b = (+) a b\n\
            \bar = (<$>) 1 2\n"
    defs = [
      Def (UnresolvedName "foo") [UnresolvedName "a", UnresolvedName "b"]
       (Call (UnresolvedName "add")
        [VarRef (UnresolvedName "a"), VarRef (UnresolvedName "b"), FloatLiteral 3]),
      Def (UnresolvedName "bar") []
       (Call (UnresolvedName "foo")
        [FloatLiteral 1, FloatLiteral 2]),
      Def (UnresolvedName "baz") []
       (Call (UnresolvedName "foo")
        [VarRef (UnresolvedName "bar"), VarRef (UnresolvedName "bar")]),
      Def (UnresolvedName "foo") [UnresolvedName "a", UnresolvedName "b"]
       (Call (UnresolvedName "+")
        [VarRef (UnresolvedName "a"), VarRef (UnresolvedName "b")]),
      Def (UnresolvedName "bar") []
       (Call (UnresolvedName "<$>")
        [FloatLiteral 1, FloatLiteral 2])]

testOpsBasic :: Assertion
testOpsBasic = parseAndCheck input (Module [] defs)
  where
    input = "bind a b = a >>= b\n\
            \equal3 one two three = one == two == three\n\
            \bar' a b c = b * c + a\n\
            \bar a b c = a + b * c\n\
            \foo qw er = er + 1 * 2 / qw - 1234.56\n"
    defs = [
      Def (UnresolvedName "bind") [UnresolvedName "a", UnresolvedName "b"]
        (Op (UnresolvedName ">>=")
          (VarRef (UnresolvedName "a"))
          (VarRef (UnresolvedName "b"))),
      Def (UnresolvedName "equal3")
          [UnresolvedName "one", UnresolvedName "two", UnresolvedName "three"]
        (Op (UnresolvedName "==")
          (Op (UnresolvedName "==")
             (VarRef (UnresolvedName "one"))
             (VarRef (UnresolvedName "two")))
          (VarRef (UnresolvedName "three"))),
      Def (UnresolvedName "bar'") [UnresolvedName "a", UnresolvedName "b", UnresolvedName "c"]
        (Op (UnresolvedName "+")
          (Op (UnresolvedName "*")
            (VarRef (UnresolvedName "b"))
            (VarRef (UnresolvedName "c")))
          (VarRef (UnresolvedName "a"))),
      Def (UnresolvedName "bar") [UnresolvedName "a", UnresolvedName "b", UnresolvedName "c"]
        (Op (UnresolvedName "+")
          (VarRef (UnresolvedName "a"))
          (Op (UnresolvedName "*")
            (VarRef (UnresolvedName "b"))
            (VarRef (UnresolvedName "c")))),
      Def (UnresolvedName "foo") [UnresolvedName "qw", UnresolvedName "er"]
        (Op (UnresolvedName "-")
          (Op (UnresolvedName "+")
            (VarRef (UnresolvedName "er"))
            (Op (UnresolvedName "/")
              (Op (UnresolvedName "*")
                (FloatLiteral 1)
                (FloatLiteral 2))
              (VarRef (UnresolvedName "qw"))))
          (FloatLiteral 1234.56))]


testOpsAndFunctions :: Assertion
testOpsAndFunctions = parseAndCheck input (Module [] defs)
  where
    input = "f = g a * b\n\
            \f' = g' a (*) b\n"
    defs = [
      Def (UnresolvedName "f") []
        (Op (UnresolvedName "*")
          (Call (UnresolvedName "g") [VarRef (UnresolvedName "a")])
          (VarRef (UnresolvedName "b"))),
      Def (UnresolvedName "f'") [] (Call (UnresolvedName "g'")
        [VarRef (UnresolvedName "a"), VarRef (UnresolvedName "*"),
                             VarRef (UnresolvedName "b")])]
        
testDeclarations :: Assertion
testDeclarations = parseAndCheck input (Module decls [])
  where
    input = "foo :: Double -> Double -> Double\n\
            \bar :: String -> Integer\n\
            \pi :: Double\n"
    decls = [ Decl (UnresolvedName "foo")
                       (FuncType
                        (TypeName "Double")
                        (FuncType
                         (TypeName "Double")
                         (TypeName "Double"))),
              Decl (UnresolvedName "bar")
                       (FuncType
                        (TypeName "String")
                        (TypeName "Integer")),
              Decl (UnresolvedName "pi") (TypeName "Double") ]