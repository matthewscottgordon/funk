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


parseAndCheck :: String -> (Module RawName) -> Assertion
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
      Def (rawName "foo") [rawName "a", rawName "b"]
       (Call (rawName "add")
        [VarRef (rawName "a"), VarRef (rawName "b"), FloatLiteral 3]),
      Def (rawName "bar") []
       (Call (rawName "foo")
        [FloatLiteral 1, FloatLiteral 2]),
      Def (rawName "baz") []
       (Call (rawName "foo")
        [VarRef (rawName "bar"), VarRef (rawName "bar")]) ]

testPrefixOp :: Assertion
testPrefixOp = parseAndCheck input (Module [] expectedDefs)
  where
    input = "foo a b = (+) a b\n\
            \bar = (<$>) 1 2\n"
    expectedDefs = [
      Def (rawName "foo") [rawName "a", rawName "b"]
        (Call (rawName "+")
           [VarRef (rawName "a"), VarRef (rawName "b")]),
      Def (rawName "bar") []
        (Call (rawName "<$>")
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
      Def (rawName "foo") [rawName "a", rawName "b"]
       (Call (rawName "add")
        [VarRef (rawName "a"), VarRef (rawName "b"), FloatLiteral 3]),
      Def (rawName "bar") []
       (Call (rawName "foo")
        [FloatLiteral 1, FloatLiteral 2]),
      Def (rawName "baz") []
       (Call (rawName "foo")
        [VarRef (rawName "bar"), VarRef (rawName "bar")]),
      Def (rawName "foo") [rawName "a", rawName "b"]
       (Call (rawName "+")
        [VarRef (rawName "a"), VarRef (rawName "b")]),
      Def (rawName "bar") []
       (Call (rawName "<$>")
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
      Def (rawName "bind") [rawName "a", rawName "b"]
        (Op (rawName ">>=")
          (VarRef (rawName "a"))
          (VarRef (rawName "b"))),
      Def (rawName "equal3")
          [rawName "one", rawName "two", rawName "three"]
        (Op (rawName "==")
          (Op (rawName "==")
             (VarRef (rawName "one"))
             (VarRef (rawName "two")))
          (VarRef (rawName "three"))),
      Def (rawName "bar'") [rawName "a", rawName "b", rawName "c"]
        (Op (rawName "+")
          (Op (rawName "*")
            (VarRef (rawName "b"))
            (VarRef (rawName "c")))
          (VarRef (rawName "a"))),
      Def (rawName "bar") [rawName "a", rawName "b", rawName "c"]
        (Op (rawName "+")
          (VarRef (rawName "a"))
          (Op (rawName "*")
            (VarRef (rawName "b"))
            (VarRef (rawName "c")))),
      Def (rawName "foo") [rawName "qw", rawName "er"]
        (Op (rawName "-")
          (Op (rawName "+")
            (VarRef (rawName "er"))
            (Op (rawName "/")
              (Op (rawName "*")
                (FloatLiteral 1)
                (FloatLiteral 2))
              (VarRef (rawName "qw"))))
          (FloatLiteral 1234.56))]


testOpsAndFunctions :: Assertion
testOpsAndFunctions = parseAndCheck input (Module [] defs)
  where
    input = "f = g a * b\n\
            \f' = g' a (*) b\n"
    defs = [
      Def (rawName "f") []
        (Op (rawName "*")
          (Call (rawName "g") [VarRef (rawName "a")])
          (VarRef (rawName "b"))),
      Def (rawName "f'") [] (Call (rawName "g'")
        [VarRef (rawName "a"), VarRef (rawName "*"),
                             VarRef (rawName "b")])]
        
testDeclarations :: Assertion
testDeclarations = parseAndCheck input (Module decls [])
  where
    input = "foo :: Double -> Double -> Double\n\
            \bar :: String -> Integer\n\
            \pi :: Double\n"
    decls = [ Decl (rawName "foo")
                       (FuncType
                        (TypeName "Double")
                        (FuncType
                         (TypeName "Double")
                         (TypeName "Double"))),
              Decl (rawName "bar")
                       (FuncType
                        (TypeName "String")
                        (TypeName "Integer")),
              Decl (rawName "pi") (TypeName "Double") ]