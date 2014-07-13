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

module Test.Lexer (tests) where

import Test.HUnit
import Test.Framework (testGroup)
import qualified Test.Framework
import Test.Framework.Providers.HUnit

import Data.List (intercalate)

import Funk.Lexer as Target


tests :: Test.Framework.Test
tests = testGroup "Lexer Tests"  [
  testGroup "Individual Tokens" [
    testSingleToken "foreign" KeywordForeign,
    testSingleToken "=" DefOp,
    testSingleToken "(" OpenParen,
    testSingleToken ")" CloseParen,
    testSingleToken "\n" Eol,
    testSingleToken "::" TypeOp,
    testSingleToken "->" ToOp,
    testGroup "Float Literals" [
      testSingleToken "1" (FloatLiteral 1),
      testSingleToken "0" (FloatLiteral 0),
      testSingleToken "1.00" (FloatLiteral 1),
      testSingleToken "0.4" (FloatLiteral 0.4),
      testSingleToken "12345" (FloatLiteral 12345),
      testSingleToken "9876.5432" (FloatLiteral 9876.5432)],
    testGroup "Identifiers" [
      testSingleToken "x" (Id "x"),
      testSingleToken "x'" (Id "x'"),
      testSingleToken "x''" (Id "x''"),
      testSingleToken "x's" (Id "x's"),
      testSingleToken "xs" (Id "xs"),
      testSingleToken "foo" (Id "foo"),
      testSingleToken "fooBar" (Id "fooBar"),
      testSingleToken "foo_bar" (Id "foo_bar"),
      testSingleToken "quiteLongCamelCaseIdentifier"
                      (Id "quiteLongCamelCaseIdentifier"),
      testSingleToken "another_quite_long_identifier"
                      (Id "another_quite_long_identifier")],
    testGroup "Type Identifiers" [
      testSingleToken "Double" (TypeId "Double"),
      testSingleToken "Int" (TypeId "Int"),
      testSingleToken "String" (TypeId "String") ],
    testGroup "Operators in parentheses" [
      testSingleToken "(+)" (Id "+"),
      testSingleToken "(/)" (Id "/"),
      testSingleToken "(?!!)" (Id "?!!")],
    testGroup "Operators with precedence 1" [
      testSingleToken "*" (Op 1 "*"),
      testSingleToken "/" (Op 1 "/")],
    testGroup "Operators with precedence 2" [
      testSingleToken "+" (Op 2 "+"),
      testSingleToken "-" (Op 2 "-")],
    testGroup "Operators with precedence 3" [
      testSingleToken "++" (Op 7 "++"),
      testSingleToken "==/" (Op 7 "==/"),
      testSingleToken "?" (Op 7 "?"),
      testSingleToken "@" (Op 7 "@"),
      testSingleToken "$$$*^$@?/>>" (Op 7 "$$$*^$@?/>>")],
    testGroup "Identifiers in backticks" [
      testSingleToken "`foo`" (Op 7 "foo")],
    testGroup "Bad Tokens" [
      testSingleToken "2a" (BadToken "2a"),
      testSingleToken "_foo" (BadToken "_foo")]
    ],
  testGroup "Short Programs" (testPrograms testProgramList)
  ]

assertSingleToken :: Token -> [(Posn, Token)] -> Assertion
assertSingleToken t ((_, t'):[]) = assertEqual "Wrong token" t t'
assertSingleToken _ [] = assertFailure "Empty result"
assertSingleToken _ _  =
  assertFailure "Multiple tokens where only one expected."

testSingleToken :: String -> Token -> Test.Framework.Test
testSingleToken s t = testCase msg $ assertSingleToken t (Target.lex s)
  where
    msg = "\"" ++ s ++ "\" is \"" ++ (show t) ++ "\""

assertTokenList :: [Token] -> [(Posn, Token)] -> Assertion
assertTokenList = f 1
  where
    f n (e:es) ((p,t):ts) = if e==t 
                               then f (n+1) es ts
                               else assertFailure (failMsg e t n p)
    f n [] []            = return ()
    f _ [] _             = assertFailure "Found more tokens than expected"
    f n _ []             = assertFailure ("Found " ++ (show n) ++
                              " tokens but expected more.")
    failMsg e t n p = intercalate " "
      ["Expected token", (show n), "to be", (show e), "but found",
       (show t), "at", (show p), "instead."]

data TestProgram = TestProgram String String [Token]

testPrograms :: [TestProgram] -> [Test.Framework.Test]
testPrograms = map f
  where
    f (TestProgram d i e) =
      testCase d $ assertTokenList e (Target.lex i)

testProgramList :: [TestProgram]
testProgramList =
  [ TestProgram "Simple program"
      "foo a b = add a b 3\n\
      \bar = foo 1 2\n\
      \baz = foo bar bar\n"
      [ Id "foo", Id "a", Id "b", DefOp, Id "add", Id "a",
        Id "b", FloatLiteral 3, Eol, Id "bar", DefOp, Id "foo",
        FloatLiteral 1, FloatLiteral 2, Eol, Id "baz", DefOp,
        Id "foo", Id "bar", Id "bar", Eol ],
    TestProgram "Program with operators"
      "bind a b = a >>= b\n\
      \equal3 one two three = one == two == three\n\
      \bar' a b c = b * c + a\n\
      \bar a b c = a + b * c\n\
      \foo qw er = er + 1 * 2 / qw - 1234.56\n"
      [ Id "bind", Id "a", Id "b", DefOp, Id "a", Op 7 ">>=",
        Id "b", Eol,
        Id "equal3", Id "one", Id "two", Id "three", DefOp, Id "one",
        Op 4 "==", Id "two", Op 4 "==", Id "three", Eol,
        Id "bar'", Id "a", Id "b", Id "c", DefOp, Id "b", Op 1 "*",
        Id "c", Op 2 "+", Id "a", Eol,
        Id "bar", Id "a", Id "b", Id "c", DefOp, Id "a", Op 2 "+",
        Id "b", Op 1 "*", Id "c", Eol,
        Id "foo", Id "qw", Id "er", DefOp, Id "er", Op 2 "+",
        FloatLiteral 1, Op 1 "*", FloatLiteral 2, Op 1 "/",
        Id "qw", Op 2 "-", FloatLiteral 1234.56, Eol ]
 ]

