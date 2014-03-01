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

import Funk.Lexer as Target


tests :: Test.Framework.Test
tests = testGroup "Lexer Tests"  [
  testGroup "Individual Tokens" [
    testSingleToken "foreign" KeywordForeign,
    testSingleToken "=" DefOp,
    testSingleToken "(" OpenParen,
    testSingleToken ")" CloseParen,
    testSingleToken "\n" Eol,
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
    testGroup "Operators in parentheses" [
      testSingleToken "(+)" (Id "+"),
      testSingleToken "(/)" (Id "/"),
      testSingleToken "(?!!)" (Id "?!!")],
    testGroup "Operators with precedence 1" [
      testSingleToken "*" (Op1 "*"),
      testSingleToken "/" (Op1 "/")],
    testGroup "Operators with precedence 2" [
      testSingleToken "+" (Op2 "+"),
      testSingleToken "-" (Op2 "-")],
    testGroup "Operators with precedence 3" [
      testSingleToken "++" (Op "++"),
      testSingleToken "==/" (Op "==/"),
      testSingleToken "?" (Op "?"),
      testSingleToken "@" (Op "@"),
      testSingleToken "$$$*^$@?/>>" (Op "$$$*^$@?/>>")],
    testGroup "Identifiers in backticks" [
      testSingleToken "`foo`" (Op "foo")],
    testGroup "Bad Tokens" [
      testSingleToken "2a" (BadToken "2a"),
      testSingleToken "_foo" (BadToken "_foo")]
    ]]

assertSingleToken :: Token -> [(Posn, Token)] -> Assertion
assertSingleToken t ((_, t'):[]) = assertEqual "Wrong token" t t'
assertSingleToken _ [] = assertFailure "Empty result"
assertSingleToken _ _  =
  assertFailure "Multiple tokens where only one expected."

testSingleToken :: String -> Token -> Test.Framework.Test
testSingleToken s t = testCase msg $ assertSingleToken t (Target.lex s)
  where
    msg = "\"" ++ s ++ "\" is \"" ++ (show t) ++ "\""

