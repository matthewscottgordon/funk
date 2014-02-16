-- Copyright 2013 Matthew Gordon.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express orimplied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{
module Funk.Lexer
       (
         lex,
         Token(..),
         Posn(..)
       ) where

import Prelude hiding (lex)

import Debug.Trace (trace)
}

%wrapper "posn"

$digit       = [0-9]
$alpha       = [a-zA-Z]
$alpha_upper = [A-Z]
$alpha_lower = [a-z]
$id_char     = [a-zA-Z0-9_]
$op_char     = [\~\!\@\$\%\^\&\*\+\-\=\:\;\<\>\?\/\|]
$eol         = \n
$white_no_nl = $white # $eol

@name = $alpha_lower $id_char*

tokens :-

  $white_no_nl+            ;
  "foreign"                { mkToken (\_ -> KeywordForeign) }
  $digit+ ("." $digit+)?   { mkToken (\s -> FloatLiteral (read s)) }
  @name                    { mkToken Name }
  "="                      { mkToken (\_ -> DefOp) }
  "(" $op_char+ ")"        { mkToken (Name . init . tail) }
  $op_char+ 			   { mkToken OpName }
  "`" @name "`"            { mkToken (OpName . init . tail) }
  "("                      { mkToken (\_ -> OpenParen) }
  ")"                      { mkToken (\_ -> CloseParen) }
  $eol                     { mkToken (\_ -> Eol) }
  .                        { mkToken BadToken }{

data Posn = Posn Int Int Int
          deriving (Show)

data Token = FloatLiteral Double
           | KeywordForeign
           | DefOp
           | Name String
           | OpName String
           | OpenParen
           | CloseParen
           | Eol
           | BadToken String
           deriving (Show)

mkToken :: (String -> Token) -> AlexPosn -> String -> (Posn, Token)
mkToken f (AlexPn a l c) s = ((Posn a l c), f s)

mkToken' :: String -> (String -> Token) -> AlexPosn -> String -> (Posn, Token)
mkToken' n f (AlexPn a l c) s = ((Posn a l c), f (trace (n ++ "(" ++ s ++ ")") s))

lex :: String -> [(Posn, Token)]
lex = alexScanTokens

}
