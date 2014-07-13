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
$id_char     = [a-zA-Z0-9_']
$op_char     = [\~\!\@\$\%\^\&\*\+\-\=\:\;\<\>\?\/\|]
$eol         = \n
$white_no_nl = $white # $eol

@identifier = $alpha_lower $id_char*
@typeIdentifier = $alpha_upper $id_char*
@op1 = [\*\/\%]
@op2 = [\+\-]
@op3 = [\<\>]|\<\=|\>\=
@op4 = \=\=|\!\=
@op5 = \&\&
@op6 = \|\|

tokens :-

  $white_no_nl+                   ;
  "foreign"                       {mkToken (\_ -> KeywordForeign)}
  $digit+ ("." $digit+)?          { mkToken (\s -> FloatLiteral (read s)) }
  @identifier                     { mkToken Id }
  @typeIdentifier                 { mkToken TypeId }
  "="                             { mkToken (\_ -> DefOp) }
  "::"                            { mkToken (\_ -> TypeOp) }
  "->"                            { mkToken (\_ -> ToOp) }
  "(" $op_char+ ")"               { mkToken (Id . init . tail) }
  @op1                            { mkToken (Op 1) }
  @op2                            { mkToken (Op 2) }
  @op3                            { mkToken (Op 3) }
  @op4                            { mkToken (Op 4) }
  @op5                            { mkToken (Op 5) }
  @op6                            { mkToken (Op 6) }
  $op_char+                       { mkToken (Op 7) }
  "`" @identifier "`"             { mkToken ((Op 7) . init . tail) }
  "("                             { mkToken (\_ -> OpenParen) }
  ")"                             { mkToken (\_ -> CloseParen) }
  $eol                            { mkToken (\_ -> Eol) }
  $digit+ @identifier             { mkToken BadToken }
  "_" @identifier                 { mkToken BadToken }
  .                               { mkToken BadToken }

{

data Posn = Posn Int Int Int
          deriving (Show)

data Token = FloatLiteral Double
           | KeywordForeign
           | DefOp
           | TypeOp
           | ToOp
           | Id String
           | TypeId String
           | Op Integer String
           | OpenParen
           | CloseParen
           | Eol
           | BadToken String
           deriving (Show, Eq)

mkToken :: (String -> Token) -> AlexPosn -> String -> (Posn, Token)
mkToken f (AlexPn a l c) s = ((Posn a l c), f s)

mkToken' :: String -> (String -> Token) -> AlexPosn -> String -> (Posn, Token)
mkToken' n f (AlexPn a l c) s = ((Posn a l c), f (trace (n ++ "(" ++ s ++ ")") s))

lex :: String -> [(Posn, Token)]
lex = alexScanTokens

}
