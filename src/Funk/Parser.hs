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

module Funk.Parser
       ( parse,
         RawName(..)
       ) where

import Funk.AST
import qualified Funk.Lexer as Lex
import Funk.Names (RawName(..))

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String
import Text.Parsec.Pos (newPos)
import Text.Parsec ((<|>), many, manyTill, many1, ParseError,
                    eof, parsecMap)

import Control.Applicative ((<$>))
import Data.Either (partitionEithers)


type Parser a = Text.Parsec.String.GenParser (Lex.Posn, Lex.Token) () a
--token :: (tok -> String) -> (tok -> SourcePos) -> (tok -> Maybe a)
--         -> GenParser tok st a
token :: (Lex.Token -> Maybe a) -> Parser a
token test
  = Parsec.token showTok posFromTok testTok
  where
    showTok (_, t) = show t
    posFromTok ((Lex.Posn _ l c), _) = newPos "TODO" l c
    testTok (_, t) = test t

floatLiteral :: Parser Double
floatLiteral = token $ \tok -> case tok of
  Lex.FloatLiteral v -> Just v
  _                  -> Nothing

defOp :: Parser ()
defOp = token $ \tok -> case tok of
  Lex.DefOp -> Just ()
  _         -> Nothing

identifier :: Parser String
identifier = token $ \tok -> case tok of
  Lex.Id s -> Just s
  _          -> Nothing

op :: Parser String
op = token $ \tok -> case tok of
  Lex.Op  s -> Just s
  _             -> Nothing

op1 :: Parser String
op1 = token $ \tok -> case tok of
  Lex.Op1  s -> Just s
  _             -> Nothing

op2 :: Parser String
op2 = token $ \tok -> case tok of
  Lex.Op2  s -> Just s
  _             -> Nothing

openParen :: Parser ()
openParen = token $ \tok -> case tok of
  Lex.OpenParen -> Just ()
  _             -> Nothing

closeParen :: Parser ()
closeParen = token $ \tok -> case tok of
  Lex.CloseParen -> Just ()
  _              -> Nothing

eol :: Parser ()
eol = token $ \tok -> case tok of
  Lex.Eol   -> Just ()
  _         -> Nothing

keywordForeign :: Parser ()
keywordForeign = token $ \tok -> case tok of
  Lex.KeywordForeign -> Just ()
  _                  -> Nothing


parse :: String -> String -> Either ParseError (Module RawName)
parse filename input = Parsec.parse module' filename (Lex.lex input)

module' :: Parser (Module RawName)
module' = ((uncurry Module) . partitionEithers) <$> manyTill def' eof


-- Def' -> Def | ForeignDef
def' :: Parser (Either (Def RawName) (ForeignDef RawName))
def' = (parsecMap Right foreignDef) <|> (parsecMap Left def)


-- ParamList -> identifier ParamList
-- ParamList -> 
paramList :: Parser [RawName]
paramList = fmap RawName <$> (many identifier)


-- ForeignDef -> "keywordForeign identifier ParamList defOp Expr
foreignDef :: Parser (ForeignDef RawName)
foreignDef = do
  keywordForeign
  n <- RawName <$> identifier
  params <- paramList
  defOp
  n' <- RawName <$> identifier
  eol
  return (ForeignDef n params n')
  

-- Def -> identifier ParamList defOp Expr
def :: Parser (Def RawName)
def = do
  n <- RawName <$> identifier
  params <- paramList
  defOp
  e <- expr
  eol
  return (Def n params e)


-----------------------------------------------------------------
-- Expression parsing
-----------------------------------------------------------------
--
-- Ambiguous but easy-to-understand grammar (Basically EBNF):
--
--   expr = opExpr3
-- 
--   opExpr3 = opExpr3 OP3 opExpr2 | opExpr2
-- 
--   opExpr2 = opExpr2 OP2 opExpr1 | opExpr1
-- 
--   opExpr1 = opExpr1 OP1 atomicExpr | atomicExpr
-- 
--   funcExpr = ID opExpr3 {opExpr3} | atomicExpr
-- 
--   atomicExpr = ID | LITERAL | "(" expr ")"
--
-----------------------------------------------------------------
--
-- Unambigious LR(1) grammar (reduction rules):
--
--   expr -> opExpr3
--   
--   opExpr3 -> opExpr2 opExpr3'
--   opExpr3' -> OP3 opEcpr2 opExpr3'
--   opExpr3' ->
--   
--   opExpr2 -> opExpr1 opExpr2'
--   opExpr2' -> OP2 opExpr1 opExpr2'
--   opExpr2' ->
--   
--   opExpr1 -> funcExpr opExpr1'
--   opExpr1' -> OP1 atomicExpr opExpr1'
--   opExpr1' ->
--   
--   funcExpr -> ID idExpr
--   funcExpr -> atomicExpr
--
--   idExpr -> funcCall
--   idExpr ->
--
--   funcCall -> argExpr funcCall
--   funcCall ->
--
--   argExpr -> ID
--   argExpr -> atomicExpr
--   
--   atomicExpr -> LITERAL | "(" expr ")"
--
-----------------------------------------------------------------

expr :: Parser (Expr RawName)
expr = opExpr3

opExpr3 :: Parser (Expr RawName)
opExpr3 = do
  e <- opExpr2
  opExpr3' e <|> return e

opExpr3' :: (Expr RawName) -> Parser (Expr RawName)
opExpr3' left = do
  n <- RawName <$> op
  right <- opExpr2
  let e = Op n left right
  opExpr3' e <|> return e
  
opExpr2 :: Parser (Expr RawName)
opExpr2 = do
  e <- opExpr1
  opExpr2' e <|> return e

opExpr2' :: (Expr RawName) -> Parser (Expr RawName)
opExpr2' left = do
  n <- RawName <$> op2
  right <- opExpr1
  let e = Op n left right
  opExpr2' e <|> return e

opExpr1 :: Parser (Expr RawName)
opExpr1 = do
  e <- funcExpr
  opExpr1' e <|> return e

opExpr1' :: (Expr RawName) -> Parser (Expr RawName)
opExpr1' left = do
  n <- RawName <$> op1
  right <- funcExpr
  let e = Op n left right
  opExpr1' e <|> return e

funcExpr :: Parser (Expr RawName)
funcExpr = idExpr <|> atomicExpr

idExpr :: Parser (Expr RawName)
idExpr = do
  n <- RawName <$> identifier
  funcCall n <|> return (VarRef n)

funcCall :: RawName -> Parser (Expr RawName)
funcCall n = Call n <$> many1 argExpr

argExpr :: Parser (Expr RawName)
argExpr = ((VarRef . RawName) <$> identifier) <|> atomicExpr

atomicExpr :: Parser (Expr RawName)
atomicExpr = parenExpr <|> (FloatLiteral <$> floatLiteral)

parenExpr :: Parser (Expr RawName)
parenExpr = do
  openParen
  r <- expr
  closeParen
  return r
