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

{-# LANGUAGE FlexibleContexts #-}

module Funk.Parser
       ( parse,
         RawName
       ) where

import Funk.AST
import qualified Funk.Lexer as Lex
import Funk.Names (RawName,rawName,UnresolvedName(..))

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String
import Text.Parsec.Pos (newPos)
import Text.Parsec ((<|>), many, manyTill, many1, ParseError,
                    eof, parsecMap)

import Control.Applicative ((<$>))
import Data.Either (partitionEithers)
import Control.Monad.Error


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

op :: Integer -> Parser String
op precedence = token $ \tok -> case tok of
  Lex.Op p s -> if p == precedence then Just s else Nothing
  _          -> Nothing

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


parse :: MonadError String m => String -> String -> m (Module RawName)
parse = (convertError .) . parse'
  where
    convertError (Right r) = return r
    convertError (Left e) = fail (show e)

parse' :: String -> String -> Either ParseError (Module RawName)
parse' filename input = Parsec.parse module' filename (Lex.lex input)

module' :: Parser (Module RawName)
module' = Module <$> manyTill def' eof


-- Def' -> Def | ForeignDef
def' :: Parser (Def RawName)
def' = def <|> foreignDef


-- ParamList -> identifier ParamList
-- ParamList -> 
paramList :: Parser [RawName]
paramList = fmap rawName <$> (many identifier)


-- ForeignDef -> "keywordForeign identifier ParamList defOp Expr
foreignDef :: Parser (Def RawName)
foreignDef = do
  keywordForeign
  n <- rawName <$> identifier
  params <- paramList
  defOp
  n' <- rawName <$> identifier
  eol
  return (ForeignDef n params n')
  

-- Def -> identifier ParamList defOp Expr
def :: Parser (Def RawName)
def = do
  n <- rawName <$> identifier
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
expr = opExpr lowestOperatorPrecedence
  where
    lowestOperatorPrecedence = 7

opExpr :: Integer -> Parser (Expr RawName)
opExpr precedence = do
  e <- if precedence > 1 then opExpr (precedence-1) else funcExpr
  opExpr' precedence e <|> return e
  
opExpr' :: Integer -> (Expr RawName) -> Parser (Expr RawName)
opExpr' precedence left = do
  n <- rawName <$> op precedence
  right <- if precedence > 1 then opExpr (precedence-1) else funcExpr
  let e = Op n left right
  opExpr' precedence e <|> return e

funcExpr :: Parser (Expr RawName)
funcExpr = idExpr <|> atomicExpr

idExpr :: Parser (Expr RawName)
idExpr = do
  n <- rawName <$> identifier
  funcCall n <|> return (VarRef n)

funcCall :: RawName -> Parser (Expr RawName)
funcCall n = Call n <$> many1 argExpr

argExpr :: Parser (Expr RawName)
argExpr = ((VarRef . rawName) <$> identifier) <|> atomicExpr

atomicExpr :: Parser (Expr RawName)
atomicExpr = parenExpr <|> (FloatLiteral <$> floatLiteral)

parenExpr :: Parser (Expr RawName)
parenExpr = do
  openParen
  r <- expr
  closeParen
  return r
