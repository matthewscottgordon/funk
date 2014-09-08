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
       ) where

import Funk.AST
import qualified Funk.Lexer as Lex
import Funk.Names (UnresolvedName(..))

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String
import Text.Parsec.Pos (newPos)
import Text.Parsec ((<|>), many, manyTill, many1, ParseError,
                    eof, parsecMap)

import Control.Applicative ((<$>))
import Data.Either (partitionEithers)
import Control.Monad.Error

import Debug.Trace


type Parser a = Text.Parsec.String.GenParser (Lex.Posn, Lex.Token) () a
token :: (Lex.Token -> Maybe a) -> Parser a
token test
  = Parsec.token showTok posFromTok testTok
  where
    showTok (_, t) = show t
    posFromTok (Lex.Posn _ l c, _) = newPos "TODO" l c
    testTok (_, t) = test t

floatLiteral :: Parser Double
floatLiteral = token $ \tok -> case tok of
  Lex.FloatLiteral v -> Just v
  _                  -> Nothing

defOp :: Parser ()
defOp = token $ \tok -> case tok of
  Lex.DefOp -> Just ()
  _         -> Nothing

typeOp :: Parser ()
typeOp = token $ \tok -> case tok of
  Lex.TypeOp -> Just ()
  _          -> Nothing

toOp :: Parser ()
toOp = token $ \tok -> case tok of
  Lex.ToOp -> Just ()
  _        -> Nothing

identifier :: Parser String
identifier = token $ \tok -> case tok of
  Lex.Id s -> Just s
  _          -> Nothing

typeId :: Parser String
typeId = token $ \tok -> case tok of
  Lex.TypeId s -> Just s
  _            -> Nothing

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


parse :: MonadError String m => String -> String -> m (Module UnresolvedName)
parse = (convertError .) . parse'
  where
    convertError (Right r) = return r
    convertError (Left e) = fail (show e)

parse' :: String -> String -> Either ParseError (Module UnresolvedName)
parse' filename input = Parsec.parse module' filename (Lex.lex input)


module' :: Parser (Module UnresolvedName)
module' = (uncurry Module . partitionEithers) <$> manyTill declOrDef eof

declOrDef :: Parser (Either (Decl UnresolvedName) (Def UnresolvedName))
declOrDef = do
  n <- UnresolvedName <$> identifier
  (Left <$> decl n) <|> (Right <$> def n)

decl :: UnresolvedName -> Parser (Decl UnresolvedName)
decl n = do
  typeOp
  r <- Decl n <$> typeExpr
  eol
  return r

typeExpr :: Parser Type
typeExpr = do
  t <- TypeName <$> typeId
  typeExpr' t <|> return t

typeExpr' :: Type -> Parser Type
typeExpr' t = do
  toOp
  FuncType t <$> typeExpr

-- ParamList -> identifier ParamList
-- ParamList -> 
paramList :: Parser [UnresolvedName]
paramList = fmap UnresolvedName <$> many identifier


-- Def -> identifier ParamList defOp Expr
def :: UnresolvedName -> Parser (Def UnresolvedName)
def n = do
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

expr :: Parser (Expr UnresolvedName)
expr = opExpr lowestOperatorPrecedence
  where
    lowestOperatorPrecedence = 7

opExpr :: Integer -> Parser (Expr UnresolvedName)
opExpr precedence = do
  e <- if precedence > 1 then opExpr (precedence-1) else funcExpr
  opExpr' precedence e <|> return e
  
opExpr' :: Integer -> Expr UnresolvedName -> Parser (Expr UnresolvedName)
opExpr' precedence left = do
  n <- UnresolvedName <$> op precedence
  right <- if precedence > 1 then opExpr (precedence-1) else funcExpr
  let e = Op n left right
  opExpr' precedence e <|> return e

funcExpr :: Parser (Expr UnresolvedName)
funcExpr = idExpr <|> atomicExpr

idExpr :: Parser (Expr UnresolvedName)
idExpr = do
  n <- UnresolvedName <$> identifier
  funcCall n <|> return (VarRef n)

funcCall :: UnresolvedName -> Parser (Expr UnresolvedName)
funcCall n = Call n <$> many1 argExpr

argExpr :: Parser (Expr UnresolvedName)
argExpr = ((VarRef . UnresolvedName) <$> identifier) <|> atomicExpr

atomicExpr :: Parser (Expr UnresolvedName)
atomicExpr = parenExpr <|> (FloatLiteral <$> floatLiteral)

parenExpr :: Parser (Expr UnresolvedName)
parenExpr = do
  openParen
  r <- expr
  closeParen
  return r
