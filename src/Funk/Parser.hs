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
       ( parse
       ) where

import Funk.AST
import qualified Funk.Lexer as Lex

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String
import Text.Parsec.Pos (newPos)
import Text.Parsec ((<|>), many, ParseError, eof)

import Control.Applicative ((<$>))

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

name :: Parser String
name = token $ \tok -> case tok of
  Lex.Name s -> Just s
  _          -> Nothing

op :: Parser String
op = token $ \tok -> case tok of
  Lex.OpName  s -> Just s
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

inParens :: Parser a -> Parser a
inParens p = do
  openParen
  v <- p
  closeParen
  return v

parse :: String -> String -> Either ParseError Module
parse filename input = Parsec.parse module' filename (Lex.lex input)

module' :: Parser Module
module' = do
  r <- Module <$> many defs
  eof
  return r


-- Def     -> name ArgList defOp Expr
-- ArgList -> name ArgList
-- ArgList -> 
defs :: Parser Def
defs = do
  n <- Name <$> name
  params <- fmap Name <$> (many name)
  defOp
  e <- expr
  eol
  return (Def n params e)


expr :: Parser Expr
expr = funcCall           -- Expr -> name Expr
       <|> inParens expr  -- Expr -> "(" Expr ")"
       <|> literal        -- Expr -> floatLiteral
       <|> varRef         -- Expr -> name


-- Expr -> name
varRef :: Parser Expr
varRef = do
  n <- Name <$> name
  return (VarRef n)


-- Expr -> floatLiteral
literal :: Parser Expr
literal = FloatLiteral <$> floatLiteral


-- Expr -> name Expr
funcCall :: Parser Expr
funcCall = do
  n <- Name <$> (name <|> inParens op)
  args <-  many expr
  return (Call n args)
