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

import Text.Parsec.String (Parser)
import Text.Parsec ((<|>),many)

import Control.Applicative ((<$>))


parse :: Parser Module
parse = module'

module' :: Parser Module
module' = Module <$> many defs


defs :: Parser Def
defs = do
  name <- Name <$> Lex.identifier
  params <- fmap Name <$> (many Lex.identifier)
  Lex.defOp
  e <- expr
  return (Def name params e)
  
  
expr :: Parser Expr
expr = Lex.parens expr <|> literal <|> opCall <|> funcCall


literal :: Parser Expr
literal = FloatLiteral <$> Lex.float


opCall :: Parser Expr
opCall = do
  name <- Name <$> Lex.op
  args <- many expr
  return (Call name args)


funcCall :: Parser Expr
funcCall = do
  name <- Name <$> Lex.identifier
  args <-  many expr
  return (Call name args)

  
  