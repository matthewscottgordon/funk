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

module Funk.Lexer
       (
         float,
         identifier,
         op,
         defOp,
         parens
       ) where

import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Char (char, letter, alphaNum, oneOf)

import Control.Applicative ((<$>))


lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser def
  where
    def = emptyDef {
      Tok.commentLine = "#",
      Tok.identStart = letter <|> char '_',
      Tok.identLetter = alphaNum <|> char '_',
      Tok.opStart = oneOf "~!@$%^&*-+=|\\:;<>?/",
      Tok.opLetter = oneOf "~!@$%^&*-+=|\\:;<>?/",
      Tok.reservedOpNames = ["="]
      }

float :: Parser Double
float = (fromIntegral <$> Tok.integer lexer) <|> Tok.float lexer

identifier :: Parser String
identifier = Tok.identifier lexer

op :: Parser String
op = Tok.operator lexer

defOp :: Parser ()
defOp = Tok.reservedOp lexer "="

parens :: Parser a -> Parser a
parens = Tok.parens lexer