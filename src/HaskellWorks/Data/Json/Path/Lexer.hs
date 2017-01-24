module HaskellWorks.Data.Json.Path.Lexer where

import            Data.Functor.Identity
import            Text.Parsec
import qualified  Text.Parsec.Token as P
import            Text.Parsec.Language (haskellDef)

expr :: ParsecT String u Identity String
expr  =   parens expr
      <|> identifier


lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser haskellDef

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = P.parens lexer

braces :: ParsecT String u Identity a -> ParsecT String u Identity a
braces      = P.braces lexer

identifier :: ParsecT String u Identity String
identifier  = P.identifier lexer

reserved :: String -> ParsecT String u Identity ()
reserved    = P.reserved lexer
