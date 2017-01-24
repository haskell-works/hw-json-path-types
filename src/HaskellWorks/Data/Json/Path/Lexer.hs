module HaskellWorks.Data.Json.Path.Lexer where

import            Data.Functor.Identity
import            Text.Parsec
import qualified  Text.Parsec.Token     as T
import            Text.Parsec.Language  as L

jsonPathStyle  :: L.LanguageDef st
jsonPathStyle   = emptyDef
  { T.commentStart   = "{-"
  , T.commentEnd     = "-}"
  , T.commentLine    = "--"
  , T.nestedComments = True
  , T.identStart     = letter
  , T.identLetter    = alphaNum <|> oneOf "_'"
  , T.reservedNames  = []
  , T.reservedOpNames= []
  , T.caseSensitive  = False
  }

expr :: ParsecT String u Identity String
expr  = parens expr <|> identifier

lexer :: T.GenTokenParser String u Identity
lexer = T.makeTokenParser haskellDef

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = T.parens lexer

braces :: ParsecT String u Identity a -> ParsecT String u Identity a
braces = T.braces lexer

identifier :: ParsecT String u Identity String
identifier = T.identifier lexer

reserved :: String -> ParsecT String u Identity ()
reserved = T.reserved lexer
