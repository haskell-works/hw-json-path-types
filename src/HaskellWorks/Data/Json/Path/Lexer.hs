{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE PolymorphicComponents        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -Wno-unused-do-bind       #-}
{-# OPTIONS_GHC -Wno-type-defaults        #-}

module HaskellWorks.Data.Json.Path.Lexer where

import Data.Char (isAlpha, toLower, toUpper, isSpace, digitToInt)
import Data.List (nub, sort)
import Control.Monad.Identity
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator

type Parser u = ParsecT String u Identity

commentStart :: String
commentStart = ""

commentEnd :: String
commentEnd = ""

commentLine :: String
commentLine = ""

nestedComments :: Bool
nestedComments = True

identStart :: Parser u Char
identStart = letter <|> char '_'

identLetter :: Parser u Char
identLetter = alphaNum <|> oneOf "_'"

opStart :: Parser u Char
opStart = opLetter

opLetter :: Parser u Char
opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"

reservedNames :: [String]
reservedNames = []

reservedOpNames :: [String]
reservedOpNames = []

caseSensitive :: Bool
caseSensitive = True

parens :: Parser u a -> Parser u a
parens = between (symbol "(") (symbol ")")

braces :: Parser u a -> Parser u a
braces = between (symbol "{") (symbol "}")

angles :: Parser u a -> Parser u a
angles = between (symbol "<") (symbol ">")

brackets :: Parser u a -> Parser u a
brackets = between (symbol "[") (symbol "]")

semi :: Parser u String
semi = symbol ";"

comma :: Parser u String
comma = symbol ","

dot :: Parser u String
dot = symbol "."

colon :: Parser u String
colon = symbol ":"

commaSep :: Parser u a -> Parser u [a]
commaSep p = sepBy p comma

semiSep :: Parser u a -> Parser u [a]
semiSep p = sepBy p semi

commaSep1 :: Parser u a -> Parser u [a]
commaSep1 p = sepBy1 p comma

semiSep1 :: Parser u a -> Parser u [a]
semiSep1 p = sepBy1 p semi

charLiteral :: Parser u Char
charLiteral = lexeme (between (char '\'')
                                  (char '\'' <?> "end of character")
                                  characterChar )
                <?> "character"

characterChar :: Parser u Char
characterChar = charLetter <|> charEscape
                <?> "literal character"

charEscape :: Parser u Char
charEscape = do { char '\\'; escapeCode }

charLetter :: Parser u Char
charLetter = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

stringLiteral :: Parser u String
stringLiteral = lexeme (
                  do{ str <- between (char '"')
                                     (char '"' <?> "end of string")
                                     (many stringChar)
                    ; return (foldr (maybe id (:)) "" str)
                    }
                  <?> "literal string")

stringChar :: Parser u (Maybe Char)
stringChar =   do{ c <- stringLetter; return (Just c) }
                <|> stringEscape
                <?> "string character"

stringLetter :: Parser u Char
stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

stringEscape :: Parser u (Maybe Char)
stringEscape = do { char '\\'
                    ;     do{ escapeGap  ; return Nothing }
                      <|> do{ escapeEmpty; return Nothing }
                      <|> do{ esc <- escapeCode; return (Just esc) }
                    }

escapeEmpty :: Parser u Char
escapeEmpty = char '&'

escapeGap :: Parser u Char
escapeGap = do
  many1 space
  char '\\' <?> "end of string gap"

escapeCode :: Parser u Char
escapeCode = charEsc <|> charNum <|> charAscii <|> charControl <?> "escape code"

charControl :: Parser u Char
charControl = do
  char '^'
  code <- upper
  return (toEnum (fromEnum code - fromEnum 'A' + 1))

charNum :: Parser u Char
charNum = do
  code <- decimal
          <|> (char 'o' >> number 8 octDigit)
          <|> (char 'x' >> number 16 hexDigit)
  if code > 0x10FFFF
    then fail "invalid escape sequence"
    else return (toEnum (fromInteger code))

charEsc :: Parser u Char
charEsc = choice (map parseEsc escMap)
  where parseEsc (c, code) = char c >> return code

charAscii :: Parser u Char
charAscii = choice (map parseAscii asciiMap)
  where parseAscii (asc,code) = try (do{ string asc; return code })

escMap :: [(Char, Char)]
escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"

asciiMap :: [(String, Char)]
asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

ascii2codes :: [String]
ascii2codes = [ "BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI", "EM", "FS", "GS", "RS", "US", "SP" ]

ascii3codes :: [String]
ascii3codes = [ "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "DLE", "DC1", "DC2", "DC3"
              , "DC4", "NAK", "SYN", "ETB", "CAN", "SUB", "ESC", "DEL"]

ascii2 :: String
ascii2 =  [ '\BS', '\HT', '\LF', '\VT', '\FF', '\CR', '\SO', '\SI', '\EM', '\FS', '\GS', '\RS', '\US', '\SP' ]

ascii3 :: String
ascii3 =  [ '\NUL', '\SOH', '\STX', '\ETX', '\EOT', '\ENQ', '\ACK'
          , '\BEL', '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK'
          , '\SYN', '\ETB', '\CAN', '\SUB', '\ESC', '\DEL'
          ]

naturalOrFloat :: Parser u (Either Integer Double)
naturalOrFloat = lexeme natFloat   <?> "number"

float :: Parser u Double
float = lexeme floating   <?> "float"

integer :: Parser u Integer
integer = lexeme int <?> "integer"

natural :: Parser u Integer
natural = lexeme nat <?> "natural"

floating :: Parser u Double
floating = do
  n <- decimal
  fractExponent n

natFloat :: Parser u (Either Integer Double)
natFloat = (char '0' >> zeroNumFloat) <|> decimalFloat

zeroNumFloat :: Parser u (Either Integer Double)
zeroNumFloat = nonDecimal <|> decimalFloat <|> fractFloat 0 <|> return (Left 0)
  where nonDecimal = do
          n <- hexadecimal <|> octal
          return (Left n)

decimalFloat :: Parser u (Either Integer Double)
decimalFloat = do
  n <- decimal
  option (Left n) (fractFloat n)

fractFloat :: (Show a, Read b) => a -> Parser u (Either a1 b)
fractFloat n = do
  f <- fractExponent n
  return (Right f)

fractExponent :: (Show a1, Read a) => a1 -> Parser u a
fractExponent n = fractExponent1 <|> fractExponent2
  where readDouble s = case reads s of
          [(x, "")] -> return x
          _         -> parserZero
        fractExponent1 = do
          fract <- fraction
          expo  <- option "" exponent'
          readDouble (show n ++ fract ++ expo)
        fractExponent2 = do
          expo <- exponent'
          readDouble (show n ++ expo)

fraction ::  Parser u String
fraction = do
      char '.'
      digits <- many1 digit <?> "fraction"
      return ('.' : digits)
  <?> "fraction"

exponent' :: Parser u String
exponent' = do
      oneOf "eE"
      sign' <- fmap (:[]) (oneOf "+-") <|> return ""
      e <- decimal <?> "exponent"
      return ('e' : sign' ++ show e)
  <?> "exponent"

int :: Parser u Integer
int = do
  f <- lexeme sign
  n <- nat
  return (f n)

sign :: Parser u (Integer -> Integer)
sign  =   (char '-' >> return negate)
      <|> (char '+' >> return id)
      <|> return id

nat :: Parser u Integer
nat = zeroNumber <|> decimal

zeroNumber :: Parser u Integer
zeroNumber = do
      char '0'
      hexadecimal <|> octal <|> decimal <|> return 0
  <?> ""

decimal :: Parser u Integer
decimal = number 10 digit

hexadecimal :: Parser u Integer
hexadecimal = oneOf "xX" >> number 16 hexDigit

octal :: Parser u Integer
octal = oneOf "oO" >> number 8 octDigit

number :: Stream s m t => Integer -> ParsecT s u m Char -> ParsecT s u m Integer
number base baseDigit = do
  digits <- many1 baseDigit
  let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
  seq n (return n)

reservedOp :: String -> Parser u ()
reservedOp name = lexeme $ try $ do
  string name
  notFollowedBy opLetter <?> ("end of " ++ show name)

operator :: Parser u String
operator = lexeme $ try $ do
  name <- oper
  if isReservedOp name
    then unexpected ("reserved operator " ++ show name)
    else return name

oper :: Parser u String
oper = do
      c <- opStart
      cs <- many opLetter
      return (c:cs)
  <?> "operator"

isReservedOp :: String -> Bool
isReservedOp = isReserved (sort reservedOpNames)

reserved :: String -> Parser u ()
reserved name = lexeme $ try $ do
  caseString name
  notFollowedBy identLetter <?> ("end of " ++ show name)

caseString :: Stream s m Char => String -> ParsecT s u m String
caseString name
    | caseSensitive  = string name
    | otherwise      = walk name >> return name
    where
      walk []     = return ()
      walk (c:cs) = do{ caseChar c <?> msg; walk cs }
      caseChar c  | isAlpha c  = char (toLower c) <|> char (toUpper c)
                  | otherwise  = char c
      msg         = show name

identifier :: Parser u String
identifier = lexeme $ try $ do
  name <- ident
  if isReservedName name
    then unexpected ("reserved word " ++ show name)
    else return name

ident :: Parser u String
ident = do
      c <- identStart
      cs <- many identLetter
      return (c:cs)
  <?> "identifier"

isReservedName :: String -> Bool
isReservedName name = isReserved theReservedNames caseName
  where caseName  | caseSensitive = name
                  | otherwise     = map toLower name

isReserved :: Ord t => [t] -> t -> Bool
isReserved names name = scan names
  where scan []     = False
        scan (r:rs) = case compare r name of
          LT  -> scan rs
          EQ  -> True
          GT  -> False

theReservedNames :: [String]
theReservedNames
    | caseSensitive = sort reserved
    | otherwise     = sort . map (map toLower) $ reserved
    where reserved = reservedNames

symbol :: String -> Parser u String
symbol name = lexeme (string name)

lexeme :: Parser u a -> Parser u a
lexeme p = do
  x <- p
  whiteSpace
  return x

whiteSpace :: Parser u ()
whiteSpace
    | noLine && noMulti  = skipMany (simpleSpace <?> "")
    | noLine             = skipMany (simpleSpace <|> multiLineComment <?> "")
    | noMulti            = skipMany (simpleSpace <|> oneLineComment <?> "")
    | otherwise          = skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
    where
      noLine  = null commentLine
      noMulti = null commentStart

simpleSpace :: Parser u ()
simpleSpace =
    skipMany1 (satisfy isSpace)

oneLineComment :: Parser u ()
oneLineComment = do
  try (string commentLine)
  skipMany (satisfy (/= '\n'))
  return ()

multiLineComment :: Parser u ()
multiLineComment = do
  try (string commentStart)
  inComment

inComment :: Parser u ()
inComment
    | nestedComments  = inCommentMulti
    | otherwise       = inCommentSingle

inCommentMulti :: Parser u ()
inCommentMulti
    =   do{ try (string commentEnd) ; return () }
    <|> do{ multiLineComment                     ; inCommentMulti }
    <|> do{ skipMany1 (noneOf startEnd)          ; inCommentMulti }
    <|> do{ oneOf startEnd                       ; inCommentMulti }
    <?> "end of comment"
    where
      startEnd   = nub (commentEnd ++ commentStart)

inCommentSingle :: Parser u ()
inCommentSingle
    =   do{ try (string commentEnd); return () }
    <|> do{ skipMany1 (noneOf startEnd)         ; inCommentSingle }
    <|> do{ oneOf startEnd                      ; inCommentSingle }
    <?> "end of comment"
    where
      startEnd   = nub (commentEnd ++ commentStart)
