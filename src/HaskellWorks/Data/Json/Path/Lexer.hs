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

commentStart :: String
commentStart = ""

commentEnd :: String
commentEnd = ""

commentLine :: String
commentLine = ""

nestedComments :: Bool
nestedComments = True

identStart :: ParsecT String u Identity Char
identStart = letter <|> char '_'

identLetter :: ParsecT String u Identity Char
identLetter = alphaNum <|> oneOf "_'"

opStart :: ParsecT String u Identity Char
opStart = opLetter

opLetter :: ParsecT String u Identity Char
opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"

reservedNames :: [String]
reservedNames = []

reservedOpNames :: [String]
reservedOpNames = []

caseSensitive :: Bool
caseSensitive = True

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = between (symbol "(") (symbol ")")

braces :: ParsecT String u Identity a -> ParsecT String u Identity a
braces = between (symbol "{") (symbol "}")

angles :: ParsecT String u Identity a -> ParsecT String u Identity a
angles = between (symbol "<") (symbol ">")

brackets :: ParsecT String u Identity a -> ParsecT String u Identity a
brackets = between (symbol "[") (symbol "]")

semi :: ParsecT String u Identity String
semi = symbol ";"

comma :: ParsecT String u Identity String
comma = symbol ","

dot :: ParsecT String u Identity String
dot = symbol "."

colon :: ParsecT String u Identity String
colon = symbol ":"

commaSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep p = sepBy p comma

semiSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
semiSep p = sepBy p semi

commaSep1 :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep1 p = sepBy1 p comma

semiSep1 :: ParsecT String u Identity a -> ParsecT String u Identity [a]
semiSep1 p = sepBy1 p semi

charLiteral :: ParsecT String u Identity Char
charLiteral = lexeme (between (char '\'')
                                  (char '\'' <?> "end of character")
                                  characterChar )
                <?> "character"

characterChar :: ParsecT String u Identity Char
characterChar = charLetter <|> charEscape
                <?> "literal character"

charEscape :: ParsecT String u Identity Char
charEscape = do { char '\\'; escapeCode }

charLetter :: ParsecT String u Identity Char
charLetter = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

stringLiteral :: ParsecT String u Identity String
stringLiteral = lexeme (
                  do{ str <- between (char '"')
                                     (char '"' <?> "end of string")
                                     (many stringChar)
                    ; return (foldr (maybe id (:)) "" str)
                    }
                  <?> "literal string")

stringChar :: ParsecT String u Identity (Maybe Char)
stringChar =   do{ c <- stringLetter; return (Just c) }
                <|> stringEscape
                <?> "string character"

stringLetter :: ParsecT String u Identity Char
stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

stringEscape :: ParsecT String u Identity (Maybe Char)
stringEscape = do{ char '\\'
                    ;     do{ escapeGap  ; return Nothing }
                      <|> do{ escapeEmpty; return Nothing }
                      <|> do{ esc <- escapeCode; return (Just esc) }
                    }

escapeEmpty :: ParsecT String u Identity Char
escapeEmpty = char '&'

escapeGap :: ParsecT String u Identity Char
escapeGap = do{ many1 space
                    ; char '\\' <?> "end of string gap"
                    }

escapeCode :: ParsecT String u Identity Char
escapeCode = charEsc <|> charNum <|> charAscii <|> charControl <?> "escape code"

charControl :: ParsecT String u Identity Char
charControl = do
  char '^'
  code <- upper
  return (toEnum (fromEnum code - fromEnum 'A' + 1))

charNum :: ParsecT String u Identity Char
charNum = do
  code <- decimal
          <|> (char 'o' >> number 8 octDigit)
          <|> (char 'x' >> number 16 hexDigit)
  if code > 0x10FFFF
    then fail "invalid escape sequence"
    else return (toEnum (fromInteger code))

charEsc :: ParsecT String u Identity Char
charEsc = choice (map parseEsc escMap)
                where
                  parseEsc (c,code)     = do{ char c; return code }

charAscii :: ParsecT String u Identity Char
charAscii       = choice (map parseAscii asciiMap)
                where
                  parseAscii (asc,code) = try (do{ string asc; return code })

escMap :: [(Char, Char)]
escMap          = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"

asciiMap :: [(String, Char)]
asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

ascii2codes :: [String]
ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                   "FS","GS","RS","US","SP"]

ascii3codes :: [String]
ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                   "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                   "CAN","SUB","ESC","DEL"]

ascii2 :: String
ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                   '\EM','\FS','\GS','\RS','\US','\SP']

ascii3 :: String
ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                   '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                   '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']


-----------------------------------------------------------
-----------------------------------------------------------
naturalOrFloat :: ParsecT String u Identity (Either Integer Double)
naturalOrFloat  = lexeme natFloat   <?> "number"

float :: ParsecT String u Identity Double
float           = lexeme floating   <?> "float"

integer :: ParsecT String u Identity Integer
integer         = lexeme int        <?> "integer"

natural :: ParsecT String u Identity Integer
natural         = lexeme nat        <?> "natural"

floating :: ParsecT String u Identity Double
floating        = do{ n <- decimal
                    ; fractExponent n
                    }

natFloat :: ParsecT String u Identity (Either Integer Double)
natFloat        = do{ char '0'
                    ; zeroNumFloat
                    }
                  <|> decimalFloat

zeroNumFloat :: ParsecT String u Identity (Either Integer Double)
zeroNumFloat    =  do{ n <- hexadecimal <|> octal
                     ; return (Left n)
                     }
                <|> decimalFloat
                <|> fractFloat 0
                <|> return (Left 0)

decimalFloat :: ParsecT String u Identity (Either Integer Double)
decimalFloat    = do{ n <- decimal
                    ; option (Left n)
                             (fractFloat n)
                    }

fractFloat :: (Show a, Read b) => a -> ParsecT String u Identity (Either a1 b)
fractFloat n    = do{ f <- fractExponent n
                    ; return (Right f)
                    }

fractExponent :: (Show a1, Read a) => a1 -> ParsecT String u Identity a
fractExponent n = do{ fract <- fraction
                    ; expo  <- option "" exponent'
                    ; readDouble (show n ++ fract ++ expo)
                    }
                <|>
                  do{ expo <- exponent'
                    ; readDouble (show n ++ expo)
                    }
                  where
                    readDouble s =
                      case reads s of
                        [(x, "")] -> return x
                        _         -> parserZero

fraction ::  ParsecT String u Identity String
fraction        = do{ char '.'
                    ; digits <- many1 digit <?> "fraction"
                    ; return ('.' : digits)
                    }
                  <?> "fraction"

exponent' :: ParsecT String u Identity String
exponent'       = do{ oneOf "eE"
                    ; sign' <- fmap (:[]) (oneOf "+-") <|> return ""
                    ; e <- decimal <?> "exponent"
                    ; return ('e' : sign' ++ show e)
                    }
                  <?> "exponent"

int :: ParsecT String u Identity Integer
int             = do{ f <- lexeme sign
                    ; n <- nat
                    ; return (f n)
                    }

sign :: ParsecT String u Identity (Integer -> Integer)
sign            =   (char '-' >> return negate)
                <|> (char '+' >> return id)
                <|> return id

nat :: ParsecT String u Identity Integer
nat             = zeroNumber <|> decimal

zeroNumber :: ParsecT String u Identity Integer
zeroNumber      = do{ char '0'
                    ; hexadecimal <|> octal <|> decimal <|> return 0
                    }
                  <?> ""

decimal :: ParsecT String u Identity Integer
decimal         = number 10 digit

hexadecimal :: ParsecT String u Identity Integer
hexadecimal     = do{ oneOf "xX"; number 16 hexDigit }

octal :: ParsecT String u Identity Integer
octal           = do{ oneOf "oO"; number 8 octDigit  }

number :: Stream s m t => Integer -> ParsecT s u m Char -> ParsecT s u m Integer
number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }

-----------------------------------------------------------
-----------------------------------------------------------
reservedOp :: String -> ParsecT String u Identity ()
reservedOp name =
    lexeme $ try $
    do{ string name
      ; notFollowedBy opLetter <?> ("end of " ++ show name)
      }

operator :: ParsecT String u Identity String
operator =
    lexeme $ try $
    do{ name <- oper
      ; if isReservedOp name
         then unexpected ("reserved operator " ++ show name)
         else return name
      }

oper :: ParsecT String u Identity String
oper =
    do{ c <- opStart
      ; cs <- many opLetter
      ; return (c:cs)
      }
    <?> "operator"

isReservedOp :: String -> Bool
isReservedOp = isReserved (sort reservedOpNames)


-----------------------------------------------------------
-----------------------------------------------------------
reserved :: String -> ParsecT String u Identity ()
reserved name =
    lexeme $ try $
    do{ caseString name
      ; notFollowedBy identLetter <?> ("end of " ++ show name)
      }

caseString :: Stream s m Char => String -> ParsecT s u m String
caseString name
    | caseSensitive  = string name
    | otherwise      = do{ walk name; return name }
    where
      walk []     = return ()
      walk (c:cs) = do{ caseChar c <?> msg; walk cs }

      caseChar c  | isAlpha c  = char (toLower c) <|> char (toUpper c)
                  | otherwise  = char c

      msg         = show name


identifier :: ParsecT String u Identity String
identifier =
    lexeme $ try $
    do{ name <- ident
      ; if isReservedName name
         then unexpected ("reserved word " ++ show name)
         else return name
      }

ident :: ParsecT String u Identity String
ident
    = do{ c <- identStart
        ; cs <- many identLetter
        ; return (c:cs)
        }
    <?> "identifier"

isReservedName :: String -> Bool
isReservedName name
    = isReserved theReservedNames caseName
    where
      caseName      | caseSensitive  = name
                    | otherwise               = map toLower name

isReserved :: Ord t => [t] -> t -> Bool
isReserved names name
    = scan names
    where
      scan []       = False
      scan (r:rs)   = case compare r name of
                        LT  -> scan rs
                        EQ  -> True
                        GT  -> False

theReservedNames :: [String]
theReservedNames
    | caseSensitive  = sort reserved
    | otherwise                  = sort . map (map toLower) $ reserved
    where
      reserved = reservedNames



-----------------------------------------------------------
-----------------------------------------------------------
symbol :: String -> ParsecT String u Identity String
symbol name
    = lexeme (string name)

lexeme :: ParsecT String u Identity a -> ParsecT String u Identity a
lexeme p
    = do{ x <- p; whiteSpace; return x  }

whiteSpace :: ParsecT String u Identity ()
whiteSpace
    | noLine && noMulti  = skipMany (simpleSpace <?> "")
    | noLine             = skipMany (simpleSpace <|> multiLineComment <?> "")
    | noMulti            = skipMany (simpleSpace <|> oneLineComment <?> "")
    | otherwise          = skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
    where
      noLine  = null commentLine
      noMulti = null commentStart

simpleSpace :: ParsecT String u Identity ()
simpleSpace =
    skipMany1 (satisfy isSpace)

oneLineComment :: ParsecT String u Identity ()
oneLineComment =
    do{ try (string commentLine)
      ; skipMany (satisfy (/= '\n'))
      ; return ()
      }

multiLineComment :: ParsecT String u Identity ()
multiLineComment =
    do { try (string commentStart)
       ; inComment
       }

inComment :: ParsecT String u Identity ()
inComment
    | nestedComments  = inCommentMulti
    | otherwise                = inCommentSingle

inCommentMulti :: ParsecT String u Identity ()
inCommentMulti
    =   do{ try (string commentEnd) ; return () }
    <|> do{ multiLineComment                     ; inCommentMulti }
    <|> do{ skipMany1 (noneOf startEnd)          ; inCommentMulti }
    <|> do{ oneOf startEnd                       ; inCommentMulti }
    <?> "end of comment"
    where
      startEnd   = nub (commentEnd ++ commentStart)

inCommentSingle :: ParsecT String u Identity ()
inCommentSingle
    =   do{ try (string commentEnd); return () }
    <|> do{ skipMany1 (noneOf startEnd)         ; inCommentSingle }
    <|> do{ oneOf startEnd                      ; inCommentSingle }
    <?> "end of comment"
    where
      startEnd   = nub (commentEnd ++ commentStart)
