import Data.Map qualified as Map
import Text.Parsec
import Text.Parsec.Prim (many)
import Text.Parsec.String (Parser)
import Text.Parsec.Token qualified as P

main :: IO ()
main = do
  input <- getContents :: IO String
  let result = parse lexer "(unknown)" input
  case result of
    Left err -> print err
    Right tokens -> print tokens

data Token
  = KeywordToken Keyword
  | IdentifierToken Identifier
  | LiteralToken Literal
  | OperatorToken Operator
  | DelimiterToken Delimiter
  deriving (Show)

data Keyword = If | Else | While | For | Struct | Break | Continue | Return | New
  deriving (Show)

keywordsMap :: Map.Map String Keyword
keywordsMap =
  Map.fromList
    [ ("if", If),
      ("else", Else),
      ("while", While),
      ("for", For),
      ("struct", Struct),
      ("break", Break),
      ("continue", Continue),
      ("return", Return),
      ("new", New)
    ]

type Identifier = String

data Literal
  = IntegerLiteralToken IntegerLiteral
  | FloatingLiteralToken FloatingLiteral
  | StringLiteralToken StringLiteral
  | BooleanLiteralToken BooleanLiteral
  | NullLiteralToken
  deriving (Show)

data IntegerLiteral = IntLiteral Int | LongLiteral Integer
  deriving (Show)

type FloatingLiteral = Float

type StringLiteral = String

type BooleanLiteral = Bool

data Operator
  = PlusOp
  | MinusOp
  | StarOp
  | SlashOp
  | PercentOp
  | PipeOp
  | AmpersandOp
  | BangOp
  | LTOp
  | GTOp
  | LTEOp
  | GTEOp
  | EqEqOp
  | NotEqOp
  | EqOp
  | PlusPlusOp
  | MinusMinusOp
  | PoundOp
  | ShiftLeftOp
  | ShiftRightOp
  deriving (Show)

operatorMap =
  Map.fromList
    [ ("+", PlusOp),
      ("-", MinusOp),
      ("*", StarOp),
      ("/", SlashOp),
      ("% ", PercentOp),
      ("||", PipeOp),
      ("&&", AmpersandOp),
      ("!", BangOp),
      ("<", LTOp),
      (">", GTOp),
      ("<=", LTEOp),
      (">=", GTEOp),
      ("==", EqEqOp),
      ("!=", NotEqOp),
      ("=", EqOp),
      ("++", PlusPlusOp),
      ("--", MinusMinusOp),
      ("#", PoundOp),
      ("<<", ShiftLeftOp),
      (">>", ShiftRightOp)
    ]

data Delimiter
  = OpenParen
  | CloseParen
  | OpenBracket
  | CloseBracket
  | OpenBrace
  | CloseBrace
  | Comma
  | Dot
  | Semicolon
  deriving (Show)

delimiterMap =
  Map.fromList
    [ ("(", OpenParen),
      (")", CloseParen),
      ("[", OpenBracket),
      ("]", CloseBracket),
      ("{", OpenBrace),
      ("}", CloseBrace),
      (",", Comma),
      (".", Dot),
      (";", Semicolon)
    ]

-- a uc program is a sequence of whitespace, comments, and tokens
-- the lexer removes the whitespace and the comments and provides a list of tokens
-- Left error, Right Tokenstream
lexer :: Parser [Token]
lexer = do
  let ignoreParser = whitespaceParser <|> commentParser
  tokens <- many $ many ignoreParser >> tokenParser <* many ignoreParser
  eof
  return tokens

whitespaceParser :: Parser ()
whitespaceParser = oneOf " \t\r\v\f\n" >> return ()

commentParser :: Parser ()
commentParser = try delimitedComment <|> eolComment
  where
    delimitedComment = try (string "/*") >> manyTill anyChar (try (string "*/")) >> return ()
    eolComment = try (string "//") >> manyTill anyChar (try (char '\n')) >> return ()

tokenParser :: Parser Token
tokenParser =
  try (KeywordToken <$> keywordParser)
    <|> try (LiteralToken <$> literalParser)
    <|> try (OperatorToken <$> operatorParser)
    <|> try (DelimiterToken <$> delimiterParser)
    <|> try (IdentifierToken <$> identifierParser)
    <?> "token"

keywordParser :: Parser Keyword
keywordParser = oneOfStringParserFromMap keywordsMap "not a keyword "

literalParser :: Parser Literal
literalParser =
  try (string "null" >> return NullLiteralToken)
    <|> try (string "true" >> return (BooleanLiteralToken True))
    <|> try (string "false" >> return (BooleanLiteralToken False))
    <|> try (IntegerLiteralToken <$> integerLiteralParser)
    <|> try (FloatingLiteralToken <$> floatingLiteralParser)
    <|> try (StringLiteralToken <$> stringLiteralParser)
    <?> "literal"

integerLiteralParser :: Parser IntegerLiteral
integerLiteralParser = do
  d <- many1 digit
  ending <- optionMaybe (oneOf "lL")
  case ending of
    Just _ -> return $ IntLiteral (read d :: Int)
    Nothing -> return $ LongLiteral (read d :: Integer)

floatingLiteralParser :: Parser FloatingLiteral
floatingLiteralParser =
  try
    ( do
        d <- dot
        dgs <- digits
        exp <- option "" exponentParser
        return . read $ ('0' : d ++ dgs ++ exp) -- haskell doesn't like numbers like .2, so we prefix with 0
    )
    <|> try
      ( do
          dgs <- digits
          d <- dot
          moredgs <- option "" digits
          exp <- option "" exponentParser
          return . read $ (dgs ++ d ++ moredgs ++ exp)
      )
    <|> try
      ( do
          dgs <- digits
          exp <- exponentParser
          return . read $ dgs ++ exp
      )
  where
    dot = string "."
    digits = many1 digit
    exponentParser = do
      e <- char 'e'
      sign <- option '+' (oneOf "-+")
      ds <- digits
      return $ e : sign : ds

stringLiteralParser :: Parser StringLiteral
stringLiteralParser = char '"' >> manyTill stringChar (try (char '"')) <?> "string literal"
  where
    stringChar = try escapedChar <|> unescapedChar
    escapedChar =
      choice $
        map
          try
          [ '\"' <$ string "\\\"",
            '\\' <$ string "\\\\",
            '\a' <$ string "\\a",
            '\b' <$ string "\\b",
            '\n' <$ string "\\n",
            '\t' <$ string "\\t",
            '\f' <$ string "\\f",
            '\r' <$ string "\\r"
          ] -- holy escapes batman!
    unescapedChar = noneOf ['\\', '\"', '\n']

operatorParser :: Parser Operator
operatorParser =
  oneOfStringParserFromMap operatorMap "not a operator "
    <?> "operator"

delimiterParser :: Parser Delimiter
delimiterParser =
  oneOfStringParserFromMap delimiterMap "not a delimiter "
    <?> "delimiter"

identifierParser :: Parser Identifier
identifierParser =
  ( do
      l <- letter
      rest <- many (letter <|> char '_' <|> digit)
      return $ l : rest
  )
    <?> "identifier"

{- HELPER FUNCTIONS -}

oneOfStringParserFromMap :: Map.Map String a -> String -> Parser a
oneOfStringParserFromMap mp errormsg =
  choice $ map parserFromString (Map.keys mp)
  where
    parserFromString str = case Map.lookup str mp of
      Just val -> string str >> return val
      Nothing -> unexpected (errormsg ++ str)