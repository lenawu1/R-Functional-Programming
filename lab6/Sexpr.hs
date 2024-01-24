--
-- S-expression parser.
--

module Sexpr where

import Text.Parsec
import Text.Parsec.String

----------------------------------------------------------------------
-- Datatypes.
----------------------------------------------------------------------

data Atom =
    BoolA  Bool
  | IntA   Integer
  | FloatA Double
  | IdA    String  -- identifier
  | StringA String
  deriving (Show)


-- deleted QuoteS
data Sexpr =
    AtomS Atom
  | ListS [Sexpr]
  deriving (Show)

----------------------------------------------------------------------
-- Parsers.
----------------------------------------------------------------------

parseBool :: Parser Bool
parseBool =
  char '#' >>
  ((char 'f' >> return False)
   <|> (char 't' >> return True))
  <?> "boolean"

parseInt :: Parser Integer
parseInt = do
  sign <- option "" (string "-")
  digits <- many1 digit  -- many1 (oneOf "0123456789")
  return (read (sign ++ digits) :: Integer)
  <?> "integer"


-- C.4 Helper function to parse exponent
parseExp :: Parser [Char]
parseExp = do
	exp <- oneOf "eE"
	sign <- option "" (string "-" <|> string "+")
	digits <- many1 digit
	return ([exp] ++ sign ++ digits)
	
-- C.4 changed to add exponents
parseFloat :: Parser Double
parseFloat = do
  sign <- option "" (string "-")
  digits <- many1 digit
  char '.'
  f <- many1 digit
  exp <- option "" parseExp
  return (read (sign ++ digits ++ "." ++ f ++ exp) :: Double)
  <?> "floating-point number"



parseId :: Parser String
parseId = many1 (alphaNum <|> oneOf "_+-*/=?!") <?> "identifier"

-- C.5, parse a string
parseString :: Parser String
parseString = do
	char '\"'
	str <- many (noneOf "\"")
	char '\"'
	return str
	<?> "string"

-- C.5 added in parseString in same format
parseAtom :: Parser Atom
parseAtom =
  (parseBool >>= return . BoolA)
  <|> try (parseFloat >>= return . FloatA)
  <|> try (parseInt >>= return . IntA)
  <|> try (parseString >>= return . StringA)
  <|> (parseId >>= return . IdA)
  <?> "atom"

parseComment :: Parser ()
parseComment = do
  char ';'
  many (noneOf "\n")
  char '\n'
  return ()

parseWhitespace :: Parser ()
parseWhitespace = many1 space >> return ()

-- Parse a separator (whitespace or comment).
parseSep :: Parser ()
parseSep = 
  many1 (parseComment <|> parseWhitespace) >> return ()
  <?> "separator"


-- C.2 Helper function
parseListHelp :: Char -> Char -> Parser [Sexpr]
parseListHelp a b = do
	char a
	optional parseSep
	ss <- parseSexpr `sepEndBy` parseSep
	char b
	return ss
	<?> "delimited S-experessions"

-- Parse a list of S-expressions, delimited by parentheses,
-- separated by whitespace/comments.

-- C.2 updated this to deal with paranthesees, curly braces, and 
-- brackets.
parseList :: Parser [Sexpr]
parseList = do
	parseListHelp '(' ')'
	<|> parseListHelp '[' ']'
	<|> parseListHelp '{' '}'
	<?> "list of S-expressions"
-- C.3
{-
We do not need to use a try function because the <|> operator is
the equavalent of an or statement. this means that it will try all
three possibilities and match on each case of each one.
-}  
  
  

-- Parse a quoted expression.
parseQuote :: Parser Sexpr
parseQuote = char '\'' >> parseSexpr
  <?> "quoted S-expression"

-- Parse a single S-expressions.
-- removed QuoteS
parseSexpr :: Parser Sexpr
parseSexpr = 
  (parseAtom >>= return . AtomS)
  <|> (parseList >>= return . ListS)
  <|> parseQuote
  <?> "S-expression"

-- Parse a series of Sexprs from a string representing the entire contents of a
-- file.
parseSexprsFromFile :: Parser [Sexpr]
parseSexprsFromFile = do
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  eof
  return ss
  <?> "file of S-expressions"

----------------------------------------------------------------------
-- Pretty-printer.
----------------------------------------------------------------------

indent :: Int -> String
indent i = replicate i ' '

-- Pretty-print a Sexpr.
-- Removed QuoteS case
ppSexpr :: Int -> Sexpr -> String
ppSexpr i (AtomS a)  = indent i ++ "AtomS[" ++ show a ++ "]"
ppSexpr i (ListS ss) = 
  indent i
  ++ "ListS[\n" 
  ++ concatMap (\s -> ppSexpr (i + 2) s ++ "\n") ss
  ++ indent i ++ "]"

-- Parse all expressions in a file and run the pretty-printer on them.
runPpSexpr :: FilePath -> IO ()
runPpSexpr f = do
  p <- parseFromFile parseSexprsFromFile f
  case p of
    Left err -> putStrLn $ "ERROR: " ++ show err
    Right ss -> 
      mapM_ (\s -> do
        putStrLn (ppSexpr 0 s)
        putStrLn "") ss

----------------------------------------------------------------------
-- Tests.
----------------------------------------------------------------------

test :: IO ()
test = runPpSexpr "test.scm"

