module Parser
  ( Parse(..), parseFile
  ) where

import System.IO.Error (catchIOError)

import Text.Parsec hiding (parse)

import Type

-- Type class for parsing
class Parse a where
  parse :: String -> Either String a
  parseWithVars :: String -> Either String (a, [(VarIndex, String)])
  
  parse = fmap fst . parseWithVars

-- Try to parse a goal
instance Parse Goal where
  parseWithVars = adjustErrorMessage . simpleParse (withVars goal)
    where adjustErrorMessage (Left e) = Left ("Parse error (goal" ++ drop 19 e)
          adjustErrorMessage r        = r

-- Try to parse a program
instance Parse Prog where
  parseWithVars = simpleParse (withVars prog)

-- Try to parse a file
parseFile :: Parse a => FilePath -> IO (Either String a)
parseFile fn = flip catchIOError (const $ return $ Left "Could not read file.")
  (readFile fn >>= return . parse)

-- INTERNAL

-- Parser type
type Parser a = Parsec String [(String, VarIndex)] a

-- Apply a parser to a string
simpleParse :: Parser a -> String -> Either String a
simpleParse p =
  either (Left . ("Parse error " ++) . show) Right . runParser p [] ""

-- Modify a parser to return also the variable mapping
withVars :: Parser a -> Parser (a, [(VarIndex, String)])
withVars = flip (<*>) (map (\(x, y) -> (y, x)) <$> getState) . ((,) <$>)

-- Parse a goal
goal :: Parser Goal
goal = Goal <$> (whitespaces *> commaSep term <* symbol "." <* eof)

-- Parse a program
prog :: Parser Prog
prog = Prog <$> (whitespaces *> many rule <* eof)

-- Parse a rule
rule :: Parser Rule
rule = (:-) <$> term <*> rhs

-- Parse the right hand side of a rule
rhs :: Parser [Term]
rhs = symbol "." *> pure [] <|> symbol ":-" *> commaSep term <* symbol "."

-- Parse a term
term :: Parser Term
term = parens term <|> var <|> list <|> comb

-- Parse a variable term
var :: Parser Term
var = Var <$> do
  name <- varName
  state <- getState
  let newIdx = let idx = maximum (-1 : map snd state) + 1
               in updateState ((name, idx) :) *> pure idx
  case name of
    "_" -> newIdx
    _   -> case lookup name state of
      Just idx -> pure idx
      Nothing  -> newIdx

-- Parse a variable name
varName :: Parser String
varName = ((:) <$> upper <*> many (letter <|> digit <|> char '_') <|>
  (:) <$> char '_' <*> many (letter <|> digit <|> char '_')) <* whitespaces

-- Parse a list
list :: Parser Term
list = symbol "[" *> whitespaces *> do
  let nil = Comb "[]" []
      cons x xs = Comb "." [x, xs]
  try (cons <$> term <* whitespaces <* symbol "|" <*> term <* symbol "]") <|>
    foldr cons nil <$> commaSep term <* symbol "]"

-- Parse a combination term
comb :: Parser Term
comb = do
  f <- atom
  args <- maybe [] id <$> optionMaybe (parens (commaSep term))
  whitespaces
  pure (Comb f args)

-- Parse an atom
atom :: Parser String
atom = (:) <$> lower <*> many (letter <|> digit <|> char '_') <|>
  number <|> many1 (oneOf "+-*/<=>'\\:.?@#$&^~")

-- Parse a number
number :: Parser String
number = try ((:) <$> char '-' <*> (try float <|> int)) <|> try float <|> int
  where float = (++) <$> int <*>
          ((:) <$> char '.' <*> ((reverse . trim . reverse) <$> many1 digit))
        int   = trim <$> many1 digit
        trim  = show . (read :: String -> Integer)

-- Parse a symbol
symbol :: String -> Parser ()
symbol s = string s *> whitespaces

-- Parse a comment
comment :: Parser ()
comment = () <$ (char '%' *> many (noneOf "\n") *> char '\n') <?> "comment"

-- Parse whitespaces or a comment
whitespaces :: Parser ()
whitespaces =  skipMany (() <$ space <|> comment)

-- Parse a list separated by commas
commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` symbol ","

-- Parse something enclosed in parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

