module ParseStim where

import GenerateStim hiding (main)
import Text.ParserCombinators.Parsec hiding (space, spaces)
import Data.Char
import Numeric
import Control.Monad

type ErrMsg = String

--fromStim "asd" = 1

-- Converts a Stim file to a bunch of function calls
--fromStim :: String -> Either ErrMsg String
--fromStim s = case s of
-- TODO: use tagless final for the above too?

data Statement
  = IntAssignment String Int
  | Delay Int
  deriving Show

--This will reset the state back if parsing fails for either parser a or b. For the normal choice operator: if the parser consumed input it will not reset the state back!
a <||> b = try a <|> b

--End of Line
eol = char '\n'

-- *> is then (sequencing).
-- a comment can be many of anything exceot \n, then a newline.
commentParser = symb "//" *> many (noneOf "\n") *> eol

whitespace = oneOf " \n\r\t" <|> commentParser <?> "whitespace"

space :: Parser () -- () for unit because we don't want it to return anything.
space = skipMany whitespace

-- Takes a String and makes a parser that parses that string and then whitespace after it, but only returns the parsed string and not the whitespace that came after [via tok].
symb :: String -> Parser String
symb = tok . string

-- https://stackoverflow.com/questions/7215967/adding-readhex-to-a-parsec-parser
parseHex = do
    char '0'
    char 'x'
    digits <- many1 hexDigit   -- hexDigit is defined by Parsec.
    return (fst (readHex digits !! 0))

-- Parse decimal numbers (base 10)
parseDec :: Parser Int
parseDec = read <$> many1 digit

-- Parse hex or regular base 10 digits
nat :: Parser Int
nat = parseHex <||> parseDec

-- Like "nat" but consumes trailing whitespace.
natural = tok nat

digitp :: Char -> Bool
digitp x = x `elem` ['0' .. '9']

-- Transforms a parser into one that also consumes trailing whitespace. runs parser p, then runs the space parser but returns only the result returned from p, ignoring the spaces.
tok p = p <* space

-- The initial character of an indentifier must satisfy this predicate.
initp x = isAlpha x || elem x "_"

-- The subsequent characters of an indentifier must satisfy this predicate.
-- These subsequent characters can be what the initial characters can be, or also digits.
subseqp c = initp c || digitp c

-- An identifier without considering trailing whitespace.
ident = do
  x <- satisfy initp
  xs <- many (satisfy subseqp)
  return (x:xs)

identifier = tok ident

intAssignment = do
  --lhs = Left Hand Side
  lhs <- identifier
  symb "="
  rhs <- natural
  return (IntAssignment lhs rhs)
  
delayParser = do
  symb "#"
  delay_ <- natural
  return (Delay delay_)

-- "<*" == can be pronounced "before". << is for monads but this is for applicatives.

parseStim = space *> (intAssignment <||> delayParser) `sepBy` space <* eof

runStimParser str = parse parseStim "" str

main = do
  contents <- readFile "LogFile.stim"
  let result = runStimParser contents
  case result of
    Right res -> putStrLn (show res)
    Left err -> print err
