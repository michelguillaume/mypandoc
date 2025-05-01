-- src/Parser/Core.hs
{-# LANGUAGE LambdaCase #-}
module Parser.Core
  ( Parser
  , runParser
  , satisfy
  , char
  , string
  , many
  , some
  , choice
  , spaces
  , lexeme
  , parseQuoted
  ) where

import Control.Applicative hiding (many, some)
import Data.Char
import Prelude hiding (pred)

-- | Parser type: takes input and returns Maybe (result, remaining)
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- | Functor instance
instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> do
    (a, s') <- p s
    return (f a, s')

-- | Applicative instance
instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (Parser pf) <*> (Parser pa) = Parser $ \s -> do
    (f, s')  <- pf s
    (a, s'') <- pa s'
    return (f a, s'')

-- | Alternative instance
instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

-- | Monad instance for do-notation
instance Monad Parser where
  return = pure
  (Parser pa) >>= f = Parser $ \s -> do
    (a, s') <- pa s
    runParser (f a) s'

-- | MonadFail to handle pattern-match failures in do
instance MonadFail Parser where
  fail _ = Parser $ const Nothing

-- | Basic combinators
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ \case
  (c:cs) | pred c -> Just (c, cs)
  _                -> Nothing

char :: Char -> Parser Char
char = satisfy . (==)

string :: String -> Parser String
string = traverse char

many :: Parser a -> Parser [a]
many p = some p <|> pure []

some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p

choice :: [Parser a] -> Parser a
choice = asum

-- | Whitespace handling
spaces :: Parser String
spaces = many (satisfy isSpace)

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

-- | Quoted string parser (for XML/JSON attributes)
parseQuoted :: Parser String
parseQuoted = char '"' *> many (satisfy (/= '"')) <* char '"'
