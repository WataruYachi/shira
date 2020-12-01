{-# LANGUAGE LambdaCase #-}
module Parser  where

import           Control.Applicative
import           Control.Monad
import           Data.Char

newtype Parser a = P {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
    fmap f p =
        P
            ( \input ->
                case runParser p input of
                    Just (a, s) -> Just (f a, s)
                    Nothing     -> Nothing
            )

instance Applicative Parser where
    pure a = P (\input -> Just (a, input))
    fp <*> gp =
        P
            ( \input ->
                case runParser fp input of
                    Just (a, s) -> runParser (fmap a gp) s
                    Nothing     -> Nothing
            )

instance Alternative Parser where
    empty = P $ const Nothing
    p <|> q = P (\input -> case runParser p input of
                                Nothing -> runParser q input
                                Just x  -> Just x)

instance Monad Parser where
    p >>= f =
        P
            ( \input ->
                case runParser p input of
                    Just (a, s) -> runParser (f a) s
                    Nothing     -> Nothing
            )

item :: Parser Char
item = P (\case
            (x:xs) -> Just (x,xs)
            []     -> Nothing

        )

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isLetter

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char p = sat (== p)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
    char x
    string xs
    return (x:xs)

ident :: Parser String
ident = do
    x <- lower
    xs <- many alphanum
    return (x:xs)

nat :: Parser Int
nat = do
    xs <- some digit
    return (read xs)

int :: Parser Int
int = do
    char '-'
    n <- nat
    return (-n)
  <|> nat

space :: Parser ()
space = do
    many (sat isSpace)
    return ()

token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)
