module Parser where

import Control.Applicative
import Control.Monad

newtype Parser a = P {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
    fmap f p =
        P
            ( \input ->
                case runParser p input of
                    Just (a, s) -> Just (f a, s)
                    Nothing -> Nothing
            )

instance Applicative Parser where
    pure a = P (\_ -> Just (a, []))
    fp <*> gp =
        P
            ( \input -> case runParser fp input of
                Just (a, s) -> runParser (fmap a gp) s
                Nothing -> Nothing
            )

instance Monad Parser where
    p >>= f =
        P
            ( \input -> case runParser p input of
                Just (a, s) -> runParser (f a) s
                Nothing -> Nothing
            )