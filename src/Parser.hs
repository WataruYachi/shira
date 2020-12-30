module Parser where

import Control.Applicative
import PrimParser
import Syntax

infix2 :: Expr0 -> Parser Infix2
infix2 l = do
    symbol "+" *> (Add l <$> expr)
        <|> symbol "-" *> (Sub l <$> expr)

expr :: Parser Expr
expr = do
    l <- expr0
    OpE <$> infix2 l <|> return (Expr l)

expr'' :: Parser (Expr -> Expr)
expr'' =
    do
        symbol "+"
        l <- expr0
        return $ OpE . Add l
        <|> do
            symbol "-"
            l <- expr0
            return $ OpE . Sub l

expr' :: Parser Expr
expr' = do
    l <- expr0'
    rs <- many expr''
    let a = foldl (\l r -> r l) (Expr l) rs
    return a

expr0'' :: Parser (Expr0 -> Expr0)
expr0'' =
    do
        symbol "*"
        l <- expr1
        return $ OpE0 . Mul l
        <|> do
            symbol "/"
            l <- expr1
            return $ OpE0 . Div l

expr0' :: Parser Expr0
expr0' = do
    l <- expr1
    rs <- many expr0''
    let a = foldl (\l r -> r l) (Expr0 l) rs
    return a

infix3 :: Expr1 -> Parser Infix3
infix3 l = do
    symbol "*" *> (Mul l <$> expr0)
        <|> symbol "/" *> (Div l <$> expr0)

expr0 :: Parser Expr0
expr0 = do
    l <- expr1
    OpE0 <$> infix3 l <|> return (Expr0 l)

expr1 :: Parser Expr1
expr1 =
    do
        symbol "(" *> (Expr1 <$> expr <* symbol ")")
        <|> (Int <$> integer)
        <|> (Var <$> identifier)

parseLet :: Parser Statement
parseLet = do
    symbol "let"
    i <- identifier
    symbol "="
    AS . Let i <$> expr'

parsePrint :: Parser Statement
parsePrint = symbol "print" *> (Print <$> expr')

parseStatement :: Parser Statement
parseStatement =
    parseLet
        <|> parsePrint

parseProgram :: Parser [Statement]
parseProgram = many (parseStatement <* symbol ";")

makeAST = runParser parseProgram