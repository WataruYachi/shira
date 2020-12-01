module Generater where

import           Parser
import           Syntax

import           Control.Applicative

makeAST = runParser expr

eval s = do
    ast <- makeAST s
    return $ evalE $ fst ast

evalE :: Expr -> Int
evalE (Expr t) = evalT t
evalE (Add t e) = evalT t + evalE e
evalE (Sub t e) = evalT t - evalE e

evalT :: Term -> Int
evalT (Term f) = evalF f
evalT (Mul f t) = evalF f * evalT t
evalT (Div f t) = evalF f `div` evalT t

evalF :: Factor -> Int
evalF (Factor f) = evalE f
evalF (Int n) = n

expr :: Parser Expr
expr = do
    t <- term
    symbol "+" *> (Add t <$> expr)
        <|> symbol "-" *> (Sub t <*> expr)
        <|> return (Expr t)

term :: Parser Term
term = do
    f <- factor
    symbol "*" *> (Mul f <$> term)
        <|> symbol "/" *> (Div f <$> term)
        <|> return (Term f)

factor :: Parser Factor
factor = symbol "(" *> ( Factor <$> expr <* symbol ")")
        <|> (Int <$> natural)

generateAsm = undefined
