module Generater where

import           Parser
import           Syntax

import           Control.Applicative

eval :: String -> Maybe (Int, String)
eval = runParser (evalE <$> expr)

evalE :: Expr -> Int
evalE (Expr t) = evalT t
evalE (Add t e) = evalE t + evalE e
evalE (Sub t e) = evalE t - evalE e

evalT :: Term -> Int
evalT (Term f) = evalF f
evalT (Mul f t) = evalT f * evalT t
evalT (Div f t) = evalT f `div` evalT t

evalF :: Factor -> Int
evalF (Factor f) = evalE f
evalF (Int n) = n

expr2 :: Expr -> Parser Expr
expr2 e = do
    symbol "+"
    nt <- term
    expr2 (Add e (Expr nt))
  <|> do symbol "-"
         nt <- term
         expr2 (Sub e (Expr nt))
  <|> return e

e2 :: Parser Expr
e2 = do
    t <- term
    symbol "+" *> (Add (Expr t) . Expr <$> term)
        <|> symbol "-" *> (Sub (Expr t) . Expr <$> term)
        <|> return (Expr t)

expr :: Parser Expr
expr = do
    e <- e2
    expr2 e

term2 :: Term -> Parser Term
term2 t = do
    symbol "*"
    f <- factor
    term2 (Mul t (Term f))
  <|> do symbol "/"
         f <- factor
         term2 (Div t (Term f))
  <|> return t

term :: Parser Term
term = do
    f <- term'
    term2 f
    where 
        term' = do
            f <- factor
            symbol "*" *> (Mul (Term f) . Term <$> factor)
                <|> symbol "/" *> (Div (Term f) . Term <$> factor)
                <|> return (Term f)

factor :: Parser Factor
factor = symbol "(" *> ( Factor <$> expr <* symbol ")")
        <|> (Int <$> natural)

generateAsm = undefined
