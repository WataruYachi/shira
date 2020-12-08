module Expr.Expr where

import Expr.EParser ( makeExprAST )

import Expr.Syntax ( Factor(..), Term(..), Expr(..) )

eval :: String -> Maybe Int
eval input = evalE <$> makeExprAST input

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

