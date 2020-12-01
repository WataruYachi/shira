module Syntax where

data Expr
    = Expr Term
    | Add Term Expr
    | Sub Term Expr
    deriving Show

data Term 
    = Term Factor
    | Mul Factor Term
    | Div Factor Term
    deriving Show

data Factor
    = Factor Expr
    | Int Int
    deriving Show

