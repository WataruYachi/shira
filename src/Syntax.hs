module Syntax where

data Expr
    = Expr Term
    | Add Expr Expr
    | Sub Expr Expr
    deriving Show

data Term 
    = Term Factor
    | Mul Term Term
    | Div Term Term
    deriving Show

data Factor
    = Factor Expr
    | Int Int
    deriving Show

