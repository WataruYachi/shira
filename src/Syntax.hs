module Syntax where

type Ident = String

-- data Program = St Statement | Sts Statement Program deriving Show

type Program = [Statement]

type Args = [Ident]
data Function = Func Ident Args [Statement] deriving Show

data Statement = AS Assignment | Return Expr deriving Show

data Assignment = Let Ident Expr deriving Show

data Infix2
    = Add Expr0 Expr
    | Sub Expr0 Expr
    deriving Show

data Infix3
    = Mul Expr1 Expr0
    | Div Expr1 Expr0
    deriving Show

data Expr
    = Expr Expr0
    | OpE Infix2
    deriving Show

data Expr0 
    = Expr0 Expr1
    | OpE0 Infix3
    deriving Show

data Expr1
    = Expr1 Expr
    | Int Int
    | Var Ident
    deriving Show
