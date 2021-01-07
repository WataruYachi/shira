module IL where

import Syntax

data Opc
    = ADD  Opr Opr
    | SUB  Opr Opr
    | MUL  Opr Opr
    | DIV  Opr Opr
    | PUSH Opr
    | POP  Opr
    deriving Show

data Opr
    = Im Int
    | Addr Addr
    | R Reg
    deriving Show

data Reg
    = R1
    | R2
    deriving Show

type Addr = Int
type IL = [Opc]

data Env = Env {listOfVars :: [(Ident, Addr)]}

encode :: Program -> IL
encode = undefined