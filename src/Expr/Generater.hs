module Expr.Generater where

import Expr.IL2

gen :: Code -> [String]
gen [] = []
gen (c:cs) =
    case c of
        PUSH o ->
            case o of
                Im i -> ("push " ++ show i) : gen cs
                R R1 -> "push rax" : gen cs
                R R2 -> "push rcx" : gen cs
        
        POP o ->
            case o of
                R R1 -> "pop rax" : gen cs
                R R2 -> "pop rcx" : gen cs
        ADD _ _ -> "add rax, rcx" : gen cs
        SUB _ _ -> "sub rax, rcx" : gen cs
        MUL _ _ -> "imul rax, rcx" : gen cs
        DIV _ _ -> "cqo" : "idiv rcx" : gen cs

header = ["global _start"
    , "section .text"
    , "_start:"
    ]

printRdi = ["pop rdi"
    , "mov rax, 60"
    , "syscall"
    ]

genASM :: String -> [String]
genASM s = 
    case encode s of
        Nothing -> []
        Just cc -> header ++ gen cc ++ printRdi
