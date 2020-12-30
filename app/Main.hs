module Main where

import System.Environment (getArgs)
import System.IO
import Expr.Generater

import Generater

path :: [Char]
path = "./tmp.asm"

genAsm :: String -> [String]
genAsm x =
    [ "global _start"
    , "section .text"
    , "_start:"
    , "mov rdi, " ++ x
    , "mov rax, 60"
    , "syscall"
    ]

main :: IO ()
main = asmGen
