module Main where

import System.Environment (getArgs)
import System.IO
import Expr.Generater

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
main = do
    args <- getArgs
    fp <- openFile path WriteMode
    mapM_ (hPutStrLn fp) (genASM (args !! 0))
    hClose fp
