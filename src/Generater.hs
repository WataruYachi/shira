module Generater where

import IL
import Parser

import System.Environment
import System.IO

import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Control.Monad.State

{-
type OffsetFromSP = Int
data Compiler
    = Compiler
    { ast :: Program
    , localVarsList :: [(Ident, OffsetFromSP)]
    , numberOflocalVars :: Int
    , generated :: [String]
    }

-- 構文木を読み進めながら、各ステートメントに対応したアセンブラを生成する。
complier :: State Compiler Compiler
complier = undefined 
-}

prologue :: Int -> [[Char]]
prologue i =
    [ "push rbp"
    , "mov rbp, rsp"
    , "sub rsp, " ++ show (i*8)]

gen :: IL -> [String]
gen [] = []
gen (c:cs) =
    case c of
        PUSH o ->
            let opr = case o of
                        Im i -> show i
                        R R1 -> "rax"
                        R R2 -> "rcx"
                in ("push " ++ opr) : gen cs
        POP o ->
            let opr = case o of
                        R R1 -> "rax"
                        R R2 -> "rcx"
                in ("pop " ++ opr) : gen cs
        ADD _ _ -> "add rax, rcx" : gen cs
        SUB _ _ -> "sub rax, rcx" : gen cs
        MUL _ _ -> "imul rax, rcx" : gen cs
        DIV _ _ -> "cqo" : "idiv rcx" : gen cs
        LD (R r) (Addr a)
            -> case r of
                R1 -> ("mov rax, [rbp - " ++ (show $ a*offset) ++ "]") : gen cs
                R2 -> ("mov rcx, [rbp - " ++ (show $ a*offset) ++ "]") : gen cs
        ST (Addr a) (R r) -> ("mov [rbp - " ++ (show $ a*offset) ++ "], rax") : gen cs
        RT _ -> ["mov rsp, rbp", "pop rbp", "ret"] ++ gen cs

-- 2021/01/27
-- 8にしないと動かない　どうして？
-- WORDのサイズ？　それともWORDx8 = 64bitか
-- メモリのアドレスについて勉強するべき
offset = 8

generateAsm s = 
    case makeAST s of
        Just (p,s) -> let (il, e) = encoder p
                        in header ++ prologue (varNum e) ++ gen il
        Nothing -> []

header =
    [ "global _start"
    , "section .text"
    , "_start:"
    ]

readCodeFromFile :: FilePath -> IO String
readCodeFromFile f = do
    fp <- openFile f ReadMode
    code <- TIO.hGetContents fp
    hClose fp
    return $ T.unpack code

writeAsm :: FilePath -> [String] -> IO ()
writeAsm f a = do
    fp <- openFile f WriteMode
    mapM_ (TIO.hPutStrLn fp <$> T.pack) a
    hClose fp
    return ()

asmGen :: IO ()
asmGen = do
    args <- getArgs
    if length args < 2 then return ()
    else do
        let inputFile = head args
            outputFile = args !! 1
        code <- readCodeFromFile inputFile
        writeAsm outputFile $ generateAsm code
