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

prologue =
    [ "push rbp"
    , "mov rbp, rsp"
    , ""]

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
        LD r a -> case r of
                    R R1 -> ""

generateAsm = undefined

readCodeFromFile :: IO String
readCodeFromFile = do
    args <- getArgs
    if null args then return ""
    else do
        fp <- openFile (head args) ReadMode
        code <- TIO.hGetContents fp
        hClose fp
        return $ T.unpack code

asmGen :: IO ()
asmGen = do 
    code <- readCodeFromFile
    print $ makeAST code
