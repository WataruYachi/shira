module Generater where

import Syntax

import Parser

import System.Environment
import System.IO

import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Control.Monad.State

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
