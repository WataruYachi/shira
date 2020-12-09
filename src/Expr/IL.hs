module Expr.IL (Op(..), Code, encode, printCode, codeList) where

import Expr.Syntax
import Expr.EParser

data Op
    = ADD
    | SUB
    | MUL
    | DIV
    | PUSH Int
    | POP
    deriving Show

type Code = [Op]

encode :: String -> Maybe Code
encode s = encExp <$> makeExprAST s

encExp :: Expr -> Code
encExp (Expr t) = encTrm t
encExp (Add e1 e2) = encExp e1 ++ encExp e2 ++ [ADD]
encExp (Sub e1 e2) = encExp e1 ++ encExp e2 ++ [SUB]

encTrm :: Term -> Code
encTrm (Term f) = encFac f
encTrm (Mul t1 t2) = encTrm t1 ++ encTrm t2 ++ [MUL]
encTrm (Div t1 t2) = encTrm t1 ++ encTrm t2 ++ [DIV]

encFac :: Factor -> Code
encFac (Factor e) = encExp e
encFac (Int i) = [PUSH i]

printCode :: Maybe Code -> IO ()
printCode Nothing = putStrLn "Error"
printCode (Just c) = putStr $ codeList 1 c

codeList :: Int -> Code -> String
codeList _ [] = ""
codeList i (x:xs) =
    show i ++ ": " ++ show x ++ "\n" ++ codeList (i + 1) xs