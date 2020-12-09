module Expr.IL2 where

import Expr.Syntax ( Factor(..), Term(..), Expr(..) )
import Expr.EParser ( makeExprAST )

data Opc
    = ADD Opr Opr
    | SUB Opr Opr
    | MUL Opr Opr
    | DIV Opr Opr
    | PUSH Opr
    | POP Opr
    deriving Show

data Opr
    = Im Int | R Reg
    deriving Show

data Reg
    = R1
    | R2
    deriving Show

type Code = [Opc]

encode :: String -> Maybe Code
encode s = encExp <$> makeExprAST s

encExp :: Expr -> Code
encExp (Expr t) = encTrm t
encExp (Add e1 e2) =
    encExp e1 ++ encExp e2 ++
        [ POP (R R1)
        , POP (R R2)
        , ADD (R R2) (R R1)
        , PUSH (R R2)
        ]
encExp (Sub e1 e2) =
    encExp e1 ++ encExp e2 ++
    [ POP (R R1)
    , POP (R R2)
    , SUB (R R2) (R R1)
    , PUSH (R R2)
    ]

encTrm :: Term -> Code
encTrm (Term f) = encFac f
encTrm (Mul t1 t2) =
    encTrm t1 ++ encTrm t2 ++
    [ POP (R R1)
    , POP (R R2)
    , MUL (R R2) (R R1)
    , PUSH (R R2)
    ]

encTrm (Div t1 t2) =
    encTrm t1 ++ encTrm t2 ++
    [ POP (R R1)
    , POP (R R2)
    , DIV (R R2) (R R1)
    , PUSH (R R2)
    ]

encFac :: Factor -> Code
encFac (Factor e) = encExp e
encFac (Int i) = [PUSH (Im i)]

printCode :: Maybe Code -> IO ()
printCode Nothing = putStrLn "Error"
printCode (Just c) = putStr $ codeList 1 c

codeList :: Int -> Code -> String
codeList _ [] = ""
codeList i (x:xs) =
    show i ++ ": " ++ show x ++ "\n" ++ codeList (i + 1) xs