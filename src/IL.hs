module IL where

import Syntax
import Control.Monad.State
import qualified Data.Map.Strict as M

data Opc
    = ADD  Opr Opr
    | SUB  Opr Opr
    | MUL  Opr Opr
    | DIV  Opr Opr
    | PUSH Opr
    | POP  Opr
    | LD Opr Opr
    | ST Opr Opr
    | RT Opr
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

type VarList = M.Map Ident Addr

data Env = Env {varNum :: Int, varList :: VarList, code :: Program} deriving Show

type Encoder = State Env IL


encoder p = runState encode (newEnv 0 M.empty p)

newEnv :: Int -> VarList -> Program -> Env
newEnv vn vl c = Env {varNum = vn, varList = vl, code = c} 

encode :: Encoder
encode = do
    env <- get
    if not . null $ code env then do
        let (c:cs) = code env
            varl = varList env
            varn = varNum env
        case c of
            AS (Let i e) -> do
                let newvarl = M.insert i varn varl
                    il = encExpr newvarl e
                put $ newEnv (varn+1) newvarl cs
                next <- encode
                return $ il ++ next
            Return e -> do
                let il = encExpr varl e
                put $ newEnv varn varl cs
                return $ il ++ [POP (R R1), RT (R R1)]
    else do
        return []

encExpr :: M.Map Ident Addr -> Expr -> [Opc]
encExpr varl (Expr e) = encExpr0 varl e
encExpr varl (OpE  i) = encInfix2 varl i

encInfix2 :: M.Map Ident Addr -> Infix2 -> [Opc]
encInfix2 varl (Add e0 e) =
    encExpr0 varl e0 ++ encExpr varl e ++
         [ POP (R R2)
        , POP (R R1)
        , ADD (R R1) (R R2)
        , PUSH (R R1)
        ]
encInfix2 varl (Sub e0 e) =
    encExpr0 varl e0 ++ encExpr varl e ++
        [ POP (R R2)
        , POP (R R1)
        , SUB (R R1) (R R2)
        , PUSH (R R1)
        ]

encExpr0 :: M.Map Ident Addr -> Expr0 -> [Opc]
encExpr0 varl (Expr0 e1) = encExpr1 varl e1
encExpr0 varl (OpE0 i) = encInfix3 varl i

encInfix3 :: M.Map Ident Addr -> Infix3 -> [Opc]
encInfix3 varl (Mul e1 e0) =
    encExpr1 varl e1 ++ encExpr0 varl e0 ++
        [ POP (R R2)
        , POP (R R1)
        , MUL (R R1) (R R2)
        , PUSH (R R1)
        ]
encInfix3 varl (Div e1 e0) =
    encExpr1 varl e1 ++ encExpr0 varl e0 ++
        [ POP (R R2)
        , POP (R R1)
        , DIV (R R1) (R R2)
        , PUSH (R R1)
        ]

encExpr1 :: M.Map Ident Addr -> Expr1 -> [Opc]
encExpr1 varl (Expr1 e) = encExpr varl e
encExpr1 varl (Int i) = [PUSH (Im i)]
encExpr1 varl (Var ident) = 
    let a = M.lookup ident varl in
        case a of
            Just a -> 
                [ LD (R R1) (Addr a)
                , PUSH (R R1)
                ]