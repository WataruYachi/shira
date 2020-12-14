module Expr.SVM2 where

import Expr.IL2 as IL

type Stack =  [Int]

data AReg
    = AReg
    { r1 :: Int
    , r2 :: Int
    } deriving Show

data VM
    = VM
    { stack :: Stack
    , code :: Code
    , reg :: AReg
    } deriving Show

initVM c = VM { stack = [], code = c, reg = AReg {r1 = 0, r2 = 0} }

vm :: VM -> VM
vm v = let cc = code v in
    if null cc then v else let (c:cs) = cc in
    case c of
        (PUSH o) ->
            case o of
                Im i -> vm $ newVM (i:oldStack) cs oldReg
                R R1 -> vm $ newVM (r1 oldReg:oldStack) cs oldReg
                R R2 -> vm $ newVM (r2 oldReg:oldStack) cs oldReg
        (POP o) ->
            let (s:ss) = oldStack in
                case o of
                    R r -> vm $ newVM ss cs $ movReg oldReg r s
        ADD (R l) (R r) ->
            let lInt = takeReg oldReg l
                rInt = takeReg oldReg r in
                    vm $ newVM oldStack cs $ movReg oldReg l (lInt + rInt)
        SUB (R l) (R r) ->
            let lInt = takeReg oldReg l
                rInt = takeReg oldReg r in
                    vm $ newVM oldStack cs $ movReg oldReg l (lInt - rInt)
        MUL (R l) (R r) ->
            let lInt = takeReg oldReg l
                rInt = takeReg oldReg r in
                    vm $ newVM oldStack cs $ movReg oldReg l (lInt * rInt)
        DIV (R l) (R r) ->
            let lInt = takeReg oldReg l
                rInt = takeReg oldReg r in
                    vm $ newVM oldStack cs $ movReg oldReg l (lInt `div` rInt)

    where
        oldStack = stack v
        oldReg = reg v


newVM :: Stack -> Code -> AReg -> VM
newVM s c r = VM {stack = s, code = c, reg = r}

movReg :: AReg -> Reg -> Int -> AReg
movReg reg IL.R1 x = AReg { r1 = x, r2 = r2 reg }
movReg reg IL.R2 x = AReg { r1 = r1 reg, r2 = x }

takeReg :: AReg -> Reg -> Int
takeReg reg IL.R1 = r1 reg
takeReg reg IL.R2 = r2 reg

runVM :: String -> IO ()
runVM s = do
    let code = encode s
    printCode code
    let vmState = case code of
            Just c -> vm $ initVM c
            Nothing -> initVM []
    print vmState 
    let result = stack vmState
    putStr $  s ++ "="
    print $ head result