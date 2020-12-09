module Expr.SVM where

import Expr.IL
    ( Code, Op(DIV, PUSH, POP, ADD, SUB, MUL), encode, printCode )

type Stack = [Int]

type VM = (Code, Stack)

vm :: VM -> VM
vm e@([], _) = e
vm ((PUSH i):cs, s) = vm (cs, i:s)
vm (POP:cs, s) = vm (cs, tail s)
vm (c:cs, s1:s2:ss) =
    case c of
        ADD -> vm (cs, (s2+s1):ss)
        SUB -> vm (cs, (s2-s1):ss)
        MUL -> vm (cs, (s2*s1):ss)
        DIV -> vm (cs, (s2 `div` s1):ss) 

runVM :: String -> IO ()
runVM s = do
    let code = encode s
    printCode code
    let result = case code of
                    Just c -> show . snd $ vm (c, [])
                    Nothing -> "ERROR"
    putStrLn result 
