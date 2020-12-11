module Expr.SVM2 where

import Expr.IL2 as IL

type Stack =  [Int]

data AReg
    = AReg
    { r1 :: Int
    , r2 :: Int
    }

data VM
    = VM
    { stack :: Stack
    , code :: Code
    , reg :: AReg
    }

vm :: VM -> VM
vm v = let c:cs = code v in
    case c of
        (PUSH o) ->
            case o of
                Im i -> newVM (i:os) cs or
                R R1 -> VM { stack = r1 or:os, code = cs, reg = or}
                R R2 -> VM { stack = r2 or:os, code = cs, reg = or}
        (POP r) ->
            let (s:ss) = os in
                case r of
                    R R1 -> VM {stack = ss, code = cs, reg = movReg or IL.R1 s}
    where
        os = stack v
        or = reg v


newVM s c r = VM {stack = s, code = c, reg = r} 

movReg :: AReg -> Reg -> Int -> AReg
movReg reg IL.R1 x = AReg { r1 = x, r2 = r2 reg }
movReg reg IL.R2 x = AReg { r1 = r1 reg, r2 = x }
        