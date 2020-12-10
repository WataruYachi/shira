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
vm v =
    case code v of
        (PUSH o :cs) ->
            case o of
                Im i -> VM {stack = i:os, code = cs, reg = or}
                R R1 -> VM { stack = r1 or:os, code = cs, reg = or}
                R R2 -> VM { stack = r2 or:os, code = cs, reg = or}
    where
        os = stack v
        or = reg v


movReg :: AReg -> Reg -> Int -> AReg
movReg reg IL.R1 x = AReg { r1 = x, r2 = r2 reg }
movReg reg IL.R2 x = AReg { r1 = r1 reg, r2 = x }
        