module VM where

import Control.Monad.State
import qualified Data.Map.Strict as M
import IL
import Parser

type Stack = [Int]

data VReg = VReg
    { r1 :: Int
    , r2 :: Int
    }
    deriving (Show)

data VEnv = VEnv
    { stack :: Stack
    , program :: IL
    , reg :: VReg
    , varList :: VarList
    }
    deriving (Show)

type VM = State VEnv Int

vm :: VM
vm = do
    e <- get
    let s = stack e
        p : ps = program e
        rs = reg e
        vs = varList e
    case p of
        (PUSH o) ->
            case o of
                Im i -> next $ newVM (i : s) ps rs vs
                R R1 -> next $ newVM (r1 rs : s) ps rs vs
                R R2 -> next $ newVM (r2 rs : s) ps rs vs
        (POP o) ->
            let (s' : ss) = s
             in case o of
                    R r -> next $ newVM ss ps (movReg rs r s') vs
        ADD (R r1) (R r2) ->
            let l = referVReg rs r1
                r = referVReg rs r2
                new = newVM s ps (movReg rs r1 (l + r)) vs
             in next new
        SUB (R r1) (R r2) ->
            let l = referVReg rs r1
                r = referVReg rs r2
                new = newVM s ps (movReg rs r1 (l - r)) vs
             in next new
        MUL (R r1) (R r2) ->
            let l = referVReg rs r1
                r = referVReg rs r2
                new = newVM s ps (movReg rs r1 (l * r)) vs
             in next new
        DIV (R r1) (R r2) ->
            let l = referVReg rs r1
                r = referVReg rs r2
                new = newVM s ps (movReg rs r1 (l + r)) vs
             in next new
        LD (R r) (Addr a) ->
            let a' = (length s - a) - 1
             in next $ newVM s ps (movReg rs r (s !! a')) vs
        ST a b -> undefined
        RT (R r) -> return $ referVReg rs r
  where
    next e = put e >> vm

newVM :: Stack -> IL -> VReg -> VarList -> VEnv
newVM s l r v = VEnv{stack = s, program = l, reg = r, varList = v}

movReg :: VReg -> IL.Reg -> Int -> VReg
movReg regs IL.R1 x = VReg{r1 = x, r2 = r2 regs}
movReg regs IL.R2 x = VReg{r1 = r1 regs, r2 = x}

referVReg :: VReg -> IL.Reg -> Int
referVReg reg IL.R1 = r1 reg
referVReg reg IL.R2 = r2 reg

runVM s = runState vm $ newVM [] (encoder p) VReg{r1 = 0, r2 = 0} M.empty
  where
    p = case makeAST s of
        Just (c, s) -> c
        Nothing -> []