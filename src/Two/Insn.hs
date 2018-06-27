module Two.Insn
    ( Insn(..)
    , Op(..)
    , BaseReg(..)
    , MemStruct(..)
    , Cond(..)
    , CondOp(..)
    , CondExp(..)
    , CmpOp(..)
    , replaceOps
    , toList
    ) where

import Data.List (intercalate)
import Data.Word
import Data.Int
import Data.Maybe (maybeToList)

import Common

data CondOp =
      Plus -- add, inc (maybe)
    | Minus -- sub, cmp, dec (maybe)
    | And' -- and, test
    | Or' -- or
    | Xor' -- xor
    -- | Neg dont want to do this now
    -- | Shift{R/L, U/A} dont want to do this now
    deriving (Eq)

type CondExp = (Op, CondOp, Op)

-- l {+,-,&,|} r {>,<,=...} 0
data Cond = Cond CondExp CmpOp deriving (Eq)

data MemStruct = MemStruct
    { base :: Maybe BaseReg
    , index :: Maybe (BaseReg, Int32) -- reg * scale
    , disp :: Int64
    }
    deriving (Eq)

data BaseReg = 
      BaseReg Id
    | Esp0
    deriving (Eq)

data Op =
      Reg BaseReg RegMode
    | Imm Word64
    | Mem MemStruct
    deriving (Eq)

data Insn = 
      Add Op Op -- d += s
    | And Op Op -- d &= s
    | Call Op -- s()
    | Cast Op Op Width -- d = (w) s ... aka CBW/CWDE/CDQE
    | Conv Op Op Op Width -- d:d' = (w) s ... aka CWD/CDQ/CQO
    | SDiv Op Op Op -- q' = q:q' / s
    | UDiv Op Op Op -- q' = q:q' / s 
    | Rem Op Op Op -- q = q:q' % s
    | SMul Op Op -- l = l * r
    | UMul Op Op Op -- d:d' = d' * s
    | Goto Op -- goto s
    | GotoIf Cond Op -- if (cond) goto s
    | AddrOf Op MemStruct -- d = &s
    | Mov Op Op -- d = s
    | Neg Op Op -- d = -s
    | Not Op Op -- d = ~s
    | Or Op Op -- d |= s
    | AShiftL Op Op -- d <<= s
    | AShiftR Op Op -- d >>= s
    | ShiftL Op Op -- d <<= s
    | ShiftR Op Op -- d >>= s
    | Sub Op Op -- d -= s
    | Xor Op Op -- d ^= s
    | Ret
    -- TODO:
    -- Switch Op Addr (Map opval Addr)
    -- NOTES:
    -- Dec: elab to op -= 1
    -- Inc: elab to op += 1
    -- Pop: elab to op = *esp; esp += sizeof(op)
    -- Push: elab to esp -= sizeof(op); *esp = op
    -- Cmp: elab away
    -- Nop: elab away
    -- Test: elab away
    deriving (Eq, Show)

data Function = Function Id [Insn] deriving (Show, Eq)

-- assumes that f (Mem x) = Mem y
replaceOps :: (Op -> Op) -> Insn -> Insn
replaceOps f insn = case insn of
    Add d s -> Add (f d) (f s)
    And d s -> And (f d) (f s)
    Call t -> Call (f t)
    Cast d s w -> Cast (f d) (f s) w
    Conv d d' s w -> Conv (f d) (f d') (f s) w
    SDiv q q' s -> SDiv (f q) (f q') (f s)
    UDiv q q' s -> UDiv (f q) (f q') (f s)
    Rem q q' s -> Rem (f q) (f q') (f s)
    SMul d s -> SMul (f d) (f s)
    UMul d d' s -> UMul (f d) (f d') (f s)
    Goto t -> Goto (f t)
    GotoIf (Cond (l, op, r) cmpop) t -> GotoIf (Cond (f l, op, f r) cmpop) (f t)
    AddrOf d s -> case f (Mem s) of
        Mem s' -> AddrOf (f d) s'
        _ -> error "f should return a Mem for s of AddrOf"
    Mov d s -> Mov (f d) (f s)
    Neg d s -> Neg (f d) (f s)
    Not d s -> Not (f d) (f s)
    Or d s -> Or (f d) (f s)
    AShiftL d s -> AShiftL (f d) (f s)
    AShiftR d s -> AShiftR (f d) (f s)
    ShiftL d s -> ShiftL (f d) (f s)
    ShiftR d s -> ShiftR (f d) (f s)
    Sub d s -> Sub (f d) (f s)
    Xor d s -> Xor (f d) (f s)
    Ret -> Ret
    
    
toList (Add d s) = [d, s]
toList (And d s) = [d, s]
toList (Call f) = [f]
toList (Cast d s w) = [d, s]
toList (Conv d d' s w) = [d, d', s]
toList (SDiv q q' s) = [q, q', s]
toList (UDiv q q' s) = [q, q', s]
toList (Rem q q' s) = [q, q', s]
toList (SMul d s) = [d, s]
toList (UMul d d' s) = [d, d', s]
toList (Goto t) = [t]
toList (GotoIf (Cond (l, _, r) _) _) = [l, r]
toList (AddrOf d m) = [d, Mem m]
toList (Mov d s) = [d, s]
toList (Neg d s) = [d, s]
toList (Not d s) = [d, s]
toList (Or d s) = [d, s]
toList (AShiftL d s) = [d, s]
toList (AShiftR d s) = [d, s]
toList (ShiftL d s) = [d, s]
toList (ShiftR d s) = [d, s]
toList (Sub d s) = [d, s]
toList (Xor d s) = [d, s]
toList (Ret) = []

instance Show CondOp where
    show Plus = "+"
    show Minus = "-"
    show And' = "&"
    show Or' = "|"
    show Xor' = "^"

join = intercalate

instance Show Cond where
    show (Cond (l, op, r) cmpop) = (join " " toks) ++ " 0"
      where
        toks = [show l, show op, show r, show cmpop]

instance Show MemStruct where
    show MemStruct {base = b, index = i, disp = d} =
        case i of
            Just (idx, scale) -> addr ++ "[" ++ show idx ++ "*" ++ show scale ++ "]"
            Nothing -> "*" ++ addr
      where
        addr = "(" ++ join " + " (maybeToList (fmap show b) ++ [show d]) ++ ")"

instance Show BaseReg where
    show (BaseReg i) = "R" ++ show i
    show Esp0 = "esp0"

instance Show Op where
    show (Reg baseReg mode) = show baseReg ++ "." ++ show mode
    show (Imm n) = show n
    show (Mem m) = show m
