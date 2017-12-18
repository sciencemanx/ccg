module Two.Insn
    ( Insn(..)
    , Op(..)
    , BaseReg(..)
    , MemStruct(..)
    , Cond(..)
    , CondOp(..)
    , CondExp(..)
    , CmpOp(..)
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

data CmpOp =
      Eq
    | Ne
-- signed
    | Gt
    | Ge
    | Lt
    | Le
-- unsigned
    | A
    | Ae
    | B
    | Be
    deriving (Eq)

type CondExp = (Op, CondOp, Op)

-- l {+,-,&,|} r {>,<,=...} 0
data Cond = Cond CondExp CmpOp deriving (Eq)

data MemStruct = MemStruct
    { base :: Maybe BaseReg
    , index :: Maybe (BaseReg, Int32) -- reg * scale
    , disp :: Int64 }
    deriving (Eq)

data BaseReg = BaseReg Int deriving (Eq)

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
    | SDiv Op Op Op -- q' = q:q' / s; q = q:q' % s
    | UDiv Op Op Op -- q' = q:q' / s; q = q:q' % s
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
    -- Switch Op (Map opval Addr)
    -- Mod
    -- Rem
    -- NOTES:
    -- Dec: elab to op -= 1
    -- Inc: elab to op += 1
    -- Pop: elab to op = *esp; esp += sizeof(op)
    -- Push: elab to esp -= sizeof(op); *esp = op
    -- Cmp: elab away
    -- Nop: elab away
    -- Test: elab away
    deriving (Eq, Show)

instance Show CondOp where
    show Plus = "+"
    show Minus = "-"
    show And' = "&"
    show Or' = "|"
    show Xor' = "^"

instance Show CmpOp where
    show Eq = "=="
    show Ne = "!="
    show Gt = ">s"
    show Ge = ">=s"
    show Lt = "<s"
    show Le = "<=s"
    show A = ">"
    show Ae = ">="
    show B = "<"
    show Be = "<="

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

instance Show Op where
    show (Reg baseReg mode) = show baseReg ++ "." ++ show mode
    show (Imm n) = show n
    show (Mem m) = show m
