module Three.Insn
    ( Insn(..)
    , Op(..)
    ) where

import Data.Word
import Data.Int
import Data.List (intercalate)
import Data.Maybe (maybeToList)

import Common

type Id = Int

data MemStruct = MemStruct
    { base :: Maybe Id
    , index :: Maybe (Id, Int32) -- temp * scale
    , disp :: Int64
    }
    deriving (Eq)

data Op =
      Temp Id Width
    | Imm Word64
    | Mem MemStruct
    deriving (Eq)

type CondExp = (Op, CmpOp, Op)

data Insn = 
      Add Op Op Op -- d = s + s'
    | And Op Op Op -- d = s & s'
    | Call Op -- s()
    | Cast Op Op Width -- d = (w) s ... aka CBW/CWDE/CDQE/CWD/CDQ/CQO sign extend
    | Conv Op Op Width -- d = (w) s ... aka  
    | SDiv Op Op Op -- d = s / s'
    | UDiv Op Op Op -- d = s / s'
    | Rem Op Op Op -- d = s % s'
    | SMul Op Op Op -- d = s * s'
    | UMul Op Op Op -- d = s * s'
    | Goto Op -- goto s
    | GotoIf CondExp Op -- if (cond) goto s
    | AddrOf Op MemStruct -- d = &s
    | Mov Op Op -- d = s
    | Phi Op Op Op
    | Neg Op Op -- d = -s
    | Not Op Op -- d = ~s
    | Or Op Op Op -- d = s | s'
    | AShiftL Op Op Op -- d = s << s'
    | AShiftR Op Op Op -- d = s >> s'
    | ShiftL Op Op Op -- d = s << s'
    | ShiftR Op Op Op -- d = s >> s'
    | Sub Op Op Op -- d = s - s'
    | Xor Op Op -- d = s ^ s'
    | Ret
    deriving (Eq, Show)

join = intercalate

instance Show MemStruct where
    show MemStruct {base = b, index = i, disp = d} =
        case i of
            Just (idx, scale) -> addr ++ "[" ++ show idx ++ "*" ++ show scale ++ "]"
            Nothing -> "*" ++ addr
      where
        addr = "(" ++ join " + " (maybeToList (fmap show b) ++ [show d]) ++ ")"

instance Show Op where
    show (Temp id width) = "(" ++ show width ++ ") t" ++ show id
    show (Imm n) = show n
    show (Mem m) = show m
