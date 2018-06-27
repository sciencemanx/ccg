module Three.Insn
    ( Insn(..)
    , Op(..)
    , Variable(..)
    , MemStruct(..)
    , CondExp(..)
    , toList
    ) where

import Data.Word
import Data.Int
import Data.List (intercalate)
import Data.Maybe (maybeToList)

import Common

data MemStruct = MemStruct
    { base :: Maybe Variable
    , index :: Maybe (Variable, Int32) -- temp * scale
    , disp :: Int64
    }
    deriving (Eq)

data Variable =
      Temp Id
    | Stack Int64
    deriving (Eq, Ord)

data Op =
      Var Variable Width
    | Imm Word64
    | Mem MemStruct
    deriving (Eq)

type CondExp = (Op, CmpOp, Op)

data Insn =
      Add Op Op Op -- d = s + s'
    | And Op Op Op -- d = s & s'
    | Call Symbol [Op] -- s(args)
    | Cast Op Op Width -- d = (w) s ... aka CBW/CWDE/CDQE/CWD/CDQ/CQO sign extend
    | SDiv Op Op Op -- d = s / s'
    | UDiv Op Op Op -- d = s / s'
    | Rem Op Op Op -- d = s % s'
    | SMul Op Op Op -- d = s * s'
    | UMul Op Op Op -- d = s * s'
    | Goto Op -- goto s
    | GotoIf CondExp Op -- if (cond) goto s
    | AddrOf Op MemStruct -- d = &s
    | Mov Op Op -- d = s
    | Neg Op Op -- d = -s
    | Not Op Op -- d = ~s
    | Or Op Op Op -- d = s | s'
    | AShiftL Op Op Op -- d = s << s'
    | AShiftR Op Op Op -- d = s >> s'
    | ShiftL Op Op Op -- d = s << s'
    | ShiftR Op Op Op -- d = s >> s'
    | Sub Op Op Op -- d = s - s'
    | Xor Op Op Op -- d = s ^ s'
    | Ret
    deriving (Eq)

data Function = Function Id [Op] [Insn] deriving (Show, Eq)

join = intercalate

toList :: Insn -> [Op]
toList (Add d s s') = [d, s, s']
toList (And d s s') = [d, s, s']
toList (Call f args) = args
toList (Cast d s w) = [d, s]
toList (SDiv d s s') = [d, s, s']
toList (UDiv d s s') = [d, s, s']
toList (Rem d s s') = [d, s, s']
toList (SMul d s s') = [d, s, s']
toList (UMul d s s') = [d, s, s']
toList (Goto t) = [t]
toList (GotoIf (l, c, r) t) = [l, r, t]
toList (AddrOf d s) = [d, Mem s]
toList (Mov d s) = [d, s]
toList (Neg d s) = [d, s]
toList (Not d s) = [d, s]
toList (Or d s s') = [d, s, s']
toList (AShiftL d s s') = [d, s, s']
toList (AShiftR d s s') = [d, s, s']
toList (ShiftL d s s') = [d, s, s']
toList (ShiftR d s s') = [d, s, s']
toList (Sub d s s') = [d, s, s']
toList (Xor d s s') = [d, s, s']
toList (Ret) = []

instance Show Insn where
    show (Add d s s') = show d ++ " = " ++ show s ++ " + " ++ show s'
    show (And d s s') = show d ++ " = " ++ show s ++ " & " ++ show s'
    show (Call f args) = show f ++ "()"
    show (Cast d s w) = show d ++ " = (" ++ show w ++ ") " ++ show s 
    show (SDiv d s s') = show d ++ " = " ++ show s ++ " /s " ++ show s'
    show (UDiv d s s') = show d ++ " = " ++ show s ++ " /u " ++ show s'
    show (Rem d s s') = show d ++ " = " ++ show s ++ " % " ++ show s'
    show (SMul d s s') = show d ++ " = " ++ show s ++ " *s " ++ show s'
    show (UMul d s s') = show d ++ " = " ++ show s ++ " *u " ++ show s'
    show (Goto s) = "goto " ++ show s
    show (GotoIf (l, op, r) s) = "if (" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ") goto " ++ show s
    show (AddrOf d s) = show d ++ " = &" ++ show s 
    show (Mov d s) = show d ++ " = " ++ show s
    show (Neg d s) = show d ++ " = -" ++ show s
    show (Not d s) = show d ++ " = ~" ++ show s
    show (Or d s s') = show d ++ " = " ++ show s ++ " | " ++ show s'
    show (AShiftL d s s') = show d ++ " = " ++ show s ++ " <<a " ++ show s'
    show (AShiftR d s s') = show d ++ " = " ++ show s ++ " >>a " ++ show s'
    show (ShiftL d s s') = show d ++ " = " ++ show s ++ " << " ++ show s'
    show (ShiftR d s s') = show d ++ " = " ++ show s ++ " >> " ++ show s'
    show (Sub d s s') = show d ++ " = " ++ show s ++ " - " ++ show s'
    show (Xor d s s') = show d ++ " = " ++ show s ++ " ^ " ++ show s'
    show Ret = "return"

instance Show MemStruct where
    show MemStruct {base = b, index = i, disp = d} =
        case i of
            Just (idx, scale) -> addr ++ "[" ++ show idx ++ "*" ++ show scale ++ "]"
            Nothing -> "*" ++ addr
      where
        addr = "(" ++ join " + " (maybeToList (fmap show b) ++ [show d]) ++ ")"

instance Show Op where
    show (Var var w) = "(" ++ show w ++ ") " ++ show var
    show (Imm n) = show n
    show (Mem m) = show m
 
instance Show Variable where
    show (Temp id) = "t" ++ show id
    show (Stack loc) = "s" ++ show loc