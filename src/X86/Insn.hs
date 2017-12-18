module X86.Insn 
    ( Insn(..)
    , Op(..)
    , OpValue(..)
    , MemStruct(..)
    , RegMode(..)
    , BaseReg(..)
    , fromCsInsn
    ) where

import Data.Word (Word8, Word32, Word64)
import Data.Int
import Data.Maybe (fromJust)

import qualified Hapstone.Internal.Capstone as Capstone
import qualified Hapstone.Internal.X86 as X86

import Common

data BaseReg =
      Inv
    | Rsp
    | Rbp
    | Rax
    | Rbx
    | Rcx
    | Rdx
    | Rdi
    | Rsi
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
    | Rip deriving (Eq, Show)

data Instr =
      Add
    | And
    | Call
    | Cbw
    | Cwd
    | Cmp
    | Dec
    | Div
    | Idiv
    | Imul
    | Inc
    | Jmp
    | Jcc
    | Lea
    | Mov
    | Mul
    | Neg
    | Nop
    | Not
    | Or
    | Pop
    | Push
    | Sal
    | Sar
    | Shl
    | Shr
    | Sub
    | Test
    | Xor deriving (Eq, Show)

data MemStruct = MemStruct
    { base :: BaseReg
    , index :: BaseReg
    , scale :: Int32
    , disp :: Int64
    } deriving (Eq, Show)

data OpValue =
      Reg BaseReg RegMode
    | Imm Word64
    | Mem MemStruct deriving (Eq, Show)

data Op = Op
    { value :: OpValue
    , width :: Word8
    } deriving (Eq, Show)

data Insn = Insn
    { address :: Word64
    , instr :: X86.X86Insn
    , operands :: [Op]
    , groups :: [X86.X86InsnGroup]
    , size :: Word64
    , str :: String
    } deriving (Eq)

instance Show Insn where
    show insn = str insn

getDetail = fromJust . Capstone.detail

getCsGroups :: Capstone.CsInsn -> [Word8]
getCsGroups = Capstone.groups . getDetail

getX86 (Capstone.X86 csx86) = csx86

getCsOps :: Capstone.CsInsn -> [X86.CsX86Op]
getCsOps = X86.operands . getX86 . fromJust . Capstone.archInfo . getDetail

fromCsReg X86.X86RegInvalid = (Inv, R)
fromCsReg X86.X86RegIp      = (Rsp, X)
fromCsReg X86.X86RegEip     = (Rsp, E)
fromCsReg X86.X86RegRip     = (Rsp, R)
fromCsReg X86.X86RegSpl     = (Rsp, L)
fromCsReg X86.X86RegSp      = (Rsp, X)
fromCsReg X86.X86RegEsp     = (Rsp, E)
fromCsReg X86.X86RegRsp     = (Rsp, R)
fromCsReg X86.X86RegBpl     = (Rbp, L)
fromCsReg X86.X86RegBp      = (Rbp, X)
fromCsReg X86.X86RegEbp     = (Rbp, E)
fromCsReg X86.X86RegRbp     = (Rbp, R)
fromCsReg X86.X86RegAl      = (Rax, L)
fromCsReg X86.X86RegAh      = (Rax, H)
fromCsReg X86.X86RegAx      = (Rax, X)
fromCsReg X86.X86RegEax     = (Rax, E)
fromCsReg X86.X86RegRax     = (Rax, R)
fromCsReg X86.X86RegBl      = (Rbx, L)
fromCsReg X86.X86RegBh      = (Rbx, H)
fromCsReg X86.X86RegBx      = (Rbx, X)
fromCsReg X86.X86RegEbx     = (Rbx, E)
fromCsReg X86.X86RegRbx     = (Rbx, R)
fromCsReg X86.X86RegCl      = (Rcx, L)
fromCsReg X86.X86RegCh      = (Rcx, H)
fromCsReg X86.X86RegCx      = (Rcx, X)
fromCsReg X86.X86RegEcx     = (Rcx, E)
fromCsReg X86.X86RegRcx     = (Rcx, R)
fromCsReg X86.X86RegDl      = (Rdx, L)
fromCsReg X86.X86RegDh      = (Rdx, H)
fromCsReg X86.X86RegDx      = (Rdx, X)
fromCsReg X86.X86RegEdx     = (Rdx, E)
fromCsReg X86.X86RegRdx     = (Rdx, R)
fromCsReg X86.X86RegDil     = (Rdi, L)
fromCsReg X86.X86RegDi      = (Rdi, X)
fromCsReg X86.X86RegEdi     = (Rdi, E)
fromCsReg X86.X86RegRdi     = (Rdi, R)
fromCsReg X86.X86RegSil     = (Rsi, L)
fromCsReg X86.X86RegSi      = (Rsi, X)
fromCsReg X86.X86RegEsi     = (Rsi, E)
fromCsReg X86.X86RegRsi     = (Rsi, R)
fromCsReg X86.X86RegR8b     = (R8, L)
fromCsReg X86.X86RegR8w     = (R8, X)
fromCsReg X86.X86RegR8d     = (R8, E)
fromCsReg X86.X86RegR8      = (R8, R)
fromCsReg X86.X86RegR9b     = (R9, L)
fromCsReg X86.X86RegR9w     = (R9, X)
fromCsReg X86.X86RegR9d     = (R9, E)
fromCsReg X86.X86RegR9      = (R9, R)
fromCsReg X86.X86RegR10b    = (R10, L)
fromCsReg X86.X86RegR10w    = (R10, X)
fromCsReg X86.X86RegR10d    = (R10, E)
fromCsReg X86.X86RegR10     = (R10, R)
fromCsReg X86.X86RegR11b    = (R11, L)
fromCsReg X86.X86RegR11w    = (R11, X)
fromCsReg X86.X86RegR11d    = (R11, E)
fromCsReg X86.X86RegR11     = (R11, R)
fromCsReg X86.X86RegR12b    = (R12, L)
fromCsReg X86.X86RegR12w    = (R12, X)
fromCsReg X86.X86RegR12d    = (R12, E)
fromCsReg X86.X86RegR12     = (R12, R)
fromCsReg X86.X86RegR13b    = (R13, L)
fromCsReg X86.X86RegR13w    = (R13, X)
fromCsReg X86.X86RegR13d    = (R13, E)
fromCsReg X86.X86RegR13     = (R13, R)
fromCsReg X86.X86RegR14b    = (R14, L)
fromCsReg X86.X86RegR14w    = (R14, X)
fromCsReg X86.X86RegR14d    = (R14, E)
fromCsReg X86.X86RegR14     = (R14, R)
fromCsReg X86.X86RegR15b    = (R15, L)
fromCsReg X86.X86RegR15w    = (R15, X)
fromCsReg X86.X86RegR15d    = (R15, E)
fromCsReg X86.X86RegR15     = (R15, R)

fromCsOpMemStruct m = MemStruct 
    { base = base
    , index = index
    , scale = scale
    , disp = disp
    }
  where
    (base, _) = fromCsReg $ X86.base m
    (index, _) = fromCsReg $ X86.index m 
    scale = X86.scale m
    disp = X86.disp' m

fromCsOpValue (X86.Reg r) = (uncurry Reg) $ fromCsReg r
fromCsOpValue (X86.Imm n) = Imm n
fromCsOpValue (X86.Mem m) = Mem $ fromCsOpMemStruct m

fromCsOp :: X86.CsX86Op -> Op
fromCsOp csop = Op { value = val, width = sz  }
  where
    val = fromCsOpValue $ X86.value csop
    sz = X86.size csop

fromCsInsn csinsn = Insn { address = a, instr = i, operands = ops, groups = grps, size = sz, str = s }
  where
    a = Capstone.address csinsn
    i = toEnum $ fromIntegral $ Capstone.insnId csinsn -- convert insnId (word32) to X86Insn
    ops = map fromCsOp (getCsOps csinsn)
    grps = map (toEnum . fromIntegral) (getCsGroups csinsn)
    sz = toEnum $ length $ Capstone.bytes csinsn
    s = Capstone.mnemonic csinsn ++ " " ++ Capstone.opStr csinsn
