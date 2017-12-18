module X86.State
    ( empty
    ) where

import Data.Word
import Data.Bits ((.&.), shiftR)
import qualified Data.Map as Map

import X86.Insn

data Value =
      Top
    | Const Word64 -- imm value of thing
    | Stack Word64 -- stack pointer with offset
    | Bottom

type RegMap = Map.Map BaseReg Value

data State = State
    { regs :: RegMap
    }

maskReg :: RegMode -> Word64 -> Word64
maskReg L imm = imm .&. 0xff
maskReg H imm = (imm .&. 0xff00) `shiftR` 8
maskReg X imm = imm .&. 0xffff
maskReg E imm = imm .&. 0xffffffff
maskReg R imm = imm

readReg regs (baseReg, regMode) =
    case (rawVal, regMode) of
        (Top, _) -> Top
        (Const imm, _) -> Const $ maskReg regMode imm
        (Stack _, R) -> rawVal
        (Stack _, _) -> Top
        (Bottom, _) -> Bottom
  where
    rawVal = Map.findWithDefault Bottom baseReg regs

empty = State { regs = Map.empty }