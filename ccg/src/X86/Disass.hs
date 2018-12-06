module X86.Disass 
    ( disassInstr
    ) where

import Data.Word (Word8, Word64)
import System.IO.Unsafe (unsafePerformIO)
import Data.Elf (Elf)

import Hapstone.Capstone
import Hapstone.Internal.Capstone as Capstone

import X86.Insn (Insn, fromCsInsn)
import ReadElf (readAddr)

mkDisasm :: [Word8] -> Word64 -> Disassembler ()
mkDisasm buf addr = Disassembler { 
    arch = Capstone.CsArchX86
    , modes = [Capstone.CsMode64]
    , buffer = buf -- [Word8]
    , addr = addr -- Word64
    , num = 2 -- number of instructions
    , Hapstone.Capstone.detail = True
    , skip = Nothing
    , action = defaultAction
    }

max_x86_insn_len = 15

disassInstr :: Elf -> Word64 -> Maybe Insn
disassInstr elf addr =
    let buf = readAddr elf addr 15 in
    case unsafePerformIO $ disasmSimpleIO $ mkDisasm buf addr of
        Right (insn:_) -> Just $ fromCsInsn $ insn
        _ -> Nothing

