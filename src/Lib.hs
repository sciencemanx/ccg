module Lib
    ( disass
    ) where

import Data.Word
import Numeric (showHex)
import Data.ByteString (ByteString)

import Hapstone.Capstone
import Hapstone.Internal.Capstone as Capstone

import ReadElf (readElf)

-- use example from Capstone: http://www.capstone-engine.org/lang_python.html
intel_asm_buf = [0x90, 0x48, 0x8b, 0x05, 0xb8, 0x13, 0x00, 0x00] :: [Word8]

mkDisasm buf addr = Disassembler { 
    arch = Capstone.CsArchX86 -- CsArchArm, CsArchArm64, CsArchMips, CsArchX86, CsArchPpc, CsArchSparc, CsArchSysz, CsArchXcore
    , modes = [Capstone.CsMode64] -- CsModeLittleEndian, CsModeArm, CsMode16 (16-bit x86), CsMode32 (32-bit x86), CsMode64 (64-bit x86-64/amd64 or PPC), CsModeThumb, CsModeMclass, CsModeV8 (ARMv8 A32), CsModeMicro, CsModeMips3, CsModeMips32r6, CsModeMipsGp64, CsModeV9 (SparcV9 mode), CsModeBigEndian, CsModeMips32, CsModeMips64
    , buffer = buf -- [Word8]
    , addr = addr -- Word64
    , num = 1 -- number of instructions
    , Hapstone.Capstone.detail = True
    , skip = Nothing
    , action = defaultAction
    }

disasmInstr elf addr = 0

-- disass :: ByteString -> IO (Either CsErr [(CsInsn, ())])
disass :: String -> IO ()
disass filename = do
    elf <- readElf filename
    res <- disasmIO (mkDisasm intel_asm_buf 0x1000)
    return ()