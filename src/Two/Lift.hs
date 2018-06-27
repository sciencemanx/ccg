module Two.Lift
    ( toThree
    ) where

import qualified Data.Map as Map

import Common
import Two.Subroutine
import Three.Function

import qualified Two.Cfg as Cfg2
import qualified Three.Cfg as Cfg3
import qualified Two.Insn as Insn2
import qualified Three.Insn as Insn3
import qualified Two.Signature as Sig

import ReadElf (addr2symbol)

regModeToWidth L = Byte   
regModeToWidth X = Word
regModeToWidth E = Dword
regModeToWidth R = Qword

liftMemStruct :: Insn2.MemStruct -> Insn3.Op
liftMemStruct mem2 = case (base) of
    (Just Insn2.Esp0) -> Insn3.Var (Insn3.Stack disp) Dword
    _ -> Insn3.Mem $ Insn3.MemStruct
        { Insn3.base = fmap liftBaseReg base
        , Insn3.index = fmap (\(r, s) -> (liftBaseReg r, s)) index
        , Insn3.disp = disp
        }
  where
    base = Insn2.base mem2
    index = Insn2.index mem2
    disp = Insn2.disp mem2

liftBaseReg :: Insn2.BaseReg -> Insn3.Variable
liftBaseReg (Insn2.BaseReg i) = Insn3.Temp i
liftBaseReg (Insn2.Esp0) = error "should not be lifting Esp0"

liftOp :: Insn2.Op -> Insn3.Op
liftOp (Insn2.Reg r m) = Insn3.Var (liftBaseReg r) (regModeToWidth m)
liftOp (Insn2.Imm i) = Insn3.Imm i
liftOp (Insn2.Mem m) = liftMemStruct m

liftCond (Insn2.Cond (l, Insn2.Minus, r) cmp) = (liftOp l, cmp, liftOp r)
liftCond (Insn2.Cond (l, Insn2.And', r) cmp) = case (l == r, cmp) of
    (true, Eq) -> (liftOp l, Eq, Insn3.Imm 0)
    (true, Ne) -> (liftOp l, Ne, Insn3.Imm 0)

liftInsn :: Insn2.Insn -> Insn3.Insn
liftInsn (Insn2.Add d s) = Insn3.Add (liftOp d) (liftOp d) (liftOp s)
liftInsn (Insn2.And d s) = Insn3.And (liftOp d) (liftOp d) (liftOp s)
liftInsn (Insn2.Call (Insn2.Imm addr)) = Insn3.Call (Address addr) []
-- liftInsn (Insn2.Cast Op Op Width) = Insn3.
-- liftInsn (Insn2.Conv Op Op Op Width) = Insn3.
-- liftInsn (Insn2.SDiv Op Op Op) = Insn3.
-- liftInsn (Insn2.UDiv Op Op Op) = Insn3.
-- liftInsn (Insn2.Rem Op Op Op) = Insn3.
-- liftInsn (Insn2.SMul Op Op) = Insn3.
-- liftInsn (Insn2.UMul Op Op Op) = Insn3.
liftInsn (Insn2.Goto t) = Insn3.Goto (liftOp t)
liftInsn (Insn2.GotoIf cond t) = Insn3.GotoIf (liftCond cond) (liftOp t)
-- liftInsn (Insn2.AddrOf d s) = Insn3.Ret
liftInsn (Insn2.Mov d s) = Insn3.Mov (liftOp d) (liftOp s)
liftInsn (Insn2.Neg d s) = Insn3.Neg (liftOp d) (liftOp s)
liftInsn (Insn2.Not d s) = Insn3.Not (liftOp d) (liftOp s)
liftInsn (Insn2.Or d s) = Insn3.Or (liftOp d) (liftOp d) (liftOp s)
liftInsn (Insn2.AShiftL d s) = Insn3.AShiftL (liftOp d) (liftOp d) (liftOp s)
liftInsn (Insn2.AShiftR d s) = Insn3.AShiftR (liftOp d) (liftOp d) (liftOp s)
liftInsn (Insn2.ShiftL d s) = Insn3.ShiftL (liftOp d) (liftOp d) (liftOp s)
liftInsn (Insn2.ShiftR d s) = Insn3.ShiftR (liftOp d) (liftOp d) (liftOp s)
liftInsn (Insn2.Sub d s) = Insn3.Sub (liftOp d) (liftOp d) (liftOp s)
liftInsn (Insn2.Xor d s) = Insn3.Xor (liftOp d) (liftOp d) (liftOp s)
liftInsn (Insn2.Ret) = Insn3.Ret

stackOp (Insn2.Reg (Insn2.BaseReg 0) _) = False
stackOp (Insn2.Reg (Insn2.BaseReg 1) _) = False
stackOp _ = True

hasEbpEsp = (all stackOp) . Insn2.toList

liftBB :: Cfg2.BasicBlock -> Cfg3.BasicBlock
liftBB = (map liftInsn) . (filter hasEbpEsp)

liftBlockMap :: Cfg2.BlockMap -> Cfg3.BlockMap
liftBlockMap = Map.map liftBB

liftSubroutine (Sub addr cfg2) = Func id (map liftOp sig2) cfg3
  where
    cfg3 = Cfg3.Cfg
        { Cfg3.elf = elf
        , Cfg3.entry = entry
        , Cfg3.blocks = liftBlockMap blocks
        , Cfg3.succs = succs
        }
    sig2 = Sig.find cfg2
    id = case addr2symbol elf addr of
        Just sym -> Named sym addr
        Nothing -> Address addr
    elf = Cfg2.elf cfg2
    entry = Cfg2.entry cfg2
    blocks = Cfg2.blocks cfg2
    succs = Cfg2._succs cfg2

touchUp funs (Func id sig cfg3) = Func id sig cfg3'
  where
    cfg3' = Cfg3.Cfg
        { Cfg3.elf = Cfg3.elf cfg3
        , Cfg3.entry = Cfg3.entry cfg3
        , Cfg3.blocks = Map.map (map replaceInsn) blocks
        , Cfg3.succs = Cfg3.succs cfg3
        }
    blocks = Cfg3.blocks cfg3
    replaceInsn (Insn3.Call f _) = Insn3.Call (lookupSym f) (lookupArgs f)
    replaceInsn insn = insn
    lookupSym f = case Map.lookup (getAddr f) funs of
        Just (Func f _ _) -> f
        Nothing -> error $ "function " ++ show f ++ " not found"
    lookupArgs f = case Map.lookup (getAddr f) funs of
        Just (Func _ sig _) -> sig
        Nothing -> error $ "function " ++ show f ++ " not found"
    getAddr f = case f of
        Named _ addr -> addr
        Address addr -> addr

toThree :: Program -> [Function]
toThree (Prog subs) = Map.elems $ Map.map (touchUp funs) funs
  where
    funs = Map.map liftSubroutine subs
