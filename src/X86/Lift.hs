module X86.Lift
    ( toTwo
    ) where

import Debug.Trace (trace)

import qualified Data.Map as Map
import Data.Map ((!))

import qualified Hapstone.Internal.X86 as X86

import Common
import qualified X86.FlagState as Flags
import qualified X86.Cfg as CfgX
import qualified Two.Cfg as Cfg2
import qualified X86.Insn as InsnX
import qualified Two.Insn as Insn2

liftReg InsnX.Inv = Insn2.BaseReg (-1)
liftReg InsnX.Rsp = Insn2.BaseReg 0
liftReg InsnX.Rbp = Insn2.BaseReg 1
liftReg InsnX.Rax = Insn2.BaseReg 2
liftReg InsnX.Rbx = Insn2.BaseReg 3
liftReg InsnX.Rcx = Insn2.BaseReg 4
liftReg InsnX.Rdx = Insn2.BaseReg 5
liftReg InsnX.Rdi = Insn2.BaseReg 6
liftReg InsnX.Rsi = Insn2.BaseReg 7
liftReg InsnX.R8  = Insn2.BaseReg 8
liftReg InsnX.R9  = Insn2.BaseReg 9
liftReg InsnX.R10 = Insn2.BaseReg 10
liftReg InsnX.R11 = Insn2.BaseReg 11
liftReg InsnX.R12 = Insn2.BaseReg 12
liftReg InsnX.R13 = Insn2.BaseReg 13
liftReg InsnX.R14 = Insn2.BaseReg 14
liftReg InsnX.R15 = Insn2.BaseReg 15
liftReg InsnX.Rip = Insn2.BaseReg 16

liftMemStruct m = Insn2.MemStruct 
    { Insn2.base = base
    , Insn2.index = index
    , Insn2.disp = disp
    }
  where
    base = case InsnX.base m of
        InsnX.Inv -> Nothing
        x -> Just $ liftReg x
    index = case InsnX.index m of
        InsnX.Inv -> Nothing
        idx -> Just (liftReg idx, InsnX.scale m)
    disp = InsnX.disp m

liftOp InsnX.Op {InsnX.value = val, InsnX.width = w} = case val of
    InsnX.Reg r mode -> Insn2.Reg (liftReg r) mode
    InsnX.Imm n -> Insn2.Imm n
    InsnX.Mem m -> Insn2.Mem $ liftMemStruct m

liftInstr :: Flags.FlagMap -> Addr -> X86.X86Insn -> [InsnX.Op] -> [Insn2.Insn]
liftInstr flagInfo addr = liftInstr'
  where
    liftInstr' :: X86.X86Insn -> [InsnX.Op] -> [Insn2.Insn]
    liftInstr' instr ops = case (instr, ops) of
        (X86.X86InsAdd, [d, s]) ->
            [Insn2.Add (liftOp d) (liftOp s)]
        (X86.X86InsAnd, [d, s]) ->
            [Insn2.And (liftOp d) (liftOp s)]
        (X86.X86InsCall, [f]) ->
            [Insn2.Call (liftOp f)]
        (X86.X86InsCbw, []) ->
            error "cbw not implemented"
            -- [Insn2.Cast (liftOp ax) (liftOp al) Word]
        (X86.X86InsCwde, []) ->
            error "cwde not implemented"
            -- [Insn2.Cast (liftOp eax) (liftOp ax) DWord]
        (X86.X86InsCdqe, []) ->
            error "cdqe not implemented"
            -- [Insn2.Cast (liftOp rax) (liftOp eax) QWord]
        (X86.X86InsCwd, []) ->
            error "cwd not implemented"
            -- [Insn2.Conv (liftOp dx) (liftOp ax) (liftOp ax) DWord]
        (X86.X86InsCdq, []) ->
            error "cdq not implemented"
            -- [Insn2.Conv (liftOp edx) (liftOp eax) (liftOp eax) QWord]
        (X86.X86InsCqo, []) ->
            error "cqo not implemented"
            -- [Insn2.Conv (liftOp rdx) (liftOp rax) (liftOp rax) OWord]
        (X86.X86InsIdiv, [s]) ->
            error "idiv not implemented"
            -- [Insn2.SDiv (liftOp ah) (liftOp al) (liftOp s)]
        (X86.X86InsDiv, [s]) ->
            error "div not implemented"
            -- [Insn2.UDiv (liftOp ah) (liftOp al) (liftOp s)]
        (X86.X86InsImul, _) ->
            error "imul not implemented"
        (X86.X86InsMul, [s]) ->
            error "mul not implemented"
            -- [Insn2.UMul (liftOp ax) (liftOp al) (liftOp s)]
        (X86.X86InsJmp, [t]) ->
            [Insn2.Goto (liftOp t)]
        (X86.X86InsJae, [t]) ->
            mkGotoIf X86.X86InsJae t
        (X86.X86InsJa, [t]) ->
            mkGotoIf X86.X86InsJa t
        (X86.X86InsJbe, [t]) ->
            mkGotoIf X86.X86InsJbe t
        (X86.X86InsJb, [t]) ->
            mkGotoIf X86.X86InsJb t
        (X86.X86InsJe, [t]) ->
            mkGotoIf X86.X86InsJe t
        (X86.X86InsJge, [t]) ->
            mkGotoIf X86.X86InsJge t
        (X86.X86InsJg, [t]) ->
            mkGotoIf X86.X86InsJg t
        (X86.X86InsJle, [t]) ->
            mkGotoIf X86.X86InsJle t
        (X86.X86InsJl, [t]) ->
            mkGotoIf X86.X86InsJl t
        (X86.X86InsJne, [t]) ->
            mkGotoIf X86.X86InsJne t
        (X86.X86InsLea, [d, InsnX.Op {InsnX.value = InsnX.Mem ms}]) ->
            [Insn2.AddrOf (liftOp d) (liftMemStruct ms)]
        (X86.X86InsMov, [d, s]) ->
            [Insn2.Mov (liftOp d) (liftOp s)]
        (X86.X86InsNeg, [op]) ->
            [Insn2.Neg (liftOp op) (liftOp op)]
        (X86.X86InsNot, [op]) ->
            [Insn2.Not (liftOp op) (liftOp op)]
        (X86.X86InsOr, [d, s]) ->
            [Insn2.Or (liftOp d) (liftOp s)]
        (X86.X86InsPop, [d]) ->
            [ Insn2.Mov (liftOp d) rspDeref
            , Insn2.Add rsp (Insn2.Imm 8)
            ]
        (X86.X86InsPush, [s]) ->
            [ Insn2.Sub rsp (Insn2.Imm 8)
            , Insn2.Mov rspDeref (liftOp s)
            ]    
        (X86.X86InsSal, [d, s]) ->
            [Insn2.AShiftL (liftOp d) (liftOp s)]    
        (X86.X86InsSar, [d, s]) ->
            [Insn2.AShiftR (liftOp d) (liftOp s)]    
        (X86.X86InsShl, [d, s]) ->
            [Insn2.ShiftL (liftOp d) (liftOp s)]    
        (X86.X86InsShr, [d, s]) ->
            [Insn2.ShiftR (liftOp d) (liftOp s)]    
        (X86.X86InsSub, [d, s]) ->
            [Insn2.Sub (liftOp d) (liftOp s)]
        (X86.X86InsXor, [d, s]) ->
            [Insn2.Xor (liftOp d) (liftOp s)]
        (X86.X86InsInc, [op]) ->
            [Insn2.Add (liftOp op) (Insn2.Imm 1)]
        (X86.X86InsDec, [op]) ->
            [Insn2.Sub (liftOp op) (Insn2.Imm 1)]
        (X86.X86InsLeave, _) -> 
            [Insn2.Mov rsp rbp] ++ liftInstr' X86.X86InsPop [rbpX]
        (X86.X86InsRet, _) ->
            [Insn2.Ret]
        (X86.X86InsCmp, _) -> []
        (X86.X86InsNop, _) -> []
        (X86.X86InsTest, _) -> []
    mkGotoIf :: X86.X86Insn -> InsnX.Op -> [Insn2.Insn]
    mkGotoIf jcc t =
        [Insn2.GotoIf (Insn2.Cond condExp cmpOp) (liftOp t)]
      where
        cmpOp = case jcc of
            X86.X86InsJe -> Insn2.Eq
            X86.X86InsJne -> Insn2.Ne
            X86.X86InsJg -> Insn2.Gt
            X86.X86InsJge -> Insn2.Ge
            X86.X86InsJl -> Insn2.Lt
            X86.X86InsJle -> Insn2.Le
            X86.X86InsJa -> Insn2.A
            X86.X86InsJae -> Insn2.Ae
            X86.X86InsJb -> Insn2.B
            X86.X86InsJbe -> Insn2.Be
        condExp = case Map.lookup addr flagInfo of
            Just (Flags.Flags exp) -> exp
            _ -> error "No semi-concrete flag exp at Jcc"
    rbpX = InsnX.Op {InsnX.value = InsnX.Reg InsnX.Rbp R, InsnX.width = 8}
    rsp = Insn2.Reg (liftReg InsnX.Rsp) R
    rbp = Insn2.Reg (liftReg InsnX.Rbp) R
    rspDeref = Insn2.Mem (Insn2.MemStruct 
        { Insn2.base = Just $ liftReg InsnX.Rsp
        , Insn2.index = Nothing
        , Insn2.disp = 0
        })

liftInsn :: Flags.FlagMap -> InsnX.Insn -> [Insn2.Insn]
liftInsn flagInfo xInsn = liftInstr flagInfo addr instr ops
  where
    addr = InsnX.address xInsn
    instr = InsnX.instr xInsn
    ops = InsnX.operands xInsn

-- splits insn map into basic blocks by consuming until it hits a point not
-- "doubly-linked" and start a new block after enqueueing successors in worklist
constructBlocks insns succs preds blocks [] = blocks
constructBlocks insns succs preds blocks (addr:wl) =
    case Map.lookup addr blocks of
        Just block -> constructBlocks insns succs preds blocks wl
        Nothing -> constructBlocks insns succs preds blocks' wl'
          where
            blocks' = Map.insert addr block blocks
            wl' = next ++ wl
            (block, next) = blockFrom addr
  where
    blockFrom addr = (reverse addrs, nextAddrs)
      where
        (addrs, nextAddrs) = blockFrom' addr []
        blockFrom' addr block
            -- about to branch... end with this one
            | length (succ) > 1 = (addr : block, succ)
            -- already past the end... don't include in BB but enqueue in wl
            | length (pred) > 1 = (block, [addr])
            | otherwise = case succ of
                [] -> (addr : block, [])
                [next] -> blockFrom' next (addr : block)
          where
            succ = case Map.lookup addr succs of
                Just succ -> succ
                Nothing -> error $ show addr ++ " not in succs"
            pred = case Map.lookup addr preds of
                Just pred -> pred
                Nothing -> error $ show addr ++ " not in preds"

constructSuccs succs addrBlocks = Map.map getSuccs addrBlocks
  where
    getSuccs [] = []
    getSuccs addrs = case Map.lookup (last addrs) succs of
        Just lastInsn -> lastInsn
        Nothing -> error $ "last insn " ++ show (last addrs) ++ " not in succs"

liftAddrs flagInfo insns = concat . (map liftOne)     
  where
    liftOne addr = (liftInsn flagInfo) $ insns ! addr

toTwo :: CfgX.Cfg -> Cfg2.Cfg
toTwo xCfg = Cfg2.Cfg 
    { Cfg2.elf = elf
    , Cfg2.entry = entry
    , Cfg2.blocks = blocks
    , Cfg2.succs = succsTwo}
  where
    succsTwo = constructSuccs succs addrs
    blocks = Map.map (liftAddrs flagInfo insns) addrs
    addrs = constructBlocks insns succs preds Map.empty [entry]
    flagInfo = Flags.analyze liftOp xCfg
    elf = CfgX.elf xCfg
    entry = CfgX.entry xCfg
    insns = CfgX.insns xCfg
    preds = CfgX.mkPreds xCfg
    succs = CfgX.succs xCfg
