module Three.Lift
    ( toTree
    ) where

import qualified Data.Map as Map

import Common
import Three.Function as Func3
import Tree.Function as FuncT

import qualified Three.Cfg as Cfg3
import qualified Tree.Cfg as CfgT
import qualified Three.Insn as Insn3
import qualified Tree.Insn as InsnT

type VarMap = Map.Map Insn3.Variable InsnT.Variable

mapInsns :: [Insn3.Insn] -> VarMap
mapInsns insns = vars
  where
    (vars, total_vars) = foldr mapInsn (Map.empty, 0) insns
    mapInsn :: Insn3.Insn -> (VarMap, Id) -> (VarMap, Id)
    mapInsn insn vars = foldr mapOp vars (Insn3.toList insn)
    mapOp :: Insn3.Op -> (VarMap, Id) -> (VarMap, Id)
    mapOp (Insn3.Var v w) vars = addVar v vars
    mapOp (Insn3.Imm _) vars = vars
    mapOp (Insn3.Mem _) vars = vars
    addVar :: Insn3.Variable -> (VarMap, Id) -> (VarMap, Id)
    addVar var (vars, i) = case Map.lookup var vars of
        Just _ -> (vars, i)
        Nothing -> (Map.insert var (InsnT.Variable i) vars, i + 1)

m ! k = case Map.lookup k m of
    Just v -> v
    Nothing -> error "thing not found in lift"

retVar = (Insn3.Var (Insn3.Temp 2) Dword)

liftInsn :: VarMap -> Insn3.Insn -> InsnT.Stm
liftInsn vars insn = case insn of
    Insn3.Add d s s' ->
        mov d (InsnT.Bin InsnT.Add (liftOp s) (liftOp s'))
    Insn3.And d s s' ->
        mov d (InsnT.Bin InsnT.And (liftOp s) (liftOp s'))
    Insn3.Call id args ->
        mov retVar (InsnT.Call id (map liftOp args))
    Insn3.Cast d s w ->
        mov d (InsnT.Cast w (liftOp s))
    Insn3.SDiv d s s' ->
        mov d (InsnT.Bin InsnT.Div (liftOp s) (liftOp s'))
    Insn3.UDiv d s s' ->
        mov d (InsnT.Bin InsnT.Div (liftOp s) (liftOp s'))
    Insn3.Rem d s s' ->
        mov d (InsnT.Bin InsnT.Rem (liftOp s) (liftOp s'))
    Insn3.SMul d s s' ->
        mov d (InsnT.Bin InsnT.Mul (liftOp s) (liftOp s'))
    Insn3.UMul d s s' ->
        mov d (InsnT.Bin InsnT.Mul (liftOp s) (liftOp s'))
    Insn3.Goto (Insn3.Imm addr) ->
        InsnT.Goto addr
    Insn3.GotoIf (l, op, r) (Insn3.Imm addr) ->
        InsnT.GotoIf (InsnT.Bin (InsnT.Cmp op) (liftOp l) (liftOp r)) addr
    -- Insn3.AddrOf d s ->
    --     InsnT.Return $ InsnT.Imm 0
    Insn3.Mov d s ->
        mov d (liftOp s)
    Insn3.Neg d s ->
        mov d (InsnT.Un InsnT.Neg (liftOp s))
    Insn3.Not d s ->
        mov d (InsnT.Un InsnT.Not (liftOp s))
    Insn3.Or d s s' ->
        mov d (InsnT.Bin InsnT.Or (liftOp s) (liftOp s'))
    Insn3.AShiftL d s s' ->
        mov d (InsnT.Bin InsnT.ShiftL (liftOp s) (liftOp s'))
    Insn3.AShiftR d s s' ->
        mov d (InsnT.Bin InsnT.ShiftR (liftOp s) (liftOp s'))
    Insn3.ShiftL d s s' ->
        mov d (InsnT.Bin InsnT.ShiftL (liftOp s) (liftOp s'))
    Insn3.ShiftR d s s' ->
        mov d (InsnT.Bin InsnT.ShiftR (liftOp s) (liftOp s'))
    Insn3.Sub d s s' ->
        mov d (InsnT.Bin InsnT.Sub (liftOp s) (liftOp s'))
    Insn3.Xor d s s' ->
        mov d (InsnT.Bin InsnT.Xor (liftOp s) (liftOp s'))
    Insn3.Ret ->
        InsnT.Return $ liftOp retVar
    _ -> error $ show insn ++ " not supported/matched"
  where
    varexp :: Insn3.Variable -> InsnT.VariableExp
    varexp v = InsnT.Var $ vars ! v
    memexp :: Insn3.MemStruct -> InsnT.VariableExp
    memexp m = case (Insn3.base m, Insn3.index m, Insn3.disp m) of
        (Just ptr, Nothing, 0) -> 
            InsnT.Deref $ varexp ptr
        (Just arr, Just (idx, len), 0) -> 
            InsnT.Arr (varexp arr) (liftOp (Insn3.Var idx Dword))
        _ -> error "rip the mem"
    imm n = InsnT.Imm n
    liftOp :: Insn3.Op -> InsnT.Exp
    liftOp (Insn3.Var v w) = InsnT.VarExp $ varexp v
    liftOp (Insn3.Imm n) = imm n
    liftOp (Insn3.Mem m) = InsnT.VarExp $ memexp m
    mov :: Insn3.Op -> InsnT.Exp -> InsnT.Stm
    mov (Insn3.Var d w) exp = InsnT.Mov (varexp d) exp
    mov (Insn3.Mem d) exp = InsnT.Mov (memexp d) exp

liftBB :: VarMap -> Cfg3.BasicBlock -> CfgT.BasicBlock
liftBB vars = map (liftInsn vars)

liftBlockMap :: VarMap-> Cfg3.BlockMap -> CfgT.BlockMap
liftBlockMap vars = Map.map (liftBB vars)

liftFunction (Func3.Func id args cfg3) = 
    FuncT.Func id (map (\(Insn3.Var v _) -> vars ! v) args) cfgT
  where
    cfgT= CfgT.Cfg
        { CfgT.elf = elf
        , CfgT.entry = entry
        , CfgT.blocks = liftBlockMap vars blocks
        , CfgT._succs = succs
        }
    vars = mapInsns insns
    insns = Map.elems $ Cfg3.insns cfg3
    elf = Cfg3.elf cfg3
    entry = Cfg3.entry cfg3
    blocks = Cfg3.blocks cfg3
    succs = Cfg3.succs cfg3

toTree :: [Func3.Function] -> [FuncT.Function]
toTree = map liftFunction
