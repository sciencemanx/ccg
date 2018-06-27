module Tree.Lift
    ( toAst
    ) where

import Common

import Data.Maybe (mapMaybe)

import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Set as Set

import qualified Tree.Cfg as Cfg
import qualified Tree.Function as FuncT
import qualified Tree.Insn as InsnT
import qualified Tree.Dominators as Dominators
import qualified Ast.Ast as Ast

liftUnop InsnT.Neg = Ast.Neg
liftUnop InsnT.Not = Ast.Not

liftBinop (InsnT.Add) = Ast.Add
liftBinop (InsnT.And) = Ast.And
liftBinop (InsnT.Div) = Ast.Div
liftBinop (InsnT.Mul) = Ast.Mul
liftBinop (InsnT.Rem) = Ast.Rem
liftBinop (InsnT.Or) = Ast.Or
liftBinop (InsnT.ShiftL) = Ast.ShiftL
liftBinop (InsnT.ShiftR) = Ast.ShiftR
liftBinop (InsnT.Sub) = Ast.Sub
liftBinop (InsnT.Xor) = Ast.Xor
liftBinop (InsnT.Cmp cmp) = Ast.Cmp cmp

liftVar (InsnT.Variable id) = Ast.Variable id

liftVarExp (InsnT.Var v) = Ast.Var $ liftVar v
liftVarExp (InsnT.Arr a i) = Ast.Arr (liftVarExp a) (liftExp i)
liftVarExp (InsnT.Deref p) = Ast.Deref $ liftVarExp p
liftVarExp (InsnT.AddrOf v) = Ast.AddrOf $ liftVarExp v

liftExp (InsnT.Un u e) = Ast.Un (liftUnop u) (liftExp e)
liftExp (InsnT.Bin b e e') = Ast.Bin (liftBinop b) (liftExp e) (liftExp e')
liftExp (InsnT.Call a args) = Ast.Call a (map liftExp args)
liftExp (InsnT.Cast w e) = Ast.Cast w (liftExp e) 
liftExp (InsnT.VarExp v) = Ast.VarExp (liftVarExp v)
liftExp (InsnT.Imm n) = Ast.Imm n

liftStm (InsnT.Mov v e) = Just $ Ast.Mov (liftVarExp v) (liftExp e)
liftStm (InsnT.Goto t) = Nothing
liftStm (InsnT.GotoIf c t) = Nothing
liftStm (InsnT.Return e) = Just $ Ast.Return (liftExp e)

liftBB :: [InsnT.Stm] -> [Ast.Stm]
liftBB = mapMaybe liftStm

find :: Map.Map Addr [Addr] -> Set.Set Addr -> Set.Set Addr -> Addr -> Addr
find succs allowed goal addr = next
  where
    Just next = find' addr
    find' :: Addr -> Maybe Addr
    find' addr = case (Set.member addr allowed, Set.member addr goal) of
        (False, _) -> Nothing
        (_, True) -> Just addr
        _ -> foldr dfs Nothing (succs ! addr)
          where
            dfs :: Addr -> Maybe Addr -> Maybe Addr
            dfs next_addr (Just found) = Just found
            dfs next_addr (Nothing) = find' next_addr

data Control =
      Cond InsnT.Exp Addr Addr
    | Cont Addr
    | Finish
        
liftCfg cfg = liftCfg' entry
  where
    liftCfg' addr = case control of
        Cond c t f -> if isLoop addr
            then stms ++ [Ast.While (liftExp c) (liftCfg' t)] ++ liftCfg' f
            else stms ++ [ifStm] ++ post
          where
            ifStm = if dom_a == Set.unions [Set.singleton addr, dom_t, dom_f]
                then Ast.If (liftExp c) (liftCfg' t)
                else Ast.IfElse (liftExp c) (liftCfg' t) (liftCfg' f)
            post = liftCfg' $ find succs dom_a (Set.difference dom_a dom_t) t
            dom_t = dom ! t
            dom_f = dom ! f
        Cont t -> stms ++ liftCfg' t
        Finish -> stms
      where
        control = case last block of
            InsnT.GotoIf c t -> Cond c t (if t == a then b else a)
              where
                [a, b] = succs ! addr
            InsnT.Goto t -> if isLoop t then Cont t else Finish
            _ -> Finish 
        isLoop a = length (filter (\x -> Set.member x (dom ! a)) (preds ! a)) > 0
        dom_a = dom ! addr
        stms = liftBB block
        block = bbs ! addr
    dom = Dominators.find cfg
    entry = Cfg.entry cfg
    bbs = Cfg.blocks cfg
    succs = Cfg._succs cfg
    preds = toPreds entry succs

liftFunction (FuncT.Func id args cfg) = Ast.Func id (map liftVar args) (liftCfg cfg)

toAst :: [FuncT.Function] -> Ast.Program
toAst = Ast.Prog . (map liftFunction)
