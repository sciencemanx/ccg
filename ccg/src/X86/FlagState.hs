module X86.FlagState
    ( analyze
    , FlagState(..)
    , FlagMap(..)
    ) where

import Debug.Trace

import Data.Word
import qualified Data.Map as Map

import qualified Hapstone.Internal.X86 as X86

import Common

import qualified Two.Insn as Insn2
import qualified X86.Insn as InsnX
import qualified X86.Cfg as Cfg

data FlagState =
      Top
    | Flags Insn2.CondExp
    | Bottom
    deriving (Eq, Show)

type FlagMap = Map.Map Addr FlagState

-- TODO: finish flow function
flow :: (InsnX.Op -> Insn2.Op) -> InsnX.Insn -> FlagState -> FlagState
flow lift insn state = case (instr, ops) of
    (X86.X86InsCmp, [l, r]) -> Flags (lift l, Insn2.Minus, lift r)
    (X86.X86InsTest, [l, r]) -> Flags (lift l, Insn2.And', lift r)
    _ -> state
  where
    instr = InsnX.instr insn
    ops = InsnX.operands insn

join :: [FlagState] -> FlagState
join states = foldl join2 Bottom states
  where
    join2 (Flags c) (Flags c') = if c == c' then Flags c else Top
    join2 flags Bottom = flags
    join2 Bottom flags = flags
    join2 _ _ = Top

empty :: FlagState
empty = Bottom

m ! k = case Map.lookup k m of
    Just v -> v
    Nothing -> error (show k ++ "not found in ins/outs in flagstate")

analyze :: (InsnX.Op -> Insn2.Op) -> Cfg.Cfg -> FlagMap
analyze lift cfg = analyze' init init (Map.keys insns)
  where
    analyze' ins outs [] = ins
    analyze' ins outs (addr:wl) =
        let in' = join $ map (\p -> outs ! p) (preds ! addr) in
        let out' = flow lift insn in' in
        let ins' = Map.insert addr in' ins
            outs' = Map.insert addr out' outs in
        let wl' = if out == out' then wl else (succs ! addr) ++ wl in
        analyze' ins' outs' wl'
      where
        out = outs ! addr
        insn = insns ! addr
    init = Map.map (const empty) insns
    insns = Cfg.insns cfg
    succs = Cfg.succs cfg
    preds = Cfg.mkPreds cfg
