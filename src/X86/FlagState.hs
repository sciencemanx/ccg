module X86.FlagState
    ( analyze
    , FlagState(..)
    , FlagMap(..)
    ) where

import Data.Word
import qualified Data.Map as Map

import Common
import qualified Two.Insn as Insn2
import qualified X86.Cfg as Cfg

data FlagState =
      Top
    | Flags Insn2.CondExp
    | Bottom
    deriving (Eq, Show)

type FlagMap = Map.Map Addr FlagState

flow insn state = Top

join :: [FlagState] -> FlagState
join states = foldl join2 Bottom states
  where
    join2 (Flags c) (Flags c') = if c == c' then Flags c else Top
    join2 flags Bottom = flags
    join2 Bottom flags = flags
    join2 _ _ = Top

empty = Bottom

m ! k = case Map.lookup k m of
    Just v -> v
    Nothing -> error (show k ++ "not found in ins/outs in flagstate")

analyze :: Cfg.Cfg -> FlagMap
analyze cfg = analyze' init init [entry]
  where
    analyze' ins outs [] = ins
    analyze' ins outs (addr:wl) =
        let in' = join $ map (\p -> outs ! p) (preds ! addr) in
        let out' = flow insn in' in
        let ins' = Map.insert addr in'  ins
            outs' = Map.insert addr out' outs in
        let wl' = if out == out' then wl else (succs ! addr) ++ wl in
        analyze' ins' outs' wl'
      where
        out = outs ! addr
        insn = insns ! addr
    init = Map.map (const empty) insns
    entry = Cfg.entry cfg
    insns = Cfg.insns cfg
    succs = Cfg.succs cfg
    preds = Cfg.mkPreds cfg
