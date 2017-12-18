module X86.FlagState
    ( analyze
    , FlagState(..)
    , FlagMap(..)
    ) where

import Data.Word
import qualified Data.Map as Map

import Common
import Two.Insn
import X86.Cfg

data FlagState =
      Top
    | Flags CondExp
    | Bottom

type FlagMap = Map.Map Addr FlagState

flow insn = Top

join :: FlagState -> FlagState -> FlagState
join (Flags c) (Flags c') = if c == c' then Flags c else Top
join Bottom Bottom = Bottom
join _ _ = Top

empty = Bottom

analyze cfg = Map.empty
