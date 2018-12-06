module Tree.Function 
    ( Function(..)
    ) where

import Common
import Tree.Cfg 
import Tree.Insn

data Function = Func Symbol [Variable] Cfg deriving (Show, Eq)
