module Three.Function 
    ( Function(..)
    ) where

import Common
import Three.Cfg 
import Three.Insn

data Function = Func Symbol [Op] Cfg deriving (Show, Eq)
