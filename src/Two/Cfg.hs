module Two.Cfg
    ( Cfg(..)
    , BasicBlock(..)
    ) where

import qualified Data.Map as Map
import Data.Elf (Elf)

import Common
import Two.Insn

data BasicBlock = BasicBlock
    { insns :: [Insn]
    , succ :: Addr
    , branches :: [Addr]
    }
    deriving (Eq, Show)    

type BlockMap = Map.Map Addr BasicBlock

data Cfg = Cfg
    { elf :: Elf
    , blocks :: BlockMap
    , entry :: Addr }
    deriving (Eq, Show)
