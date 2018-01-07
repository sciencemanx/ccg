module Three.Cfg
    ( Cfg(..)
    , BasicBlock(..)
    , BlockMap
    , SuccMap
    ) where

import qualified Data.Map as Map
import Data.Elf (Elf)
import Data.List (intercalate)
import Numeric (showHex)

import Common
import Three.Insn

type SuccMap = Map.Map Addr [Addr]
type BasicBlock = [Insn]
type BlockMap = Map.Map Addr BasicBlock

data Cfg = Cfg
    { elf :: Elf
    , entry :: Addr 
    , blocks :: BlockMap
    , succs :: SuccMap
    } deriving (Eq)

join = intercalate

instance Show Cfg where
    show Cfg {blocks = blocks, succs = succs} =
        join "\n" (map showBlock (Map.keys blocks))
      where
        showBlock addr =
            addrStr ++ ":\n" ++ asmStr
          where
            addrStr = show addr
            asmStr = join "\n" (map (((++) "\t") . show) block)
            block = case Map.lookup addr blocks of
                Just b -> b
                Nothing -> error $ "block at addr " ++ show addr ++ " DNE"