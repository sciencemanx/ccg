module Two.Cfg
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
import Two.Insn

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
            "0x" ++ addrStr ++ ":\n" ++ asmStr ++ "\n\tsucc: " ++ succStr
          where
            addrStr = showHex addr ""
            succStr = show $ case Map.lookup addr succs of
                Just s -> s
                Nothing -> error $ "succs at addr " ++ show addr ++ " DNE"
            asmStr = join "\n" (map (((++) "\t") . show) block)
            block = case Map.lookup addr blocks of
                Just b -> b
                Nothing -> error $ "block at addr " ++ show addr ++ " DNE"

