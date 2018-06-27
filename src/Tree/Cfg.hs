module Tree.Cfg
    ( Cfg(..)
    , BasicBlock(..)
    , BlockMap
    , SuccMap
    , succs
    , preds
    ) where

import Prelude hiding (succ)

import qualified Data.Map as Map

import Data.Map ((!))
import Data.Elf (Elf)
import Data.List (intercalate)
import Numeric (showHex)

import Common
import Tree.Insn

type SuccMap = Map.Map Addr [Addr]
type BasicBlock = [Stm]
type BlockMap = Map.Map Addr BasicBlock

data Cfg = Cfg
    { elf :: Elf
    , entry :: Addr 
    , blocks :: BlockMap
    , _succs :: SuccMap
    } deriving (Eq)
    
join = intercalate

succ :: Cfg -> Loc -> [Loc]
succ cfg (addr, idx) = 
    if idx + 1 >= len then map (\x -> (x, 0)) (ss ! addr) else [(addr, idx + 1)]
  where
    len = length (bs ! addr)
    Cfg {blocks = bs, _succs = ss} = cfg

succs :: Cfg -> Map.Map Loc [Loc]
succs cfg = succs' Map.empty [(e, 0)]
  where
    succs' ss [] = ss
    succs' ss (loc:wl) = case Map.lookup loc ss of
        Just _ -> succs' ss wl
        Nothing -> succs' (Map.insert loc (succ' loc) ss) (succ' loc ++ wl)
    succ' = succ cfg
    Cfg {entry = e} = cfg

preds :: Cfg -> Map.Map Loc [Loc]
preds cfg =
    Map.foldrWithKey addSuccs (Map.singleton (e, 0) []) ss
  where
    addSuccs loc succs preds = foldr (addSucc loc) preds succs
    addSucc loc succ preds = Map.insertWith (++) succ [loc] preds
    Cfg {entry = e} = cfg
    ss = succs cfg

instance Show Cfg where
    show Cfg {blocks = blocks, _succs = succs} =
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
