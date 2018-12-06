module Tree.Dominators
    ( Dom
    , DomMap
    , find
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Common

import Tree.Cfg

type Dom = Set.Set Addr -- dom y
type DomMap = Map.Map Addr Dom -- x dom y

flow :: Addr -> Dom -> Dom
flow = Set.insert

join :: Dom -> [Dom] -> Dom
join universe states = foldl join2 universe states
  where
    join2 :: Dom -> Dom -> Dom
    join2 = Set.intersection

m ! k = case Map.lookup k m of
    Just v -> v
    Nothing -> error (show k ++ "not found in ins/outs in state in dominators")

analyze :: Cfg -> DomMap
analyze cfg = analyze' init init (Map.keys bb)
  where
    analyze' :: DomMap -> DomMap -> [Addr] -> DomMap
    analyze' ins outs [] = outs
    analyze' ins outs (addr:wl) =
        let in' = if addr == e
            then Set.empty
            else (join universe) $ map (\p -> outs ! p) (ps ! addr) in
        let out' = flow addr in' in
        let ins' = Map.insert addr in' ins
            outs' = Map.insert addr out' outs in
        let wl' = if out == out' then wl else (ss ! addr) ++ wl in
        analyze' ins' outs' wl'
      where
        out = outs ! addr
    init :: DomMap
    init = Map.map (const universe) bb
    universe = Set.fromList $ Map.keys bb
    bb = blocks cfg
    ss = _succs cfg
    ps = toPreds e ss
    e = entry cfg

invert :: DomMap -> DomMap
invert = Map.foldrWithKey addDoms Map.empty
  where
    addDoms :: Addr -> Dom -> DomMap -> DomMap
    addDoms x ys doms = Set.foldr (addDom x) doms ys
    addDom :: Addr -> Addr -> DomMap -> DomMap
    addDom x y = Map.insertWith Set.union y (Set.singleton x)

find :: Cfg -> DomMap
find = invert . analyze
