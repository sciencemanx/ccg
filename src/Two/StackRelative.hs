module Two.StackRelative
    ( adjust
    ) where

import Data.Word
import Data.Int

import qualified Data.Map as Map

import Common

import Two.Cfg
import Two.Insn

type Info = Map.Map Id RelEsp -- basereg id, esp0 + n
type InfoMap = Map.Map Loc Info

type Mem = Map.Map Int64 RelEsp -- n, m; *(esp0 + n) = esp0 + m
type State = (Info, Mem)

data RelEsp =
      Top
    | Rel Int64
    | Val Int64
    | Bottom
    deriving (Eq, Show)

replaceOp :: Info -> Op -> Op
replaceOp info op = case op of
    Mem MemStruct {base = Just (BaseReg b), index = Nothing, disp = i} ->
        case Map.lookup b info of
            Just (Rel r) -> Mem MemStruct 
                { base = Just Esp0
                , index = Nothing
                , disp = r + i
                }
            _ -> op
    _ -> op

replace :: InfoMap -> Cfg -> Cfg
replace infomap = replaceAllOps f
  where
    f loc op = replaceOp (infomap ! loc) op

readState :: State -> Op -> RelEsp
readState (info, mem) op = case op of
    Reg (BaseReg id) m -> case Map.lookup id info of
        Just rel -> rel
        Nothing -> Bottom
    Imm i -> Val $ fromIntegral $ i
    Mem MemStruct {base = Just (BaseReg b), index = Nothing, disp = i} ->
        case Map.lookup b info of
            Just (Rel r) -> case Map.lookup (r + i) mem of
                Just rel -> rel
                Nothing -> Bottom
            _ -> Top
    _ -> Top 

writeState :: State -> Op -> RelEsp -> State
writeState (info, mem) op rel = case op of
    Reg (BaseReg id) m -> (Map.insert id rel info, mem)
    Imm i -> error "cannot write imm"
    Mem MemStruct {base = Just (BaseReg b), index = Nothing, disp = i} ->
        case Map.lookup b info of
            Just (Rel r) -> (info, Map.insert (r + i) rel mem)
            _ -> (info, mem) -- maybe this should make mem bottom -- but we will consider well behaved programs only
    _ -> (info, mem)

flow :: Insn -> State -> State
flow insn state = case insn of
    Add d s -> case (read d, read s) of
        (Rel r, Val i) -> write d $ Rel $ r   + i
        _ -> write d Top
    Sub d s -> case (read d, read s) of
        (Rel r, Val i) -> write d $ Rel $ r - i
        _ -> write d Top
    Mov d s -> write d (read s)
    -- call clobers certain registers
    Call f -> state -- TODO
    -- undefined actions
    And d s -> write d Top
    Cast d s w -> write d Top
    SDiv d d' s -> write d' Top
    UDiv d d' s -> write d' Top
    Rem d d' s -> write d Top
    SMul d s -> write d Top
    UMul d d' s -> write d' Top
    AddrOf d s -> write d Top
    Neg d s -> write d Top
    Not d s -> write d Top
    Or d s -> write d Top
    AShiftL d s -> write d Top
    AShiftR d s -> write d Top
    ShiftL d s -> write d Top
    ShiftR d s -> write d Top
    Xor d s -> write d Top
    -- all else does nothing
    _ -> state
  where
    read = readState state
    write = writeState state

bigBottom = (Map.empty, Map.empty)

join :: [State] -> State
join states = foldl join2 bigBottom states
  where
    joinLattice :: RelEsp -> RelEsp -> RelEsp
    joinLattice (Rel i) (Rel i') = if i == i' then Rel i else Top
    joinLattice (Val i) (Val i') = if i == i' then Val i else Top
    joinLattice rel Bottom = rel
    joinLattice Bottom rel = rel
    joinLattice _ _ = Top
    join2 :: State -> State -> State
    join2 (info, mem) (info', mem') =
        (Map.unionWith joinLattice info info', Map.unionWith joinLattice mem mem')

m ! k = case Map.lookup k m of
    Just v -> v
    Nothing -> error (show k ++ "not found in ins/outs in state in stackrelative")

analyze :: Cfg -> Map.Map Loc State
analyze cfg = analyze' init init (Map.keys is)
  where
    analyze' ins outs [] = ins
    analyze' ins outs (loc:wl) =
        let in' = if loc == (e, 0) 
            then entryState 
            else join $ map (\p -> outs ! p) (ps ! loc) in
        let out' = flow insn in' in
        let ins' = Map.insert loc in' ins
            outs' = Map.insert loc out' outs in
        let wl' = if out == out' then wl else (ss ! loc) ++ wl in
        analyze' ins' outs' wl'
      where
        out = outs ! loc
        insn = is ! loc
    init = Map.map (const bigBottom) is
    entryState = (Map.singleton 0 (Rel 0), Map.empty)
    is = insns cfg
    ss = succs cfg
    ps = preds cfg
    e = entry cfg

getInfo :: Cfg -> InfoMap
getInfo cfg = Map.map (\(info, mem) -> info) (analyze cfg)

adjust :: Cfg -> Cfg
adjust cfg = replace info cfg
  where
    info = getInfo cfg
