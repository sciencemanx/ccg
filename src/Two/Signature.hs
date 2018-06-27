module Two.Signature
    ( find
    ) where

import Prelude hiding (read)

import Common

import qualified Data.Map as Map

import Two.Cfg
import Two.Insn

type State = Map.Map Id FirstUse
type StateMap = Map.Map Loc State

data FirstUse =
      Top
    | Read
    | Write
    | Bottom
    deriving (Eq, Show)

manipState :: FirstUse -> Op -> State -> State
manipState action op state = case op of
    Reg (BaseReg id) m -> case Map.lookup id state of
        Just _ -> state
        Nothing -> Map.insert id action state
    Imm _ -> state
    Mem MemStruct {base = Just (BaseReg id), index = Nothing, disp = i} ->
        -- TODO add support for index
        case Map.lookup id state of
            Just _ -> state
            Nothing -> Map.insert id Read state
    _ -> state 

read :: Op -> State -> State
read = manipState Read

write :: Op -> State -> State
write = manipState Write

flow :: Insn -> State -> State
flow insn state = case insn of
    Add d s -> read s state |> read d
    And d s -> read s state |> read d
    Call t -> read t state
    Cast d s w -> read s state |> read d
    -- -- Conv d d' s w -> Conv (write d) (write d') (read s) w
    -- SDiv q q' s -> SDiv (read q) (read q') (read s)
    -- UDiv q q' s -> UDiv (read q) (read q') (read s)
    -- Rem q q' s -> Rem (read q) (read q') (read s)
    SMul d s -> read s state |> read d
    -- UMul d d' s -> UMul (read d) (read d') (read s)
    Goto t -> read t state
    GotoIf (Cond (l, _, r) _) t -> read l state |> read r |> read t
    AddrOf d s -> read (Mem s) state |> write d
    Mov d s -> read s state |> write d
    Neg d s -> read s state |> write d
    Not d s -> read s state |> write d
    Or d s -> read s state |> read d
    AShiftL d s -> read s state |> read d
    AShiftR d s -> read s state |> read d
    ShiftL d s -> read s state |> read d
    ShiftR d s -> read s state |> read d
    Sub d s -> read s state |> read d
    Xor d s -> read s state |> read d
    Ret -> state
  where
    a |> b = b a

join :: [State] -> State
join states = foldl join2 Map.empty states
  where
    joinLattice :: FirstUse -> FirstUse -> FirstUse
    joinLattice Bottom x = x
    joinLattice x Bottom = x
    joinLattice x y = if x == y then x else Top
    join2 :: State -> State -> State
    join2 state state' = Map.unionWith joinLattice state state'

m ! k = case Map.lookup k m of
    Just v -> v
    Nothing -> error (show k ++ "not found in ins/outs in state in signature")

analyze :: Cfg -> Map.Map Loc State
analyze cfg = analyze' init init (Map.keys is)
  where
    analyze' ins outs [] = outs
    analyze' ins outs (loc:wl) =
        let in' = join $ map (\p -> outs ! p) (ps ! loc) in
        let out' = flow insn in' in
        let ins' = Map.insert loc in' ins
            outs' = Map.insert loc out' outs in
        let wl' = if out == out' then wl else (ss ! loc) ++ wl in
        analyze' ins' outs' wl'
      where
        out = outs ! loc
        insn = is ! loc
    init = Map.map (const Map.empty) is
    is = insns cfg
    ss = succs cfg
    ps = preds cfg
    e = entry cfg

getRet cfg = head rets
  where
    rets = Map.keys $ Map.filter isRet $ insns cfg
    isRet Ret = True
    isRet _ = False

find :: Cfg -> [Op]
find cfg = map (\x -> Reg (BaseReg x) R) args
  where
    args = filter isArg [6, 7, 5, 4, 8, 9] -- systemv calling conv
    isArg op = case Map.lookup op retInfo of
        Just Read -> True
        Just Top -> True
        _ -> False
    retInfo = info ! (getRet cfg)
    info = analyze cfg
