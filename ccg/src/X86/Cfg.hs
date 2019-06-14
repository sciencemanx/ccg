module X86.Cfg
    ( construct
    , mkPreds
    , mkCalls
    , Cfg(..)
    , SuccMap
    , PredMap
    , InsnMap
    , CallMap
    ) where

import Prelude hiding (succ)

import Data.Maybe (fromJust, maybeToList, mapMaybe)
import Data.Elf (Elf)
import Data.Word (Word64)
import qualified Data.Map as Map

import qualified Hapstone.Internal.X86 as X86

import X86.Disass (disassInstr)
import X86.Insn
import Common (bug)

type Addr = Word64

type InsnMap = Map.Map Addr Insn
type SuccMap = Map.Map Addr [Addr]
type CallMap = Map.Map Addr Addr
type PredMap = Map.Map Addr [Addr]

data Cfg = Cfg
    { elf :: Elf
    , entry :: Addr
    , insns :: InsnMap
    , succs :: SuccMap
    } deriving (Eq)

instance Show Cfg where
    show Cfg {insns = is, succs = s} = "Cfg{insns=" ++ show is ++ "}"

mkPreds :: Cfg -> PredMap
mkPreds Cfg {entry = entry, succs = succs} =
    Map.foldrWithKey addSuccs (Map.singleton entry []) succs
  where
    addSuccs addr succs preds = foldr (addSucc addr) preds succs
    addSucc addr succ preds = Map.insertWith (++) succ [addr] preds

getCall :: Insn -> Maybe Addr
getCall Insn {instr=X86.X86InsCall, operands=[Op {value=Imm n}]} = Just n
getCall _ = Nothing

mkCalls :: Cfg -> [Addr]
mkCalls cfg = mapMaybe getCall (Map.elems $ insns cfg)

jmpTarget insn = case target of
    (Imm addr) -> addr
    _ -> error $ show target ++ ": only supports relative jump right now"
  where
    target = value $ head $ operands insn

succ insn
    | i == X86.X86InsRet       = []
    | i == X86.X86InsJmp       = [jmpTarget insn]
    | elem X86.X86GrpJump grps = [next, jmpTarget insn]
    | otherwise                = [next]
  where
    i = instr insn
    grps = groups insn
    next = address insn + size insn

recDecent cfg [] = cfg
recDecent cfg (addr:wl) =
    case disassInstr e addr of
        Nothing -> recDecent cfg wl
        Just insn -> recDecent cfg' wl'
          where
            cfg' = Cfg {elf = e, entry = entry, insns = insns', succs = succs'}
            wl' = filter (not . visited) s ++ wl
            visited addr = Map.member addr insns'
            insns' = Map.insert addr insn insns
            succs' = Map.insert addr s succs
            s = succ insn
            addr = address insn
  where
    Cfg {elf = e, entry = entry, insns = insns, succs = succs} = cfg

construct :: Elf -> Addr -> Cfg
construct elf addr = recDecent init [addr]
  where
    init = Cfg {elf = elf, entry = addr, insns = Map.empty, succs = Map.empty}
