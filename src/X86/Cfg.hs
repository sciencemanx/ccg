module X86.Cfg 
    ( construct
    , Cfg(..)
    ) where

import Prelude hiding (succ)

import Data.Maybe (fromJust, maybeToList)
import Data.Elf (Elf)
import Data.Word (Word64)
import qualified Data.Map as Map

import qualified Hapstone.Internal.X86 as X86

import X86.Disass (disassInstr)
import X86.Insn

type Addr = Word64

type InsnMap = Map.Map Addr Insn
type SuccMap = Map.Map Addr (Maybe Addr)
type BranchMap = Map.Map Addr (Maybe Addr)
type CallMap = Map.Map Addr Addr

data Cfg = Cfg 
    { elf :: Elf
    , insns :: InsnMap
    , succs :: SuccMap
    , branches :: BranchMap
    } deriving (Eq)

instance Show Cfg where
    show Cfg {insns = is} = show is

jmpTarget insn = case target of
    (Imm n) -> addr + sz + n
    _ -> error $ show target ++ ": only supports relative jump right now"
  where
    addr = address insn
    sz = size insn
    target = value $ head $ operands insn

succ insn = case instr insn of
    (X86.X86InsRet) -> Nothing
    (X86.X86InsJmp) -> Just $ jmpTarget insn
    _ -> Just $ addr + sz
  where
    addr = address insn
    sz = size insn

branch insn = case (instr insn, groups insn) of
    (X86.X86InsJmp, _) ->
        Nothing
    (_, grps) ->
        if elem X86.X86GrpJump grps then Just $ jmpTarget insn else Nothing

getCall :: Addr -> Insn -> Maybe Addr
getCall addr insn = Nothing

extractCalls :: InsnMap -> CallMap
extractCalls = Map.foldrWithKey addInsn Map.empty
  where 
    addInsn addr insn calls = maybe calls (addCall calls addr) (getCall addr insn)
    addCall calls addr target = Map.insert addr target calls

recDecent cfg [] = cfg
recDecent cfg (addr:wl) =
    case disassInstr e addr of
        Nothing -> recDecent cfg wl
        Just insn -> recDecent cfg' wl'
          where
            cfg' = Cfg {elf = e, insns = insns', succs = succs', branches = branches'}
            wl' = filter (not . visited) ((maybeToList s) ++ (maybeToList b))
            visited addr = Map.member addr insns'             
            insns' = Map.insert addr insn insns
            succs' = Map.insert addr s succs
            branches' = Map.insert addr b branches
            s = succ insn
            b = branch insn
            addr = address insn
  where
    Cfg {elf = e, insns = insns, succs = succs, branches = branches} = cfg

construct :: Elf -> Addr -> Cfg
construct elf addr = recDecent init [addr]
  where 
    init = Cfg {elf = elf, insns = Map.empty, succs = Map.empty, branches = Map.empty}
