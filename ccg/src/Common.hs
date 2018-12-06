module Common
    ( Word64
    , Addr
    , SizeT
    , Id
    , Loc
    , RegMode(..)
    , Width(..)
    , CmpOp(..)
    , Symbol(..)
    , trace
    , trace'
    , bug
    , toPreds
    ) where

import Debug.Trace
import Data.Word
import Data.Int

import qualified Data.Map as Map

type Addr = Word64
type SizeT = Word64
type Id = Int
type Loc = (Addr, Int) -- bb addr, idx

data RegMode =
      L -- low 8
    | H -- high 8
    | X -- low 16
    | E -- low 32
    | R -- full 64
    deriving (Eq, Show)

data Width =
      Byte
    | Word -- 16 bit
    | Dword -- 32 bit
    | Qword -- 64 bit
    | Oword -- 128 bit
    deriving (Eq, Show)

data CmpOp =
      Eq
    | Ne
    -- signed
    | Gt
    | Ge
    | Lt
    | Le
    -- unsigned
    | A
    | Ae
    | B
    | Be
    deriving (Eq)

data Symbol =
      Named String Addr
    | Address Addr
    deriving (Eq)

instance Show CmpOp where
    show Eq = "=="
    show Ne = "!="
    show Gt = ">"
    show Ge = ">="
    show Lt = "<"
    show Le = "<="
    show A = ">u"
    show Ae = ">=u"
    show B = "<u"
    show Be = "<=u"

instance Show Symbol where
    show (Named str addr) = str -- ++ "[" ++ show addr ++ "]"
    show (Address addr) = "fun_" ++ show addr

trace' f a = trace (f a) a

bug a = trace' show a

toPreds :: (Ord a) => a -> Map.Map a [a] -> Map.Map a [a]
toPreds entry succs =
    Map.foldrWithKey addSuccs (Map.singleton entry []) succs
  where
    addSuccs loc succs preds = foldr (addSucc loc) preds succs
    addSucc loc succ preds = Map.insertWith (++) succ [loc] preds
