module Common
    ( Addr
    , SizeT
    , RegMode(..)
    , Width(..)
    , CmpOp(..)
    , trace
    , trace'
    ) where

import Debug.Trace
import Data.Word

type Addr = Word64
type SizeT = Word64

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

instance Show CmpOp where
    show Eq = "=="
    show Ne = "!="
    show Gt = ">s"
    show Ge = ">=s"
    show Lt = "<s"
    show Le = "<=s"
    show A = ">"
    show Ae = ">="
    show B = "<"
    show Be = "<="

trace' f a = trace (f a) a