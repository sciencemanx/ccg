module Common
    ( Addr
    , SizeT
    , RegMode(..)
    , Width(..)
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

trace' f a = trace (f a) a