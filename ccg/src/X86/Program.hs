module X86.Program
    ( Program(..)
    , SubMap
    , construct
    ) where

import Common

import qualified Data.Map as Map
import Data.Elf (Elf)

import qualified X86.Cfg as Cfg

type SubMap = Map.Map Addr Cfg.Cfg
data Program = Program Addr SubMap deriving (Eq, Show)

construct :: Elf -> Addr -> Program
construct e addr = Program addr subs
  where
    subs = construct' Map.empty [addr]
    construct' subs [] = subs
    construct' subs (addr:wl) = case Map.lookup addr subs of
        Just _ -> construct' subs wl
        Nothing -> construct' (Map.insert addr cfg subs) (calls ++ wl)
          where
            calls = Cfg.mkCalls cfg
            cfg = Cfg.construct e addr
