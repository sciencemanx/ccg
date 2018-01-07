module Lib
    ( disass
    ) where

import Data.Maybe (fromMaybe)
import Data.ByteString.Char8 (pack)

import ReadElf (readElf, symbol2addr)
import X86.Cfg (construct)
import X86.Lift
import SSA.Temp

disass :: String -> IO ()
disass filename = do
    elf <- readElf filename
    let addr = fromMaybe 0x63a (symbol2addr elf (pack "main"))
    let cfgX = construct elf addr
    let cfg2 = toTwo cfgX
    let t = new ()
    putStrLn $ show t
    let t' = new ()
    putStrLn $ show t'
    -- putStrLn $ show cfgX
    putStrLn $ show cfg2
    return ()
