module Lib
    ( disass
    ) where

import Data.Maybe (fromMaybe)
import Data.ByteString.Char8 (pack)

import ReadElf (readElf, symbol2addr)
import X86.Cfg (construct)

disass :: String -> IO ()
disass filename = do
    elf <- readElf filename
    let addr = fromMaybe 0x63a (symbol2addr elf (pack "main"))
    let cfg = construct elf addr
    putStrLn $ show cfg
    return ()
