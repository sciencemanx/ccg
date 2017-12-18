{-# LANGUAGE TemplateHaskell #-}


module Lib
    ( disass
    ) where

import Data.Maybe (fromMaybe)
import Data.ByteString.Char8 (pack)

import ReadElf (readElf, symbol2addr)
import X86.Cfg (construct)
import X86.Lift

disass :: String -> IO ()
disass filename = do
    elf <- readElf filename
    let addr = fromMaybe 0x63a (symbol2addr elf (pack "main"))
    let cfgX = construct elf addr
    let cfg2 = toTwo cfgX
    putStrLn $ show cfgX
    putStrLn $ show cfg2
    return ()
