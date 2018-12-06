module Lib
    ( disass
    ) where

import Data.Maybe (fromMaybe)
import Data.ByteString.Char8 (pack)

import ReadElf (readElf, symbol2addr)
import X86.Program (construct)
import X86.Lift
import Two.Lift
import Three.Lift
import Tree.Lift

disass :: String -> IO ()
disass filename = do
    elf <- readElf filename
    let addr = fromMaybe 0x63a (symbol2addr elf (pack "main"))
    let cfgX = construct elf addr
    let cfg2 = toTwo cfgX
    let cfg3 = toThree cfg2
    let tree = toTree cfg3
    let ast = toAst tree
    -- putStrLn $ show cfgX
    -- putStrLn "Two Address:"
    -- putStrLn $ show cfg2
    -- putStrLn "Three Address:"
    -- putStrLn $ show cfg3
    -- putStrLn "Tree:"
    -- putStrLn $ show tree
    -- putStrLn "Ast:"
    putStrLn $ show ast
    return ()
