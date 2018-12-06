
module ReadElf
    ( readElf
    , symbol2addr
    , addr2symbol
    , readAddr
    ) where

import Debug.Trace (trace)

import qualified Data.ByteString.Char8

import Data.ByteString (ByteString, unpack)      
import Data.List (find)
import System.IO.MMap (mmapFileByteString)
import Data.Elf (parseElf, parseSymbolTables, Elf(..), ElfSegment(..), ElfSymbolTableEntry(..))
import Data.Word (Word8, Word64)

readElf :: String -> IO (Elf)
readElf filename = do
    file <- mmapFileByteString filename Nothing
    return $ parseElf file

symbol2addr :: Elf -> ByteString -> Maybe Word64
symbol2addr elf sym = fmap steValue $ find matchingName symtab
  where 
    symtab = concat $ parseSymbolTables elf
    matchingName symEntry = case steName symEntry of
        (_, Just name) -> name == sym
        _ -> False

addr2symbol :: Elf -> Word64 -> Maybe String
addr2symbol elf addr = extractName $ find rightAddr symtab
  where 
    symtab = concat $ parseSymbolTables elf
    rightAddr symEntry = steValue symEntry == addr
    extractName :: Maybe ElfSymbolTableEntry -> Maybe String
    extractName entry = case entry of
      Just entry -> case steName entry of
        (_, Just name) -> Just $ Data.ByteString.Char8.unpack name
        _ -> Nothing
      Nothing -> Nothing

findSeg :: [ElfSegment] -> Word64 -> Maybe ElfSegment
findSeg segments addr = find inSegment segments
  where
    inSegment ElfSegment{ elfSegmentVirtAddr = start, elfSegmentMemSize = len } =
        start <= addr && addr < (start + len)

readSeg :: ElfSegment -> Word64 -> Word64 -> [Word8]
readSeg seg addr len = drop offset wordbuf
  where
    wordbuf = unpack buf
    offset = fromIntegral $ addr - start
    ElfSegment{ elfSegmentVirtAddr = start, elfSegmentData = buf } = seg

readAddr :: Elf -> Word64 -> Word64 -> [Word8]
readAddr elf addr 0 = []
readAddr elf addr len =
    case maybe_seg of
        Nothing -> []
        Just seg -> if remaining == 0 then bytes else bytes ++ (readAddr elf addr' remaining)
          where
            addr' = addr + (fromIntegral $ length bytes)
            remaining = len - (fromIntegral $ length bytes)
            bytes = readSeg seg addr len
  where
    maybe_seg = findSeg (elfSegments elf) addr
