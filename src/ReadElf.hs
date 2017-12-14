
module ReadElf
    ( readElf
--     , symbol2addr
--     , readAddr
    ) where

import Data.ByteString (ByteString)        
import System.IO.MMap (mmapFileByteString)
import Data.Elf (parseElf, Elf)

readElf :: String -> IO (Elf)
readElf filename = do
    file <- mmapFileByteString filename Nothing
    return $ parseElf file

-- symbol2addr :: Elf -> ByteString -> Word64
-- symbol2addr elf sym =

-- readAddr :: Elf -> Word64 -> Word64 -> [Word8]
-- readAddr elf addr len =