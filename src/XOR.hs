module XOR where

import Data.ByteString as BS (ByteString, zipWith, pack, unpack)
import Data.Word (Word8)
import Data.Bits (xor)

xorfixed :: ByteString -> ByteString -> ByteString
xorfixed x y = pack $ BS.zipWith xor x y

xorsingle :: ByteString -> Word8 -> ByteString
xorsingle x y = pack $ map (xor y) (unpack x)
