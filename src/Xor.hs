module Xor where

import Data.ByteString as BS (ByteString, zipWith, pack, map)
import Data.Bits (xor)
import Data.Word (Word8)

-- XORs two ByteStrings of equal length together. 
xorFixed :: ByteString -> ByteString -> ByteString
xorFixed x y = pack $ BS.zipWith xor x y

-- XORs every element of a ByteString with a single byte.
xorSingle :: ByteString -> Word8 -> ByteString
xorSingle x y = BS.map (xor y) x

