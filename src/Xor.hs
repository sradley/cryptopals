module Xor
( xorFixed
, xorSingle
, xorRK
) where

import Data.ByteString as BS (ByteString, zipWith, pack, map, length, take)
import Data.Bits             (xor)
import Data.Word             (Word8)

-- XORs two ByteStrings of equal length together. 
xorFixed :: ByteString -> ByteString -> ByteString
xorFixed xs ys = pack $ BS.zipWith xor xs ys

-- XORs every element of a ByteString with a single byte.
xorSingle :: ByteString -> Word8 -> ByteString
xorSingle xs y = BS.map (xor y) xs

-- XORs a ByteString with a repeating key.
xorRK :: ByteString -> ByteString -> ByteString
xorRK xs ys = let key = genKey ys ys (BS.length xs) 
              in xorFixed xs key

genKey :: ByteString -> ByteString -> Int -> ByteString
genKey xs ys i | BS.length ys >= i = BS.take i ys
               | otherwise         = genKey xs (xs <> ys) i 
