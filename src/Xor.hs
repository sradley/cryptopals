module Xor
( xorFixed
, xorSingle
, xorRK
) where

import qualified Data.ByteString as BS
import           Data.Bits
import           Data.Word

-- XORs two ByteStrings of equal length together. 
xorFixed :: BS.ByteString -> BS.ByteString -> BS.ByteString
xorFixed xs ys = BS.pack $ BS.zipWith xor xs ys

-- XORs every element of a ByteString with a single byte.
xorSingle :: BS.ByteString -> Word8 -> BS.ByteString
xorSingle xs y = BS.map (xor y) xs

-- XORs a ByteString with a repeating key.
xorRK :: BS.ByteString -> BS.ByteString -> BS.ByteString
xorRK xs ys = xorFixed xs key
              where key = genKey ys ys (BS.length xs)

genKey :: BS.ByteString -> BS.ByteString -> Int -> BS.ByteString
genKey xs ys i | BS.length ys >= i = BS.take i ys
               | otherwise         = genKey xs (xs <> ys) i 
