module Cryptopals.Ciphers.XOR
( fixedLength
, singleByte
, repeatingKey
) where

import qualified Data.ByteString as BS
import           Data.Bits
import           Data.Word

-- |XORs two ByteString of equal length together.
fixedLength :: BS.ByteString -> BS.ByteString -> BS.ByteString 
fixedLength xs ys = BS.pack $ BS.zipWith xor xs ys

-- |XORs every element of a ByteString with a single byte.
singleByte :: BS.ByteString -> Word8 -> BS.ByteString
singleByte xs y = BS.map (xor y) xs

-- |XORs a ByteString with a repeating key.
repeatingKey :: BS.ByteString -> BS.ByteString -> BS.ByteString
repeatingKey xs ys = fixedLength xs key
    where
        key = generateKey ys ys (BS.length xs)
        generateKey as bs i
            | BS.length bs >= i = BS.take i bs
            | otherwise         = generateKey as (as <> bs) i
