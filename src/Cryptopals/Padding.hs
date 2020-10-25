module Cryptopals.Padding
( pkcs7
) where

import qualified Data.ByteString as BS

-- |PKCS7 padding implementation.
pkcs7 :: BS.ByteString -> Int -> BS.ByteString
pkcs7 x i = x <> BS.replicate (i - (BS.length x) `mod` i) pad
    where
        pad = fromIntegral (i - BS.length x)
