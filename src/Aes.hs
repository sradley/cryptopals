module Aes
( ecb128enc
, ecb128dec
, cbc128enc
, cbc128dec
) where

import Xor  (xorFixed)
import Util (chunkify)

import Data.ByteString     (ByteString, empty)
import Crypto.Cipher.AES   (AES128)
import Crypto.Cipher.Types (BlockCipher (ecbEncrypt, ecbDecrypt),
                            Cipher (cipherInit))
import Crypto.Error        (throwCryptoError)

-- Encrypt ByteString under AES in ECB mode.
ecb128enc :: ByteString -> ByteString -> ByteString
ecb128enc bs k = foldr (<>) empty $ map (aes128enc k) (chunkify bs 16)

-- Decrypt ByteString under AES in ECB mode.
ecb128dec :: ByteString -> ByteString -> ByteString
ecb128dec bs k = foldr (<>) empty $ map (aes128dec k) (chunkify bs 16)

-- Encrypt ByteString under AES in CBC mode.
cbc128enc :: ByteString -> ByteString -> ByteString -> ByteString
cbc128enc bs iv k = let enc (x:xs) y = curr x y : enc xs (curr x y) 
                        enc     [] _ = []
                        curr     x y = aes128enc k (xorFixed y x)
                    in foldr (<>) empty $ enc (chunkify bs 16) iv

-- Decrypt ByteString under AES in CBC mode
cbc128dec :: ByteString -> ByteString -> ByteString -> ByteString
cbc128dec bs iv k = let dec (x:xs) y = xorFixed y (aes128dec k x) : dec xs x 
                        dec     [] _ = []
                    in foldr (<>) empty $ dec (chunkify bs 16) iv

-- Encrypt ByteString (of length 16) under AES.
aes128enc :: ByteString -> ByteString -> ByteString
aes128enc k bs = ecbEncrypt (key128 k) bs

-- Decrypt ByteString (of length 16) under AES.
aes128dec :: ByteString -> ByteString -> ByteString
aes128dec k bs = ecbDecrypt (key128 k) bs

key128 :: ByteString -> AES128
key128 = throwCryptoError . cipherInit
