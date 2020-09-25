module Aes
( ecb128enc
, ecb128dec
) where

import Data.ByteString as BS (ByteString, empty)
import Crypto.Cipher.AES     (AES128)
import Crypto.Cipher.Types   ( BlockCipher (ecbEncrypt, ecbDecrypt)
                             , Cipher (cipherInit) )
import Crypto.Error          (throwCryptoError)
import Util                  (chunkify)

-- Encrypt ByteString under AES in ECB mode.
ecb128enc :: ByteString -> ByteString -> ByteString
ecb128enc bs k = foldr (<>) empty $ map (aes128enc k) (chunkify bs 16)

-- Decrypt ByteString under AES in ECB mode.
ecb128dec :: ByteString -> ByteString -> ByteString
ecb128dec bs k = foldr (<>) empty $ map (aes128dec k) (chunkify bs 16)

-- Encrypt ByteString (of length 16) under AES.
aes128enc :: ByteString -> ByteString -> ByteString
aes128enc k bs = ecbEncrypt (key128 k) bs

-- Decrypt ByteString (of length 16) under AES.
aes128dec :: ByteString -> ByteString -> ByteString
aes128dec k bs = ecbDecrypt (key128 k) bs

key128 :: ByteString -> AES128
key128 = throwCryptoError . cipherInit
