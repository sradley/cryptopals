module Aes
( ecb128enc
, ecb128dec
) where

import Data.ByteString (ByteString, empty)
import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types ( BlockCipher (ecbEncrypt, ecbDecrypt)
                           , Cipher (cipherInit) )
import Crypto.Error (throwCryptoError)

-- Encrypt using AES in ECB mode.
ecb128enc :: ByteString -> ByteString -> ByteString
ecb128enc bs k = ecbEncrypt (key128 k) bs

-- Decrypt using AES in ECB mode.
ecb128dec :: ByteString -> ByteString -> ByteString
ecb128dec bs k = ecbDecrypt (key128 k) bs

key128 :: ByteString -> AES128
key128 = throwCryptoError . cipherInit
