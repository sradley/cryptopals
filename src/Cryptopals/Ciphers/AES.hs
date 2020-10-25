module Cryptopals.Ciphers.AES
( encipherECB
, decipherECB
, encipherCBC
, decipherCBC
) where

import           Cryptopals.Util        as Util
import           Cryptopals.Ciphers.XOR as XOR
import qualified Data.ByteString        as BS
import           Crypto.Cipher.AES      as AES
import           Crypto.Cipher.Types    ( BlockCipher (ecbEncrypt, ecbDecrypt)
                                        , Cipher      (cipherInit)
                                        )
import           Crypto.Error           (throwCryptoError)

-- |Encrypt ByteString under AES in ECB mode.
encipherECB :: BS.ByteString -> BS.ByteString -> BS.ByteString
encipherECB xs k = BS.concat $ map (encipherBlock k) blocks
    where
        blocks = Util.chunks xs 16

-- |Decrypt ByteString under AES in ECB mode.
decipherECB :: BS.ByteString -> BS.ByteString -> BS.ByteString
decipherECB xs k = BS.concat $ map (decipherBlock k) blocks
    where
        blocks = Util.chunks xs 16

-- |Encrypt ByteString under AES in CBC mode.
encipherCBC :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
encipherCBC xs iv k = BS.concat $ enc (Util.chunks xs 16) iv
    where
        enc (y:ys) z = enc' y z:enc ys (enc' y z)
        enc     [] _ = []
        enc'     y z = encipherBlock k (XOR.fixedLength z y)

-- |Decrypt ByteString under AES in CBC mode.
decipherCBC :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
decipherCBC xs iv k = BS.concat $ dec (Util.chunks xs 16) iv
    where 
        dec (y:ys) z = dec' y z:dec ys y
        dec     [] _ = []
        dec'     y z = XOR.fixedLength z (decipherBlock k y)

encipherBlock :: BS.ByteString -> BS.ByteString -> BS.ByteString
encipherBlock k xs = ecbEncrypt key xs
    where
        key :: AES.AES128
        key = throwCryptoError $ cipherInit k

decipherBlock :: BS.ByteString -> BS.ByteString -> BS.ByteString
decipherBlock k xs = ecbDecrypt key xs
    where
        key :: AES.AES128
        key = throwCryptoError $ cipherInit k
