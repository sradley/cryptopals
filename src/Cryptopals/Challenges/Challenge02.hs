module Cryptopals.Challenges.Challenge02
( challenge02
) where

import qualified Cryptopals.Encoding.Hex as Hex
import           Cryptopals.Ciphers.XOR  as XOR
import qualified Data.ByteString         as BS

challenge02 :: IO ()
challenge02 = print $ Hex.encode $ XOR.fixedLength hex key

hex :: BS.ByteString
hex = Hex.decode "1c0111001f010100061a024b53535009181c"

key :: BS.ByteString
key = Hex.decode "686974207468652062756c6c277320657965"
