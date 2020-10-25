module Cryptopals.Challenges.Challenge05
( challenge05
) where

import qualified Cryptopals.Encoding.ASCII as ASCII
import qualified Cryptopals.Encoding.Hex   as Hex
import           Cryptopals.Ciphers.XOR    as XOR
import qualified Data.ByteString           as BS

challenge05 :: IO ()
challenge05 = print $ Hex.encode $ XOR.repeatingKey plaintext key
    where
        key = ASCII.decode "ICE"

plaintext :: BS.ByteString
plaintext = ASCII.decode "Burning 'em, if you ain't quick and nimble\n\
                         \I go crazy when I hear a cymbal"
