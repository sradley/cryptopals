module Cryptopals.Challenges.Challenge01
( challenge01
) where

import qualified Cryptopals.Encoding.Base64 as Base64
import qualified Cryptopals.Encoding.Hex    as Hex

-- |Set 01, Challenge 01 solution.
challenge01 :: IO ()
challenge01 = print $ (Base64.encode . Hex.decode) hex

hex :: String
hex = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6\
      \e6f7573206d757368726f6f6d"
