module Cryptopals.Challenges.Challenge09
( challenge09
) where

import qualified Cryptopals.Encoding.ASCII as ASCII
import           Cryptopals.Padding        as Pad

challenge09 :: IO ()
challenge09 = print $ Pad.pkcs7 (ASCII.decode "YELLOW SUBMARINE") 20
