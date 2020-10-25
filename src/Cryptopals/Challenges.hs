module Cryptopals.Challenges
( challenge 
) where

import Cryptopals.Challenges.Challenge01 as Challenge01
import Cryptopals.Challenges.Challenge02 as Challenge02
import Cryptopals.Challenges.Challenge03 as Challenge03
import Cryptopals.Challenges.Challenge04 as Challenge04
import Cryptopals.Challenges.Challenge05 as Challenge05
import Cryptopals.Challenges.Challenge06 as Challenge06
import Cryptopals.Challenges.Challenge07 as Challenge07
import Cryptopals.Challenges.Challenge08 as Challenge08
import Cryptopals.Challenges.Challenge09 as Challenge09
import Cryptopals.Challenges.Challenge10 as Challenge10
import Cryptopals.Challenges.Challenge11 as Challenge11
import Cryptopals.Challenges.Challenge12 as Challenge12
import Data.Map                          as Map

-- |Challenge lookup function.
challenge :: Int -> Maybe (IO ())
challenge i = Map.lookup i challenges

challenges :: Map.Map Int (IO ())
challenges = Map.fromList [ ( 1, challenge01)
                          , ( 2, challenge02) 
                          , ( 3, challenge03)
                          , ( 4, challenge04)
                          , ( 5, challenge05)
                          , ( 6, challenge06)
                          , ( 7, challenge07)
                          , ( 8, challenge08)
                          , ( 9, challenge09)
                          , (10, challenge10)
                          , (11, challenge11)
                          , (12, challenge12) ]
