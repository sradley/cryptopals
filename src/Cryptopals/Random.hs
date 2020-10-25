module Cryptopals.Random
( randBytes
) where

import qualified Data.ByteString        as BS
import           Data.Word
import           System.Random          (StdGen, getStdGen, randomR, random)

randBytes :: (Int, StdGen) -> ([Word8], StdGen)
randBytes x = randBytes' x []

randBytes' :: (Int, StdGen) -> [Word8] -> ([Word8], StdGen)
randBytes' (0, g) xs = (xs, g)
randBytes' (n, g) xs = randBytes' (n - 1, g') (b:xs) 
    where
        (b, g') = random g
