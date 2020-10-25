module Cryptopals.Challenges.Challenge03
( challenge03
) where

import qualified Cryptopals.Encoding.Hex       as Hex
import           Cryptopals.Ciphers.XOR        as XOR
import           Cryptopals.Statistics.Scoring as Scoring
import qualified Data.ByteString               as BS 
import           Data.Word
import           Data.List

challenge03 :: IO ()
challenge03 = print $ best hex
    where
        comp (_, _, x) (_, _, y) = compare x y
        best                     = maximumBy comp . scoreAll . decipherAll

hex :: BS.ByteString
hex = Hex.decode "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a\
                 \393b3736"

decipherAll :: BS.ByteString -> [(BS.ByteString, Word8)]
decipherAll x = [(XOR.singleByte x i, i) | i <- [0..127]] 

scoreAll :: [(BS.ByteString, Word8)] -> [(BS.ByteString, Word8, Double)]
scoreAll = map (\(x, y) -> (x, y, Scoring.score x))

