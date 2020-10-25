module Cryptopals.Challenges.Challenge04
( challenge04
) where

import qualified Cryptopals.Encoding.Hex       as Hex
import           Cryptopals.Ciphers.XOR        as XOR
import           Cryptopals.Statistics.Scoring as Scoring
import qualified Data.ByteString               as BS 
import           Data.Word
import           Data.List

challenge04 :: IO ()
challenge04 = fileData >>= print . maximumBy comp . map best 
    where
        comp (_, _, x) (_, _, y) = compare x y
        best                     = maximumBy comp . scoreAll . decipherAll

fileData :: IO [BS.ByteString]
fileData = readFile "data/4.txt" >>= pure . (map Hex.decode) . lines

decipherAll :: BS.ByteString -> [(BS.ByteString, Word8)]
decipherAll x = [(XOR.singleByte x i, i) | i <- [0..127]] 

scoreAll :: [(BS.ByteString, Word8)] -> [(BS.ByteString, Word8, Double)]
scoreAll = map (\(x, y) -> (x, y, Scoring.score x))
