module Cryptopals.Challenges.Challenge06
( challenge06
) where

import           Cryptopals.Util                       as Util
import qualified Cryptopals.Encoding.Base64            as Base64
import qualified Cryptopals.Encoding.ASCII             as ASCII
import           Cryptopals.Ciphers.XOR                as XOR
import           Cryptopals.Statistics.Scoring         as Score
import           Cryptopals.Statistics.HammingDistance as HD
import qualified Data.ByteString                       as BS 
import           Data.Word
import           Data.List

challenge06 :: IO ()
challenge06 = fileData >>= putStrLn . ASCII.encode . decrypt
    where
        key x     = solveKey x $ keySize x
        decrypt x = XOR.repeatingKey x (key x)

fileData :: IO BS.ByteString
fileData = readFile "data/6.txt" >>= pure . decode
    where
        decode = Base64.decode . BS.concat . map ASCII.decode . lines

decipherAll :: BS.ByteString -> [(BS.ByteString, Word8)]
decipherAll x = [(XOR.singleByte x i, i) | i <- [0..127]] 

scoreAll :: [(BS.ByteString, Word8)] -> [(BS.ByteString, Word8, Double)]
scoreAll = map (\(x, y) -> (x, y, Score.score x))

keySize :: BS.ByteString -> Int
keySize xs = snd $ minimum [(HD.normalized' (blocks i), i) | i <- [2..40]]
    where
        blocks i = Util.chunks xs i

solveKey :: BS.ByteString -> Int -> BS.ByteString
solveKey cs i = BS.pack $ map solveSingle blocks
    where
        blocks = BS.transpose $ Util.chunks cs i

solveSingle :: BS.ByteString -> Word8
solveSingle = out . maximumBy comp . scoreAll . decipherAll
    where
        comp (_, _, x) (_, _, y) = compare x y 
        out            (_, x, _) = x

