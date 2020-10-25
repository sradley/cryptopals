module Cryptopals.Statistics.HammingDistance
( distance
, normalized
, normalized'
) where

import qualified Data.ByteString as BS
import           Data.Bits

-- |Calculates the hamming distance (in terms of bits) between two ByteStrings. 
distance :: BS.ByteString -> BS.ByteString -> Double
distance x y = sum $ map (fromIntegral . popCount) $ BS.zipWith xor x y

-- |Calculates the normalized hamming distance between two ByteStrings.
normalized :: BS.ByteString -> BS.ByteString -> Double
normalized x y = distance x y / (fromIntegral . BS.length) x

-- |...
normalized' :: [BS.ByteString] -> Double
normalized' x = sum (distances x) / (fromIntegral . length) x
    where
        distances = map (normalized (x !! 0))
