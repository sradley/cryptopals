module Statistics
( hammingDist
, hammingDistNorm
, hammingDistNormA
) where

import Util

import Data.ByteString as BS (ByteString, zipWith)
import Data.Bits

-- Calculate hamming distance (int bits) between two ByteStrings.
hammingDist :: ByteString -> ByteString -> Double
hammingDist xs ys = sum $ map (fromIntegral . popCount) $ BS.zipWith xor xs ys

-- Calculate the normalised hamming distance for a given keysize.
hammingDistNorm :: Int -> ByteString -> ByteString -> Double
hammingDistNorm i xs ys = (hammingDist xs ys) / (fromIntegral i)

-- ...
hammingDistNormA :: ByteString -> Int -> Double
hammingDistNormA xs i = (sum $ map (hammingDistNorm i (blcks !! 0)) blcks) /
                           (fromIntegral $ length blcks)
                        where blcks = chunkify xs i
