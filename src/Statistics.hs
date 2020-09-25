module Statistics
( hammingDist
, hammingDistNorm
, hammingDistNormA
) where

import Data.ByteString as BS (ByteString, zipWith)
import Data.Bits             (popCount, xor)
import Util                  (chunkify)

-- Calculate hamming distance (int bits) between two ByteStrings.
hammingDist :: ByteString -> ByteString -> Double
hammingDist xs ys = sum $ map (fromIntegral . popCount) $ BS.zipWith xor xs ys

-- Calculate the normalised hamming distance for a given keysize.
hammingDistNorm :: Int -> ByteString -> ByteString -> Double
hammingDistNorm i xs ys = (hammingDist xs ys) / (fromIntegral i)

-- ...
hammingDistNormA :: ByteString -> Int -> Double
hammingDistNormA xs i = let blcks = chunkify xs i
                        in (sum $ map (hammingDistNorm i (blcks !! 0)) blcks) /
                           (fromIntegral $ length blcks)
