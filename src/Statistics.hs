{-# LANGUAGE ViewPatterns #-}
module Statistics
( hammingDist
, hammingDistNorm
, hammingDistNormA
, transpose
, chunkify
) where

import Data.ByteString as BS (ByteString, zipWith, take, drop, uncons, empty)
import Data.Bits (popCount, xor)

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

chunkify :: ByteString -> Int -> [ByteString]
chunkify (uncons -> Nothing) _ = []
chunkify xs i = [BS.take i xs] ++ chunkify (BS.drop i xs) i

transpose :: ByteString -> Int -> [ByteString]
transpose xs i = let blcks   = chunkify xs i
                     nbyte n = foldr (<>) empty $ map (BS.take 1 . BS.drop n) blcks
                 in [nbyte n | n <- [0..(i-1)]]
