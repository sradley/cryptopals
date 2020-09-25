{-# LANGUAGE ViewPatterns #-}
module Util
( transpose
, chunkify
) where

import Data.ByteString as BS (ByteString, take, drop, empty, uncons)

chunkify :: ByteString -> Int -> [ByteString]
chunkify (uncons -> Nothing) _ = []
chunkify xs i = [BS.take i xs] ++ chunkify (BS.drop i xs) i

transpose :: ByteString -> Int -> [ByteString]
transpose xs i = let blcks   = chunkify xs i
                     nbyte n = foldr (<>) empty $ map (BS.take 1 . BS.drop n) blcks
                 in [nbyte n | n <- [0..(i-1)]]
