{-# LANGUAGE ViewPatterns #-}
module Util
( transpose
, chunkify
, pkcs7pad
) where

import Data.ByteString as BS (ByteString, take, drop, empty, uncons, replicate, length)

chunkify :: ByteString -> Int -> [ByteString]
chunkify (uncons -> Nothing) _ = []
chunkify xs i = [BS.take i xs] ++ chunkify (BS.drop i xs) i

transpose :: ByteString -> Int -> [ByteString]
transpose xs i = let nbyte n = foldr (<>) empty $ map (BS.take 1 . BS.drop n) blcks
                 in [nbyte n | n <- [0..(i-1)]]
                 where blcks = chunkify xs i

pkcs7pad :: ByteString -> Int -> ByteString
pkcs7pad bs i = bs <> BS.replicate (i - BS.length bs) pad
                where pad = fromIntegral (i - BS.length bs)
