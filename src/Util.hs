{-# LANGUAGE ViewPatterns #-}
module Util
( transposify
, chunkify
, repeated
, pkcs7pad
, randBytes
) where

import qualified Data.ByteString as BS
import           Data.List
import           Control.Monad.Random

chunkify :: BS.ByteString -> Int -> [BS.ByteString]
chunkify (BS.uncons -> Nothing) _ = []
chunkify xs i = [BS.take i xs] ++ chunkify (BS.drop i xs) i

transposify :: BS.ByteString -> Int -> [BS.ByteString]
transposify xs i = let nbyte n = foldr (<>) BS.empty $ map (BS.take 1 . BS.drop n) blocks
                   in [nbyte n | n <- [0..(i-1)]]
                   where blocks = chunkify xs i

repeated :: BS.ByteString -> Int
repeated bs = length chunks - (length . nub) chunks
              where chunks = chunkify bs 16

pkcs7pad :: BS.ByteString -> Int -> BS.ByteString
pkcs7pad bs i = bs <> BS.replicate (i - (BS.length bs) `mod` i) pad
                where pad = fromIntegral (i - BS.length bs)

randBytes :: MonadRandom m => Int -> m BS.ByteString
randBytes n = BS.pack . take n <$> getRandomRs (minBound, maxBound)
