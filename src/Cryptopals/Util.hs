{-# LANGUAGE ViewPatterns #-}
module Cryptopals.Util
( chunks
, repeated
) where

import qualified Data.ByteString as BS
import           Data.List

-- |Breaks a ByteString into an array of n-sized chunks.
chunks :: BS.ByteString -> Int -> [BS.ByteString]
chunks (BS.uncons -> Nothing) _ = [] 
chunks                     xs i = [BS.take i xs] ++ chunks (BS.drop i xs) i 

-- |...
repeated :: BS.ByteString -> Int -> Int
repeated x i = length blocks - (length . nub) blocks
    where
        blocks = chunks x i
