module Main where

import Encoding
import Xor
import Score

import Data.ByteString (ByteString)

-- XORs the ciphertext against every possible character.
encipherAll :: ByteString -> [(ByteString, Int)]
encipherAll b = [(xorSingle b (fromIntegral i), i) | i <- [0..127]]

-- Scores each plaintext in the list of tuples.
scoreAll :: [(ByteString, Int)] -> [(Score, Int)]
scoreAll ls = let genScore (x, y) = (Score x (score x), y)
              in map genScore ls

main :: IO ()
main = do putStrLn "Set 01, Challenge 03\n"

          let hex = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

          print $ (maximum . scoreAll . encipherAll . hex2bytes) hex
