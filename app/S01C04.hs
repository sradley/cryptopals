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
main = do putStrLn "Set 01, Challenge 04\n"

          -- Read in ciphertexts. 
          fp <- readFile "data/4.txt"
          let ctexts = map hex2bytes $ lines fp

          print $ maximum $ map (maximum . scoreAll . encipherAll) $ ctexts
