module Main where

import Data.ByteString as BS (ByteString, empty, pack)
import Encoding              (ascii2bytes, bytes2ascii, base64decode)
import Statistics            (hammingDistNormA)
import Xor                   (xorSingle, xorRK)
import Score                 (Score (..), score)
import Util                  (transpose)

-- XORs the ciphertext against every possible character.
encipherAll :: ByteString -> [(ByteString, Int)]
encipherAll b = [(xorSingle b (fromIntegral i), i) | i <- [0..127]]

-- Scores each plaintext in the list of tuples.
scoreAll :: [(ByteString, Int)] -> [(Score, Int)]
scoreAll ls = let genScore (x, y) = (Score x (score x), y)
              in map genScore ls

-- Solves single byte xor.
solveXorSingle :: ByteString -> Int 
solveXorSingle = snd . maximum . scoreAll . encipherAll

-- Find the keysize with the best (smallest) normalized hamming distance.
findKeySize :: ByteString -> Int
findKeySize bs = snd $ minimum [(hammingDistNormA bs i, i) | i <- [2..40]]

-- Finds the key for a given keysize.
solveForKey :: ByteString -> Int -> ByteString
solveForKey cs ks = pack $ map (fromIntegral . solveXorSingle) $ blocks
                    where blocks = transpose cs ks

main :: IO ()
main = do putStrLn "Set 01, Challenge 06\n"

          -- Read in ciphertext. 
          fp <- readFile "data/6.txt"
          let cs = base64decode $ foldr (<>) empty $ map ascii2bytes (lines fp)

          -- Find most likely key.
          let ks = findKeySize cs

          -- Solve for key.
          let key = solveForKey cs ks

          -- Decrypt ciphertext.
          putStrLn $ bytes2ascii $ xorRK cs key
          
