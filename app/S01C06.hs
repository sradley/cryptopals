module Main where

import Data.ByteString as BS (ByteString, empty, pack)
import Encoding (ascii2bytes, bytes2ascii, base64decode)
import Statistics (hammingDistNormA, transpose)
import Xor (xorSingle, xorRK)
import Score (Score (..), score)

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
solveForKey cs ks = let blocks = transpose cs ks
                    in pack $ map (fromIntegral . solveXorSingle) $ blocks

main :: IO ()
main = do putStrLn "Set 01, Challenge 06\n"

          -- Read in ciphertext. 
          fData <- readFile "data/6.txt"
          let ctext = foldr (<>) empty $ map ascii2bytes (lines fData)

          -- Find most likely key.
          let ks = findKeySize (base64decode ctext)

          -- Solve for key.
          let key = solveForKey (base64decode ctext) ks

          putStrLn $ bytes2ascii $ xorRK (base64decode ctext) key
          
