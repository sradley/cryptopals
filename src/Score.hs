module Score
( score
, scoreSingle
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Map (Map, fromList, findWithDefault)
import Data.Char (toLower)

-- http://www.macfreek.nl/memory/Letter_Distribution
scores :: Map Char Double
scores = fromList [
  ('a', 6.53216), ('b', 1.25888), ('c', 2.23367), ('d', 3.28292),
  ('e', 10.2666), ('f', 1.98306), ('g', 1.62490), ('h', 4.97856),
  ('i', 5.66844), ('j', 0.09752), ('k', 0.56096), ('l', 3.31754),
  ('m', 2.02656), ('n', 5.71201), ('o', 6.15957), ('p', 1.50432),
  ('q', 0.08367), ('r', 4.98790), ('s', 5.31700), ('t', 7.51699),
  ('u', 2.27579), ('v', 0.79611), ('w', 1.70389), ('x', 0.14092),
  ('y', 1.42766), ('z', 0.05128), (' ', 18.2884)]

-- Calculates a score for a given ByteString based on letter frequency.
score :: ByteString -> Double
score bs = sum $ map scoreSingle (unpack bs)

-- Assigns a score to a given char based on letter frequency.
scoreSingle :: Char -> Double
scoreSingle c = findWithDefault 0 (toLower c) scores 
