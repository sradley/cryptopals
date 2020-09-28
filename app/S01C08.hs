module Main where

import Encoding
import Util

import Data.ByteString (ByteString)

ecbDetect :: ByteString -> Bool
ecbDetect bs = repeated bs > 0

main :: IO ()
main = do putStrLn "Set 01, Challenge 08\n"

          -- Read data from file.
          fp <- readFile "data/8.txt"
          let cs = map hex2bytes (lines fp)

          print $ map bytes2hex $ filter ecbDetect cs
