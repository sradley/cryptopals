module Main where

import Data.ByteString as BS (ByteString)
import Data.List       (nub)
import Encoding        (hex2bytes, bytes2hex)
import Util            (chunkify)

repeated :: ByteString -> Int
repeated bs = let chunks = chunkify bs 16
              in length chunks - (length . nub) chunks

main :: IO ()
main = do putStrLn "Set 01, Challenge 08\n"

          -- Read data from file.
          fp <- readFile "data/8.txt"
          let cs = map hex2bytes (lines fp)

          let rs = map (\x -> (repeated x, x)) cs
          print $ (bytes2hex . snd) $ maximum rs
