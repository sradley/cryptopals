module Main where

import Encoding (ascii2bytes, bytes2ascii, base64decode)
import Aes (ecb128dec)

main :: IO ()
main = do putStrLn "Set 01, Challenge 07\n"

          -- Read in ciphertext. 
          fp <- readFile "data/4.txt"
          let cs = base64decode $ foldr (<>) empty $ map ascii2bytes (lines fp)

          putStrLn $ bytes2ascii $ ecb128dec cs (ascii2bytes "YELLOW SUBMARINE")
          
