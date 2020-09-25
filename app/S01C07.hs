module Main where

import Data.ByteString (empty)
import Encoding        (ascii2bytes, bytes2ascii, base64decode)
import Aes             (ecb128dec)

main :: IO ()
main = do putStrLn "Set 01, Challenge 07\n"

          -- Read in ciphertext. 
          fp <- readFile "data/7.txt"
          let cs = base64decode $ foldr (<>) empty $ map ascii2bytes (lines fp)

          -- Decrypt ciphertext.
          putStrLn $ bytes2ascii $ ecb128dec cs (ascii2bytes "YELLOW SUBMARINE")
          
