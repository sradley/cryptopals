module Main where

import Encoding
import Aes

import Data.ByteString as BS (empty, replicate)

main :: IO ()
main = do putStrLn "Set 02, Challenge 10\n"

          -- Read contents of file.
          fp <- readFile "data/10.txt"
          let cs = base64decode $ foldr (<>) empty $ map ascii2bytes (lines fp)

          -- Initialise IV and key.
          let iv = BS.replicate 16 0
          let k = ascii2bytes "YELLOW SUBMARINE"

          -- Decrypt ciphertext.
          putStrLn $ bytes2ascii $ cbc128dec cs iv k
