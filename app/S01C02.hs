module Main where

import Encoding
import Xor

main :: IO ()
main = do putStrLn "Set 01, Challenge 02\n"

          let hex = "1c0111001f010100061a024b53535009181c"
          let key = "686974207468652062756c6c277320657965"

          putStrLn $ bytes2hex $ xorFixed (hex2bytes hex) (hex2bytes key)
