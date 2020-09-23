module Main where

import Encoding (hex2bytes, bytes2hex)
import Xor (xorFixed)

hex :: String
hex = "1c0111001f010100061a024b53535009181c"

key :: String
key = "686974207468652062756c6c277320657965"

main :: IO ()
main = do
    putStrLn "Set 01, Challenge 02"
    putStrLn hex
    putStrLn key
    putStrLn $ bytes2hex $ xorFixed (hex2bytes hex) (hex2bytes key)
