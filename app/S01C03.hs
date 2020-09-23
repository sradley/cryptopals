module Main where

import Base64 (hex2bytes, bytes2hex)
import XOR (xorfixed)

hex :: String
hex = "1c0111001f010100061a024b53535009181c"

main :: IO ()
main = do
    putStrLn "Set 01, Challenge 02"
    putStrLn hex
    putStrLn key
    putStrLn $ bytes2hex $ xorfixed (hex2bytes hex) (hex2bytes key)
