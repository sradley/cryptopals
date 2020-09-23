module Main where

import Base64 (bytes2ascii, bytes2base64, hex2bytes)

hex :: String
hex = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6\
      \e6f7573206d757368726f6f6d"

main :: IO ()
main = do
    putStrLn "Set 01, Challenge 01"
    putStrLn hex
    putStrLn $ (bytes2ascii . bytes2base64 . hex2bytes) hex
