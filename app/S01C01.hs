module Main where

import Encoding (base64encode, hex2bytes)

hex :: String
hex = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6\
      \e6f7573206d757368726f6f6d"

main :: IO ()
main = do putStrLn "Set 01, Challenge 01\n"
          putStrLn hex
          print $ (base64encode . hex2bytes) hex
