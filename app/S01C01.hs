module Main where

import Encoding

main :: IO ()
main = do putStrLn "Set 01, Challenge 01\n"

          let hex = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6\
                    \e6f7573206d757368726f6f6d"

          print $ (base64encode . hex2bytes) hex
