module Main where

import Encoding (ascii2bytes)
import Util     (pkcs7pad) 

main :: IO ()
main = do putStrLn "Set 02, Challenge 09\n"
          print $ pkcs7pad (ascii2bytes "YELLOW SUBMARINE") 20
