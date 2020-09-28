module Main where

import Encoding
import Util

main :: IO ()
main = do putStrLn "Set 02, Challenge 09\n"
          print $ pkcs7pad (ascii2bytes "YELLOW SUBMARINE") 20
