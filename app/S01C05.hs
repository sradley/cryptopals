module Main where

import Encoding
import Xor

import Data.ByteString (ByteString)

encrypt :: String -> String -> ByteString
encrypt xs ys = xorRK (ascii2bytes xs) (ascii2bytes ys)

main :: IO ()
main = do putStrLn "Set 01, Challenge 05\n"

          let pt = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a \
                   \cymbal"

          putStrLn $ bytes2hex $ encrypt pt "ICE"
