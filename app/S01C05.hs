module Main where

import Data.ByteString (ByteString)
import Encoding        (ascii2bytes, bytes2hex)
import Xor             (xorRK)

ptext :: String
ptext = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a \
        \cymbal"

encrypt :: String -> String -> ByteString
encrypt xs ys = xorRK (ascii2bytes xs) (ascii2bytes ys)

main :: IO ()
main = do putStrLn "Set 01, Challenge 05\n"
          putStrLn ptext
          putStrLn $ bytes2hex $ encrypt ptext "ICE"
