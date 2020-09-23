module Main where

import Data.ByteString (ByteString)
import Encoding (hex2bytes)
import Xor (xorSingle)
import Score (score)

data Scored = Scored { ctext :: ByteString
                     , key   :: Int
                     , value :: Double
                     } deriving (Show, Eq)

instance Ord Scored where
  (Scored _ _ v1) `compare` (Scored _ _ v2) = v1 `compare` v2

hex :: String
hex = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

encipherAll :: ByteString -> [(ByteString, Int)]
encipherAll b = [(xorSingle b (fromIntegral i), i) | i <- [62..122]]

scoreAll :: [(ByteString, Int)] -> [Scored]
scoreAll ls = let genScore (x, y) = Scored x y (score x)
              in map genScore ls

getBest :: [Scored] -> Scored
getBest ls = maximum ls

main :: IO ()
main = do
    putStrLn "Set 01, Challenge 03"
    putStrLn hex
    print $ (getBest . scoreAll . encipherAll . hex2bytes) hex
