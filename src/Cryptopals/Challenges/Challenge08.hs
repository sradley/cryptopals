module Cryptopals.Challenges.Challenge08
( challenge08
) where

import qualified Cryptopals.Encoding.Hex as Hex
import           Cryptopals.Util         as Util
import qualified Data.ByteString         as BS

challenge08 :: IO ()
challenge08 = fileData >>= print . map Hex.encode . filter detectECB

fileData :: IO [BS.ByteString]
fileData = readFile "data/8.txt" >>= pure . map Hex.decode . lines

detectECB :: BS.ByteString -> Bool
detectECB x = Util.repeated x 16 > 0
