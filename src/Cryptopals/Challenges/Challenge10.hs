module Cryptopals.Challenges.Challenge10
( challenge10
) where

import qualified Cryptopals.Encoding.ASCII  as ASCII
import qualified Cryptopals.Encoding.Base64 as Base64
import           Cryptopals.Ciphers.AES     as AES
import qualified Data.ByteString            as BS

challenge10 :: IO ()
challenge10 = fileData >>= putStrLn . ASCII.encode . dec
    where
        iv    = BS.replicate 16 0
        key   = ASCII.decode "YELLOW SUBMARINE"
        dec x = decipherCBC x iv key

fileData :: IO BS.ByteString
fileData = readFile "data/10.txt" >>= pure . decode
    where
        decode = Base64.decode . BS.concat . map ASCII.decode . lines
