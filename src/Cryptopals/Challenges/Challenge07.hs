module Cryptopals.Challenges.Challenge07
( challenge07
) where

import qualified Cryptopals.Encoding.ASCII  as ASCII
import qualified Cryptopals.Encoding.Base64 as Base64
import           Cryptopals.Ciphers.AES     as AES
import qualified Data.ByteString            as BS

challenge07 :: IO ()
challenge07 = fileData >>= putStrLn . ASCII.encode . dec
    where
        key   = ASCII.decode "YELLOW SUBMARINE"
        dec x = AES.decipherECB x key

fileData :: IO BS.ByteString
fileData = readFile "data/7.txt" >>= pure . decode
    where
        decode = Base64.decode . BS.concat . map ASCII.decode . lines
