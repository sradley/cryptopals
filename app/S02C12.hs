module Main where

import Encoding
import Aes
import Util

import qualified Data.ByteString as BS
import           Data.Word
import           Text.Printf

hidden :: String
hidden = "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg\
         \aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq\
         \dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg\
         \YnkK"

encrypt :: BS.ByteString -> BS.ByteString -> BS.ByteString
encrypt bs k = ecb128enc (pkcs7pad (bs <> hiddenBS) 16) k
               where hiddenBS = base64decode . ascii2bytes $ hidden

findKeySize :: BS.ByteString -> Int -> Int
findKeySize k i | dist k i > 0 = fromIntegral $ dist k i
                |    otherwise = findKeySize k (i+1)
                where dist   k' i' = encLen k' i' - encLen k' (i'-1)
                      encLen k' i' = BS.length $ encrypt (BS.replicate i' 65) k'

ecbDetect :: BS.ByteString -> Bool
ecbDetect bs = repeated bs > 0

nthByte :: Int -> BS.ByteString -> Maybe Word8
nthByte i bs = fst <$> (BS.uncons $ BS.drop i bs) 

getEncByte :: BS.ByteString -> Int -> Maybe Word8
getEncByte k i = nthByte i $ encrypt (BS.replicate (i-1) 65) k

decByte :: BS.ByteString -> Int -> Word8
decByte k i = let encByte = extract (getEncByte k i)
                  extract (Just x) = x              
                  extract  Nothing = 0
              in encByte
                  

main :: IO ()
main = do putStrLn "Set 02, Challenge 12\n"

          -- Generate a secret key.
          secretKey <- randBytes 16

          -- Find the keysize (without knowing the length of the key).
          let ks = findKeySize secretKey 1
          printf "key-size: %d\n" ks

          -- Detect whether the function is using ECB.
          let ecb = ecbDetect $ encrypt (BS.replicate 32 65) secretKey
          printf "ecb-mode: %s\n" $ if ecb then "true" else "false"

          print $ getEncByte secretKey 16
