module Main where

import Encoding
import Aes
import Util

import Data.ByteString      as BS (ByteString, replicate)
import Control.Monad.Random

genRandPText :: MonadRandom m => ByteString -> m ByteString
genRandPText bs = do l <- randBytes =<< getRandomR (5, 10)
                     r <- randBytes =<< getRandomR (5, 10)
                     pure $ l <> bs <> r

genRandCText :: MonadRandom m => ByteString -> ByteString -> ByteString -> m ByteString
genRandCText bs k iv = let enc mode | mode      = ecb128enc bs k
                                    | otherwise = cbc128enc bs k iv 
                       in do mode <- getMode
                             pure $ enc mode 
                       where getMode :: MonadRandom m => m Bool
                             getMode = getRandomR (False, True)

ecbDetect :: ByteString -> Bool
ecbDetect bs = repeated bs > 0

main :: IO ()
main = do putStrLn "Set 02, Challenge 11\n"

          let plaintext = BS.replicate 32 65

          -- Generate random key and IV.
          key <- randBytes 16
          iv  <- randBytes 16
          
          -- Generate random plaintext.
          text <- genRandPText (ascii2bytes plaintext)
          let pt = pkcs7pad text 16

          -- Generate random ciphertext.
          ct <- genRandCText pt key iv
          print $ bytes2hex ct
          
          -- Detect whether random ciphertext is ECB or CBC.
          print $ ecbDetect ct
