module Main where

import Data.ByteString      (ByteString, pack)
import Data.List            (nub)
import Control.Monad.Random (MonadRandom, getRandomR, getRandomRs)
import Encoding             (bytes2hex, ascii2bytes)
import Aes                  (ecb128enc, cbc128enc)
import Util                 (chunkify, pkcs7pad)

genRandBytes :: MonadRandom m => Int -> m ByteString
genRandBytes n = pack . take n <$> getRandomRs (minBound, maxBound)

genRandPText :: MonadRandom m => ByteString -> m ByteString
genRandPText bs = do l <- genRandBytes =<< getRandomR (5, 10)
                     r <- genRandBytes =<< getRandomR (5, 10)
                     pure $ l <> bs <> r

genRandCText :: MonadRandom m => ByteString -> ByteString -> ByteString -> m ByteString
genRandCText bs k iv = let enc mode | mode      = ecb128enc bs k
                                    | otherwise = cbc128enc bs k iv 
                       in do mode <- getMode
                             pure $ enc mode 
                       where getMode :: MonadRandom m => m Bool
                             getMode = getRandomR (False, True)

repeated :: ByteString -> Int
repeated bs = length chunks - (length . nub) chunks
              where chunks = chunkify bs 16

isECB :: ByteString -> Bool
isECB bs = repeated bs > 0

main :: IO ()
main = do putStrLn "Set 02, Challenge 11\n"

          let plaintext = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"

          -- Generate random key and IV.
          key <- genRandBytes 16
          iv  <- genRandBytes 16
          
          -- Generate random plaintext.
          text <- genRandPText (ascii2bytes plaintext)
          let pt = pkcs7pad text 16

          -- Generate random ciphertext.
          ct <- genRandCText pt key iv
          print $ bytes2hex ct
          
          -- Detect whether random ciphertext is ECB or CBC.
          print $ isECB ct
