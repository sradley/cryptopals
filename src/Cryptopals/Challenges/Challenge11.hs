module Cryptopals.Challenges.Challenge11
( challenge11
) where

import qualified Cryptopals.Encoding.Hex as Hex

import           Cryptopals.Util        as Util
import           Cryptopals.Random
import           Cryptopals.Padding     as Pad
import           Cryptopals.Ciphers.AES as AES
import qualified Data.ByteString        as BS
import           System.Random          (StdGen, getStdGen, randomR)

challenge11 :: IO ()
challenge11 = do
    g <- getStdGen
    let (key, keyg) = randBytes (16, g)
        (iv, ivg)   = randBytes (16, keyg)
        (pt, ptg)   = plainText (BS.replicate 48 65) ivg
        (ct, _, m)     = cipherText (Pad.pkcs7 pt 16) (BS.pack key) (BS.pack iv) ptg
    print $ m
    print $ Hex.encode ct
    print $ detectECB ct

plainText :: BS.ByteString -> StdGen -> (BS.ByteString, StdGen)
plainText x g = (BS.pack lpad <> x <> BS.pack rpad, rg)
    where
        (lpad, lg) = randBytes $ randomR (5, 10) g
        (rpad, rg) = randBytes $ randomR (5, 10) lg

cipherText
    :: BS.ByteString
    -> BS.ByteString
    -> BS.ByteString
    -> StdGen
    -> (BS.ByteString, StdGen, Bool)
cipherText x k iv g = (encipher mode, g', mode)
    where
        (mode, g') = randomR (False, True) g
        encipher m
            | m         = AES.encipherECB x k
            | otherwise = AES.encipherCBC x iv k

detectECB :: BS.ByteString -> Bool
detectECB x = Util.repeated x 16 > 0
