module Encoding
( ascii2bytes
, bytes2ascii
, hex2bytes
, bytes2hex
, base64decode
, base64encode
) where

import Data.ByteString as BS (ByteString, pack, unpack)
import Data.ByteString.Char8 as C8 (pack)
import Data.ByteString.Base64 (encode, decode)
import Numeric (showHex, readHex)
import Text.Printf

-- Converts an ascii encoded String to a ByteString.
ascii2bytes :: String -> ByteString
ascii2bytes = C8.pack

-- Converts a ByteString to an ascii encoded String.
bytes2ascii :: BS.ByteString -> String
bytes2ascii = map (toEnum . fromEnum) . unpack

-- Converts a hex encoded String to a ByteString.
hex2bytes :: String -> ByteString
hex2bytes hs = BS.pack $ map fst $ concatMap readHex (pairs hs)

-- Converts a ByteString to a hex encoded String.
bytes2hex :: ByteString -> String
bytes2hex bs = let toHex b = printf "%02s" $ showHex b "" 
               in concatMap toHex $ unpack bs

-- Decodes a base64 encoded ByteString.
base64decode :: ByteString -> ByteString
base64decode = let toByteString (Left a)  = ascii2bytes a
                   toByteString (Right b) = b
               in toByteString . decode

-- Encodes a ByteString to base64.
base64encode :: ByteString -> ByteString
base64encode = encode

-- Utility function for hex2bytes, breaks String to pairs of two elements.
pairs :: String -> [String]
pairs       [] = []
pairs      [_] = []
pairs (x:y:xs) = [x,y]:pairs xs
