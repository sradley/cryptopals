module Base64 where

import Data.ByteString as BS (ByteString, pack, unpack)
import Data.ByteString.Char8 as Char8 (pack)
import Data.ByteString.Base64 (encode, decode)
import Data.Char (chr)
import Numeric (showHex, readHex)

ascii2bytes :: String -> ByteString
ascii2bytes = Char8.pack

bytes2ascii :: BS.ByteString -> String
bytes2ascii = map (chr . fromEnum) . unpack

hex2bytes :: String -> ByteString
hex2bytes hex = BS.pack $ map fst $ concatMap readHex (pairs hex)

bytes2hex :: ByteString -> String
bytes2hex bytes = foldr ($) "" $ map showHex (unpack bytes)

base642bytes :: ByteString -> Either String ByteString
base642bytes = decode

bytes2base64 :: ByteString -> ByteString
bytes2base64 = encode

pairs :: String -> [String]
pairs [] = []
pairs [_] = []
pairs (x:y:xs) = [x,y]:pairs xs
