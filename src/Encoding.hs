module Encoding
( ascii2bytes
, bytes2ascii
, hex2bytes
, bytes2hex
, base64decode
, base64encode
) where

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as C8
import           Data.ByteString.Base64 (encode, decode)
import           Numeric
import           Text.Printf

-- Converts an ascii encoded String to a ByteString.
ascii2bytes :: String -> BS.ByteString
ascii2bytes = C8.pack

-- Converts a ByteString to an ascii encoded String.
bytes2ascii :: BS.ByteString -> String
bytes2ascii = map (toEnum . fromEnum) . BS.unpack

-- Converts a hex encoded String to a ByteString.
hex2bytes :: String -> BS.ByteString
hex2bytes hs = BS.pack $ map fst $ concatMap readHex (pairs hs)
               where pairs (x:y:xs) = [x, y]:pairs xs
                     pairs      [_] = []
                     pairs       [] = []

-- Converts a ByteString to a hex encoded String.
bytes2hex :: BS.ByteString -> String
bytes2hex bs = let toHex b = printf "%02s" $ showHex b "" 
               in concatMap toHex $ BS.unpack bs

-- Decodes a base64 encoded ByteString.
base64decode :: BS.ByteString -> BS.ByteString
base64decode = toBS . decode
               where toBS (Left a)  = ascii2bytes a
                     toBS (Right b) = b

-- Encodes a ByteString to base64.
base64encode :: BS.ByteString -> BS.ByteString
base64encode = encode
