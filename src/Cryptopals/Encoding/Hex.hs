module Cryptopals.Encoding.Hex
( encode
, decode
) where

import qualified Data.ByteString as BS
import           Numeric
import           Text.Printf

-- |Converts a ByteString to it's hex String equivalent.
encode :: BS.ByteString -> String
encode s = concatMap (\x -> printf "%02s" (showHex x "")) $ BS.unpack s

-- |Converts a hex String to it's ByteString equivalent.
decode :: String -> BS.ByteString
decode s = BS.pack $ map fst $ concatMap readHex $ pairs s
    where
        pairs (x:y:xs) = [x, y]:pairs xs
        pairs      [_] = []
        pairs       [] = []
