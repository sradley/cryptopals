module Cryptopals.Encoding.ASCII
( encode
, decode
) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8

-- |Convert a ByteString to it's ASCII String equivalent.
encode :: BS.ByteString -> String
encode = map (toEnum . fromEnum) . BS.unpack

-- |Convert an ASCII String to it's ByteString equivalent.
decode :: String -> BS.ByteString
decode = C8.pack
