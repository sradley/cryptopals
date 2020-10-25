module Cryptopals.Encoding.Base64
( encode
, decode
) where

import qualified Cryptopals.Encoding.ASCII as ASCII
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Base64    as Base64

-- |Encodes a ByteString to a Base64 encoded ByteString.
encode :: BS.ByteString -> BS.ByteString
encode = Base64.encode

-- |Decodes a Base64 encoded ByteString to a ByteString.
decode :: BS.ByteString -> BS.ByteString
decode = handleString . Base64.decode
    where
        handleString (Left a)  = ASCII.decode a
        handleString (Right b) = b
