module Util (decode, encode) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Codec.Binary.UTF8.String (encodeString, decodeString)
import Codec.Text.IConv (EncodingName, convert)


decode :: EncodingName -> String -> String
decode coding = decodeString . L8.unpack . convert coding "UTF-8" . L8.pack

encode :: EncodingName -> String -> String
encode coding = L8.unpack . convert "UTF-8" coding . L8.pack . encodeString
