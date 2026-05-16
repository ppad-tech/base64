{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main where

import qualified Data.ByteString as BS
import qualified "base64-bytestring" Data.ByteString.Base64 as R0
import qualified "base64" Data.ByteString.Base64 as R1
import qualified "ppad-base64" Data.ByteString.Base64 as B64
import qualified Weigh as W

inp :: BS.ByteString
inp = "jtobin was here benching stuffjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin was here benching stufjtobin w"

binp :: BS.ByteString
binp = B64.encode inp

main :: IO ()
main = W.mainWith $ do
  W.func "ppad-base64 (encode)" B64.encode inp
  W.func "base64-bytestring (encode)" R0.encode inp
  W.func "base64 (encode)" R1.encodeBase64' inp

  W.func "ppad-base64 (decode)" B64.decode binp
  W.func "base64-bytestring (decode)" R0.decode binp
  W.func "base64 (decode)" R1.decodeBase64Untyped binp
