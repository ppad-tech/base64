{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Criterion.Main
import qualified Data.ByteString as BS
import qualified "ppad-base64" Data.ByteString.Base64 as B64
import qualified "base64-bytestring" Data.ByteString.Base64 as R0
import qualified "base64" Data.ByteString.Base64 as R1

main :: IO ()
main = defaultMain [
    minimal_encode
  , minimal_decode
  ]

minimal_encode :: Benchmark
minimal_encode =
  let !a = BS.replicate 1024 0x00
  in  bench "ppad-base64/encode" $
        nf B64.encode a

minimal_decode :: Benchmark
minimal_decode =
  let !a = B64.encode (BS.replicate 768 0x00)
  in  bench "ppad-base64/decode" $
        nf B64.decode a

encode :: Benchmark
encode = bgroup "encode" [
    bench "ppad-base64" $ nf B64.encode (BS.replicate 1024 0x00)
  , bench "base64-bytestring" $ nf R0.encode (BS.replicate 1024 0x00)
  , bench "base64" $ nf R1.encodeBase64' (BS.replicate 1024 0x00)
  ]

decode :: Benchmark
decode = bgroup "decode" [
    bench "ppad-base64" $ nf B64.decode
      (B64.encode (BS.replicate 768 0x00))
  , bench "base64-bytestring" $ nf R0.decode
      (B64.encode (BS.replicate 768 0x00))
  , bench "base64" $ nf R1.decodeBase64Untyped
      (B64.encode (BS.replicate 768 0x00))
  ]

decode_various :: Benchmark
decode_various = bgroup "base64" [
    bench "1024B input" $ nf B64.decode (B64.encode (BS.replicate 768 0x00))
  , bench "1028B input" $ nf B64.decode (B64.encode (BS.replicate 771 0x00))
  , bench "1032B input" $ nf B64.decode (B64.encode (BS.replicate 774 0x00))
  , bench "1036B input" $ nf B64.decode (B64.encode (BS.replicate 777 0x00))
  , bench "1040B input" $ nf B64.decode (B64.encode (BS.replicate 780 0x00))
  , bench "1044B input" $ nf B64.decode (B64.encode (BS.replicate 783 0x00))
  , bench "1048B input" $ nf B64.decode (B64.encode (BS.replicate 786 0x00))
  , bench "1052B input" $ nf B64.decode (B64.encode (BS.replicate 789 0x00))
  ]

encode_various :: Benchmark
encode_various = bgroup "base64" [
    bench "1024B input" $ nf B64.encode (BS.replicate 1024 0x00)
  , bench "1023B input" $ nf B64.encode (BS.replicate 1023 0x00)
  , bench "1022B input" $ nf B64.encode (BS.replicate 1022 0x00)
  ]
