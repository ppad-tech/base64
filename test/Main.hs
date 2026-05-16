{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main where

import qualified Data.ByteString as BS
import qualified "ppad-base64" Data.ByteString.Base64 as B64
import qualified "base64-bytestring" Data.ByteString.Base64 as R0
import Test.Tasty
import qualified Test.Tasty.QuickCheck as Q
import qualified Test.Tasty.HUnit as H

newtype BS = BS BS.ByteString
  deriving (Eq, Show)

bytes :: Int -> Q.Gen BS.ByteString
bytes k = do
  l <- Q.chooseInt (0, k)
  v <- Q.vectorOf l Q.arbitrary
  pure (BS.pack v)

instance Q.Arbitrary BS where
  arbitrary = do
    b <- bytes 1024
    pure (BS b)

decode_inverts_encode :: BS -> Bool
decode_inverts_encode (BS bs) = case B64.decode (B64.encode bs) of
  Nothing -> False
  Just b  -> b == bs

encode_matches_reference :: BS -> Bool
encode_matches_reference (BS bs) =
  let us = B64.encode bs
      r0 = R0.encode bs
  in  us == r0

decode_matches_reference :: BS -> Bool
decode_matches_reference (BS bs) =
  let enc = R0.encode bs
      us  = B64.decode enc
      r0  = R0.decode enc
  in  case us of
        Nothing -> case r0 of
          Left _ -> True
          _ -> False
        Just du -> case r0 of
          Left _ -> False
          Right d0 -> du == d0

case_rfc_vectors :: TestTree
case_rfc_vectors = H.testCase "RFC 4648 \167 10 vectors" $ do
  let vectors = [
          ("",       "")
        , ("f",      "Zg==")
        , ("fo",     "Zm8=")
        , ("foo",    "Zm9v")
        , ("foob",   "Zm9vYg==")
        , ("fooba",  "Zm9vYmE=")
        , ("foobar", "Zm9vYmFy")
        ]
      check (input, expected) = do
        H.assertEqual ("encode " <> show input)
          expected (B64.encode input)
        H.assertEqual ("decode " <> show expected)
          (Just input) (B64.decode expected)
  mapM_ check vectors

main :: IO ()
main = defaultMain $
  testGroup "ppad-base64" [
    testGroup "property tests" [
      Q.testProperty "decode . encode ~ id" $
        Q.withMaxSuccess 5000 decode_inverts_encode
    , Q.testProperty "encode matches reference" $
        Q.withMaxSuccess 5000 encode_matches_reference
    , Q.testProperty "decode matches reference" $
        Q.withMaxSuccess 5000 decode_matches_reference
    ]
  , testGroup "unit tests" [
      case_rfc_vectors
    ]
  ]
