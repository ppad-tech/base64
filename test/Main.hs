{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified "ppad-base64" Data.ByteString.Base64 as B64
import qualified "ppad-base64" Data.ByteString.Base64.Pure as Pure
import qualified "base64-bytestring" Data.ByteString.Base64 as R0
import Data.Word (Word8)
import Test.Tasty
import qualified Test.Tasty.QuickCheck as Q
import qualified Test.Tasty.HUnit as H

-- generators -----------------------------------------------------------------

-- random bytes, as a slice at a random offset into a larger buffer, so
-- that inputs with a non-zero 'ByteString' offset are exercised
bytes :: Int -> Q.Gen BS.ByteString
bytes k = do
  l <- Q.chooseInt (0, k)
  o <- Q.chooseInt (0, 32)
  v <- Q.vectorOf (o + l) Q.arbitrary
  pure (BS.drop o (BS.pack v))

newtype BS = BS BS.ByteString
  deriving (Eq, Show)

instance Q.Arbitrary BS where
  arbitrary = do
    b <- bytes 1024
    pure (BS b)

alphabet :: BS.ByteString
alphabet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

-- valid encodings subjected to a few random mutations: substitution
-- (by an alphabet char, '=', or an arbitrary byte), deletion, or
-- insertion, with positions biased toward the final quartet
newtype Base64ish = Base64ish BS.ByteString
  deriving (Eq, Show)

instance Q.Arbitrary Base64ish where
  arbitrary = do
    b <- Q.frequency [(3, bytes 64), (1, bytes 384)]
    k <- Q.chooseInt (0, 3)
    s <- go k (B64.encode b)
    o <- Q.chooseInt (0, 32)
    p <- Q.vectorOf o (Q.elements (BS.unpack alphabet))
    pure (Base64ish (BS.drop o (BS.pack p <> s)))
    where
      go :: Int -> BS.ByteString -> Q.Gen BS.ByteString
      go 0 s = pure s
      go j s = mutate s >>= go (j - 1)

      mutate s = do
        let l = BS.length s
        i <- Q.oneof [
            Q.chooseInt (0, l)
          , Q.chooseInt (max 0 (l - 4), l)
          ]
        c <- Q.frequency [
            (6, Q.elements (BS.unpack alphabet))
          , (2, pure 0x3d)
          , (1, Q.arbitrary)
          ]
        Q.elements [
            BS.take i s <> BS.singleton c <> BS.drop (i + 1) s
          , BS.take i s <> BS.drop (i + 1) s
          , BS.take i s <> BS.singleton c <> BS.drop i s
          ]

-- decoders under test --------------------------------------------------------

decoders :: [(String, BS.ByteString -> Maybe BS.ByteString)]
decoders = [("public", B64.decode), ("pure", Pure.decode)]

ref_decode :: BS.ByteString -> Maybe BS.ByteString
ref_decode bs = case R0.decode bs of
  Left _  -> Nothing
  Right d -> Just d

-- properties -----------------------------------------------------------------

encode_matches_reference :: BS -> Bool
encode_matches_reference (BS bs) =
  let r0 = R0.encode bs
  in  B64.encode bs == r0 && Pure.encode bs == r0

decode_inverts_encode :: BS -> Bool
decode_inverts_encode (BS bs) =
  let enc = B64.encode bs
  in  all (\(_, dec) -> dec enc == Just bs) decoders

-- base64-bytestring's 'decode' is likewise strict (RFC 4648): it
-- rejects missing or misplaced padding and non-canonical final quartets
decode_matches_reference :: Base64ish -> Bool
decode_matches_reference (Base64ish bs) =
  let r0 = ref_decode bs
  in  B64.decode bs == r0 && Pure.decode bs == r0

-- unit tests -----------------------------------------------------------------

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
        H.assertEqual ("pure encode " <> show input)
          expected (Pure.encode input)
        forM_ decoders $ \(nam, dec) ->
          H.assertEqual (nam <> " decode " <> show expected)
            (Just input) (dec expected)
  mapM_ check vectors

-- malformed final quartets, alone and after a body long enough to
-- take the NEON path
malformed :: TestTree
malformed = H.testCase "malformed padding and non-canonical tails" $ do
  let bad = [
          "Zh=="     -- non-canonical: nonzero trailing bits
        , "Zm9="     -- non-canonical: nonzero trailing bits
        , "Zm=v"     -- '=' before a data char
        , "===="
        , "Z==="
        , "=Zg="
        , "Zg=a"
        , "Zg=", "Zg", "Z"
        , "Zm9vY"
        , "Zm=vYmFy" -- '=' in the body
        , "Zg==Zg==" -- padding mid-stream
        , "Zm9v\n"
        ]
      good = [("Zg==", "f"), ("Zm8=", "fo"), ("Zm9v", "foo")]
      body = B64.encode (BS.replicate 72 0xa5)
  forM_ decoders $ \(nam, dec) -> do
    forM_ bad $ \s -> do
      H.assertEqual (nam <> " " <> show s) Nothing (dec s)
      H.assertEqual (nam <> " body <> " <> show s) Nothing (dec (body <> s))
    forM_ good $ \(s, d) ->
      H.assertEqual (nam <> " body <> " <> show s)
        (Just (BS.replicate 72 0xa5 <> d)) (dec (body <> s))

-- every byte value, at every position of encodings that span the NEON
-- body loop, the scalar body tail, and each kind of final quartet,
-- decodes exactly as the reference does; in the body, a byte is
-- accepted iff it is an alphabet char
every_byte_every_position :: TestTree
every_byte_every_position =
  H.testCase "every byte at every position" $
    forM_ [28, 29, 30 :: Int] $ \n -> do
      let base = B64.encode (BS.replicate n 0x00)
          l    = BS.length base
      forM_ [0 .. 255 :: Int] $ \c ->
        forM_ [0 .. l - 1] $ \i -> do
          let w   = fromIntegral c :: Word8
              inp = BS.take i base <> BS.singleton w <> BS.drop (i + 1) base
              pec = ref_decode inp
              msg = "n = " <> show n <> ", byte " <> show c
                    <> ", position " <> show i
          when (i < l - 4) $
            H.assertEqual ("reference, " <> msg)
              (BS.elem w alphabet) (pec /= Nothing)
          forM_ decoders $ \(nam, dec) ->
            H.assertEqual (nam <> ", " <> msg) pec (dec inp)

-- for every length spanning several NEON iterations and each final
-- quartet shape, a single invalid byte at any position (or '=' anywhere
-- in the body) causes decoding to fail
single_corruption :: TestTree
single_corruption = H.testCase "single invalid byte anywhere" $ do
  let bad = BS.pack [0x00, 0x20, 0x7f, 0x80, 0xff] <> "-_.:@[`{"
  forM_ [1 .. 100 :: Int] $ \n -> do
    let raw = BS.pack (fmap fromIntegral [7 * j + 3 | j <- [1 .. n]])
        enc = B64.encode raw
        l   = BS.length enc
        put i w = BS.take i enc <> BS.singleton w <> BS.drop (i + 1) enc
    forM_ decoders $ \(nam, dec) ->
      H.assertEqual (nam <> ", intact, n = " <> show n) (Just raw) (dec enc)
    forM_ [0 .. l - 1] $ \i -> do
      let ws = BS.unpack bad <> (if i < l - 4 then [0x3d] else [])
      forM_ ws $ \w -> do
        let msg = "n = " <> show n <> ", position " <> show i
                  <> ", byte " <> show w
        forM_ decoders $ \(nam, dec) ->
          H.assertEqual (nam <> ", " <> msg) Nothing (dec (put i w))

bad_lengths :: TestTree
bad_lengths = H.testCase "lengths not a multiple of 4" $
  forM_ (filter (\l -> l `rem` 4 /= 0) [1 .. 99 :: Int]) $ \l ->
    forM_ decoders $ \(nam, dec) ->
      H.assertEqual (nam <> ", length " <> show l) Nothing
        (dec (B8.replicate l 'A'))

main :: IO ()
main = defaultMain $
  testGroup "ppad-base64" [
    testGroup "property tests" [
      Q.testProperty "encode matches reference" $
        Q.withMaxSuccess 5000 encode_matches_reference
    , Q.testProperty "decode . encode ~ id" $
        Q.withMaxSuccess 5000 decode_inverts_encode
    , Q.testProperty "decode matches reference" $
        Q.withMaxSuccess 10000 decode_matches_reference
    ]
  , testGroup "unit tests" [
      case_rfc_vectors
    , malformed
    , every_byte_every_position
    , single_corruption
    , bad_lengths
    ]
  ]
