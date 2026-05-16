{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Data.ByteString.Base64
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Pure base64 encoding and decoding of strict bytestrings.

module Data.ByteString.Base64 (
    encode
  , decode
  ) where

import qualified Data.Bits as B
import Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word8, Word16, Word32, Word64)

to_strict :: BSB.Builder -> BS.ByteString
to_strict = BS.toStrict . BSB.toLazyByteString
{-# INLINE to_strict #-}

to_strict_small :: BSB.Builder -> BS.ByteString
to_strict_small = BS.toStrict
  . BE.toLazyByteStringWith (BE.safeStrategy 128 BE.smallChunkSize) mempty
{-# INLINE to_strict_small #-}

fi :: (Num a, Integral b) => b -> a
fi = fromIntegral
{-# INLINE fi #-}

b64_charset :: BS.ByteString
b64_charset =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

-- 3 input bytes -> 4 output chars packed in a Word32 (big-endian)
expand_w24 :: Word8 -> Word8 -> Word8 -> Word32
expand_w24 a b c =
  let !v = (fi a `B.shiftL` 16 :: Word32)
       .|. (fi b `B.shiftL`  8)
       .|.  fi c
      !c0 = BU.unsafeIndex b64_charset (fi ((v `B.shiftR` 18) .&. 0x3F))
      !c1 = BU.unsafeIndex b64_charset (fi ((v `B.shiftR` 12) .&. 0x3F))
      !c2 = BU.unsafeIndex b64_charset (fi ((v `B.shiftR`  6) .&. 0x3F))
      !c3 = BU.unsafeIndex b64_charset (fi  (v               .&. 0x3F))
  in      fi c0 `B.shiftL` 24
      .|. fi c1 `B.shiftL` 16
      .|. fi c2 `B.shiftL`  8
      .|. fi c3
{-# INLINE expand_w24 #-}

-- 6 input bytes -> 8 output chars packed in a Word64 (big-endian)
expand_w48 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word64
expand_w48 a b c d e f =
  let !hi = expand_w24 a b c
      !lo = expand_w24 d e f
  in  (fi hi `B.shiftL` 32) .|. fi lo
{-# INLINE expand_w48 #-}

-- | Encode a base256 'ByteString' as base64.
--
--   >>> encode "hello world"
--   "aGVsbG8gd29ybGQ="
encode :: BS.ByteString -> BS.ByteString
encode bs@(BI.PS _ _ l)
    | l < 64    = to_strict_small loop
    | otherwise = to_strict loop
  where
    loop
      | l `rem` 6 == 0 =
               go64 bs
      | (l - 3) `rem` 6 == 0 = case BS.splitAt (l - 3) bs of
          (chunk, etc) ->
               go64 chunk
            <> go32 etc
      | (l - 1) `rem` 6 == 0 = case BS.splitAt (l - 1) bs of
          (chunk, etc) ->
               go64 chunk
            <> tail1 etc
      | (l - 2) `rem` 6 == 0 = case BS.splitAt (l - 2) bs of
          (chunk, etc) ->
               go64 chunk
            <> tail2 etc
      | (l - 4) `rem` 6 == 0 = case BS.splitAt (l - 4) bs of
          (chunk, etc) ->
               go64 chunk
            <> go32 (BU.unsafeTake 3 etc)
            <> tail1 (BU.unsafeDrop 3 etc)
      | (l - 5) `rem` 6 == 0 = case BS.splitAt (l - 5) bs of
          (chunk, etc) ->
               go64 chunk
            <> go32 (BU.unsafeTake 3 etc)
            <> tail2 (BU.unsafeDrop 3 etc)
      | otherwise =
               mempty -- unreachable: l `rem` 6 in [0..5]

    go64 b = case BS.splitAt 6 b of
      (chunk, etc)
        | BS.null chunk -> mempty
        | otherwise ->
            let !w64 = expand_w48
                  (BU.unsafeIndex chunk 0)
                  (BU.unsafeIndex chunk 1)
                  (BU.unsafeIndex chunk 2)
                  (BU.unsafeIndex chunk 3)
                  (BU.unsafeIndex chunk 4)
                  (BU.unsafeIndex chunk 5)
            in  BSB.word64BE w64 <> go64 etc

    go32 b = case BS.splitAt 3 b of
      (chunk, etc)
        | BS.null chunk -> mempty
        | otherwise ->
            let !w32 = expand_w24
                  (BU.unsafeIndex chunk 0)
                  (BU.unsafeIndex chunk 1)
                  (BU.unsafeIndex chunk 2)
            in  BSB.word32BE w32 <> go32 etc

    -- final 1 byte -> "XX==" (one Word32 BE)
    tail1 b =
      let !a  = BU.unsafeIndex b 0
          !c0 = BU.unsafeIndex b64_charset (fi (a `B.shiftR` 2))
          !c1 = BU.unsafeIndex b64_charset (fi ((a .&. 0x03) `B.shiftL` 4))
          !w32 = (fi c0 `B.shiftL` 24 :: Word32)
             .|. (fi c1 `B.shiftL` 16)
             .|.  0x00003D3D
      in  BSB.word32BE w32

    -- final 2 bytes -> "XXX=" (one Word32 BE)
    tail2 b =
      let !a  = BU.unsafeIndex b 0
          !c  = BU.unsafeIndex b 1
          !c0 = BU.unsafeIndex b64_charset (fi (a `B.shiftR` 2))
          !c1 = BU.unsafeIndex b64_charset
                  (fi (((a .&. 0x03) `B.shiftL` 4) .|. (c `B.shiftR` 4)))
          !c2 = BU.unsafeIndex b64_charset (fi ((c .&. 0x0F) `B.shiftL` 2))
          !w32 = (fi c0 `B.shiftL` 24 :: Word32)
             .|. (fi c1 `B.shiftL` 16)
             .|. (fi c2 `B.shiftL`  8)
             .|.  0x0000003D
      in  BSB.word32BE w32

-- word8 base64 character -> 6-bit value
word6 :: Word8 -> Maybe Word8
word6 c
  | c >= 65 && c <=  90 = pure $! c - 65 -- A-Z
  | c >= 97 && c <= 122 = pure $! c - 71 -- a-z
  | c >= 48 && c <=  57 = pure $! c + 4  -- 0-9
  | c == 43             = pure 62        -- '+'
  | c == 47             = pure 63        -- '/'
  | otherwise           = Nothing
{-# INLINE word6 #-}

-- decode 4 chars at offset i to a 24-bit value (in low bits of Word32)
dec_quartet :: BS.ByteString -> Int -> Maybe Word32
dec_quartet b i = do
  !v0 <- word6 (BU.unsafeIndex b i)
  !v1 <- word6 (BU.unsafeIndex b (i + 1))
  !v2 <- word6 (BU.unsafeIndex b (i + 2))
  !v3 <- word6 (BU.unsafeIndex b (i + 3))
  pure $! (fi v0 `B.shiftL` 18 :: Word32)
      .|. (fi v1 `B.shiftL` 12)
      .|. (fi v2 `B.shiftL`  6)
      .|.  fi v3
{-# INLINE dec_quartet #-}

-- | Decode a base64 'ByteString' to base256.
--
--   Invalid inputs (including incorrectly-padded or non-canonical
--   inputs) will produce 'Nothing'.
--
--   >>> decode "aGVsbG8gd29ybGQ="
--   Just "hello world"
--   >>> decode "aGVsbG8gd29ybGQ" -- missing padding
--   Nothing
decode :: BS.ByteString -> Maybe BS.ByteString
decode bs@(BI.PS _ _ l)
    | l == 0                 = pure BS.empty
    | l `rem` 4 /= 0         = Nothing
    | (l `quot` 4) * 3 < 128 = fmap to_strict_small loop
    | otherwise              = fmap to_strict loop
  where
    !bl    = l - 4
    !body  = BU.unsafeTake bl bs
    !final = BU.unsafeDrop bl bs

    loop = do
      !b0 <- decode_body body
      !b1 <- decode_final final
      pure (b0 <> b1)

    decode_body b
      | bl `rem` 32 == 0 =
            go64 mempty b
      | (bl - 4) `rem` 32 == 0 = case BS.splitAt (bl - 4) b of
          (chunk, etc) -> do
            !acc <- go64 mempty chunk
            go16 acc etc
      | (bl - 8) `rem` 32 == 0 = case BS.splitAt (bl - 8) b of
          (chunk, etc) -> do
            !acc <- go64 mempty chunk
            go32 acc etc
      | (bl - 12) `rem` 32 == 0 = case BS.splitAt (bl - 12) b of
          (chunk, etc) -> do
            !acc0 <- go64 mempty chunk
            !acc1 <- go32 acc0 (BU.unsafeTake 8 etc)
            go16 acc1 (BU.unsafeDrop 8 etc)
      | (bl - 16) `rem` 32 == 0 = case BS.splitAt (bl - 16) b of
          (chunk, etc) -> do
            !acc <- go64 mempty chunk
            go48 acc etc
      | (bl - 20) `rem` 32 == 0 = case BS.splitAt (bl - 20) b of
          (chunk, etc) -> do
            !acc0 <- go64 mempty chunk
            !acc1 <- go48 acc0 (BU.unsafeTake 16 etc)
            go16 acc1 (BU.unsafeDrop 16 etc)
      | (bl - 24) `rem` 32 == 0 = case BS.splitAt (bl - 24) b of
          (chunk, etc) -> do
            !acc0 <- go64 mempty chunk
            !acc1 <- go48 acc0 (BU.unsafeTake 16 etc)
            go32 acc1 (BU.unsafeDrop 16 etc)
      | (bl - 28) `rem` 32 == 0 = case BS.splitAt (bl - 28) b of
          (chunk, etc) -> do
            !acc0 <- go64 mempty chunk
            !acc1 <- go48 acc0 (BU.unsafeTake 16 etc)
            !acc2 <- go32 acc1 (BU.unsafeTake 8 (BU.unsafeDrop 16 etc))
            go16 acc2 (BU.unsafeDrop 24 etc)
      | otherwise = Nothing -- unreachable

    decode_final b =
      let !c0 = BU.unsafeIndex b 0
          !c1 = BU.unsafeIndex b 1
          !c2 = BU.unsafeIndex b 2
          !c3 = BU.unsafeIndex b 3
      in  case (c2 == 0x3D, c3 == 0x3D) of
            (True, True) -> do
              !v0 <- word6 c0
              !v1 <- word6 c1
              if v1 .&. 0x0F /= 0
                then Nothing
                else
                  let !w8 = (v0 `B.shiftL` 2) .|. (v1 `B.shiftR` 4)
                  in  pure $! BSB.word8 w8
            (False, True) -> do
              !v0 <- word6 c0
              !v1 <- word6 c1
              !v2 <- word6 c2
              if v2 .&. 0x03 /= 0
                then Nothing
                else
                  let !w16 = (fi v0 `B.shiftL` 10 :: Word16)
                         .|. (fi v1 `B.shiftL`  4)
                         .|. (fi v2 `B.shiftR`  2)
                  in  pure $! BSB.word16BE w16
            (True, False) -> Nothing
            (False, False) -> do
              !v0 <- word6 c0
              !v1 <- word6 c1
              !v2 <- word6 c2
              !v3 <- word6 c3
              let !w24 = (fi v0 `B.shiftL` 18 :: Word32)
                     .|. (fi v1 `B.shiftL` 12)
                     .|. (fi v2 `B.shiftL`  6)
                     .|.  fi v3
                  !w16 = fi (w24 `B.shiftR` 8) :: Word16
                  !w8  = fi w24 :: Word8
              pure $! BSB.word16BE w16 <> BSB.word8 w8

    -- 4 chars -> 3 bytes (1 word16BE + 1 word8)
    go16 acc b = case BS.splitAt 4 b of
      (chunk, etc)
        | BS.null chunk -> pure acc
        | otherwise -> do
            !q <- dec_quartet chunk 0
            let !w16 = fi (q `B.shiftR` 8) :: Word16
                !w8  = fi q :: Word8
            go16 (acc <> BSB.word16BE w16 <> BSB.word8 w8) etc

    -- 8 chars -> 6 bytes (1 word32BE + 1 word16BE)
    go32 acc b = case BS.splitAt 8 b of
      (chunk, etc)
        | BS.null chunk -> pure acc
        | otherwise -> do
            !q0 <- dec_quartet chunk 0
            !q1 <- dec_quartet chunk 4
            let !w48 = (fi q0 `B.shiftL` 24 :: Word64)
                   .|.  fi q1
                !w32 = fi (w48 `B.shiftR` 16) :: Word32
                !w16 = fi w48 :: Word16
            go32 (acc <> BSB.word32BE w32 <> BSB.word16BE w16) etc

    -- 16 chars -> 12 bytes (1 word64BE + 1 word32BE)
    go48 acc b = case BS.splitAt 16 b of
      (chunk, etc)
        | BS.null chunk -> pure acc
        | otherwise -> do
            !q0 <- dec_quartet chunk 0
            !q1 <- dec_quartet chunk 4
            !q2 <- dec_quartet chunk 8
            !q3 <- dec_quartet chunk 12
            let !w64 = (fi q0 `B.shiftL` 40 :: Word64)
                   .|. (fi q1 `B.shiftL` 16)
                   .|.  fi (q2 `B.shiftR` 8)
                !w32 = ((q2 .&. 0xFF) `B.shiftL` 24) .|. q3
            go48 (acc <> BSB.word64BE w64 <> BSB.word32BE w32) etc

    -- 32 chars -> 24 bytes (3 × word64BE)
    go64 acc b = case BS.splitAt 32 b of
      (chunk, etc)
        | BS.null chunk -> pure acc
        | otherwise -> do
            !q0 <- dec_quartet chunk 0
            !q1 <- dec_quartet chunk 4
            !q2 <- dec_quartet chunk 8
            !q3 <- dec_quartet chunk 12
            !q4 <- dec_quartet chunk 16
            !q5 <- dec_quartet chunk 20
            !q6 <- dec_quartet chunk 24
            !q7 <- dec_quartet chunk 28
            let !w64a = (fi q0 `B.shiftL` 40 :: Word64)
                    .|. (fi q1 `B.shiftL` 16)
                    .|.  fi (q2 `B.shiftR` 8)
                !w64b = (fi (q2 .&. 0xFF) `B.shiftL` 56 :: Word64)
                    .|. (fi q3 `B.shiftL` 32)
                    .|. (fi q4 `B.shiftL`  8)
                    .|.  fi (q5 `B.shiftR` 16)
                !w64c = (fi (q5 .&. 0xFFFF) `B.shiftL` 48 :: Word64)
                    .|. (fi q6 `B.shiftL` 24)
                    .|.  fi q7
            go64 (acc <> BSB.word64BE w64a
                      <> BSB.word64BE w64b
                      <> BSB.word64BE w64c) etc
