{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
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
import qualified Data.ByteString.Internal as BI
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import System.IO.Unsafe (unsafeDupablePerformIO)

fi :: (Num a, Integral b) => b -> a
fi = fromIntegral
{-# INLINE fi #-}

-- 64-byte table.  Indexed by 6-bit value (0..63), yields the
-- corresponding base64 alphabet character.  All-ASCII content means
-- the bytestring 'IsString' rule rewrites this to 'unsafePackAddress'
-- and the bytes live in static rodata.
enc_tab :: BS.ByteString
enc_tab =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
{-# NOINLINE enc_tab #-}

-- 256-byte table.  Index by an ASCII byte to obtain its 6-bit value;
-- valid base64 chars ('A'..'Z', 'a'..'z', '0'..'9', '+', '/') map to
-- 0x40..0x7f, every other byte (including '=') maps to 0x80.
--
-- The encoding is chosen so the literal is strictly ASCII and contains
-- no embedded NUL, which is what the bytestring 'IsString' rule needs
-- to rewrite it into 'unsafePackAddress' (cf. 'enc_tab') — the bytes
-- end up in static rodata, with no CAF allocation.
--
-- The 0x80 sentinel is distinguished by bit 7; no value 0x40..0x7f
-- carries that bit, so 'decode' OR-folds every lookup into an
-- accumulator and tests 'acc .&. 0x80 == 0' once at the end.  The
-- low 6 bits of each entry are the 6-bit value, possibly contaminated
-- by the 0x40 flag bit; the b0/b1/b2 formulas mask each subexpression
-- before combining so the flag never bleeds into the output bytes.
dec_tab :: BS.ByteString
dec_tab =
  "\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\
  \\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\
  \\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x7E\x80\x80\x80\x7F\
  \\x74\x75\x76\x77\x78\x79\x7A\x7B\x7C\x7D\x80\x80\x80\x80\x80\x80\
  \\x80\x40\x41\x42\x43\x44\x45\x46\x47\x48\x49\x4A\x4B\x4C\x4D\x4E\
  \\x4F\x50\x51\x52\x53\x54\x55\x56\x57\x58\x59\x80\x80\x80\x80\x80\
  \\x80\x5A\x5B\x5C\x5D\x5E\x5F\x60\x61\x62\x63\x64\x65\x66\x67\x68\
  \\x69\x6A\x6B\x6C\x6D\x6E\x6F\x70\x71\x72\x73\x80\x80\x80\x80\x80\
  \\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\
  \\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\
  \\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\
  \\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\
  \\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\
  \\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\
  \\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\
  \\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80"
{-# NOINLINE dec_tab #-}

-- | Encode a base256 'ByteString' as base64.
--
--   >>> encode "hello world"
--   "aGVsbG8gd29ybGQ="
encode :: BS.ByteString -> BS.ByteString
encode = encode_scalar
{-# INLINABLE encode #-}

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
decode = decode_scalar
{-# INLINABLE decode #-}

encode_scalar :: BS.ByteString -> BS.ByteString
encode_scalar (BI.PS sfp soff l) =
  case enc_tab of
    BI.PS tfp toff _ ->
      BI.unsafeCreate ((l + 2) `quot` 3 * 4) $ \dst ->
        withForeignPtr sfp $ \sp0 ->
        withForeignPtr tfp $ \tp0 -> do
          let !sp = sp0 `plusPtr` soff :: Ptr Word8
              !tp = tp0 `plusPtr` toff :: Ptr Word8
              !nfull = l `quot` 3
              !rmn   = l - nfull * 3
              loop !i
                | i == nfull = pure ()
                | otherwise = do
                    let !ii = i * 3
                        !oo = i * 4
                    b0 <- peekElemOff sp ii
                    b1 <- peekElemOff sp (ii + 1)
                    b2 <- peekElemOff sp (ii + 2)
                    c0 <- peekElemOff tp (fi (b0 `B.shiftR` 2))
                    c1 <- peekElemOff tp (fi
                            (((b0 .&. 0x03) `B.shiftL` 4)
                         .|.  (b1 `B.shiftR` 4)))
                    c2 <- peekElemOff tp (fi
                            (((b1 .&. 0x0F) `B.shiftL` 2)
                         .|.  (b2 `B.shiftR` 6)))
                    c3 <- peekElemOff tp (fi (b2 .&. 0x3F))
                    pokeElemOff dst  oo      (c0 :: Word8)
                    pokeElemOff dst (oo + 1) c1
                    pokeElemOff dst (oo + 2) c2
                    pokeElemOff dst (oo + 3) c3
                    loop (i + 1)
          loop 0
          case rmn of
            0 -> pure ()
            1 -> do
              let !ii = nfull * 3
                  !oo = nfull * 4
              b0 <- peekElemOff sp ii
              c0 <- peekElemOff tp (fi (b0 `B.shiftR` 2))
              c1 <- peekElemOff tp (fi ((b0 .&. 0x03) `B.shiftL` 4))
              pokeElemOff dst  oo      (c0 :: Word8)
              pokeElemOff dst (oo + 1) c1
              pokeElemOff dst (oo + 2) 0x3D
              pokeElemOff dst (oo + 3) 0x3D
            _ -> do
              let !ii = nfull * 3
                  !oo = nfull * 4
              b0 <- peekElemOff sp ii
              b1 <- peekElemOff sp (ii + 1)
              c0 <- peekElemOff tp (fi (b0 `B.shiftR` 2))
              c1 <- peekElemOff tp (fi
                      (((b0 .&. 0x03) `B.shiftL` 4)
                   .|.  (b1 `B.shiftR` 4)))
              c2 <- peekElemOff tp (fi ((b1 .&. 0x0F) `B.shiftL` 2))
              pokeElemOff dst  oo      (c0 :: Word8)
              pokeElemOff dst (oo + 1) c1
              pokeElemOff dst (oo + 2) c2
              pokeElemOff dst (oo + 3) 0x3D

decode_scalar :: BS.ByteString -> Maybe BS.ByteString
decode_scalar (BI.PS sfp soff l)
  | l == 0          = Just BS.empty
  | l .&. 0x03 /= 0 = Nothing
  | otherwise = case dec_tab of
      BI.PS tfp toff _ -> unsafeDupablePerformIO $
        withForeignPtr sfp $ \sp0 ->
        withForeignPtr tfp $ \tp0 -> do
          let !sp = sp0 `plusPtr` soff :: Ptr Word8
              !tp = tp0 `plusPtr` toff :: Ptr Word8
          c_pre <- peekElemOff sp (l - 2)
          c_end <- peekElemOff sp (l - 1)
          let !pad_pre = c_pre == 0x3D
              !pad_end = c_end == 0x3D
          if pad_pre && not pad_end
            then pure Nothing
            else do
              let !pad = (if pad_pre then 2 else if pad_end then 1 else 0)
                       :: Int
                  !nfull  = l `B.shiftR` 2
                  !nbody  = if pad > 0 then nfull - 1 else nfull
                  !outlen = nfull * 3 - pad
              fp <- BI.mallocByteString outlen
              ok <- withForeignPtr fp $ \dst -> do
                let body_loop !acc !i
                      | i == nbody = pure acc
                      | otherwise = do
                          let !ii = i `B.shiftL` 2
                              !oo = i * 3
                          c0 <- peekElemOff sp  ii
                          c1 <- peekElemOff sp (ii + 1)
                          c2 <- peekElemOff sp (ii + 2)
                          c3 <- peekElemOff sp (ii + 3)
                          v0 <- peekElemOff tp (fi c0)
                          v1 <- peekElemOff tp (fi c1)
                          v2 <- peekElemOff tp (fi c2)
                          v3 <- peekElemOff tp (fi c3)
                          let !b0 = (v0 `B.shiftL` 2)
                                .|. ((v1 `B.shiftR` 4) .&. 0x03)
                              !b1 = ((v1 .&. 0x0F) `B.shiftL` 4)
                                .|. ((v2 `B.shiftR` 2) .&. 0x0F)
                              !b2 = ((v2 .&. 0x03) `B.shiftL` 6)
                                .|. (v3 .&. 0x3F)
                          pokeElemOff dst  oo      b0
                          pokeElemOff dst (oo + 1) b1
                          pokeElemOff dst (oo + 2) b2
                          body_loop
                            (acc .|. v0 .|. v1 .|. v2 .|. v3) (i + 1)
                acc <- body_loop 0 0
                if acc .&. 0x80 /= 0
                  then pure False
                  else case pad of
                    0 -> pure True
                    1 -> do
                      let !ii = nbody `B.shiftL` 2
                          !oo = nbody * 3
                      c0 <- peekElemOff sp  ii
                      c1 <- peekElemOff sp (ii + 1)
                      c2 <- peekElemOff sp (ii + 2)
                      v0 <- peekElemOff tp (fi c0)
                      v1 <- peekElemOff tp (fi c1)
                      v2 <- peekElemOff tp (fi c2)
                      let !tail_acc = v0 .|. v1 .|. v2
                      if tail_acc .&. 0x80 /= 0 || v2 .&. 0x03 /= 0
                        then pure False
                        else do
                          let !b0 = (v0 `B.shiftL` 2)
                                .|. ((v1 `B.shiftR` 4) .&. 0x03)
                              !b1 = ((v1 .&. 0x0F) `B.shiftL` 4)
                                .|. ((v2 `B.shiftR` 2) .&. 0x0F)
                          pokeElemOff dst  oo      b0
                          pokeElemOff dst (oo + 1) b1
                          pure True
                    _ -> do
                      let !ii = nbody `B.shiftL` 2
                          !oo = nbody * 3
                      c0 <- peekElemOff sp  ii
                      c1 <- peekElemOff sp (ii + 1)
                      v0 <- peekElemOff tp (fi c0)
                      v1 <- peekElemOff tp (fi c1)
                      let !tail_acc = v0 .|. v1
                      if tail_acc .&. 0x80 /= 0 || v1 .&. 0x0F /= 0
                        then pure False
                        else do
                          let !b0 = (v0 `B.shiftL` 2)
                                .|. ((v1 `B.shiftR` 4) .&. 0x03)
                          pokeElemOff dst oo b0
                          pure True
              pure $! if ok then Just (BI.PS fp 0 outlen) else Nothing
