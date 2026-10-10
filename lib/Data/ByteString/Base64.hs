{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module: Data.ByteString.Base64
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Base64 encoding and decoding of strict bytestrings.

module Data.ByteString.Base64 (
    encode
  , decode
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.Arm as Arm
import qualified Data.ByteString.Base64.Pure as Pure

-- | Encode a base256 'ByteString' as base64.
--
--   Uses ARM NEON extensions when available, otherwise a pure
--   Haskell scalar loop.
--
--   >>> encode "hello world"
--   "aGVsbG8gd29ybGQ="
encode :: BS.ByteString -> BS.ByteString
encode bs
  | Arm.base64_arm_available = Arm.encode bs
  | otherwise                = Pure.encode bs
{-# INLINABLE encode #-}

-- | Decode a base64 'ByteString' to base256.
--
--   Uses ARM NEON extensions when available, otherwise a pure
--   Haskell scalar loop.  Invalid inputs (including incorrectly-
--   padded or non-canonical inputs) will produce 'Nothing'.
--
--   >>> decode "aGVsbG8gd29ybGQ="
--   Just "hello world"
--   >>> decode "aGVsbG8gd29ybGQ" -- missing padding
--   Nothing
decode :: BS.ByteString -> Maybe BS.ByteString
decode bs
  | Arm.base64_arm_available = Arm.decode bs
  | otherwise                = Pure.decode bs
{-# INLINABLE decode #-}
