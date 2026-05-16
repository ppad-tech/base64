{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Data.ByteString.Base64.Arm
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- ARM NEON support for base64 encoding and decoding.

module Data.ByteString.Base64.Arm (
    base64_arm_available
  , encode
  , decode
  ) where

import qualified Data.Bits as B
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import Data.Word (Word8)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekElemOff)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- ffi ------------------------------------------------------------------------

foreign import ccall unsafe "base64_encode_arm"
  c_base64_encode :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()

foreign import ccall unsafe "base64_decode_arm"
  c_base64_decode :: Ptr Word8 -> Ptr Word8 -> CSize -> CSize -> IO CInt

foreign import ccall unsafe "base64_arm_available"
  c_base64_arm_available :: IO CInt

-- utilities ------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- api ------------------------------------------------------------------------

-- | Are ARM NEON extensions available?
base64_arm_available :: Bool
base64_arm_available =
  unsafeDupablePerformIO c_base64_arm_available /= 0
{-# NOINLINE base64_arm_available #-}

-- | Encode a base256 'ByteString' as base64 using NEON.
encode :: BS.ByteString -> BS.ByteString
encode (BI.PS sfp soff l) =
  BI.unsafeCreate ((l + 2) `quot` 3 * 4) $ \dst ->
    withForeignPtr sfp $ \sp0 ->
      c_base64_encode (sp0 `plusPtr` soff) dst (fi l)

-- | Decode a base64 'ByteString' to base256 using NEON.  Returns
--   'Nothing' on malformed input.
decode :: BS.ByteString -> Maybe BS.ByteString
decode (BI.PS sfp soff l)
  | l == 0          = Just BS.empty
  | l .&. 0x03 /= 0 = Nothing
  | otherwise = unsafeDupablePerformIO $
      withForeignPtr sfp $ \sp0 -> do
        let !sp = sp0 `plusPtr` soff :: Ptr Word8
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
                !outlen = nfull * 3 - pad
            fp <- BI.mallocByteString outlen
            ok <- withForeignPtr fp $ \dst ->
              c_base64_decode sp dst (fi l) (fi outlen)
            pure $! if ok /= 0
              then Just (BI.PS fp 0 outlen)
              else Nothing
