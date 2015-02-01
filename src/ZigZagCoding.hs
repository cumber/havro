{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
  #-}

module ZigZagCoding
  ( zigZagEncode
  , zigZagDecode
  , toUnsigned
  , toSigned
  )
where

import Data.Bits ((.&.), FiniteBits, finiteBitSize, shiftL, shiftR, xor)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)


class (Integral s, FiniteBits s, Integral u, FiniteBits u)
   => SignedUnsigned s u | s -> u, u -> s
  where toUnsigned :: s -> u
        toUnsigned = fromIntegral

        toSigned :: u -> s
        toSigned = fromIntegral

instance SignedUnsigned Int Word
instance SignedUnsigned Int8 Word8
instance SignedUnsigned Int16 Word16
instance SignedUnsigned Int32 Word32
instance SignedUnsigned Int64 Word64


zigZagEncode :: SignedUnsigned a b => a -> b
zigZagEncode x = toUnsigned $ (x `shiftL` 1) `xor` (x `shiftR` (finiteBitSize x - 1))

zigZagDecode :: SignedUnsigned a b => b -> a
zigZagDecode x = toSigned $ (x `shiftR` 1) `xor` (negate $ x .&. 1)
--  = let mask = ((bit 0 .&. x) `rotateR` 1) `shiftR` (finiteBitSize x - 1)
--    in  (x `xor` mask) `shiftR` 1


