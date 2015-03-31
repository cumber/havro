{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
  #-}

module ZigZagCoding
  ( zigZagEncode
  , zigZagDecode
  , SignedUnsigned
      ( toUnsigned
      , toSigned
      )
  )
where

import Data.Bits ((.&.), FiniteBits, finiteBitSize, shiftL, shiftR, xor)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)


-- |    Relates signed and unsigned integer types of the same bit-width.
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


{-| Encode a signed integer as a same-size unsigned integer, using "zig-zag"
    coding. This ensures that small-magnitute numbers (whether negative or
    positive) have a small-magitude encoding, rather than all negative numbers
    having a very large coding as an unsigned word.

    See <https://developers.google.com/protocol-buffers/docs/encoding?csw=1#types>
-}
zigZagEncode :: SignedUnsigned a b => a -> b
zigZagEncode x = toUnsigned $ (x `shiftL` 1) `xor` (x `shiftR` (finiteBitSize x - 1))


-- |    Decodes signed numbers encoded using 'zigZagEncode'
zigZagDecode :: SignedUnsigned a b => b -> a
zigZagDecode x = toSigned $ (x `shiftR` 1) `xor` negate (x .&. 1)
