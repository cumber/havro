{-# LANGUAGE FlexibleInstances
           , OverloadedStrings
           , TupleSections
           , TypeSynonymInstances
  #-}

module Avro.Schema
  ( Schema
  , avroNull
  , avroBool
  , avroInt
  , avroLong
  , avroString

  , Encoder
  , encode
  )
where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))

import Data.Attoparsec.ByteString.Lazy (Parser, anyWord8)
import qualified Data.Attoparsec.ByteString.Lazy as APS

import Data.Bool (bool)
import Data.Bits ((.&.), FiniteBits, setBit, shiftL, shiftR, testBit)
import Data.Int (Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)

import Data.Monoid ((<>), mappend)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, word8, byteString)

import ZigZagCoding (zigZagEncode, zigZagDecode)


class Schema t
  where avroNull :: t ()
        avroBool :: t Bool
        avroInt :: t Int32
        avroLong :: t Int64
        avroString :: t ByteString


instance Schema Parser
  where avroNull = return ()
        avroBool = (/= 0) <$> anyWord8
        avroInt = zigZagDecode . decodeVarWord <$> getVarWordBytes
        avroLong = zigZagDecode . decodeVarWord <$> getVarWordBytes
        avroString = APS.take . fromIntegral =<< avroLong


newtype Encoder t = Encoder { encode :: t -> Builder }

instance Schema Encoder
  where avroNull = Encoder $ const ""
        avroBool = Encoder $ bool "\0" "\1"
        avroInt = Encoder $ encodeVarWord . zigZagEncode
        avroLong = Encoder $ encodeVarWord . zigZagEncode
        avroString
          = Encoder $   uncurry mappend
                      . (   encode avroLong . fromIntegral . BS.length
                        &&& byteString
                        )



class (FiniteBits a, Integral a) => VarWord a
  where encodeVarWord :: a -> Builder
        encodeVarWord x
          = let low7 = fromIntegral $ x .&. 0x7f
                rest = x `shiftR` 7
            in  if rest == 0
                  then  word8 low7
                  else  word8 (setBit low7 7) <> encodeVarWord rest

        decodeVarWord :: ByteString -> a
        decodeVarWord = BS.foldr' f 0
          where f b x = x `shiftL` 7 + fromIntegral (b .&. 0x7f)


instance VarWord Word
instance VarWord Word8
instance VarWord Word16
instance VarWord Word32
instance VarWord Word64


getVarWordBytes :: Parser ByteString
getVarWordBytes
  = APS.scan True
      $ \s b -> if s
                  then  Just $ testBit b 7
                  else  Nothing
