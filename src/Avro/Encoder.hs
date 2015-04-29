{-# LANGUAGE BangPatterns
           , DataKinds
           , OverloadedStrings
           , TypeFamilies
  #-}

module Avro.Encoder
  ( Encoder (Encoder, encode)
  )
where

import Control.Arrow ((&&&), (***))

import Data.Bool (bool)
import Data.Bits ((.&.), FiniteBits, setBit, shiftR)
import Data.List (foldl')

import qualified Data.Map as Map

import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, byteString, doubleLE, floatLE, word8)

import Data.Monoid ((<>))

import Data.Functor.Contravariant ((>$<), Contravariant(contramap))
import Data.Functor.Contravariant.Divisible
  ( Divisible(divide, conquer)
  , divided
  )

import Data.Text.Encoding (encodeUtf8)

import Data.Vinyl (Rec((:&), RNil), HList, rmap)
import Data.Vinyl.Functor (Identity(Identity))


import Avro.Records (Field(fieldSchema))
import Avro.Schema
  ( Schema
      ( avroNull
      , avroBool
      , avroInt
      , avroFloat
      , avroDouble
      , avroLong
      , avroBytes
      , avroString

      , avroRecord
      , avroEnum
      , avroArray
      , avroMap
      , avroFixed
      )
  )

import ZigZagCoding (zigZagEncode)

import Data.Functor.Polyvariant
  ( Polyvariant(VarianceOf)
  , Variance(Contravariance)
  )


-- |    Basically (-> Builder) if we had type-level operator sections.
newtype Encoder a = Encoder { encode :: a -> Builder }

instance Contravariant Encoder
  where contramap f = Encoder . ( . f) . encode

instance Divisible Encoder
  where conquer = Encoder $ const mempty
        divide f n m
          = Encoder $   uncurry mappend
                      . (encode n *** encode m)
                      . f

instance Polyvariant Encoder
  where type VarianceOf Encoder = 'Contravariance


-- |    An 'Encoder' serialises Haskell values according to an Avro schema.
instance Schema Encoder
  where avroNull = Encoder $ const ""
        avroBool = Encoder $ bool "\0" "\1"
        avroInt = Encoder $ encodeVarWord . zigZagEncode
        avroLong = Encoder $ encodeVarWord . zigZagEncode
        avroFloat = Encoder floatLE
        avroDouble = Encoder doubleLE
        avroBytes
          = Encoder $   uncurry mappend
                      . (   encode avroLong . fromIntegral . BS.length
                        &&& byteString
                        )
        avroString = encodeUtf8 >$< avroBytes

        avroRecord _ = recEncoder . rmap fieldSchema

        avroEnum = fromIntegral . fromEnum >$< avroInt

        avroArray itemSchema = Encoder $ \items
         -> let encodeInc (!n, !b) x = (n + 1, b <> encode itemSchema x)
                (count, itemData) = foldl' encodeInc (0, mempty) items
            in  if count == 0
                  then  word8 0
                  else  encode avroLong count <> itemData <> word8 0

        avroMap
          = contramap Map.toList . avroArray . divided avroString

        avroFixed n
          = Encoder
              $ byteString
                  . \bs ->  let l = BS.length bs
                                n' = fromIntegral n
                            in  if  l == n'
                                  then  bs
                                  else  bs <> BS.replicate (n' - l) 0x0


encodeVarWord :: (FiniteBits a, Integral a) => a -> Builder
encodeVarWord x
  = let low7 = fromIntegral $ x .&. 0x7f
        rest = x `shiftR` 7
    in  if rest == 0
          then  word8 low7
          else  word8 (setBit low7 7) <> encodeVarWord rest


recEncoder :: Rec Encoder as -> Encoder (HList as)
recEncoder RNil = Encoder $ const ""
recEncoder (e :& es)
  = Encoder $ \(Identity x :& xs) -> encode e x <> encode (recEncoder es) xs
