{-# LANGUAGE BangPatterns
           , DataKinds
           , FlexibleInstances
           , GADTs
           , KindSignatures
           , OverloadedStrings
           , QuasiQuotes
           , TupleSections
           , TypeFamilies
           , TypeOperators
  #-}

module Avro.Schema
  ( Schema
      ( avroNull
      , avroBool
      , avroInt
      , avroFloat
      , avroDouble
      , avroLong
      , avroBytes
      , avroString

      , avroArray
      )

  , Encoder (Encoder, encode)
  )
where

import Control.Arrow ((&&&), (***))

import qualified Data.Attoparsec.ByteString.Lazy as APS

import Data.Bool (bool)
import Data.Bits ((.&.), FiniteBits, setBit, shiftL, shiftR, testBit)
import Data.Int (Int32, Int64)
import Data.List (foldl', genericReplicate)

import Data.HList (HList)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Monoid ((<>))

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (Builder, byteString, doubleLE, floatLE, word8)

import Data.Binary.Get (runGet)
import Data.Binary.IEEE754 (getFloat32le, getFloat64le)

import Data.Functor.Contravariant ((>$<), Contravariant(contramap))
import Data.Functor.Contravariant.Divisible (Divisible(divide, conquer))

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)

import ZigZagCoding (zigZagEncode, zigZagDecode)

import Data.Functor.Polyvariant
  ( Polyvariant(VarianceOf)
  , Variance(Covariance, Contravariance)
  )


{-| Class of type constructors that can be used to interpret Avro schemas.

    Given a @schema :: 'Schema' s => s a@, @a@ is a Haskell type corresponding
    to the Avro type described by @schema@.
-}
class Schema (s :: * -> *)
  where avroNull :: s ()
        avroBool :: s Bool
        avroInt :: s Int32
        avroLong :: s Int64
        avroFloat :: s Float
        avroDouble :: s Double
        avroBytes :: s ByteString
        avroString :: s Text

        avroArray :: s a -> s [a]

        avroRecord
         :: ByteString -> Maybe ByteString -> [ByteString] -> Fields s as
            -> s (HList as)


-- |    Attoparsec parsers can parse according to an Avro schema.
instance Schema APS.Parser
  where avroNull = return ()
        avroBool = (/= 0) <$> APS.anyWord8
        avroInt = zigZagDecode . decodeVarWord <$> getVarWordBytes
        avroLong = zigZagDecode . decodeVarWord <$> getVarWordBytes
        avroFloat = runGet getFloat32le . LBS.fromStrict <$> APS.take 4
        avroDouble = runGet getFloat64le . LBS.fromStrict <$> APS.take 8
        avroBytes = APS.take . fromIntegral =<< avroLong
        avroString = either (fail . show) return . decodeUtf8' =<< avroBytes

        avroArray itemSchema
          = do  count <- avroLong
                let arrayBlock = flip genericCount itemSchema . abs
                if count /= 0
                  then  (++) <$> arrayBlock count <*> avroArray itemSchema
                  else  return []


-- |    Basically (-> Builder) if we had type-level operator sections.
newtype Encoder t = Encoder { encode :: t -> Builder }

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

instance Polyvariant APS.Parser
  where type VarianceOf APS.Parser = 'Covariance


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

        avroArray itemSchema = Encoder $ \items
         -> let encodeInc (!n, !b) x = (n + 1, b <> encode itemSchema x)
                (count, itemData) = foldl' encodeInc (0, mempty) items
            in  if count == 0
                  then  word8 0
                  else  encode avroLong count <> itemData <> word8 0


encodeVarWord :: (FiniteBits a, Integral a) => a -> Builder
encodeVarWord x
  = let low7 = fromIntegral $ x .&. 0x7f
        rest = x `shiftR` 7
    in  if rest == 0
          then  word8 low7
          else  word8 (setBit low7 7) <> encodeVarWord rest

decodeVarWord :: (FiniteBits a, Integral a) => ByteString -> a
decodeVarWord = BS.foldr' f 0
  where f b x = x `shiftL` 7 + fromIntegral (b .&. 0x7f)


getVarWordBytes :: APS.Parser ByteString
getVarWordBytes
  = APS.scan True
      $ \s b -> if s
                  then  Just $ testBit b 7
                  else  Nothing


genericCount :: (Monad m, Integral i) => i -> m a -> m [a]
genericCount n p = sequence (genericReplicate n p)


data Field (s :: * -> *) a
  = Field
      { fieldName :: ByteString
      , fieldType :: s a
      , fieldDefault :: Maybe a
      , fieldAttrs :: Map ByteString ByteString
      }


data Fields (s :: * -> *) (as :: [*])
  where Stop :: Fields s '[]
        Go :: Field s a -> Fields s as -> Fields s (a ': as)
