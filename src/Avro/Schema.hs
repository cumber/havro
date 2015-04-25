{-# LANGUAGE BangPatterns
           , DataKinds
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , GADTs
           , KindSignatures
           , MultiParamTypeClasses
           , OverloadedStrings
           , QuasiQuotes
           , TemplateHaskell
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

  , FieldDesc
      ( FieldDesc
      , fieldName
      , fieldDoc
      , fieldDefaultValue
      , fieldOrder
      , fieldAliases
      , fieldAttrs
      )
  , Order (Ascending, Descending, Ignore)
  , Field (Field, fieldDesc, fieldSchema)
  , RecordDesc
      ( RecordDesc
      , recordName
      , recordDoc
      , recordAliases
      )
  , field
  , record
  , name
  , doc
  , defaultValue
  , order
  , aliases
  , attrs
  , desc
  , schema
  , withDoc
  , withDefault
  , withOrder
  , withAlias
  , withAttr
  , (|::)
  , (|--)
  )
where

import Control.Arrow ((&&&), (***))

import Control.Lens ((&), (.~), (%~))
import Control.Lens.TH (makeLensesWith, abbreviatedFields)

import qualified Data.Attoparsec.ByteString.Lazy as APS

import Data.Bool (bool)
import Data.Bits ((.&.), FiniteBits, setBit, shiftL, shiftR, testBit)
import Data.Int (Int32, Int64)
import Data.List (foldl', genericReplicate)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Binary.Get (runGet)
import Data.Binary.IEEE754 (getFloat32le, getFloat64le)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (Builder, byteString, doubleLE, floatLE, word8)

import Data.Monoid ((<>))

import Data.Functor.Contravariant ((>$<), Contravariant(contramap))
import Data.Functor.Contravariant.Divisible (Divisible(divide, conquer))

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)

import Data.Vinyl (Rec((:&), RNil), HList, rmap, rtraverse)
import Data.Vinyl.Functor (Identity(Identity))

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

        avroRecord :: RecordDesc -> Rec (Field s) as -> s (HList as)
        avroArray :: s a -> s [a]
        avroFixed :: Int32 -> s ByteString


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

        avroRecord _ = rtraverse (fmap Identity . fieldSchema)

        avroArray itemSchema
          = do  count <- avroLong
                let arrayBlock = flip genericCount itemSchema . abs
                if count /= 0
                  then  (++) <$> arrayBlock count <*> avroArray itemSchema
                  else  return []

        avroFixed = APS.take . fromIntegral


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

        avroRecord _ = recEncoder . rmap fieldSchema

        avroArray itemSchema = Encoder $ \items
         -> let encodeInc (!n, !b) x = (n + 1, b <> encode itemSchema x)
                (count, itemData) = foldl' encodeInc (0, mempty) items
            in  if count == 0
                  then  word8 0
                  else  encode avroLong count <> itemData <> word8 0

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


recEncoder :: Rec Encoder as -> Encoder (HList as)
recEncoder RNil = Encoder $ const ""
recEncoder (e :& es)
  = Encoder $ \(Identity x :& xs) -> encode e x <> encode (recEncoder es) xs


data FieldDesc a
  = FieldDesc
      { fieldName :: ByteString
      , fieldDoc :: Maybe ByteString
      , fieldDefaultValue :: Maybe a
      , fieldOrder :: Order
      , fieldAliases :: [ByteString]
      , fieldAttrs :: Map ByteString ByteString
      }
  deriving  (Eq, Show)


data Order = Ascending | Descending | Ignore
  deriving  (Eq, Show)


data Field s a
  = Field
      { fieldDesc :: FieldDesc a
      , fieldSchema :: s a
      }


field :: ByteString -> FieldDesc a
field name
  = FieldDesc
      { fieldName = name
      , fieldDoc = Nothing
      , fieldDefaultValue = Nothing
      , fieldOrder = Ascending
      , fieldAliases = []
      , fieldAttrs = Map.empty
      }

data RecordDesc
  = RecordDesc
      { recordName :: ByteString
      , recordDoc :: Maybe ByteString
      , recordAliases :: [ByteString]
      }
  deriving  (Eq, Show)


record :: ByteString -> RecordDesc
record name
  = RecordDesc
      { recordName = name
      , recordDoc = Nothing
      , recordAliases = []
      }


(|::) :: Schema s => FieldDesc a -> s a -> Rec (Field s) '[a]
desc |:: sa = Field desc sa :& RNil
infixr 6 |::


(|--) :: Schema s => RecordDesc -> Rec (Field s) as -> s (HList as)
desc |-- fields = avroRecord desc fields
infixl 1 |--


$(makeLensesWith abbreviatedFields ''FieldDesc)
$(makeLensesWith abbreviatedFields ''Field)
$(makeLensesWith abbreviatedFields ''RecordDesc)


withDoc :: HasDoc r ByteString => r -> ByteString -> r
f `withDoc` d = f & doc .~ d

withDefault :: HasDefaultValue r (Maybe a) => r -> a -> r
f `withDefault` d = f & defaultValue .~ Just d

withOrder :: HasOrder r Order => r -> Order -> r
f `withOrder` o = f & order .~ o

withAlias :: HasAliases r [a] => r -> a -> r
f `withAlias` a = f & aliases %~ (a:)

withAttr :: HasAttrs r (Map ByteString ByteString) => r -> (ByteString, ByteString) -> r
f `withAttr` (k, v) = f & attrs %~ Map.insert k v
