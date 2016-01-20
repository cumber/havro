{-# LANGUAGE DataKinds
           , GADTs
           , KindSignatures
           , PolyKinds
           , RankNTypes
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

      , avroRecord
      , avroEnum
      , avroArray
      , avroMap
      , avroFixed
      )

  , (|::)
  , (|--)
  )
where

import Data.Int (Int32, Int64)

import Data.Map (Map)

import Data.ByteString (ByteString)

import Data.Text (Text)

import Data.Vinyl (Rec((:&), RNil), HList)

import Avro.Records
  ( FieldDesc
  , Field(Field)
  , RecordDesc
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

        -- NOTE: this type is not safe, since the data file may contain an index
        -- that doesn't correspond to any element, and the result when parsing is
        -- an exception rather than a parse failure.
        avroEnum :: Enum a => s a

        avroArray :: s a -> s [a]
        avroMap :: s a -> s (Map Text a)
        avroFixed :: Int32 -> s ByteString


(|::) :: Schema s => FieldDesc a -> s a -> Rec (Field s) '[a]
desc |:: sa = Field desc sa :& RNil
infixr 6 |::


(|--) :: Schema s => RecordDesc -> Rec (Field s) as -> s (HList as)
desc |-- fields = avroRecord desc fields
infixl 1 |--
