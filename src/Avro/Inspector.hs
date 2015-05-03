{-# LANGUAGE DataKinds
           , GADTs
           , KindSignatures
  #-}

module Avro.Inspector
  ( Inspector
      ( ANull
      , ABool
      , AInt
      , ALong
      , AFloat
      , ADouble
      , ABytes
      , AString

      , ARecord
      , AEnum
      , AArray
      , AMap
      , AFixed
      )
  )
where

import Data.ByteString (ByteString)

import Data.Int (Int32, Int64)

import Data.Map (Map)

import Data.Text (Text)

import Data.Vinyl (Rec, HList)

import Avro.Records
  ( Field
  , RecordDesc
  )

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


data Inspector a
  where ANull :: Inspector ()
        ABool :: Inspector Bool
        AInt :: Inspector Int32
        ALong :: Inspector Int64
        AFloat :: Inspector Float
        ADouble :: Inspector Double
        ABytes :: Inspector ByteString
        AString :: Inspector Text

        ARecord :: RecordDesc -> Rec (Field Inspector) as -> Inspector (HList as)
        AEnum :: Enum a => Inspector a
        AArray :: Inspector a -> Inspector [a]
        AMap :: Inspector a -> Inspector (Map Text a)
        AFixed :: Int32 -> Inspector ByteString


instance Schema Inspector
  where avroNull = ANull
        avroBool = ABool
        avroInt = AInt
        avroLong = ALong
        avroFloat = AFloat
        avroDouble = ADouble
        avroBytes = ABytes
        avroString = AString

        avroRecord = ARecord
        avroEnum = AEnum
        avroArray = AArray
        avroMap = AMap
        avroFixed = AFixed
